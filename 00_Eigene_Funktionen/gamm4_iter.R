gamm4 <- function (formula, random = NULL, family = gaussian(), data = list(), 
          weights = NULL, subset = NULL, na.action, knots = NULL, drop.unused.levels = TRUE, 
          ...) 
{
    if (packageVersion("lme4") < package_version("0.999999-999")) {
        old.lme4 <- TRUE
    }
    else old.lme4 <- FALSE
    if (!is.null(random)) {
        if (!inherits(random, "formula")) 
            stop("gamm4 requires `random' to be a formula")
        random.vars <- all.vars(random)
    }
    else random.vars <- NULL
    gp <- interpret.gam(formula)
    mf <- match.call(expand.dots = FALSE)
    mf$formula <- gp$fake.formula
    mf$family <- mf$scale <- mf$knots <- mf$random <- mf$... <- NULL
    mf$drop.unused.levels <- drop.unused.levels
    mf[[1]] <- as.name("model.frame")
    pmf <- mf
    gmf <- eval(mf, parent.frame())
    gam.terms <- attr(gmf, "terms")
    if (length(random.vars)) {
        mf$formula <- as.formula(paste(paste(deparse(gp$fake.formula, 
                                                     backtick = TRUE), collapse = ""), "+", paste(random.vars, 
                                                                                                  collapse = "+")))
        mf <- eval(mf, parent.frame())
    }
    else mf <- gmf
    rm(gmf)
    if (nrow(mf) < 2) 
        stop("Not enough (non-NA) data to do anything meaningful")
    Terms <- attr(mf, "terms")
    vars <- all.vars(gp$fake.formula[-2])
    inp <- parse(text = paste("list(", paste(vars, collapse = ","), 
                              ")"))
    dl <- eval(inp, data, parent.frame())
    names(dl) <- vars
    var.summary <- mgcv:::variable.summary(gp$pf, dl, nrow(mf))
    mvars <- vars[!vars %in% names(mf)]
    if (length(mvars) > 0) 
        for (i in 1:length(mvars)) mf[[mvars[i]]] <- dl[[mvars[i]]]
    rm(dl)
    pmf$formula <- gp$pf
    pmf <- eval(pmf, parent.frame())
    pTerms <- attr(pmf, "terms")
    if (is.character(family)) 
        family <- eval(parse(text = family))
    if (is.function(family)) 
        family <- family()
    if (is.null(family$family)) 
        stop("family not recognized")
    if (family$family == "gaussian" && family$link == "identity") 
        linear <- TRUE
    else linear <- FALSE
    G <- gamm4:::gamm4.setup(gp, pterms = pTerms, data = mf, knots = knots)
    G$var.summary <- var.summary
    n.sr <- length(G$random)
    if (is.null(random) && n.sr == 0) 
        stop("gamm4 models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect")
    offset.name <- attr(mf, "names")[attr(attr(mf, "terms"), 
                                          "offset")]
    yname <- new.name("y", names(mf))
    eval(parse(text = paste("mf$", yname, "<-G$y", sep = "")))
    Xname <- new.name("X", names(mf))
    eval(parse(text = paste("mf$", Xname, "<-G$X", sep = "")))
    lme4.formula <- paste(yname, "~", Xname, "-1")
    if (length(offset.name)) {
        lme4.formula <- paste(lme4.formula, "+", offset.name)
    }
    r.name <- names(G$random)
    if (n.sr) 
        for (i in 1:n.sr) {
            mf[[r.name[i]]] <- factor(rep(1:ncol(G$random[[i]]), 
                                          length = nrow(G$random[[i]])))
            lme4.formula <- paste(lme4.formula, "+ (1|", r.name[i], 
                                  ")")
        }
    if (!is.null(random)) {
        lme4.formula <- paste(lme4.formula, "+", substring(deparse(random), 
                                                           first = 2))
    }
    lme4.formula <- as.formula(lme4.formula)
    if (old.lme4) 
        return(gamm4.oldwork(G, mf, n.sr, r.name, family, formula, 
                             gam.terms, G$pterms, lme4.formula, linear))
    b <- if (linear) 
        lFormula(lme4.formula, data = mf, weights = G$w, ...)
    else glFormula(lme4.formula, data = mf, family = family, 
                   weights = G$w, ...)
    if (n.sr) {
        tn <- names(b$reTrms$cnms)
        ind <- 1:length(tn)
        sn <- names(G$random)
        for (i in 1:n.sr) {
            k <- ind[sn[i] == tn]
            ii <- (b$reTrms$Gp[k] + 1):b$reTrms$Gp[k + 1]
            b$reTrms$Zt[ii, ] <- as(t(G$random[[i]]), "dgCMatrix")
            b$reTrms$cnms[[k]] <- attr(G$random[[i]], "s.label")
        }
    }
    ret <- list()
    if (linear) {
        devfun <- do.call(mkLmerDevfun, b)
        opt <- optimizeLmer(devfun, optimizer = "bobyqa")
        ret$mer <- mkMerMod(environment(devfun), opt, b$reTrms, 
                            fr = b$fr)
    }
    else {
        devfun <- do.call(mkGlmerDevfun, b)
        opt <- optimizeGlmer(devfun, optimizer = "bobyqa", control = list(maxfun = 1000000))
        devfun <- updateGlmerDevfun(devfun, b$reTrms)
        opt <- optimizeGlmer(devfun, stage = 2, optimizer = "bobyqa", control = list(maxfun = 1000000))
        ret$mer <- mkMerMod(environment(devfun), opt, b$reTrms, 
                            fr = b$fr, ...)
    }
    rm(b)
    object <- list(model = mf, formula = formula, smooth = G$smooth, 
                   nsdf = G$nsdf, family = family, df.null = nrow(G$X), 
                   y = getME(ret$mer, "y"), terms = gam.terms, pterms = G$pterms, 
                   xlevels = G$xlevels, contrasts = G$contrasts, assign = G$assign, 
                   na.action = attr(mf, "na.action"), cmX = G$cmX, var.summary = G$var.summary)
    pvars <- all.vars(delete.response(object$terms))
    object$pred.formula <- if (length(pvars) > 0) 
        reformulate(pvars)
    else NULL
    B <- Matrix(0, ncol(G$Xf), ncol(G$Xf))
    diag(B) <- 1
    Xfp <- G$Xf
    bf <- as.numeric(lme4::fixef(ret$mer))
    br <- lme4::ranef(ret$mer)
    if (G$nsdf) 
        p <- bf[1:G$nsdf]
    else p <- array(0, 0)
    if (G$m > 0) 
        for (i in 1:G$m) {
            fx <- G$smooth[[i]]$fixed
            first <- G$smooth[[i]]$first.f.para
            last <- G$smooth[[i]]$last.f.para
            if (first <= last) 
                beta <- bf[first:last]
            else beta <- array(0, 0)
            if (fx) 
                b <- beta
            else {
                b <- rep(0, 0)
                for (k in 1:length(G$smooth[[i]]$lmer.name)) b <- c(b, 
                                                                    as.numeric(br[[G$smooth[[i]]$lmer.name[k]]][[1]]))
                b <- b[G$smooth[[i]]$rind]
                b <- c(b, beta)
                b <- G$smooth[[i]]$trans.D * b
                if (!is.null(G$smooth[[i]]$trans.U)) 
                    b <- G$smooth[[i]]$trans.U %*% b
            }
            p <- c(p, b)
            ind <- G$smooth[[i]]$first.para:G$smooth[[i]]$last.para
            if (!fx) {
                D <- G$smooth[[i]]$trans.D
                if (is.null(G$smooth[[i]]$trans.U)) 
                    B[ind, ind] <- Diagonal(length(D), D)
                else B[ind, ind] <- t(D * t(G$smooth[[i]]$trans.U))
            }
            Xfp[, ind] <- G$Xf[, ind, drop = FALSE] %*% B[ind, 
                                                          ind, drop = FALSE]
        }
    object$coefficients <- p
    vr <- lme4::VarCorr(ret$mer)
    scale <- as.numeric(attr(vr, "sc"))^2
    if (!is.finite(scale) || scale == 1) {
        scale <- 1
        object$scale.estimated <- FALSE
    }
    else object$scale.estimated <- TRUE
    sp <- rep(-1, n.sr)
    Zt <- Matrix(0, 0, ncol(getME(ret$mer, "Zt")))
    if (n.sr == 0) 
        sn <- NULL
    rn <- names(vr)
    ind <- rep(0, 0)
    for (i in 1:length(vr)) {
        if (is.null(sn) || !rn[i] %in% sn) {
            Gp <- getME(ret$mer, "Gp")
            ind <- c(ind, (Gp[i] + 1):Gp[i + 1])
        }
        else if (!is.null(sn)) {
            k <- (1:n.sr)[rn[i] == sn]
            if (as.numeric(vr[[i]] > 0)) 
                sp[k] <- scale/as.numeric(vr[[i]])
            else sp[k] <- 1e+10
        }
    }
    if (length(ind)) {
        Zt <- getME(ret$mer, "Zt")[ind, ]
        root.phi <- getME(ret$mer, "Lambdat")[ind, ind]
    }
    object$prior.weights <- G$w
    if (linear) {
        object$weights <- object$prior.weights
        V <- Diagonal(n = length(object$weights), x = scale/object$weights)
    }
    else {
        object$weights <- ret$mer@resp$sqrtWrkWt()^2
        V <- Diagonal(x = 1/object$weights) * scale
    }
    if (nrow(Zt) > 0) 
        V <- V + crossprod(root.phi %*% Zt) * scale
    R <- Matrix::chol(V, pivot = TRUE)
    piv <- attr(R, "pivot")
    G$Xf <- as(G$Xf, "dgCMatrix")
    Xfp <- as(Xfp, "dgCMatrix")
    if (is.null(piv)) {
        WX <- as(solve(t(R), Xfp), "matrix")
        XVX <- as(solve(t(R), G$Xf), "matrix")
    }
    else {
        WX <- as(solve(t(R), Xfp[piv, ]), "matrix")
        XVX <- as(solve(t(R), G$Xf[piv, ]), "matrix")
    }
    qrz <- qr(XVX, LAPACK = TRUE)
    object$R <- qr.R(qrz)
    object$R[, qrz$pivot] <- object$R
    XVX <- crossprod(object$R)
    object$sp <- sp
    colx <- ncol(G$Xf)
    Sp <- matrix(0, colx, colx)
    first <- G$nsdf + 1
    k <- 1
    if (G$m > 0) 
        for (i in 1:G$m) {
            if (!object$smooth[[i]]$fixed) {
                ii <- object$smooth[[i]]$first.para:object$smooth[[i]]$last.para
                for (j in 1:length(object$smooth[[i]]$S)) {
                    ind <- ii[object$smooth[[i]]$pen.ind == j]
                    diag(Sp)[ind] <- sqrt(object$sp[k])
                    k <- k + 1
                }
            }
            first <- last + 1
        }
    qrx <- qr(rbind(WX, Sp/sqrt(scale)), LAPACK = TRUE)
    Ri <- backsolve(qr.R(qrx), diag(ncol(WX)))
    ind <- qrx$pivot
    ind[ind] <- 1:length(ind)
    Ri <- Ri[ind, ]
    Vb <- B %*% Ri
    Vb <- Vb %*% t(Vb)
    object$edf <- rowSums(Vb * t(XVX))
    object$df.residual <- length(object$y) - sum(object$edf)
    object$sig2 <- scale
    if (linear) {
        object$method <- "lmer.REML"
    }
    else {
        object$method <- "glmer.ML"
    }
    object$Vp <- as(Vb, "matrix")
    object$Ve <- as(Vb %*% XVX %*% Vb, "matrix")
    class(object) <- "gam"
    if (!is.null(G$original.smooth)) {
        object$smooth <- G$smooth <- G$original.smooth
    }
    if (!is.null(G$P)) {
        object$coefficients <- G$P %*% object$coefficients
        object$Vp <- G$P %*% object$Vp %*% t(G$P)
        object$Ve <- G$P %*% object$Ve %*% t(G$P)
    }
    object$linear.predictors <- predict.gam(object, type = "link")
    object$fitted.values <- object$family$linkinv(object$linear.predictors)
    object$residuals <- residuals(ret$mer)
    if (G$nsdf > 0) 
        term.names <- colnames(G$X)[1:G$nsdf]
    else term.names <- array("", 0)
    n.smooth <- length(G$smooth)
    if (n.smooth) 
        for (i in 1:n.smooth) {
            k <- 1
            for (j in object$smooth[[i]]$first.para:object$smooth[[i]]$last.para) {
                term.names[j] <- paste(object$smooth[[i]]$label, 
                                       ".", as.character(k), sep = "")
                k <- k + 1
            }
        }
    names(object$coefficients) <- term.names
    names(object$edf) <- term.names
    names(object$sp) <- names(G$sp)
    object$gcv.ubre <- if (isREML(ret$mer)) 
        REMLcrit(ret$mer)
    else deviance(ret$mer)
    if (!is.null(G$Xcentre)) 
        object$Xcentre <- G$Xcentre
    ret$gam <- object
    class(gamm4) <- c("gamm4", "list")
    ret
}
