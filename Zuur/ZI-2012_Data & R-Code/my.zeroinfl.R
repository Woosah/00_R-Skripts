my.zeroinfl <- function (formula, data, subset, na.action, weights, offset, 
    dist = c("poisson", "negbin", "geometric"), link = c("logit", 
        "probit", "cloglog", "cauchit", "log"), control = zeroinfl.control(...), 
    model = TRUE, y = TRUE, x = FALSE, fixed.log.theta=NULL, ...) 
{
    ziPoisson <- function(parms) {
        mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
        phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + kz)] + 
            offsetz))
        loglik0 <- log(phi + exp(log(1 - phi) - mu))
        loglik1 <- log(1 - phi) + dpois(Y, lambda = mu, log = TRUE)
        loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
            loglik1[Y1])
        loglik
    }
    ziNegBin <- function(parms) {
        mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
        phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + kz)] + 
            offsetz))

# --- SAA ----------------------------------------------------------------------------
        if(is.null(fixed.log.theta)) {    
          theta <- exp(parms[(kx + kz) + 1])
        } else {
          theta <- exp(fixed.log.theta)
        }  
# --- SAA ----------------------------------------------------------------------------

        loglik0 <- log(phi + exp(log(1 - phi) + suppressWarnings(dnbinom(0, 
            size = theta, mu = mu, log = TRUE))))
        loglik1 <- log(1 - phi) + suppressWarnings(dnbinom(Y, 
            size = theta, mu = mu, log = TRUE))
        loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
            loglik1[Y1])
        loglik
    }
    ziGeom <- function(parms) ziNegBin(c(parms, 0))
    gradPoisson <- function(parms) {
        eta <- as.vector(X %*% parms[1:kx] + offsetx)
        mu <- exp(eta)
        etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
        muz <- linkinv(etaz)
        clogdens0 <- -mu
        dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
            clogdens0)
        wres_count <- ifelse(Y1, Y - mu, -exp(-log(dens0) + log(1 - 
            muz) + clogdens0 + log(mu)))
        wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
            (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
        colSums(cbind(wres_count * weights * X, wres_zero * weights * 
            Z))
    }
    gradGeom <- function(parms) {
        eta <- as.vector(X %*% parms[1:kx] + offsetx)
        mu <- exp(eta)
        etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
        muz <- linkinv(etaz)
        clogdens0 <- dnbinom(0, size = 1, mu = mu, log = TRUE)
        dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
            clogdens0)
        wres_count <- ifelse(Y1, Y - mu * (Y + 1)/(mu + 1), -exp(-log(dens0) + 
            log(1 - muz) + clogdens0 - log(mu + 1) + log(mu)))
        wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
            (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
        colSums(cbind(wres_count * weights * X, wres_zero * weights * 
            Z))
    }
    gradNegBin <- function(parms) {
        eta <- as.vector(X %*% parms[1:kx] + offsetx)
        mu <- exp(eta)
        etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
        muz <- linkinv(etaz)

# --- SAA ----------------------------------------------------------------------------
        if(is.null(fixed.log.theta)) {    
          theta <- exp(parms[(kx + kz) + 1])
        } else {
          theta <- exp(fixed.log.theta)
        }  
# --- SAA ----------------------------------------------------------------------------

        clogdens0 <- dnbinom(0, size = theta, mu = mu, log = TRUE)
        dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
            clogdens0)
        wres_count <- ifelse(Y1, Y - mu * (Y + theta)/(mu + theta), 
            -exp(-log(dens0) + log(1 - muz) + clogdens0 + log(theta) - 
                log(mu + theta) + log(mu)))
        wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
            (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
        wres_theta <- theta * ifelse(Y1, digamma(Y + theta) - 
            digamma(theta) + log(theta) - log(mu + theta) + 1 - 
            (Y + theta)/(mu + theta), exp(-log(dens0) + log(1 - 
            muz) + clogdens0) * (log(theta) - log(mu + theta) + 
            1 - theta/(mu + theta)))

# --- SAA ----------------------------------------------------------------------------
        if(is.null(fixed.log.theta)) {    
          colSums(cbind(wres_count * weights * X, wres_zero * weights * Z, wres_theta))
        } else {
          colSums(cbind(wres_count * weights * X, wres_zero * weights * Z))
        }  
# --- SAA ----------------------------------------------------------------------------

    }
    dist <- match.arg(dist)
    loglikfun <- switch(dist, poisson = ziPoisson, geometric = ziGeom, 
        negbin = ziNegBin)
    gradfun <- switch(dist, poisson = gradPoisson, geometric = gradGeom, 
        negbin = gradNegBin)
    linkstr <- match.arg(link)
    linkobj <- make.link(linkstr)
    linkinv <- linkobj$linkinv
    if (control$trace) 
        cat("Zero-inflated Count Model\n", paste("count model:", 
            dist, "with log link\n"), paste("zero-inflation model: binomial with", 
            linkstr, "link\n"), sep = "")
    cl <- match.call()
    if (missing(data)) 
        data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", 
        "offset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    if (length(formula[[3]]) > 1 && identical(formula[[3]][[1]], 
        as.name("|"))) {
        ff <- formula
        formula[[3]][1] <- call("+")
        mf$formula <- formula
        ffc <- . ~ .
        ffz <- ~.
        ffc[[2]] <- ff[[2]]
        ffc[[3]] <- ff[[3]][[2]]
        ffz[[3]] <- ff[[3]][[3]]
        ffz[[2]] <- NULL
    }
    else {
        ffz <- ffc <- ff <- formula
        ffz[[2]] <- NULL
    }
    if (length(grep("in formula and no", try(terms(ffz), silent = TRUE), 
        fixed = TRUE)) > 0) {
        ffz <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])), 
            deparse(ffz))))
    }
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    mtX <- terms(ffc, data = data)
    X <- model.matrix(mtX, mf)
    mtZ <- terms(ffz, data = data)
    mtZ <- terms(update(mtZ, ~.), data = data)
    Z <- model.matrix(mtZ, mf)
    Y <- model.response(mf, "numeric")
    if (length(Y) < 1) 
        stop("empty model")
    if (all(Y > 0)) 
        stop("invalid dependent variable, minimum count is not zero")
    if (!isTRUE(all.equal(as.vector(Y), as.integer(round(Y + 
        0.001))))) 
        stop("invalid dependent variable, non-integer values")
    Y <- as.integer(round(Y + 0.001))
    if (any(Y < 0)) 
        stop("invalid dependent variable, negative counts")
    if (control$trace) {
        cat("dependent variable:\n")
        tab <- table(factor(Y, levels = 0:max(Y)), exclude = NULL)
        names(dimnames(tab)) <- NULL
        print(tab)
    }
    n <- length(Y)
    kx <- NCOL(X)
    kz <- NCOL(Z)
    Y0 <- Y <= 0
    Y1 <- Y > 0
    weights <- model.weights(mf)
    if (is.null(weights)) 
        weights <- 1
    if (length(weights) == 1) 
        weights <- rep.int(weights, n)
    weights <- as.vector(weights)
    names(weights) <- rownames(mf)
    offsetx <- model_offset_2(mf, terms = mtX, offset = TRUE)
    if (is.null(offsetx)) 
        offsetx <- 0
    if (length(offsetx) == 1) 
        offsetx <- rep.int(offsetx, n)
    offsetx <- as.vector(offsetx)
    offsetz <- model_offset_2(mf, terms = mtZ, offset = FALSE)
    if (is.null(offsetz)) 
        offsetz <- 0
    if (length(offsetz) == 1) 
        offsetz <- rep.int(offsetz, n)
    offsetz <- as.vector(offsetz)
    start <- control$start
    if (!is.null(start)) {
        valid <- TRUE
        if (!("count" %in% names(start))) {
            valid <- FALSE
            warning("invalid starting values, count model coefficients not specified")
            start$count <- rep.int(0, kx)
        }
        if (!("zero" %in% names(start))) {
            valid <- FALSE
            warning("invalid starting values, zero-inflation model coefficients not specified")
            start$zero <- rep.int(0, kz)
        }
        if (length(start$count) != kx) {
            valid <- FALSE
            warning("invalid starting values, wrong number of count model coefficients")
        }
        if (length(start$zero) != kz) {
            valid <- FALSE
            warning("invalid starting values, wrong number of zero-inflation model coefficients")
        }
        if (dist == "negbin") {
            if (!("theta" %in% names(start))) {

# --- SAA ----------------------------------------------------------------------------
              if(is.null(fixed.log.theta)) {    
                  start$theta <- 1
              } else {
                  start$theta <- exp(fixed.log.theta)
              }  
# --- SAA ----------------------------------------------------------------------------

            }    
            start <- list(count = start$count, zero = start$zero, theta = as.vector(start$theta[1]))
        }
        else {
            start <- list(count = start$count, zero = start$zero)
        }
        if (!valid) 
            start <- NULL
    }
    if (is.null(start)) {
        if (control$trace) 
            cat("generating starting values...")
        model_count <- glm.fit(X, Y, family = poisson(), weights = weights, offset = offsetx)
        model_zero <- glm.fit(Z, as.integer(Y0), weights = weights, 
            family = binomial(link = linkstr), offset = offsetz)
        start <- list(count = model_count$coefficients, zero = model_zero$coefficients)
    
# --- SAA ----------------------------------------------------------------------------
        if (dist == "negbin") {
          if(is.null(fixed.log.theta)) {    
            start$theta <- 1
          } else {
            start$theta <- exp(fixed.log.theta)
          }  
        }    
# --- SAA ----------------------------------------------------------------------------
     
        if (control$EM & dist == "poisson") {
            mui <- model_count$fitted
            probi <- model_zero$fitted
            probi <- probi/(probi + (1 - probi) * dpois(0, mui))
            probi[Y1] <- 0
            ll_new <- loglikfun(c(start$count, start$zero))
            ll_old <- 2 * ll_new
            while (abs((ll_old - ll_new)/ll_old) > control$reltol) {
                ll_old <- ll_new
                model_count <- glm.fit(X, Y, weights = weights * 
                  (1 - probi), offset = offsetx, family = poisson(), 
                  start = start$count)
                model_zero <- suppressWarnings(glm.fit(Z, probi, 
                  weights = weights, offset = offsetz, family = binomial(link = linkstr), 
                  start = start$zero))
                mui <- model_count$fitted
                probi <- model_zero$fitted
                probi <- probi/(probi + (1 - probi) * dpois(0, 
                  mui))
                probi[Y1] <- 0
                start <- list(count = model_count$coefficients, 
                  zero = model_zero$coefficients)
                ll_new <- loglikfun(c(start$count, start$zero))
            }
        }
        if (control$EM & dist == "geometric") {
            mui <- model_count$fitted
            probi <- model_zero$fitted
            probi <- probi/(probi + (1 - probi) * dnbinom(0, 
                size = 1, mu = mui))
            probi[Y1] <- 0
            ll_new <- loglikfun(c(start$count, start$zero))
            ll_old <- 2 * ll_new
            if (!require("MASS")) {
                ll_old <- ll_new
                warning("EM estimation of starting values not available")
            }
            while (abs((ll_old - ll_new)/ll_old) > control$reltol) {
                ll_old <- ll_new
                model_count <- suppressWarnings(glm.fit(X, Y, 
                  weights = weights * (1 - probi), offset = offsetx, 
                  family = negative.binomial(1), start = start$count))
                model_zero <- suppressWarnings(glm.fit(Z, probi, 
                  weights = weights, offset = offsetz, family = binomial(link = linkstr), 
                  start = start$zero))
                start <- list(count = model_count$coefficients, 
                  zero = model_zero$coefficients)
                mui <- model_count$fitted
                probi <- model_zero$fitted
                probi <- probi/(probi + (1 - probi) * dnbinom(0, 
                  size = 1, mu = mui))
                probi[Y1] <- 0
                ll_new <- loglikfun(c(start$count, start$zero))
            }
        }
        if (control$EM & dist == "negbin") {
            mui <- model_count$fitted
            probi <- model_zero$fitted
            probi <- probi/(probi + (1 - probi) * dnbinom(0, size = start$theta, mu = mui))
            probi[Y1] <- 0
            ll_new <- loglikfun(c(start$count, start$zero, log(start$theta)))
            ll_old <- 2 * ll_new
            if (!require("MASS")) {
                ll_old <- ll_new
                warning("EM estimation of starting values not available")
            }
            offset <- offsetx
            while (abs((ll_old - ll_new)/ll_old) > control$reltol) {
                ll_old <- ll_new

# --- SAA ----------------------------------------------------------------------------
                if(is.null(fixed.log.theta)) {
                  model_count <- suppressWarnings(my.glm.nb(Y ~ 0 + 
                    X + offset(offset), weights = weights * (1 - 
                    probi), start = start$count, init.theta = start$theta))
                } else {
                  model_count <- suppressWarnings(my.glm.nb(Y ~ 0 + 
                    X + offset(offset), weights = weights * (1 - 
                    probi), start = start$count, init.theta = start$theta),   
                    fixed.theta=exp(fixed.log.theta)) #SAA                    
                }
# --- SAA ----------------------------------------------------------------------------

                model_zero <- suppressWarnings(glm.fit(Z, probi, 
                  weights = weights, offset = offsetz, family = binomial(link = linkstr), 
                  start = start$zero))
                start <- list(count = model_count$coefficients, 
                  zero = model_zero$coefficients, theta = model_count$theta)
                mui <- model_count$fitted
                probi <- model_zero$fitted
                probi <- probi/(probi + (1 - probi) * dnbinom(0, 
                  size = start$theta, mu = mui))
                probi[Y1] <- 0
                ll_new <- loglikfun(c(start$count, start$zero, log(start$theta)))
            }
        }
        if (control$trace) 
            cat("done\n")
    } else {
    
# --- SAA ----------------------------------------------------------------------------
      if(!is.null(fixed.log.theta)) {    
        start$theta <- exp(fixed.log.theta)
      }    
# --- SAA ----------------------------------------------------------------------------
     
    }
    if (control$trace) 
        cat("calling optim() for ML estimation:\n")
    method <- control$method
    hessian <- control$hessian
    ocontrol <- control
    control$method <- control$hessian <- control$EM <- control$start <- NULL

# --- SAA ----------------------------------------------------------------------------
    if(is.null(fixed.log.theta)) {
      fit <- optim(fn = loglikfun, gr = gradfun, par = c(start$count, 
          start$zero, if (dist == "negbin") log(start$theta) else NULL), 
          method = method, hessian = hessian, control = control)
    } else {
      fit <- optim(fn = loglikfun, gr = gradfun, par = c(start$count, start$zero, NULL), 
          method = method, hessian = hessian, control = control)
    }
# --- SAA ----------------------------------------------------------------------------

    if (fit$convergence > 0) 
        warning("optimization failed to converge")
    coefc <- fit$par[1:kx]
    names(coefc) <- names(start$count) <- colnames(X)
    coefz <- fit$par[(kx + 1):(kx + kz)]
    names(coefz) <- names(start$zero) <- colnames(Z)
    
# --- SAA convergence -----------------------------------------------------------------
    foo<-try(vc <- -solve(as.matrix(fit$hessian)))
    if(class(foo) == "try-error") {
      warning("fail to fet Hessian, return diag(1)")
      vc<-diag(1,kx + kz)
    }
# --- SAA ----------------------------------------------------------------------------

    if (dist == "negbin") {
        np <- kx + kz + 1

# --- SAA ----------------------------------------------------------------------------
        if(is.null(fixed.log.theta)) {    
          theta <- as.vector(exp(fit$par[np]))
          SE.logtheta <- as.vector(sqrt(diag(vc)[np]))
        } else {
          theta <- as.vector(exp(fixed.log.theta))
          SE.logtheta <- as.vector(0.1)
        }  
# --- SAA ----------------------------------------------------------------------------

        vc <- vc[-np, -np, drop = FALSE]
    }
    else {
        theta <- NULL
        SE.logtheta <- NULL
    }
    colnames(vc) <- rownames(vc) <- c(paste("count", colnames(X), 
        sep = "_"), paste("zero", colnames(Z), sep = "_"))
    mu <- exp(X %*% coefc + offsetx)[, 1]
    phi <- linkinv(Z %*% coefz + offsetz)[, 1]
    Yhat <- (1 - phi) * mu
    res <- sqrt(weights) * (Y - Yhat)
    nobs <- sum(weights > 0)
    rval <- list(coefficients = list(count = coefc, zero = coefz), 
        residuals = res, fitted.values = Yhat, optim = fit, method = method, 
        control = ocontrol, start = start, weights = if (identical(as.vector(weights), 
            rep.int(1L, n))) NULL else weights, offset = list(count = if (identical(offsetx, 
            rep.int(0, n))) NULL else offsetx, zero = if (identical(offsetz, 
            rep.int(0, n))) NULL else offsetz), n = nobs, df.null = nobs - 
            2, df.residual = nobs - (kx + kz + (dist == "negbin")), 
        terms = list(count = mtX, zero = mtZ, full = mt), theta = theta, 
        SE.logtheta = SE.logtheta, loglik = fit$value, vcov = vc, 
        dist = dist, link = linkstr, linkinv = linkinv, converged = fit$convergence < 
            1, call = cl, formula = ff, levels = .getXlevels(mt, 
            mf), contrasts = list(count = attr(X, "contrasts"), 
            zero = attr(Z, "contrasts")))
    if (model) 
        rval$model <- mf
    if (y) 
        rval$y <- Y
    if (x) 
        rval$x <- list(count = X, zero = Z)
    class(rval) <- "zeroinfl"
    return(rval)
}
environment(my.zeroinfl) <- environment(fun = zeroinfl)

