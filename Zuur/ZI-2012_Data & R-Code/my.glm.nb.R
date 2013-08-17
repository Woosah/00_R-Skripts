my.glm.nb<-function (formula, data, weights, subset, na.action, start = NULL, 
    etastart, mustart, control = glm.control(...), method = "glm.fit", 
    model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, fixed.theta=NULL, ..., 
    init.theta, link = log) 
{
    loglik <- function(n, th, mu, y, w) sum(w * (lgamma(th + 
        y) - lgamma(th) - lgamma(y + 1) + th * log(th) + y * 
        log(mu + (y == 0)) - (th + y) * log(th + mu)))
    link <- substitute(link)

# --- SAA ----------------------------------------------------------------------------
    if(!is.null(fixed.theta)) {
        init.theta <- fixed.theta 
     }
# --- SAA ----------------------------------------------------------------------------

    fam0 <- if (missing(init.theta)) 
        do.call("poisson", list(link = link))
      else 
        do.call("negative.binomial", list(theta = init.theta, link = link))
    dots <- list(...)
    mf <- Call <- match.call()
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "etastart", "mustart", "offset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval.parent(mf)
    Terms <- attr(mf, "terms")
    if (method == "model.frame") 
        return(mf)
    Y <- model.response(mf, "numeric")
    X <- if (!is.empty.model(Terms)) 
        model.matrix(Terms, mf, contrasts)
    else matrix(, NROW(Y), 0)
    w <- model.weights(mf)
    if (!length(w)) 
        w <- rep(1, nrow(mf))
    else if (any(w < 0)) 
        stop("negative weights not allowed")
    offset <- model.offset(mf)
    mustart <- model.extract(mf, "mustart")
    etastart <- model.extract(mf, "etastart")
    n <- length(Y)
    if (!missing(method)) {
        if (!exists(method, mode = "function")) 
            stop("unimplemented method: ", sQuote(method))
        glm.fitter <- get(method)
    }
    else {
        method <- "glm.fit"
        glm.fitter <- stats::glm.fit
    }
    if (control$trace > 1) 
        message("Initial fit:")
    fit <- glm.fitter(x = X, y = Y, w = w, start = start, etastart = etastart, 
        mustart = mustart, offset = offset, family = fam0, control = list(maxit = control$maxit, 
            epsilon = control$epsilon, trace = control$trace > 
                1), intercept = attr(Terms, "intercept") > 0)
    class(fit) <- c("glm", "lm")
    mu <- fit$fitted.values

# --- SAA ----------------------------------------------------------------------------
    if(is.null(fixed.theta)) {
      th <- as.vector(theta.ml(Y, mu, sum(w), w, limit = control$maxit, trace = control$trace > 2))
    } else {
      temp.theta<-fixed.theta
      attr(temp.theta, "SE") <- 0.1
      th <- as.vector(temp.theta)
    }
# --- SAA ----------------------------------------------------------------------------
      
    if (control$trace > 1) 
        message("Initial value for theta:", signif(th))
    fam <- do.call("negative.binomial", list(theta = th, link = link))
    iter <- 0
    d1 <- sqrt(2 * max(1, fit$df.residual))
    d2 <- del <- 1
    g <- fam$linkfun
    Lm <- loglik(n, th, mu, Y, w)
    Lm0 <- Lm + 2 * d1
    while ((iter <- iter + 1) <= control$maxit && (abs(Lm0 - 
        Lm)/d1 + abs(del)/d2) > control$epsilon) {
        eta <- g(mu)
        fit <- glm.fitter(x = X, y = Y, w = w, etastart = eta, 
            offset = offset, family = fam, control = list(maxit = control$maxit, 
                epsilon = control$epsilon, trace = control$trace > 
                  1), intercept = attr(Terms, "intercept") > 
                0)
        t0 <- th
        
# --- SAA ----------------------------------------------------------------------------
        if(is.null(fixed.theta)) {
          th <- theta.ml(Y, mu, sum(w), w, limit = control$maxit, trace = control$trace > 2)
        } else {
          temp.theta<-fixed.theta
          attr(temp.theta, "SE") <- 0.1
          th <- as.vector(temp.theta)
        }
# --- SAA ----------------------------------------------------------------------------        
        
        fam <- do.call("negative.binomial", list(theta = th, link = link))
        mu <- fit$fitted.values
        del <- t0 - th
        Lm0 <- Lm
        Lm <- loglik(n, th, mu, Y, w)
        if (control$trace) {
            Ls <- loglik(n, th, Y, Y, w)
            Dev <- 2 * (Ls - Lm)
            message("Theta(", iter, ") =", signif(th), ", 2(Ls - Lm) =", 
                signif(Dev))
        }
    }
    if (!is.null(attr(th, "warn"))) 
        fit$th.warn <- attr(th, "warn")
    if (iter > control$maxit) {
        warning("alternation limit reached")
        fit$th.warn <- gettext("alternation limit reached")
    }
    if (length(offset) && attr(Terms, "intercept")) {
        null.deviance <- if (length(Terms)) 
            glm.fitter(X[, "(Intercept)", drop = FALSE], Y, w, 
                offset = offset, family = fam, control = list(maxit = control$maxit, 
                  epsilon = control$epsilon, trace = control$trace > 
                    1), intercept = TRUE)$deviance
        else fit$deviance
        fit$null.deviance <- null.deviance
    }
    class(fit) <- c("negbin", "glm", "lm")
    fit$terms <- Terms
    fit$formula <- as.vector(attr(Terms, "formula"))
    Call$init.theta <- signif(as.vector(th), 10)
    Call$link <- link
    fit$call <- Call
    if (model) 
        fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    if (x) 
        fit$x <- X
    if (!y) 
        fit$y <- NULL
    fit$theta <- as.vector(th)
    fit$SE.theta <- attr(th, "SE")
    fit$twologlik <- as.vector(2 * Lm)
    fit$aic <- -fit$twologlik + 2 * fit$rank + 2
    fit$contrasts <- attr(X, "contrasts")
    fit$xlevels <- .getXlevels(Terms, mf)
    fit$method <- method
    fit$control <- control
    fit$offset <- offset
    fit
}
environment(my.glm.nb) <- environment(fun = glm.nb)
