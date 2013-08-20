dropvar <- function(object, ...) {
    form0 <- form <- formula(object)
    ## hacks for lme4: don't include vars from random-effects terms somewhat
    ## unstable due to development status ...
    if (inherits(object, "merMod")) {
        form <- formula(object, fixed.only = TRUE)
    }
    if (inherits(object, "mer")) {
        form <- lme4.0:::nobars(form)
    }
    vars <- all.vars(form[[3]])
    res <- setNames(vector(length(vars), mode = "list"), vars)
    labs <- attr(terms(object), "term.labels")
    for (i in seq_along(res)) {
        v <- vars[i]


        tdrop <- grep(paste0("(^|:)", v, "(:|$)"), labs, value = TRUE)
        newform <- reformulate(paste(". -", paste(tdrop, collapse = "-")), response = ".")
        m_reduced <- update(object, newform)
        res[[i]] <- anova(object, m_reduced, ...)
    }
    ## allow switch to return res here?
    res2 <- do.call(rbind, c(list(head(res[[1]], 1)), lapply(res, tail, 1)))
    rownames(res2) <- c("<none>", vars)
    attr(res2, "heading") <- c("Complete deletions of variables", "\nModel:", 
        deparse(form0))
    res2
}