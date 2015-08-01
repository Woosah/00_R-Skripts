### Funktion, um aus aov- oder htest-Objekten (aov oder chisq.test) mit mehr als
### 2 Gruppen paarweise Vergeleiche durchzuführen und die Namen der Gruppen 
### auszugeben, die sich signifikant von der Referenzgruppe unterscheiden:

extractGroupHSD <- function(mod,
                            reference = "Schizophrenie",
                            aov.variable = "Gruppe",
                            threshold = 0.05,
                            tukey = FALSE,
                            results.disp = FALSE, 
                            chi.correct = FALSE,
                            chi.popsInRows = FALSE, 
                            chi.message.disp = FALSE, 
                            control = "fdr",
                            digits = 4) {
    
    chisqPostHoc <- function(chi,
                             popsInRows = chi.popsInRows,
                             message.disp = chi.message.disp,
                             correct = chi.correct,
                             control = control, 
                             tukey = tukey,
                             reference = reference) {
        
        tbl <- chi$observed
        if (!popsInRows) tbl <- t(tbl)
        popsNames <- rownames(tbl)
        
        if (!is.null(reference) && !reference %in% popsNames) {
            
            message("The specified reference level '", reference,
                    "' is not listed within the rows of the data table.\n\nThe first entry ('",
                    popsNames[1], "') is used instead.\n\nMaybe you need to change the argument for 'chi.popsInRows'?\n\n")
            reference <- popsNames[1]
            assign("reference", reference, envir = parent.frame())
            
        }
        
        prs <- combn(1:nrow(tbl), 2)
        tests <- ncol(prs)
        pvals <- numeric(tests)
        lbls <- character(tests)
        for (i in 1:tests) {
            pvals[i] <- chisq.test(tbl[prs[, i], ], correct = correct)$p.value
            lbls[i] <- paste(popsNames[prs[, i]], collapse = " - ")
        }
        dat <- data.frame(comparison = lbls, raw.p = pvals)
        
        if (!tukey && !is.null(reference)) {
            
            dat <- dat[grep(reference, dat$comparison), , drop = FALSE]
            assign("testN", nrow(dat), envir = parent.frame()) 
            dat$adj.p <- p.adjust(dat$raw.p, method = control)
            
        } else if (tukey && !is.null(reference)) {
            
            assign("testN", nrow(dat), envir = parent.frame()) 
            dat$adj.p <- p.adjust(dat$raw.p, method = control)
            
        } else if (!tukey && is.null(reference)) {
            
            assign("oldref", NULL, parent.frame())
            message("No reference level is specified, although the 'tukey' argument ist set to ", tukey,
                    ",\nindicating the wish for treatment contrasts. Therefore, the entry in the first row\nof the data table ('",
                    popsNames[1], "') is used as a reference level, but still,\ncomplete group comparisons will be listed as the output.\n\nMaybe you need to change the argument for 'tukey' or 'reference'?\n\n")
            reference <- popsNames[1]
            assign("reference", reference, envir = parent.frame())            
            dat <- dat[grep(reference, dat$comparison), , drop = FALSE]
            assign("testN", nrow(dat), envir = parent.frame())            
            dat$adj.p <- p.adjust(dat$raw.p, method = control)
            
        } else if (tukey && is.null(reference)) {
            
            assign("testN", nrow(dat), envir = parent.frame()) 
            dat$adj.p <- p.adjust(dat$raw.p, method = control)
            
        }
        
        if (message.disp) cat("Adjusted p-values used the", control, "method.\n\n")
        
        return(dat)
    }
    
    fitMC <- function(mod, tukey = NULL, variable = aov.variable, control = control) {
        
        mod.mc <- eval(parse(text = paste0("multcomp::glht(mod, linfct = multcomp::mcp(",
                                           variable, " = '", tukey, "'))")))
        mod1 <- summary(mod.mc, test = multcomp::adjusted(type = control))
        pq <- mod1$test
        mtests <- data.frame(cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues))
        error <- attr(pq$pvalues, "error")
        pname <- paste("Pr(>|", ifelse(mod1$df == 0, "z", "t"), "|)", sep = "")
        colnames(mtests) <- c("Estimate", "Std. Error", ifelse(mod1$df == 0, "z value", "t value"), pname)
        testN <- nrow(mtests)
        return(list(mod1 = mod1, mtests = mtests, testN = testN))
        
    }
    
    stripDiffs <- function(mtest, ref = reference) {
        
        Diffs <- rownames(mtest)
        Diffs <- gsub("-", "", Diffs)
        Diffs <- gsub(ref, "", Diffs)
        Diffs <- gsub("^\\s+|\\s+$", "", Diffs)
        return(Diffs)
        
    }
    
    # # Die Funktion wird nicht mehr benoetigt, ich lass sie aber hier, damit sie nicht verloren geht...
    #     getResponse <- function(formula) {
    #         tt <- terms(formula)
    #         vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
    #         response <- attr(tt, "response") # index of response var
    #         vars[response] 
    #     }
    
    
    if (inherits(mod, "aov")) {
        
        if (!control %in% c("fdr","BH","BY","bonferroni","holm","hochberg","hommel", "none", "single-step", "free", "Shaffer", "Westfall")) {
            message(paste0("'", control, "' is not available within 'adjusted' from 'multcomp', 'fdr' is used instead.\n\n"))
            control <- "fdr"
        }
        
        call <- getCall(mod)
        datName <- call$data
        newDat <- eval(parse(text = datName))
        groups <- attr(mod$terms , "term.labels")
        group <- groups[groups == aov.variable]
        
        if (length(group) != 1) stop("Selected variable '", aov.variable,
                                     "' does not exist in the model or is ambiguous!")
        
        if (!is.factor(newDat[, group])) stop("Chosen variable", aov.variable, "is not a factor!")
        
        if (!is.null(reference) && levels(newDat[, group])[1] != reference) {
            
            message("Original reference level of '", aov.variable,
                    "' ('", levels(newDat[, group])[1], "') ", "is changed to '",
                    reference, "'.\n")
            newDat[, group] <- relevel(newDat[, group], ref = reference)
            mod <- update(mod, .~., data = newDat)
            
        }
        
        if (!is.null(reference) && !reference %in% levels(newDat[, group])) {
            
            message("The specified reference level '", reference,
                    "' is not a level of the specified variable '", aov.variable,
                    "'.\n\nThe first level of '", aov.variable, "' ('",
                    levels(newDat[, group])[1], "') is used instead!\n\n")    
            reference <- levels(newDat[, group])[1]
            
        }
        
        if (tukey && !is.null(reference)) {
            
            mod_test <- fitMC(mod, tukey = "Tukey", variable = aov.variable, control = control)
            mtests <- mod_test$mtests
            mtests <- mtests[mtests[, grep("^Pr", colnames(mtests))] < threshold, , drop = FALSE]
            mtests <- mtests[grep(reference, rownames(mtests)), ]
            if (nrow(mtests) < 1) Diffs <- NULL
            Diffs <- stripDiffs(mtests, ref = reference) 
            
        } else if (!tukey && !is.null(reference)) {
            
            mod_test <- fitMC(mod, tukey = "Dunnett", variable = aov.variable, control = control)
            mtests <- mod_test$mtests
            mtests <- mtests[mtests[, grep("^Pr", colnames(mtests))] < threshold, , drop = FALSE]
            if (nrow(mtests) < 1) Diffs <- NULL
            Diffs <- stripDiffs(mtests, ref = reference) 
            
        } else if (tukey && is.null(reference)) {
            
            mod_test <- fitMC(mod, tukey = "Tukey", variable = aov.variable, control = control)
            mtests <- mod_test$mtests
            mtests <- mtests[mtests[, grep("^Pr", colnames(mtests))] < threshold, , drop = FALSE]
            if (nrow(mtests) < 1) Diffs <- NULL
            Diffs <- rownames(mtests)
            
        } else if (!tukey && is.null(reference)) {
            
            mod_test <- fitMC(mod, tukey = "Dunnett", variable = aov.variable, control = control)
            mtests <- mod_test$mtests
            mtests <- mtests[mtests[, grep("^Pr", colnames(mtests))] < threshold, , drop = FALSE]
            if (nrow(mtests) < 1) Diffs <- NULL
            Diffs <- rownames(mtests)
            
        }
        
        if (results.disp) {cat("p-value adjustment is done for", mod_test$testN,
                               "comparisons.\nThreshold for results is alpha =",
                               threshold, "\n\n"); print(mod_test$mod1, digits = digits); cat("\n")}

        return(Diffs)
        

    } else if (inherits(mod, "htest")) {
        
        if (!control %in% c("fdr","BH","BY","bonferroni","holm","hochberg","hommel", "none")) {
            message(paste0("'", control, "' is not available within 'p.adjust', 'fdr' is used instead.\n"))
            control <- "fdr"
        }
        
        mod1 <- suppressWarnings(chisqPostHoc(mod, popsInRows = chi.popsInRows, message.disp = chi.message.disp,
                                              tukey = tukey, reference = reference,
                                              correct = chi.correct, control = control))
        
        if (results.disp) {cat("All comparisons are displayed with p-values adjusted with the", control,
                               "method.\n\np-value adjustment is done for", testN,
                               "comparisons.\nThreshold for results is alpha =", threshold, "\n\n"); print(mod1, digits = digits); cat("\n")}
        
        if (tukey && is.null(reference)) {
            
            mod1 <- mod1[mod1[, "adj.p"] < threshold, ]
            if (nrow(mod1) < 1) return(NULL)
            return(as.character(mod1$comparison))
            
        } else if (tukey && !is.null(reference)) {
            
            mod1 <- mod1[grep(reference, as.character(mod1$comparison)), ]
            mod1 <- mod1[mod1[, "adj.p"] < threshold, ]
            if (nrow(mod1) < 1) return(NULL)
            mod1 <- as.character(mod1$comparison)
            mod1 <- gsub("-", "", mod1)
            mod1 <- gsub(reference, "", mod1)
            mod1 <- gsub("^\\s+|\\s+$", "", mod1)
            return(mod1)
            
        } else if (!tukey && !is.null(reference)) {
            
            mod1 <- mod1[mod1[, "adj.p"] < threshold, ]
            if (nrow(mod1) < 1) return(NULL)
            mod1 <- as.character(mod1$comparison)
            mod1 <- gsub("-", "", mod1)
            mod1 <- gsub(reference, "", mod1)
            mod1 <- gsub("^\\s+|\\s+$", "", mod1)
            return(mod1)
               
        } else if (!tukey && is.null(oldref)) {
            
            mod1 <- mod1[mod1[, "adj.p"] < threshold, ]
            if (nrow(mod1) < 1) return(NULL)
            mod1 <- as.character(mod1$comparison)
            return(mod1)
        
        }
        
    } else if (!inherits(mod, c("aov", "htest"))) {
        
        stop("Object is not of class 'aov' or 'htest'!")
        
    }
}
