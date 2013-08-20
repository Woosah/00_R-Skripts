dump.format <- 
function (data = list()) {
    if (length(data) == 2 & is.null(names(data)) & class(data[[1]]) == 
        "character" & length(data[[1]]) == 1) {
        names <- data
        data <- list(data[[2]])
        names(data) <- names[[1]]
    }
    if (class(data) != "list" | length(data) == 0) 
        stop("Data must be provided as a named list")
    if (any(names(data) == "") | is.null(names(data))) 
        stop("Data must be provided as a named list")
    variable = names(data)
    value <- data
    if (any(variable == ".RNG.name")) {
        n <- which(variable == ".RNG.name")
        split <- strsplit(value[[n]], split = "")[[1]]
        if (split[1] != "\"" & split[length(split)] != "\"") {
            split <- c("\"", split, "\"")
            value[[n]] <- paste(split, collapse = "")
        }
    }
    output.string <- ""
    for (i in 1:length(variable)) {
        if (length(value[[i]]) == 1 && length(dim(value[[i]])) == 
            0) {
            value.string <- as.character(value[[i]])
        }
        else {
            dims <- dim(value[[i]])
            if (length(dims) > 1) {
                value.string <- "structure(c("
            }
            else {
                value.string <- "c("
            }
            value.string <- paste(value.string, paste(value[[i]], 
                collapse = ", "), ")", sep = "")
            if (length(dims) > 1) {
                value.string <- paste(value.string, ", .Dim = c(", 
                  paste(dims, collapse = ", "), "))", sep = "")
            }
        }
        output.string <- paste(output.string, "\"", variable[[i]], 
            "\" <- ", value.string, "\n", sep = "")
    }
    return(output.string)
}



read.coda <- function (output.file, index.file, start, end, thin, quiet = FALSE) {
    index <- read.table(index.file, row.names = 1, col.names = c("", 
        "begin", "end"))
    vnames <- row.names(index)
    if (is.R()) {
        temp <- scan(output.file, what = list(iter = 0, val = 0), 
            quiet = TRUE)
    }
    else {
        temp <- scan(output.file, what = list(iter = 0, val = 0))
    }
    start.vec <- end.vec <- thin.vec <- numeric(nrow(index))
    for (i in 1:length(vnames)) {
        iter.i <- temp$iter[index[i, "begin"]:index[i, "end"]]
        thin.i <- unique(diff(iter.i))
        thin.vec[i] <- if (length(thin.i) == 1) 
            thin.i
        else NA
        start.vec[i] <- iter.i[1]
        end.vec[i] <- iter.i[length(iter.i)]
    }
    if (any(is.na(start.vec)) || any(thin.vec != thin.vec[1]) || 
        any((start.vec - start.vec[1])%%thin.vec[1] != 0)) {
        iter <- sort(unique(temp$iter))
        old.thin <- unique(diff(iter))
        if (length(old.thin) == 1) 
            is.regular <- TRUE
        else {
            if (all(old.thin%%min(old.thin) == 0)) 
                old.thin <- min(old.thin)
            else old.thin <- 1
            is.regular <- FALSE
        }
    }
    else {
        iter <- seq(from = min(start.vec), to = max(end.vec), 
            by = thin.vec[1])
        old.thin <- thin.vec[1]
        is.regular <- TRUE
    }
    if (missing(start)) 
        start <- min(start.vec)
    else if (start < min(start.vec)) {
        warning("start not changed")
        start <- min(start.vec)
    }
    else if (start > max(end.vec)) 
        stop("Start after end of data")
    else iter <- iter[iter >= start]
    if (missing(end)) 
        end <- max(end.vec)
    else if (end > max(end.vec)) {
        warning("end not changed")
        end <- max(end.vec)
    }
    else if (end < min(start.vec)) 
        stop("End before start of data")
    else iter <- iter[iter <= end]
    if (missing(thin)) 
        thin <- old.thin
    else if (thin%%old.thin != 0) {
        thin <- old.thin
        warning("thin not changed")
    }
    else {
        new.iter <- iter[(iter - start)%%thin == 0]
        new.thin <- unique(diff(new.iter))
        if (length(new.thin) != 1 || new.thin != thin) 
            warning("thin not changed")
        else {
            iter <- new.iter
            end <- max(iter)
            is.regular <- TRUE
        }
    }
    out <- matrix(NA, nrow = length(iter), ncol = nrow(index))
    dimnames(out) <- list(iter, vnames)
    for (v in vnames) {
        if (!quiet) 
            cat("Abstracting", v, "... ")
        inset <- index[v, "begin"]:index[v, "end"]
        iter.v <- temp$iter[inset]
        if (!is.regular) {
            use.v <- duplicated(c(iter, iter.v))[-(1:length(iter))]
            use <- duplicated(c(iter.v, iter))[-(1:length(iter.v))]
        }
        else {
            use.v <- (iter.v - start)%%thin == 0 & iter.v >= 
                start & iter.v <= end
            use <- (iter.v[use.v] - start)%/%thin + 1
        }
        if (length(use) > 0 && any(use.v)) 
            out[use, v] <- temp$val[inset[use.v]]
        if (!quiet) 
            cat(length(use), "valid values\n")
    }
    if (is.regular) 
        out <- mcmc(out, start = start, end = end, thin = thin)
    else warning("not returning an mcmc object")
    return(out)
}
