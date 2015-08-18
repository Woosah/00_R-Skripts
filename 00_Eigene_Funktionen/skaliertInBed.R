### Funktion fuer das Skalieren von Variablen in einem Datensatz mit beliebigen 
### Long-Format. Durch die spezifische Benutzungsweise wahrscheinlich noch etwas
### buggy und mit noch fehlenden Prüfungen und Fehlermeldungen. Das Ausgabeformat
### kann anhand des 'form' spezifiziert werden (hier z.B. ID, Gruppe oder Bedingung),
### und im 'grouping'-Argument kann das Level der Standardisierung (hier z.B.
### auf Gruppenebene) angegeben werden. Das 'grouping'-Argument darf nicht leer
### oder Null sein (was ja einem Wide-Datensatz entspraeche...dann gleich scale 
### nutzen!). 

skaliertInBed <- function(var = NULL,
                          mainData = Stress_data,
                          form = "ID + Gruppe + Bedingung", 
                          grouping = "Bedingung",
                          aggFun = mean, 
                          Center = TRUE,
                          Scale = TRUE,
                          ending = "_sc",
                          returnAGG = FALSE) {
    
    arguments <- formals(skaliertInBed)
    if (is.null(form)) stop("The argument 'form' must not be NULL! At least one group is necessary, otherwise use 'scale' directly!")
    if (grepl("^$", (form)))stop("The argument 'form' must not be an empty string! At least one group is necessary, otherwise use 'scale' directly!")
    
    if (is.null(grouping)) stop("The argument 'grouping' must not be NULL! At least one group is necessary, otherwise use 'scale' directly!")
    if (grepl("^$", (grouping)))stop("The argument 'grouping' must not be an empty string! At least one group is necessary, otherwise use 'scale' directly!")
    
    if (!is.character(form)) stop("The argument 'form' must be specified as a character string!")
    if (!is.character(grouping)) stop("The argument 'grouping' must be specified as a character string!")
    
    nameData <- deparse(substitute(mainData))
    nameFUN <- deparse(substitute(aggFun))
    newName <- paste0(var, ending)
    form <- gsub("\\s", "", form)
    variables <- unlist(strsplit(form, "\\+"))
    grouping <- gsub("\\s", "", grouping)
    groupVars <- unlist(strsplit(grouping, "\\+"))
    
    
    if (any(!groupVars %in% variables)) stop("All variables in 'grouping' must also be named in 'form'!")
    
    if (is.character(arguments$mainData)) stop("The argument 'mainData' must specify an existing data.frame object!")
    if (!returnAGG) {
        if (newName %in% colnames(mainData)) message("A scaled version of ", var, " ('", newName, "') already seems to exist in ", as.character(arguments$mainData), "! Old version will be overwritten!")
    }
    if (!is.character(var)) stop("The argument 'var' must be specified as a character!")
    if (!var %in% colnames(mainData)) stop("The argument 'var' must name an existing variable in ", as.character(arguments$mainData), "!")
    if (!is.numeric(mainData[, var]) && !is.integer(mainData[, var])) stop(var, " must specifiy an integer or numeric variable!")
    if (!all(variables %in% colnames(mainData))) stop("The argument 'form' must contain only variables also present in ", as.character(arguments$mainData), "!")
    
    
    df <- eval(parse(text = paste("aggregate(", var, " ~ ", form, ", data = ", nameData, ", FUN = ", nameFUN, ", na.action = na.pass, na.rm = TRUE)", sep = "")))
    colnames(df) <- c(variables, var)
    df[, var][is.nan(df[, var])] <- NA
    
    eval(parse(text = paste("df$", newName, " <- NA", sep = "")))
    df[, newName] <- eval(parse(text = paste("as.vector(unlist(aggregate(", var, " ~ ", grouping,
                                             ", data = df, FUN = scale, na.action = na.pass, center = ", Center,
                                             ", scale = ", Scale, ")[, '", var, "']))", sep = "")))
    df[, newName][is.nan(df[, newName])] <- NA
    
    if(returnAGG) return(df)
    
    df[, var] <- NULL
    DAT <- merge(mainData, df, all.x = TRUE)
    
    return(DAT)
}
