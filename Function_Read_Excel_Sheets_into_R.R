excelToCsv <- function(file_path, keep_sheets = NULL, target_dir = NULL, ...) {
  
  temp_already <- list.files(tempdir())
  
  if(is.null(target_dir)) {
    file_root <- gsub("([[:print:]]+(/|\\\\))[[:print:]]+", "\\1", file_path)
  } else if(!is.null(target_dir) & target_dir != FALSE) {
    file_root <- target_dir
  }
  
  file_name <- gsub("[[:print:]]+(/|\\\\)", "", file_path)
  file_ext <- gsub("[[:print:]]+(.xls.?)", "\\1", file_path)
  
  converter_file <- file(paste0(tempdir(),"/", "converter.vbs"))
  
  writeLines(
    c('rem  XLS_To_CSV.vbs',
      'rem =============================================================',
      'rem  convert all NON-empty worksheets in an Excel file to csv',
      'rem  CSV file names will default to Sheet names',
      'rem  output folder defaults to the folder where the script resides or',
      'rem  if path is specified with the input file, that path is used',
      'rem  ',
      'rem  input parameter 1:  Excel path\\file in argument 1 ',
      'rem                     (if path is not specified, the current path is defaulted)',
      'rem  ',
      'rem ============================================================',
      '',
      'Dim strExcelFileName',
      'Dim strCSVFileName',
      '',
      'strExcelFileName = WScript.Arguments.Item(0)',
      '',
      'rem get path where script is running',
      'Set fso = CreateObject ("Scripting.FileSystemObject")',
      'strScript = Wscript.ScriptFullName',
      'strScriptPath = fso.GetAbsolutePathName(strScript & "\\..")',
      '',
      'rem If the Input file is NOT qualified with a path, default the current path',
      'LPosition = InStrRev(strExcelFileName, "\\") ',
      'if LPosition = 0 Then ',
      '    strExcelFileName = strScriptPath & "\\" & strExcelFileName',
      'strScriptPath = strScriptPath & "\\" ',
      'else ',
      'strScriptPath = Mid(strExcelFileName, 1, LPosition) ',
      'End If',
      'rem msgbox LPosition & " - " & strExcelFileName & " - " & strScriptPath',
      '',
      'Set objXL = CreateObject("Excel.Application")',
      'Set objWorkBook = objXL.Workbooks.Open(strExcelFileName)',
      'objXL.DisplayAlerts = False',
      '',
      'rem loop over worksheets',
      '  For Each sheet In objWorkBook.Sheets  ',
      'if objXL.Application.WorksheetFunction.CountA(sheet.Cells) <> 0 Then ',
      'rem             sheet.Rows(1).delete',
      'sheet.SaveAs strScriptPath & sheet.Name & ".csv", 6',
      '   End If',
      '  Next',
      '',
      'rem clean up  ',
      'objWorkBook.Close ',
      'objXL.quit',
      'Set objXL = Nothing ',
      'Set objWorkBook = Nothing',
      'Set fso = Nothing',
      '',
      'rem end script'),
    con = converter_file)
  
  close(converter_file)
  
  file.copy(file_path, tempdir())
  
  orig_wd <- getwd()
  setwd(tempdir())
  
  file.rename(file_name, paste0("filetoconvert", file_ext))
  
  shell(paste("converter.vbs", 
    paste0("filetoconvert", file_ext)), intern = TRUE)
  
  setwd(orig_wd)
  
  if(is.null(keep_sheets)) {
    keep_sheets <- gsub("\\.csv", "", list.files(tempdir(), pattern = "\\.csv"))
  }
  
  file_flags <- paste0(keep_sheets, ".csv")
  
  if(is.null(target_dir) | (!is.null(target_dir) & target_dir != FALSE)) {
    for(i in 1:length(file_flags)) {
      file.copy(
        paste0(tempdir(), "/", file_flags[i]), file_root, overwrite = TRUE)
    }
  } else {
    
    all_files <- lapply(file_flags, function(x) {
      csv_file <- read.csv(paste0(tempdir(), "/", x), 
        as.is = TRUE, na.strings = c("#N/A", "NA", "N/A", "?", ""))
      
      csv_file[,sapply(csv_file, function(y) mean(is.na(y), na.rm = TRUE)) < 1]
    })
    
    if(length(all_files) == 1) {
      all_files <- all_files[[1]]
    } else {
      names(all_files) <- keep_sheets
    }
  }
  
  suppressWarnings(file.remove(
    paste0(tempdir(),
      "/",
      list.files(tempdir())[!(list.files(tempdir()) %in% temp_already)])))
  
  if(!is.null(target_dir) & target_dir == FALSE) {
    all_files
  }
}
