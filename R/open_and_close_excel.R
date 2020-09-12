## c:/Dropbox/Rpackages/CLmisc/R/open_and_close_excel.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2020-09-11

#' Open and Close an excel file
#'
#' Using Visual Basic to open and close an excel file. Must be on
#' windows. The script will create a temporary visual basic file
#' `temp__.vbs`
#'
#' @param .file.path A path to the excel file to open and close
#' @param time.sleep The number of seconds to leave the excel file
#'   open. Defaults to 30 seconds
#' @export
open_and_close_excel <- function(.file.path, time.sleep = 30) {

  if (Sys.info()['sysname'] != "Windows")
    stop("Error: Only windows is currently supported")

  if (!file.exists(.file.path))
    stop("Error: The file path does not exist")

  ##https://stackoverflow.com/a/56903869/1317443
  .file.path <- file.path(normalizePath(dirname(.file.path)), .file.path, fsep = "\\")

  print("opening and closing the file with the path: ")
  print(.file.path)

  time.sleep <- sprintf("%02.f", time.sleep)



  ##https://www.automateexcel.com/vba/open-close-workbook/
  vb.script <- sprintf('
\'For getting a vbscript from a vba file
\'See https://help.mjtnet.com/article/19-converting-office-vba-to-vbscript
\n

\'Set Excel app
Set ExcelApp = CreateObject("Excel.Application")
ExcelApp.Visible = true
\n

\'Open the workbook
Set wb = ExcelApp.Workbooks.Open("%1$s")
\n


\'Make the Script sleep
\'https://stackoverflow.com/a/1729106
WScript.Sleep 1000 * %2$s
\n

\'Save the Workbook
wb.Save
\n

\'Close the Workbook
wb.Close

\'Close Excel
ExcelApp.Quit

', .file.path, time.sleep)

  writeLines(vb.script, con = "temp__.vbs")

  ##From https://stackoverflow.com/a/45631703/1317443
  system(command = 'WScript "temp__.vbs"', wait = TRUE)

  file.remove("temp__.vbs")

  return(invisible())
}
