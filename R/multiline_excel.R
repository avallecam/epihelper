#' Read excel datasets with column names in multiple lines
#'
#' @description read multiline excel files
#'
#' @describeIn read_excel_multiline implements the solution proposed by brianwdavis here https://github.com/tidyverse/readxl/issues/486#issuecomment-398224438
#'
#' @param filename excel filename
#' @param row_collapse number of rows to collapse
#'
#' @return tibble with the column names from multiple lines
#'
#' @import readxl
#'
#' @examples
#'
#' \dontrun{
#'
#' # example ---------------------------------------------------
#'
#' #library(readxl)
#'
#' #file_path <- "https://github.com/brianwdavis/public/raw/master/example.xlsx"
#' #download.file(file_path, "example.xlsx", mode = "wb")
#' #read_excel_multiline("example.xlsx", row_collapse = 2)
#'
#' }
#'
#' @export read_excel_multiline

read_excel_multiline <- function(filename, row_collapse = 1, ...) {
  nms <- read_excel(filename, range = cell_rows(seq_len(row_collapse)), col_names = F)
  nms <- lapply(nms, na.omit)
  nms <- lapply(nms, paste, collapse = "_")

  read_excel(filename, skip = row_collapse, col_names = unlist(nms), ...)
}

