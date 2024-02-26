#' Read excel datasets with column names in multiple lines
#'
#' @description read multiline excel files
#'
#' @details
#' This function reads multiline Excel files using the solution proposed by brianwdavis.
#'
#' @param filename excel filename
#' @param row_collapse number of rows to collapse
#' @param ... additional parameter(s) passed on to readxl::read_excel
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
#'
#' @seealso \code{\link[readxl]{read_excel}}
#'
#' @references
#' See the solution proposed by brianwdavis: https://github.com/tidyverse/readxl/issues/486#issuecomment-398224438


read_excel_multiline <- function(filename, row_collapse = 1, ...) {
  nms <- read_excel(filename, range = cell_rows(seq_len(row_collapse)), col_names = F)
  nms <- lapply(nms, na.omit)
  nms <- lapply(nms, paste, collapse = "_")

  read_excel(filename, skip = row_collapse, col_names = unlist(nms), ...)
}

