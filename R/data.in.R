#' Read in the data for analysis by using the read.delim and read.xlsx functions
#'
#' @param file.path the file path to be read in
#' @param colname If TRUE, the first row of data will be used as column names.
#' @param rowname If TRUE, first column of data will be used as row names.
#' @param sheet The name or index of the sheet to read data from.
#'
#' @return a matrix or a data frame
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' test <- data.in("sum_phylum.txt", rowname = FALSE)
#' test <- data.in("sum_phylum.txt")
#' test <- data.in("group.xlsx", rowname = F)
data.in <- function(file.path = "./data.txt",
                    colname = TRUE, rowname = TRUE, sheet = 1){
  cat(paste("The input data is: ", file.path, "\n"))
  l <- strsplit(x = file.path, split = "\\.")  |> unlist()
  type <- l[length(l)]
  if (type == "txt") {
    if (rowname) {
      file <- read.delim(file = file.path, row.names = 1, sep = "\t")
    } else {
      file <- read.delim(file = file.path, sep = "\t")
    }
  }
  if (type == "csv") {
    if (rowname) {
      file <- read.csv(file = file.path, row.names = 1, sep = ",")
    } else {
      file <- read.csv(file = file.path, sep = ",")
    }
  }
  if (type == "xlsx") {
    file <- openxlsx::read.xlsx(xlsxFile = file.path, sheet = sheet,
                                colNames = colname, rowNames = rowname)
  }
  message("Data read in ...... Finished!")
  return(file)
}
