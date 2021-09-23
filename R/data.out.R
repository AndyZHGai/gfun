#' Save data using write.table function
#'
#' @param file the file to be saved, usually a data frame or a matrix
#' @param colname either a logical value indicating whether the column names of x are to be written along with x, or a character vector of column names to be written.
#' @param rowname either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' gfun::data.out(file = "mtcars")
data.out <- function(file = "mtcars", colname = TRUE, rowname = FALSE){
  cat(paste("The file to be saved is:", file, "\n"))
  file.name <- paste(file, "_", format(Sys.time(), format = "%y%m%d"), ".txt", sep = "")
  data <- get(file)
  write.table(data, file = file.name, sep = "\t", col.names = colname, row.names = rowname)
  message(paste("Save: ", file.name, "...... Finished!"))
}
