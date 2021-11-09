#' Find columns from a data frame by pattern
#'
#' @param data a date frame
#' @param select a list or a vector containing the select information, characters or numbers!
#' @param reverse a logical value to indicate whether to reverse the select, default value is FALSE
#' @param drop a logical value used in select only one column
#'
#' @return a data frame or a vector
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' slice_vertical(data = iris, select = list("^Sepal"))
#' slice_vertical(data = iris, select = c("^Sepal", "Width$"))
#' slice_vertical(data = iris, select = c(3,5), reverse = TRUE)
#' slice_vertical(data = iris, select = c(3), reverse = FALSE, drop = TRUE)
#' slice_vertical(data = iris, select = c("\\."))
#' slice_vertical(data = mtcars, select = c("g"))  |> head()
slice_vertical <- function(data, select, reverse = FALSE, drop = FALSE){
  stopifnot(is.data.frame(data))
  if (is.list(select)) {
    num <- vapply(X = select, FUN = is.numeric, FUN.VALUE = logical(1))  |> all()
    cha <- vapply(X = select, FUN = is.character, FUN.VALUE = logical(1)) |> all()
  } else if (is.vector(select)) {
    num <- is.numeric(select)
    cha <- is.character(select)
  }
  name <- names(data)
  if (num) {
    n <- seq_along(data)
  }else if (cha) {
    n <- names(data)
  }else{
    stop("Pls check the select parameter! Should be a list containg characters or numbers only!")
  }
  sel <- sapply(X = select, FUN = grepl, n)
  ind <- as.logical(apply(sel, 1, sum))
  if (reverse) ind <- !ind
  stopifnot(length(ind) == ncol(data))
  cat(paste("The columns you selected is: ", paste(name[ind], collapse = ", "), "\n\n"))
  res <- data[, ind, drop = drop]
  return(res)
}
