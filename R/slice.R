#' slice lets you index rows by a grouping variable, it's a wrapper function of grepl
#'
#' @param select a vector to be used for slice, character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector.
#' @param vector the group variable, the column containing the grouping information
#' @param data a data frame or NULL
#'
#' @return a vector or data frame
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' ind <- slice(select = c("setosa$", "^virginica"), group = iris$Species, data = NULL)
#' iris.ind <- slice(select = c("setosa$", "^virginica"), group = iris$Species, data = iris)
#' data <- iris
#' colnames(data)[5] <- "group"
#' iris.ind2 <- slice(select = c("setosa$", "^virginica"), data = iris)
#' identical(iris.ind, iris.ind2)
slice <- function (select = c("setosa$", "^virginica"), group = iris$Species,
          data = NULL) {
  # step 1 check the input
  if (is.null(group) | is.null(group)) {
    stop("The group information is missing!")
  }
  # step 2 check the input
  if (!is.null(data) & is.null(group)) {
    group <- data$group
  }
  # step 3 obtain the logical information
  sel <- sapply(X = select, FUN = grepl, group)
  # step 4 obtain the index information
  ind <- as.logical(apply(sel, 1, sum))
  stopifnot(length(ind) == length(group))
  if (!is.null(data)) {
    data <- data[ind, ]
    return(data)
  }
  else {
    return(ind)
  }
}



