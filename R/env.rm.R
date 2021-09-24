#' Delete objects in the envirement using grepl function
#'
#' @param del a vector containing the names of objects to be deleted
#'
#' @return a list
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' Day6 <- 1
#' l <- ls()
#' rm(list = env.rm(c("^Day")))
#' rm(l)
env.rm <- function(del = c("^Day", "Day9$")){

  sel <- sapply(X = del, FUN = grepl, l)
  if (length(del) == 1L) {
    ind <- sel
  } else {
    ind <- as.logical(apply(sel, 1, sum))
  }
  delete <- l[ind]
  dput(delete)
  return(delete)
}
