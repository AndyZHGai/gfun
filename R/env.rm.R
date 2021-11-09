#' Delete objects in the envirement using grepl function
#'
#' @param del a vector containing the names of objects to be deleted
#'
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' Day6 <- 1
#' a_Day9 <- 2
#' temp <- 3
#' env_rm(del = c("^Day"))
env_rm <- function(del = c("^Day", "Day9$")){
  l <- ls(envir = parent.frame())
  sel <- sapply(X = del, FUN = grepl, l)
  if (length(del) == 1L) {
    ind <- sel
  } else {
    ind <- as.logical(apply(sel, 1, sum))
  }
  delete <- l[ind]
  message("The delete objects are:")
  dput(delete)
  rm(list = intersect(ls(envir = parent.frame()), delete),
     envir = parent.frame())
}

#'  Hold objects in the envirement using grepl function
#'
#' @param hold a vector containing the names of objects to be hold
#'
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' Day6 <- 1
#' a_Day9 <- 2
#' temp <- 3
#' env_hold(hold = c("temp"))
env_hold <- function(hold = c("temp")){
  l <- ls(envir = parent.frame())
  sel <- sapply(X = hold, FUN = grepl, l)
  if (length(hold) == 1L) {
    ind <- sel
  } else {
    ind <- as.logical(apply(sel, 1, sum))
  }
  h <- l[ind]
  message("The dobserved objects are:")
  dput(h)
  rm(list = setdiff(ls(envir = parent.frame()), h),
     envir = parent.frame())
}
