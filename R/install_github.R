#' Install packages form github
#'
#' @param pkg the package name
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' gfun::install_github("ggroup")
install_github <- function(pkg = "ggroup", ...){
  pkg <- paste0("ZhonghuiGai/", pkg)
  devtools::install_github(pkg)
}
