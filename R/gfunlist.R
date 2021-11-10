#' Show the list of packages avilable
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' gfun::gfunlist()
gfunlist <- function() {
  list <- c("ggroup", "gglefse", "veganEx", "ggheatmap", "saveplot", "groutable",
            "ggfcmc", "postUsearch", "ggtheme", "gglm", "litsence", "ggcnmap",
            "ggvennEx", "ggtreeEx", "ggpca", "gfun", "ggpatch", "ggfacet")
  dput(list)
}
