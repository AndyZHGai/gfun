#' A quick and dirty way to show colours in a base plot
#'
#' @param ncol Number of columns. If not supplied, tries to be as square as possible.
#' @param colors A character vector of colours
#'
#' @return
#' @export
#'
#' @author adapt from scales package show_col function
#' @examples
#' show.color(c("red", "green"))
show.color <- function (colors, ncol = NULL) {
  n <- length(colors)
  ncol <- ncol %||% ceiling(sqrt(length(colors)))
  nrow <- ceiling(n/ncol)
  colors <- c(colors, rep(NA, nrow * ncol - length(colors)))
  colors <- matrix(colors, ncol = ncol, byrow = TRUE)
  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))
  size <- max(dim(colors))
  plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "",
       axes = FALSE)
  rect(col(colors) - 1, -row(colors) + 1, col(colors), -row(colors),
       col = colors, border = "white")
  labels <-  TRUE
  if (labels) {
    hcl <- farver::decode_colour(colors, "rgb", "hcl")
    label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
    text(col(colors) - 0.5, -row(colors) + 0.5, colors,
         cex = 1, col = label_col)
  }
}


`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}
