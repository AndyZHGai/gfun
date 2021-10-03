#' Show all the shape of points
#'
#' @return a ggplot2 obj
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' shapeplot()
shapeplot <- function(){
  library(ggplot2)
  p <- ggplot(data = data.frame(x = rep(1:5, 5),
                           y = c(rep(1,5), rep(2,5),
                                 rep(3,5), rep(4,5),
                                 rep(5,5))),
         aes(x = x, y = y)) +
    geom_point(aes(), shape = 1:25,
               color = "#E64B35",
               size = 15,
               fill = "#7fc97f") +
    theme_minimal() +
    xlab(NULL) +
    ylab(NULL) +
    geom_text(label = 1:25,
              color = c(rep("#3C5488", 14),
                        rep("white", 6),
                        rep("red", 5)),
              fontface = "bold",
              size = 5) +
    theme(axis.text = element_blank())
  return(p)
}
