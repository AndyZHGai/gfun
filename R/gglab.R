#' Greek, bold, and superscript symbols in ggplot2 labs
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' gglab_Greek_bold_superscript()
gglab_Greek_bold_superscript <- function(){
  library(ggplot2)
  message("ylab:")
  cat(paste0('ylab(label = "TNF \\u03b1 (pg/mL)")\n'))
  message("labs:")
  cat(paste0('labs(x = expression(bold(paste("X", bold(italic(R)), " (", θ^bold("-2"),")", sep=""))))\n'))
  p <- ggplot(data = data.frame(x = 1, y = 1), aes(x = x, y = y)) +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold", size = 18),
          axis.text = element_blank()) +
    labs(x = expression(bold(paste("X", bold(italic(R)), " (", θ^bold("-2"),")", sep="")))) +
    ylab(label = "TNF \u03b1 (pg/mL)")
  return(p)
}

#' Greek and Cross point shapes for ggplot2
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' ggpoint_Greek_Cross()
ggpoint_Greek_Cross <- function(){
  library(ggtext)
  library(ggplot2)
  p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_point(aes(shape = Species), size = 3) +
    theme_minimal() +
    scale_color_discrete(labels=c('**setosa**','versicolor','virginica')) +
    theme(legend.text = element_markdown()) +
    scale_shape_manual(values = c(symbols::Cross()$symbol[c(4,11)], "\u03b1"))
  return(p)
}

