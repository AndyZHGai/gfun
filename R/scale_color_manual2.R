#' A wrapper function of set colors for ggplot2 manually
#'
#' @param ... other parameters to be set
#' @param type the type of journal, one of npg aaas nejm lancet jama jco uchicago d3 ijv locus simpsons futurma richandorty startrek tron gsea pub
#' @param order the order and the number colors chooses for using
#' @param values a color vector to be used, when this values used the type will be ognined
#' @param breaks breaks
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' ggplot(data = data.frame(x = as.factor(1:5), y = 1:5), aes(x,y)) + geom_point(aes(color = x)) +
#' gfun::scale_color_manual2(order = c(2,5,7,1,4))
#' library(ggplot2)
#' library(ggroup)
#' data <- iris
#' colnames(data)[5] <- "group"
#' colorplot(type = "npg")
#' ggbarplot(data = data, variable = "Petal.Width",
#'           comparisons = list(c("setosa", "versicolor"),
#'                              c("versicolor", "virginica"),
#'                              c("setosa", "virginica"))) +  gfun::scale_color_manual2(type = "npg", order = c(3,5,6))
scale_color_manual2 <- function (type = "npg", order = c(3, 5, 6),
                                values = NULL, breaks = waiver(), ...) {
  stopifnot(is.numeric(order) | is.null(order))
  stopifnot(all(order > 0))
  aesthetics <- c("color", "fill")
  # You can set color and fill aesthetics at the same time
  if (!is.null(values)) {
    values <- values
  } else if (is.character(type)) {
    values <- colorset(type = type)
    stopifnot(max(order) <= length(values))
    values <- c(values[order], values[!1:length(values) %in% order])
    names(values) <- NULL
    cat("The colors you choose is: \n")
    cat(values[1:length(order)])
     }
  ggplot2:::manual_scale(aesthetics, values, breaks, ..., na.value = "grey50")
}


colorset<- function(type = "npg"){
  cat("You can choose one of:\n npg aaas nejm lancet jama jco\n uchicago d3 ijv locus simpsons futurma\n richandorty startrek tron gsea pub\n")
  message("You can run the colorplot function to choose the colors you loved ...... \n")
  switch (type,
          npg = color.db$npg$nrc,
          aaas = color.db$aaas$default,
          nejm = color.db$nejm$default,
          lancet = color.db$lancet$lanonc,
          jama = color.db$jama$default,
          jco = color.db$jco$default,
          uchicago = color.db$uchicago$default,
          d3 = color.db$d3$category10,
          ijv = color.db$igv$default,
          locus = color.db$locuszoom$default,
          simpsons = color.db$simpsons$springfield,
          futurma = color.db$futurama$planetexpress,
          rickandorty = color.db$rickandmorty$schwifty,
          startrek = color.db$startrek$uniform,
          tron = color.db$tron$legacy,
          gsea = color.db$gsea$default,
          pub = c("#386cb0","#fdb462","#7fc97f","#ef3b2c", "#662506","#a6cee3",
                  "#fb9a99","#984ea3","#ffff33"),
          message(paste(type, "is not obtained......"))
  )
}
