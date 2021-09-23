#' plot the colors with values
#'
#' @param type the type of journal
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' colorplot(type = "npg")
colorplot <- function(type = "npg"){
  cat("You can choose one of:\n npg aaas nejm lancet jama jco\n uchicago d3 ijv locus simpsons futurma\n richandorty startrek tron gsea pub\n")
  switch (type,
    npg = show.color(color.db$npg$nrc),
    aaas = show.color(color.db$aaas$default),
    nejm = show.color(color.db$nejm$default),
    lancet = show.color(color.db$lancet$lanonc),
    jama = show.color(color.db$jama$default),
    jco = show.color(color.db$jco$default),
    uchicago = show.color(color.db$uchicago$default),
    d3 = show.color(color.db$d3$category10),
    ijv = show.color(color.db$igv$default),
    locus = show.color(color.db$locuszoom$default),
    simpsons = show.color(color.db$simpsons$springfield),
    futurma = show.color(color.db$futurama$planetexpress),
    rickandorty = show.color(color.db$rickandmorty$schwifty),
    startrek = show.color(color.db$startrek$uniform),
    tron = show.color(color.db$tron$legacy),
    gsea = show.color(color.db$gsea$default),
    pub = show.color(c("#386cb0","#fdb462","#7fc97f","#ef3b2c",
                       "#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")),
    message(paste(type, "is not obtained......"))
  )
}
