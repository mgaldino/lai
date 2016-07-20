## pegando lista de órgãos públicos do queremossaber

library(XML)
library(RCurl)

setInternet2(TRUE)
options(timeout=500)


url <- "http://queremossaber.org.br/body/list/all"

pegaLinks1 <- function ( url.inicial, padrao.inicial, arg.xpath="//a/@href") {
  #browser()
  doc <- htmlParse( url.inicial)   # parseia url
  linksAux <- xpathSApply(doc, arg.xpath)   # coleta os links
  linksMandato <- unique(linksAux[grep(padrao.inicial, linksAux)]) # me traz apenas os links certos
  free(doc)
  return(linksMandato)
}

get_org <- function(url, arg.xpath="//a/@href") {
  html <- getURL(url)
  doc <- htmlParse(html)   # parseia url
  linksAux <- xpathSApply(doc, path="//a/@href")   # coleta os links
  free(doc)
  return(linksAux)
}

aux <- get_org(url= url)
