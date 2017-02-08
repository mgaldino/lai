### Slug url
## órgãos públicos

setwd("C:/Users/mgaldino/2017/Ford/Achados & Pedidos/Arquivos/")

remove_acento <- function(vec, Toupper=F) {
  vec <- tolower(vec)
  vec <- gsub('á', 'a', vec) 
  vec <- gsub('ã', 'a', vec)
  vec <- gsub('à', 'a', vec)
  vec <- gsub('â', 'a', vec)
  vec <- gsub('é', 'e', vec) 
  vec <- gsub('ê', 'e', vec)
  vec <- gsub('í', 'i', vec)
  vec <- gsub('ó', 'o', vec) 
  vec <- gsub('ô', 'o', vec)
  vec <- gsub('õ', 'o', vec)
  vec <- gsub('ú', 'u', vec)
  vec <- gsub('ç', 'c', vec)
  #  vec <- gsub('\'', '', vec)
  if ( Toupper==T) vec <- toupper(vec)
  return(vec)
}

orgaos <- read.table("lista_orgaos_publicos_v1.csv", encoding="UTF-8",
                     header=T, sep=",", as.is=T, comment.char = "", quote = "\"")

library(dplyr)

orgaos <- orgaos %>%
  filter(Link != "") %>%
  mutate(slug = remove_acento(Link),
         slug = gsub("/[^A-Za-z0-9-]/", "", slug),
         slug = gsub("--", "-", slug))

View(valid)

orgaos %>%
  group_by(Poder, NivelFederativo) %>%
  summarise(n())

x <- orgaos %>%
  filter(Poder == "Executivo", NivelFederativo == "Municipal",
         grepl("Prefeitura", Nome))
head(x)
dim(x)
