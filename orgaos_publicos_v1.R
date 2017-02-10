### Slug url
## órgãos públicos

## Tribunal Regional Federal da 1a Região
setwd("C:/Users/mgaldino/2017/Ford/Achados & Pedidos/Arquivos/")

remove_acento <- function(vec, Toupper=F, apostrofo=T) {
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
  if(apostrofo) {
    vec <- gsub("'", '', vec)
  }
  #  vec <- gsub('\'', '', vec)
  if ( Toupper==T) vec <- toupper(vec)
  return(vec)
}

orgaos <- read.table("lista_orgaos_publicos_v1.csv", encoding="UTF-8",
                     header=T, sep=",", as.is=T, comment.char = "", quote = "\"")

library(dplyr)
library(RCurl)
library(tidyr)
library(stringr)

y <- orgaos %>%
  filter(grepl("Tribunal Regional Federal da 1", Nome))

orgaos_v2 <- orgaos %>%
  mutate(Codigo = "",
         UF = ifelse(NivelFederativo == "Federal", "", UF),
         UF = gsub(" \\(interior\\)", "", UF),
         Cidade = ifelse(NivelFederativo == "Estadual", "", Cidade),
         Cidade = ifelse(NivelFederativo == "Federal", "", Cidade)) %>%
  separate_rows(UF, sep=",") %>%
  select(c(1,2,3,8,4,5,6,7))

orgaos_v2$UF <- trimws(orgaos_v2$UF)
orgaos_v2$Cidade <- trimws(orgaos_v2$Cidade)

View(orgaos_v2)

write.table(orgaos_v2, file="orgaos_v2.csv", sep=",", fileEncoding= "UTF-8", row.names = F)

y2 <- y1 %>%
  filter(grepl("Tribunal Regional Federal da 1", Nome))
  
View(y2)  
mutate(Codigo = 1:n()) %>%
  group_by(UF) %>%
  summarise(Codigo = max(Codigo), 
            Poder = max(Poder, na.rm=T),
            NivelFederativo = max(NivelFederativo, na.rm=T),
            Cidade = max(Cidade, na.rm=T),
            Nome = max(Nome, na.rm=T),
            Descricao = max(Descricao, na.rm=T),
            Link = max(Link, na.rm=T))

View(y1)
orgaos <- orgaos %>%
  mutate(slug = remove_acento(Link),
         slug = gsub("/[^A-Za-z0-9-]/", "", slug),
         slug = gsub("--", "-", slug))

View(valid)

orgaos %>%
  group_by(Poder, NivelFederativo) %>%
  summarise(n())

x <- orgaos %>%
  filter(Poder == "Executivo", NivelFederativo == "Municipal",
         grepl("Prefeitura", Nome)) %>%
  mutate(link_try = remove_acento(paste(Nome, UF, "gov.br", sep=".")),
         link_try = gsub("prefeitura municipal de ", "", link_try),
         link_try = gsub(" ", "", link_try))

bol_exist <- numeric()
n <- nrow(x)
for(i in 1:n) {
  bol_exist[i] <- as.numeric(url.exists(x$link_try[i]))
  if(i %% 100 == 0) {
    print(i)
  }
}

save(bol_exist, file="bol_exist.RData")
x <- x %>%
  mutate(link_bom = bol_exist)
