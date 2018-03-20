
##########################################
## LDA Pedidos do Achados e Pedidos ######
##########################################


# pacotes
library(XML)
library(tm)
library(SnowballC)
library(dplyr)
 library(ggplot2)
library(seqinr)
library(RTextTools)
library(topicmodels)
library(data.table)
library(devtools)
devtools::install_github("sfirke/janitor")
library(janitor)
devtools::install_github("mgaldino/tbaep")
library(tbaep)

setwd("C:\\Users\\mgaldino\\2018\\Geral TB\\Mailing")

pedidos <- fread("govmg15.csv", encoding="UTF-8")

# remove acentos, lowecase etc.
pedidos <- pedidos %>%
  clean_names() %>%
  mutate(descricao_demanda = snakecase::to_any_case(descricao_demanda, case = "none",
                                                    transliterations = c("Latin-ASCII")))


## baixar stopwords do link:
## https://raw.githubusercontent.com/stopwords-iso/stopwords-pt/master/stopwords-pt.txt

pt_stop_words <- fread("stopwords-pt.txt")

pedidos1 <- pedidos %>%
  select(descricao_demanda)
  

## precisa remover acento antes

# dtm.control <- list(
#   tolower = T,
#   removePunctuation = T,
#   removeNumbers = T,
#   stopwords = c(stopwords("portuguese"),pt_stop_words),
#   stemming = T,
#   wordLengths = c(3,Inf),
#   weighting = weightTf
# )

# transforma DF em vetor 
pedidos2 <- pedidos1$descricao_demanda 

ped <- Corpus(VectorSource(pedidos2)) # , readerControl = dtm.control
ped
inspect(ped[1:2])
# my_stopwords <- unique(unlist(c(stopwords("portuguese"),pt_stop_words)))

ped <- tm_map(ped, removeNumbers)
ped <- tm_map(ped, removePunctuation)
ped <- tm_map(ped , stripWhitespace)
ped <- tm_map(ped, tolower)
ped <- tm_map(ped, removeWords, stopwords("portuguese") ) # demora um pouco
ped <- tm_map(ped, stemDocument, language = "portuguese")

inspect(ped[1:2])
dtm.control <- list(wordLengths = c(2,Inf),
                     weighting = weightTf)

dtm <- DocumentTermMatrix(ped, control = dtm.control)
dim(dtm)
inspect(dtm[1:10,1:20])

## cada doc é uma linha
# cada palavra uma coluna
# número de colunas e igual número de palavras únicas em todos os docs


freq_words <- rowSums(as.matrix(dtm)) # Quantas palavras cada documento (linha) tem
index <- which(freq_words==0) # índice de documentos em que não há palavras
# exemplo: 4600
inspect(dtm[4600, 1:10])


inspect(dtm1[1:10, 1:10])


dtm1 <- dtm[-index, ] # remove # palavras que não ocorrem em nenhum documento.
dim(dtm1)

# a partir daqui começa LDA

set.seed(51)
trainpoints <- sample(1:nrow(dtm1), 1*nrow(dtm1),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8

k <- 10 # número de tópicos. Estou chutando

## função pra extrair termos
SpecificTerms <- function(lda.model,n=1) {
  p <- posterior(lda.model)$terms
  n <- min(n,ncol(p))
  cumulativep <- colSums(p)
  specificp <- t(apply(p,1,function(x) ((x) + (x/cumulativep) )))
  
  topterms <- t(apply(specificp, 1, function(x)
    (colnames(specificp)[order(x,decreasing=T)[1:n]]) ))
  
  topterms
}

set.seed(2)
system.time(lda1 <- LDA(dtm1, k))

# t termos mais prováveis por tópico
t <- 15
View(terms(lda, t))

## vamos olhar o Topic 17
## Aprov Eleitoral? Bush, Clinton etc.



# t termos com prob acima de minimo
minimo <- .02
terms(lda, t, threshold=minimo)



# tópicos mais prováveis por documentos
head(topics(lda), 10)
