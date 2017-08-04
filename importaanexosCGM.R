## Script para preparar anexos a serem inseridos no achados e pedidos
## gera um csv que é importado no mysql e inserido na base de dados

library(stringr)
library(dplyr)
library(archive)
library(janitor) # versão dev github 1.6



remove_acento <- function(vec, Toupper=F) {
  vec <- tolower(vec)
  vec <- vec %>%
    gsub('à', 'a', .) %>%
    gsub('á', 'a', vec) %>%
    gsub('â', 'a', vec) %>%
    gsub('ã', 'a', vec) %>%
    gsub('é', 'e', vec) %>%
    gsub('ê', 'e', vec) %>%
    gsub('í', 'i', vec) %>%
    gsub('ó', 'o', vec) %>%
    gsub('ô', 'o', vec) %>%
    gsub('õ', 'o', vec) %>%
    gsub('ú', 'u', vec) %>%
    gsub('ç', 'c', vec) %>% 
    gsub("'", '', vec) %>%
    gsub("`", '', vec)
  #  gsub('\'', '', vec)
  if ( Toupper==T) vec <- toupper(vec)
  return(vec)
}


## Primeiro passo ? descompactar arquivos em uma pasta
# fazendo manualmente por enquanto
# arx <- archive(c("1esicanexados4a894.part1.rar))
# archive_extract(dir, "cgm descompactado")

## muda nome de onde foi descompactado
file.rename(file.path(getwd(), "cgm descompactado", "1esicanexados4a894"), 
            file.path(getwd(),"cgm descompactado", "parte1"))


# fun??o que cria nomes das pastas para cada protocolo
pasta_protocolo <- function(folder_anexos, remove_testes = F) {
  
  # fun??o que cria nomes das pastas para cada protocolo
  # argumentos: folder_main: caminho onde est?o os anexos
  # remove_testes, se ? para remover anexos testes
  
  tmp_dir <- file.path(folder_anexos , "parte1")
  
  # higieniza nomes dos arquivos
  nome_original <- file.path(tmp_dir, list.files(tmp_dir))
  nome_sem_acento <- file.path(tmp_dir, remove_acento(list.files(tmp_dir)))
  
  file.rename( nome_original, 
               nome_sem_acento)
  print("nomes dos arquivos est?o em caixa baixa e sem acento")

  # aponta para onde vai criar pastas
  
  if (remove_testes) {
    lista_anexos <- list.files(tmp_dir)
    lista_anexos <- lista_anexos[!grepl("teste", lista_anexos)]
  } else {
    lista_anexos <- list.files(tmp_dir)
  }
  
  num_pasta_aux <- character()
  
  n <- length(lista_anexos)
  for(i in 1:n) {
    num_pasta_aux[i] <- str_extract(lista_anexos[i], "[0-9]+_")
  }
  num_pasta_aux <- num_pasta_aux[!is.na(num_pasta_aux)]
  
  num_pasta_aux <- unique(num_pasta_aux) # protocolos ?nicos com _
  num_pasta <- gsub("_", "", num_pasta_aux) # protocolos ?nicos sem _
  
  sub_dir <- paste0("pasta_cgm_", num_pasta) # criando pasta_cgm_#protocolo
  return(list(sub_dir, num_pasta_aux, lista_anexos))
}

# fun??o que cria pasta para cada protocolo e colocar os anexos nas pastas
cria_pasta <- function(folder_main,
                       folder_a_criar = "Controladoria Geral do Municipio", 
                       folder_anexos=folder_main, remove_testes=T) {
  
  # cria pasta para cada protocolo
  # argumentos: folder_main: pasta onde ficar?o as sub pastas
  # folder_anexos, pasta onde est?o os anexos. Por default, igual foldeR_main
  # remove_testes, argumento de pasta_protocolo
  
  tmp_dir <- file.path(folder_main, folder_a_criar)
  dir.create(tmp_dir)
  
  result_pasta_protocolo <- pasta_protocolo(folder_anexos, remove_testes)
  sub_dir = result_pasta_protocolo[[1]]
  num_pasta_aux <- result_pasta_protocolo[[2]]
  lista_anexos <- result_pasta_protocolo[[3]]
  lista_anexos <- lista_anexos[!grepl("pasta_cgm", lista_anexos)]
  
  k <- length(sub_dir)
  
  to_folder <- character()
  
  # cria pastas
  for ( i in 1:k) {
    to_folder[i] <- file.path(tmp_dir, sub_dir[i])
    dir.create(to_folder[i])
  }
  
  print(paste(length(to_folder), "pastas criadas", sep=" "))
  
  arquivos_para_copiar <- list()
  
  j <- length(num_pasta_aux)
  for ( i in 1:j) {
    arquivos_para_copiar[[i]] <- lista_anexos[str_detect(lista_anexos, paste0("^",num_pasta_aux[i]))]
    file.copy(file.path(folder_anexos , "parte1" ,arquivos_para_copiar[[i]]), to_folder[i])
  }

  return(arquivos_para_copiar)
}



dir <- "C:/Users/mgaldino/2017/Ford/AchadosePedidos/Arquivos/Anexos/CGM"
setwd(dir)

#setando  diret?rio
main_dir <- file.path(dir,"cgm descompactado")

lista_arquivos <- cria_pasta(folder_main = main_dir, remove_testes=T)
vec_arquivos <- unlist(lista_arquivos)

id_arquivos <- gsub("_", "", str_extract(vec_arquivos, "[0-9]+_"))

###
path_default <- file.path("", "var", "www", "stage.achadosepedidos.org.br", "webroot",
                       "uploads", "pedidos", "Controladoria Geral do Municipio")
path_default1 <- file.path(path_default, list.files(file.path(dir,"cgm descompactado", "aep")))

protocolo <- list.files(file.path(dir,"cgm descompactado", "aep"))
protocolo1 <- sort(as.numeric(gsub("pasta_cgm_", "", protocolo)))

tabela_anexos <- data.frame(Codigo=NA, Protocolo=protocolo1, CodigoPedidoInteracao=NA,
                            ArquivoFullPath = path_default1,
                            Ativo = 1, Criacao=NA, Alteracao=NA,
                            CodigoStatusExportacaoES="extraido")

arquivod = data.frame(Protocolo = as.numeric(id_arquivos), 
                      Arquivo = file.path("Controladoria Geral do Municipio", 
                                          paste("pasta_cgm", id_arquivos, sep="_"),
                                          vec_arquivos))

tabela_anexos1 <- tabela_anexos %>%
  inner_join(arquivod, by="Protocolo")

View(tabela_anexos1)
write.table(tabela_anexos1, file="tabela_anexos_cgm_parte1.csv", 
            sep=";", row.names=F, na="NULL", fileEncoding = "UTF-8")
