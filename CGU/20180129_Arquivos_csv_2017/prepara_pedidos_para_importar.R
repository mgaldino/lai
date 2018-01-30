
library(data.table)
library(dplyr)
library(stringi)
library(magrittr)
library(RMySQL)



setwd("C:/Users/mgaldino/2017/Ford/AchadosePedidos/lai/CGU/20180129_Arquivos_csv_2017")

# carrega cabeçalho do banco
load("head_pedidos.RData")

## abre tbl no bloco de notas e salva como utf-8

cgu1 <- fread("20180129_Pedidos_csv_2017_utf8.txt", encoding = "UTF-8")

cgu1 <- cgu1 %>%
  set_colnames(as.character(nome$nome))


ucscDb <- dbConnect(RMySQL::MySQL(),
                    user="tblaistage", 
                    groups = "mysql",
                    password="Cl$x@T4b9",
                    host="104.197.226.92",
                    port = 3306,
                    dbname = "tblai")


agentes <- dbGetQuery(ucscDb, "SELECT * 
                      FROM agentes where CodigoPoder = 2 and CodigoNivelFederativo = 1")

head(agentes)




# removendo órgãos públicos que o match é complicado. Deixandode fora para reslver depois
agentes1 <- agentes %>%
  filter(!grepl("[A-Za-z]*/", Nome)) %>% # siglas com barra ex. MCA/UFBA
  filter(!grepl("[A-Za-z]+-[A-Za-z]", Nome)) %>% # siglas com -, ex. CEASA-MG
  filter(!grepl("CP II", Nome)) # Colégio Pedro II

# corrigindo problema da importação do "-", ue por encode importaa dois gipos diferentes
traco <- substr(agentes$Nome[2],5,5)

agentes1 <- agentes1 %>%
  mutate(Nome = gsub(traco, "@", Nome),
         Nome = gsub("-", "@", Nome),
    nome_join = gsub(" @", "", stri_extract_first(Nome, regex="[A-Za-z]* @"))) %>%
  filter(grepl("@", Nome)) %>% # exclui quem não tem sigla ex. Alfândega do Brasil
  group_by(nome_join) %>%
  filter(row_number()==1) %>% # filtrando órgãos com silgas duplicadas, para evitar associar ao errado
  ungroup()

# 269 órgãos. Originalmente, 322 

cgu1 <- cgu1 %>%
  mutate(nome_join = stri_extract_first(OrgaoDestinatario, regex="[A-Za-z]*"))

cgu2 <- cgu1 %>%
  inner_join(select(agentes1, "Codigo", "nome_join", "Nome"), by= "nome_join")

# ficamos com 85007 pedidos, originalmente 92925



# ClassificacaoTipoResposta # se em branco, em Tramitação


cgu2 <- cgu2 %>%
  mutate(CodigoTipoPedidoSituacao = ifelse(ClassificacaoTipoResposta == '', 1, 2)) %>% #trmtacao
  rename(CodigoAgente = Codigo)
  
## importando recursos

recursos <- fread("20180129_Recursos_csv_2017_utf8.txt", encoding = "UTF-8")

# var <- read.table("clipboard", header=T)
# save(var, file="head_recursos.RData")
load("head_recursos.RData")

nome_var <- as.character(var$var)
nome_var <- c(nome_var[1:3], nome_var[16], nome_var[c(4:15,17)]) # ordem das colunas errada

recursos <- recursos %>%
  set_colnames(nome_var) %>%
  mutate(ProtocoloPedido = as.character(ProtocoloPedido))

## pedidos sem recursos
cgu_sem_recursos <-  cgu2 %>%
  mutate(ProtocoloPedido = as.character(ProtocoloPedido)) %>%
  left_join(select(recursos, "ProtocoloPedido", "Instancia"), by="ProtocoloPedido") %>%
  filter(is.na(Instancia))

#78230 pedidos sem recursos

## Preparando pedidos para serem salvos

cgu_sem_recursos_sql <- cgu_sem_recursos %>%
  mutate(Codigo = "NULL",
         CodigoUsuario = 12,
         CodigoTipoOrigem = 3,
         CodigoStatusPedido = "NULL",
         CodigoStatusPedidoInterno = 4, # checar
         IdentificadorExterno = "NULL",
         Slug = NA) %>%
  select(Codigo, CodigoUsuario, CodigoAgente, CodigoTipoOrigem,
         CodigoTipoPedidoSituacao, CodigoStatusPedido, 
         CodigoStatusPedidoInterno, IdentificadorExterno,
         ProtocoloPedido, ResumoSolicitacao, Slug, DetalhamentoSolicitacao,
         DataRegistro, FoiProrrogado, OrgaoDestinatario,
         OrgaoSuperiorAssociadoaoDestinatario) %>%
  mutate(Anonimo = 0, 
         Ativo = 1,
         Criacao = "NULL",
         Alteracao = "NULL") %>%
  rename(Protocolo = ProtocoloPedido,
         Titulo = ResumoSolicitacao,
         Descricao = DetalhamentoSolicitacao,
         DataEnvio = DataRegistro)

# gerando slug (url que vai aparecer no site)
new_names <- cgu_sem_recursos$ResumoSolicitacao %>%
  gsub("'", "", .) %>%
  gsub("\"", "", .) %>%
  gsub("%", "percent", .) %>%
  gsub("^[ ]+", "", .) %>%
  make.names(.) %>% 
  gsub("[.]+", "-", .) %>% 
  gsub("[_]+", "-", .) %>% 
  tolower(.) %>% 
  gsub("_$", "", .) %>%
  stringi::stri_trans_general("latin-ascii")

# melhorar isso
dupe_count <- sapply(1:length(new_names), function(i) {
  sum(new_names[i] == new_names[1:i])
})

new_names[dupe_count > 1] <- paste(new_names[dupe_count > 
                                               1], dupe_count[dupe_count > 1], sep = "_")

cgu_sem_recursos_sql <- cgu_sem_recursos_sql %>%
  mutate( Slug = new_names,
          FoiProrrogado = as.numeric(FoiProrrogado=="sim"))

# removendo colunas desnecessárias
cgu_sem_recursos_sql_final <- cgu_sem_recursos_sql %>%
  select(-OrgaoDestinatario, -OrgaoSuperiorAssociadoaoDestinatario) %>%
  mutate(IdentificadorExterno = "NULL",
         Criacao = "NULL",
         Alteracao = "NULL",
         DataEnvio = gsub("/", "-", DataEnvio),
         CodigoStatusPedido = ifelse(CodigoStatusPedido == "Acesso Concedido",
                                     1,
                                     ifelse(CodigoStatusPedido == "Acesso Parcialmente Concedido",3,
                                            ifelse(CodigoStatusPedido == "Acesso Negado",2, 4))))

# salvando arquivo, para ser importado pelo SQL
write.table(cgu_sem_recursos_sql_final, file="pedidos_cgu_2017_sem_recurso.csv", sep=";",
            fileEncoding = "UTF-8", row.names=F)

amostra <- cgu_sem_recursos_sql_final %>%
  head()

aaa <- fread("sample.csv")

write.table(amostra, file="sample.csv", sep=";",
            fileEncoding = "UTF-8", row.names=F)

