###MAPA DA TRANSPARÊNCIA

library(tidyverse)
library(data.table)
library(janitor)

abraji <- fread("Abraji_versao_renata.csv") %>%
  clean_names()
head(abraji)

cgu_pedidos <- fread("cgu_pedido.txt", verbose=T) %>%
  clean_names()

# algumas linhas deram problema
# excluindo estas linhas
cgu_pedidos1 <- cgu_pedidos %>%
  filter(!idpedido %in%  c(538347, 538351, 538355)) %>%
  mutate(tipo1 = "resposta")

recursos <- fread("recursos_v2.txt") %>%
  clean_names()

cgu <- fread("editada para template.txt") %>%
  clean_names()

abraji <- abraji %>%
  mutate(tipo = "pedido") %>%
  left_join(dplyr::select(cgu_pedidos1, IdPedido, FoiProrrogado, ProtocoloPedido, DataHoraResposta, Resposta),
            by = c("Protocolo" = "ProtocoloPedido")) %>%
  left_join(dplyr::select(recursos, IdPedido, ProtocoloPedido, Instancia, DescRecurso, DataRegistro,RespostaRecurso, DataResposta),
            by = "IdPedido")


# Título da Solicitação # repete
# descricao_do_pedido -> Resposta e -> DescRecurso e -> RespostaRecurso
# data_de_abertura -> DataHoraResposta & DataRegistro e -> DataResposta
# tipo de interação: pedido, resposta, recurso, resposta recurso etc.
# FoiProrrogado repete
# orgao repete
# status repete
# situacao repete
# uf repete
# municipio repete
# esfera repete
# poder repete
# última parte do path de folder_path repete, se não tiver anexo é missing
# Name por interação, e separado por ";"

abraji1 <- abraji %>%
  dplyr::select(protocolo, titulo_da_solicitacao, descricao_do_pedido,
                 data_de_abertura, foi_prorrogado, orgao, status, uf, municipio, esfera,
               poder) %>%
  rename(data = data_de_abertura) %>%
  mutate(tipo_de_interacao = "pedido",
         idpedido = NA)

## preparando respostas para rbind

cgu_pedidos2 <- cgu_pedidos1 %>%
  mutate(titulo_da_solicitacao = NA,
         foi_prorrogado = NA,
         orgao = NA,
         status = NA,
         uf = NA,
         municipio = NA,
         esfera = NA,
         poder = NA,
         tipo_de_interacao = "resposta") %>%
  dplyr::select(protocolopedido, titulo_da_solicitacao, resposta,
                datahoraresposta, foi_prorrogado, orgao, status, uf, 
                municipio, esfera, poder,tipo_de_interacao , idpedido) %>%
  rename(protocolo = protocolopedido,
         data = datahoraresposta,
         descricao_do_pedido = resposta)

# criando tabela com tipo de interação  
recursos1 <- recursos %>%
  select(idpedido, protocolopedido, descrecurso, respostarecurso, instancia) %>%
  gather(tipo_de_interacao ,descricao_do_pedido ,-c(idpedido, protocolopedido, instancia)) %>%
  mutate(tipo_de_interacao = ifelse(tipo_de_interacao == "descrecurso" ,
                                    "recurso", "resposta do recurso"))
# criando tabela com data
recursos2 <- recursos %>%
  select(idpedido, protocolopedido, dataregistro, dataresposta) %>%
  gather(tipo_de_interacao, data , -c(idpedido, protocolopedido)) %>%
  mutate(tipo_de_interacao = ifelse(tipo_de_interacao == "dataregistro" ,
                                    "recurso", "resposta do recurso"))

recursos3 <- recursos1 %>%
  inner_join(recursos2, by=c("idpedido", "protocolopedido", "tipo_de_interacao")) %>%
  filter(!is.na(idpedido)) %>%
  mutate(tipo_de_interacao = paste(tipo_de_interacao, tolower(instancia), sep=" - ")) %>%
  select(-instancia)

recursos4 <- recursos3 %>%
  mutate(titulo_da_solicitacao = NA,
         foi_prorrogado = NA,
         orgao = NA,
         status = NA,
         uf = NA,
         municipio = NA,
         esfera = NA,
         poder = NA,
  ) %>%
  dplyr::select(protocolopedido, titulo_da_solicitacao, descricao_do_pedido,
                data, foi_prorrogado, orgao, status, uf, 
                municipio, esfera, poder, tipo_de_interacao , idpedido) %>%
  rename(protocolo = protocolopedido)


abraji_aux <- abraji %>%
  dplyr::select(protocolo) %>%
  mutate(aux = "sim")

abraji_final <- bind_rows(abraji1, cgu_pedidos2, recursos4) %>%
  inner_join(abraji_aux, by="protocolo") %>%
  group_by(protocolo) %>%
  mutate(titulo_da_solicitacao = max(titulo_da_solicitacao, na.rm=T),
                  foi_prorrogado = max(foi_prorrogado, na.rm=T),
                  orgao = max(orgao, na.rm=T),
                  status = max(status, na.rm=T),
                  uf = max(uf, na.rm=T),
                  municipio = max(municipio, na.rm=T),
                  esfera = max(esfera, na.rm=T),
                  poder = max(poder, na.rm=T))

valid <- abraji_final %>%
  filter(poder != "Federal")

valid1 <- abraji %>%
  filter(protocolo == "2680000866201791")

abraji_final %>%
  summarise(num_pedidos = n_distinct(protocolo),
            num_)
