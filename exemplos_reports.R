### Exemplos de reports

setwd("/Users/natalia/Documents/Manoel/LAI/projeto_git/20160720_pedidos_csv_2016")
url_dirve <- "https://docs.google.com/spreadsheets/d/1uIz0YdkhpJ7jKoIDyfT7woOGdJBjt1L4OU0ar2r3zAA/edit?usp=sharing"

library(googlesheets)
library(dplyr)
library(readr)
library(devtools)
library(ggplot2)
library(tidyr)

#dev_mode(on=T)

# devtools::install_github("jennybc/googlesheets")
# use dev ggplot2 now

# when finished do:

#dev_mode(on=F) 


gs_ls()
gcm_sheet <- gs_title("CGM Pedido Respondido 206160610")

gcm_resposta <- gcm_sheet %>%
  gs_read(ws = "3a. PUBLICADO_OK")

head(gcm_resposta)
dim(gcm_resposta)

View(gcm_resposta)

gcm_resposta <- gcm_resposta %>%
  rename(id = cd_pedido)

temp <- gcm_resposta %>%
  filter(grepl("Encaminhamento", status_nome))

View(filter(gcm_resposta,  id == 1320))
View(temp)
## Reports

## 1. top orgãos por num pedidos

top_orgaos_geral <- gcm_resposta %>%
  mutate(total_pedidos = n_distinct(id)) %>%
  group_by(orgao_nome) %>%
  summarise(num_pedidos = n_distinct(id),
            total = max(total_pedidos),
            perc_total = round(num_pedidos/total, 2)) %>%
  select(-3) %>%
  arrange(desc(num_pedidos))

View(top_orgaos_geral)

# chart

top_orgaos_geral %>%
  filter(num_pedidos > 50) %>%
  mutate( orgao_sigla = gsub(" -.*", "", orgao_nome)) %>%
  ggplot(aes(x=reorder(orgao_sigla, -num_pedidos), y=num_pedidos)) + 
  geom_bar(stat = "identity") + coord_flip() + xlab("")



## status
top_status <- gcm_resposta %>%
  filter(dc_resposta != "Finalizado") %>%
  group_by(id) %>%
  summarise(status_nome = last(status_nome)) %>%
  mutate(total_pedidos = n_distinct(id)) %>%
  group_by(status_nome) %>%
  summarise(num_pedidos = n_distinct(id),
            total = max(total_pedidos),
            perc_total = round(num_pedidos/total, 2)) %>%
  select(-3) %>%
  arrange(desc(num_pedidos))

View(top_status)

### Tempo por pedido

gcm_pedido <- gcm_resposta %>%
  filter(dc_resposta == "Pedido Registrado no Sistema") 

gcm_resposta_final <- gcm_resposta %>%
  filter(dc_resposta != "Finalizado") %>%
  mutate(dt_resposta_atendimento = as.Date(dt_resposta_atendimento)) %>%
  group_by(id) %>%
  summarise(status_nome = last(status_nome, 
                               order_by = dt_resposta_atendimento)) %>%
  inner_join(select(gcm_resposta, c(1, 3, 7)), by=c("id", "status_nome"))


View(gcm_resposta_final)

gcm_pedido_resp <- gcm_pedido %>%
  inner_join(gcm_resposta_final, by="id") %>%
  filter( id > 3 & id != 5 & dc_pedido != "Teste" & dc_pedido != "teste") %>%
  rename(data_pedido = dt_resposta_atendimento.x,
         data_resposta_final = dt_resposta_atendimento.y,
         status_final = status_nome.y) %>%
  mutate(tempo_decorrido = as.Date(data_resposta_final) - as.Date(data_pedido),
         tempo_decorrido1 = as.numeric(tempo_decorrido))


View(gcm_pedido_resp)

## prazo médio pra um pedido ser respondido total por órgão

tempo_orgao <- gcm_pedido_resp %>%
  group_by(orgao_nome) %>%
  summarise(media = mean(tempo_decorrido),
            mediana = median(tempo_decorrido),
            maximo = max(tempo_decorrido),
            minimo = min(tempo_decorrido),
            first_q = quantile(tempo_decorrido, .25),
            third_q = quantile(tempo_decorrido, .75),
            size_sample = n()) %>%
  arrange(media)

View(tempo_orgao)
