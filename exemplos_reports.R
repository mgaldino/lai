### Exemplos de reports

setwd("/Users/natalia/Documents/Manoel/LAI/projeto_git/20160720_pedidos_csv_2016")

gcm_pedidos <- read.table("20160720_Pedidos_txt_2016.txt", 
                          sep="\t", header=F, fileEncoding = "UTF-16")

gcm_resposta <- read.table("PedidoRespondido_eSIC_VFinal.txt", 
                          sep="\t", header=F, fileEncoding = "UTF-16")
  
head(gcm_pedidos)
names(gcm_pedidos) <- c("id", "codigo", "orgao", "orgao_especifico", 
                        "situacao", "data_pedido", "data_resposta",
                        "x1", "x2", "fonte_pedido", "meio_pedido",
                        "x3", "area", "area_especifica", "x4", 
                        "data_x", "status", "obs")

View(gcm_pedidos)

library(dplyr)
library(ggplot2)

## Reports

## 1. top orgãos por num pedidos

top_orgaos_geral <- gcm_pedidos %>%
  mutate(total_pedidos = n()) %>%
  group_by(orgao) %>%
  summarise(num_pedidos = n(),
            total = max(total_pedidos),
            perc_total = round(num_pedidos/total, 2)) %>%
  select(-3) %>%
  arrange(desc(num_pedidos))

top_orgaos_especifico <- gcm_pedidos %>%
  mutate(total_pedidos = n()) %>%
  group_by(orgao_especifico) %>%
  summarise(num_pedidos = n(),
            total = max(total_pedidos),
            perc_total = round(num_pedidos/total, 2)) %>%
  select(-3) %>%
  arrange(desc(num_pedidos))

View(top_orgaos_especifico)

