library(dplyr)
library(glue)
library(Microsoft365R)

ano <- 2026

sps <- get_sharepoint_site(
  site_url = "https://senacnacional.sharepoint.com/sites/GerProspecAvalEducacional",
  tenant = "common",
  app = ""
)

sharepoint <- sps$get_drive()

caminho <- glue("03 - Pesquisas/01. PNAES/{ano}")

cadastro <- sharepoint$load_rds(glue("{caminho}/5. Bases/1. População/População Alvo.rds"))
 
cadastro %>%
  group_by(DR2, semestre) %>%
  summarise(pop_a = n(),
            pop_p = sum(tem.contato)) %>%
    mutate(tx = pop_p/pop_a) %>%
  sharepoint$save_rds(
    glue("00 - Area de Influencia/pnaes/dados_p.Rds")
    )


dados_p <- sharepoint$load_rds(glue("00 - Area de Influencia/pnaes/dados_p.Rds")) %>% 
  #filter(semestre == 1) %>% 
  mutate(DR2 = DR2,
         Total = pop_a, 
         semestre = semestre,
         .keep = "none")

pesquisa <- sharepoint$load_rds(glue("{caminho}/5. Bases/2. Pesquisa/3. rds/2. Base.consistida Completa.rds")) 

pesquisa <- pesquisa %>% 
  #left_join(drs, by = "cpf") %>% 
  left_join(dados_p, by = c("DR2", "semestre"))

# Base que vai ser utilizada na aplicação Shiny de acompanhamento da coleta da PNAES
dash <- pesquisa %>% 
  select(dia,
         tempo,
         sit.ocup,
         semestre,
         dt.conclusao,
         DR = DR2,
         Total,
         tp.aparelho)



cadastro %>% 
  mutate(cpf = as.double(cpf)) %>%
  add_count(DR2, name = "Total") %>% select(DR2, cpf, Total) %>% sharepoint$save_rds(glue("00 - Area de Influencia/pnaes/pop2.Rds"))
