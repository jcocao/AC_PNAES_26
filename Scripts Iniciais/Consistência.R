rm(list = ls())
cat("\f")
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)
library(Microsoft365R)
library(glue)
options(scipen = 15)

# Caminho base dos arquivos

ano <- 2026

sharepoint <- get_sharepoint_site(site_url = "https://senacnacional.sharepoint.com/sites/GerProspecAvalEducacional",
                                  tenant = "common",
                                  app = "")

drive <- sharepoint$get_drive()

pop.pesq <- readRDS(glue("01. PNAES/{ano}/5. Bases/1. População/População_Pesquisa_Estrato.rds"))

###############################################################################.
# 0. Leitura da base ------------------------------------------------------


pesquisa <- bind_rows(base_1sem, base_2sem) %>% 
  arrange(cpf, desc(semestre)) %>% 
  group_by(cpf) %>% 
  mutate(id = row_number()) %>% 
  filter(id %in% 1) %>% 
  select(-id) %>% 
  ungroup()

rm(base_1sem, base_2sem)

# Trazendo variáveis da população de pesquisa
pesquisa <- pesquisa %>% 
  left_join(pop.pesq %>% 
              select(cpf, DR2, regiao2, nucleo2, cod.mun.res,
                     Modalidade, TipoCurso, Eixo, Segmento, mod.recurso) %>% 
              mutate(cpf = as.numeric(cpf)),
            by = "cpf")

# Renomeando as variáveis de categorias "Outro" e "Não lembro/Prefiro não responder"

pesquisa <- pesquisa %>% 
  rename(v1.02.90 = v1.02.9,
         v1.08.90 = v1.08.6,
         v4.10.99 = v4.10.5,
         v4.11.99 = v4.11.5,
         s.02.90 = s.02.7,
         s.02.99 = s.02.8,
         s.03.99 = s.03.5,
         s.04.90 = s.04.7,
         s.04.99 = s.04.8,
         s.05.90 = s.05.12,
         s.06.90 = s.06.8,
         s.06.99 = s.06.9,
         s.09.90 = s.09.12,
         s.10.90 = s.10.8,
         s.10.99 = s.10.9,
         s.15.99 = s.15.6)

###############################################################################.
# 1. Consistência das abertas ------------------------------------------------
 
#Não se aplica


###############################################################################.
# 2. Verificação dos Fluxos -----------------------------------------------

# 2.0 Erro - 00 -----------------------------------------------------------
# Termo == Não, invalida o questionário

pesquisa <- pesquisa %>% 
  mutate(across(c(starts_with("v1"),
                  starts_with("v2"),
                  starts_with("v3"),
                  starts_with("v4"),
                  "comentario",
                  starts_with("s.")), ~ case_when(termo %in% "Não" ~ NA,
                                                  T ~ .x)))

# Retirando as observações com termo == "Não"
pesquisa <- pesquisa %>% 
  filter(!termo %in% "Não")

# 2.1 Erro - 01 -----------------------------------------------------------
# Curso que não forma para uma ocupação e v1.02.1 == 1

pesquisa <- pesquisa %>%
  mutate(e01 = case_when(curso_ocup %in% "Não" & v1.02.1 == 1 ~ 1,
                         .default = 0),
         v1.02.1 = case_when(e01 == 1 ~ 0, T ~ v1.02.1),
         v1.02.1 = case_when(curso_ocup %in% "Não" ~ NA, T ~ v1.02.1))

pesquisa %>% count(aprendizagem, e01, curso_ocup, v1.02.1)

# 2.2 Erro - 02 -----------------------------------------------------------
# Curso que forma para uma ocupação e v1.02.4 == 1

pesquisa <- pesquisa %>%
  mutate(e02 = case_when(curso_ocup %in% "Sim" & v1.02.4 == 1 ~ 1,
                         .default = 0),
         v1.02.4 = case_when(e02 == 1 ~ 0, T ~ v1.02.4),
         v1.02.4 = case_when(curso_ocup %in% "Sim" ~ NA, T ~ v1.02.4))

pesquisa %>% count(aprendizagem, e02, curso_ocup, v1.02.4)

# 2.3 Erro - 03 -----------------------------------------------------------
# Inicio o curso sem esta trabalhando - v1.01 = Nao.
# Não pode marcar 3,4 e 5 na v1.02.01

pesquisa <- pesquisa %>%
  mutate(e03 = case_when(!v1.01 %in% "Sim" & v1.02.3 == 1 ~ 1,
                         !v1.01 %in% "Sim" & v1.02.4 == 1 ~ 1,
                         !v1.01 %in% "Sim" & v1.02.5 == 1 ~ 1,
                         .default = 0),
         v1.02.3 = case_when(e03 == 1 ~ 0, T ~ v1.02.3),
         v1.02.4 = case_when(e03 == 1 ~ 0, T ~ v1.02.4),
         v1.02.5 = case_when(e03 == 1 ~ 0, T ~ v1.02.5)) %>% 
  mutate(v1.02.3 = case_when(!v1.01 %in% "Sim" ~ NA, T ~ v1.02.3),
         v1.02.4 = case_when(!v1.01 %in% "Sim" ~ NA, T ~ v1.02.4),
         v1.02.5 = case_when(!v1.01 %in% "Sim" ~ NA, T ~ v1.02.5))

pesquisa %>% count(e03, aprendizagem, v1.01, v1.02.3, v1.02.4, v1.02.5)

###############################################################################.
# 2.4 Erro - 04 -----------------------------------------------------------
# Objetivo pra realizar o curso era não profissional (se selecionada apenas esta opção)
# Não deve responder as questões: v1.02a:v1.12.2

# Verifica casos com erro
pesquisa <- pesquisa %>%
  mutate(bl1.exc = case_when(v1.02.8 %in% 1 & 
                               rowSums(across(c(v1.02.1:v1.02.7, 
                                                v1.02.90, v1.02.98)), na.rm = TRUE) == 0 ~ 1, 
                             is.na(v1.02.8) ~ NA,
                             T ~ 0)) %>% 
  mutate(e04 = case_when(bl1.exc %in% 1 & 
                           (!is.na(v1.03)|!is.na(v1.04)|!is.na(v1.05)|!is.na(v1.06)|
                              !is.na(v1.07.1)|!is.na(v1.07.2)|!is.na(v1.07.3)|
                              !is.na(v1.07.4)|!is.na(v1.07.5)|!is.na(v1.07.6)|
                              !is.na(v1.07.7)|!is.na(v1.07.8)|!is.na(v1.07.9)|
                              !is.na(v1.12.1)|!is.na(v1.12.2)) ~ 1, T ~ 0))

pesquisa %>% count(e04)
pesquisa %>% filter(e04 %in% 1) %>% count(v1.02.1, v1.02.2, v1.02.3, v1.02.4, v1.02.5, v1.02.6,
                                          v1.02.7, v1.02.8, v1.02.90, v1.02.98, v1.02a)
pesquisa %>% filter(e04 %in% 1) %>% select(v1.01:v1.12.2) %>% View()

# Corrigindo casos com erro
pesquisa <- pesquisa %>% 
  mutate(across(c(v1.03:v1.07.9, v1.12.1, v1.12.2), ~ case_when(e04 %in% 1 ~ NA,
                                                                T ~ .x))) %>% 
  select(-bl1.exc)

# Confere a correção
pesquisa %>% filter(e04 == 1) %>%
  select(termo:v1.12.2) %>% 
  View()

###############################################################################.
# Imputação - v1.03 (Principal objetivo profissional) ------------------------
# Se rowSums(v1.02.1 a v1.02.7) = 1, não responderia a v1.03, pois o objetivo
# assinalado na v1.02 seria imputado na v1.03

pesquisa %>% count(v1.03)

pesquisa <- pesquisa %>%
  mutate(input.v1.03 = rowSums(across(c(v1.02.1:v1.02.7)), na.rm = TRUE)) %>%
  mutate(v1.03 = ifelse(input.v1.03 %in% "1" & v1.02.1 %in% "1", "Ingressar numa carreira/área específica",
                        ifelse(input.v1.03 %in% "1" & v1.02.2 %in% "1", "Ser mais competitivo no mercado de trabalho",
                               ifelse(input.v1.03 %in% "1" & v1.02.3 %in% "1", "Melhorar o desempenho no trabalho que exercia quando iniciou o curso",
                                      ifelse(input.v1.03 %in% "1" & v1.02.4 %in% "1", "Mudar de trabalho",
                                             ifelse(input.v1.03 %in% "1" & v1.02.5 %in% "1", "Ser promovido",
                                                    ifelse(input.v1.03 %in% "1" & v1.02.6 %in% "1", "Conseguir um trabalho com carteira assinada",
                                                           ifelse(input.v1.03 %in% "1" & v1.02.7 %in% "1", "Trabalhar por conta própria/montar um negócio próprio", v1.03)))))))) %>%
  select(-input.v1.03)

pesquisa %>% count(v1.03)

###############################################################################.
# 2.5 Erro - 05 -----------------------------------------------------------
# Informou o objetivo princical na v1.03 mas ela não foi marcada na v1.02 ou
# v1.02 é marcada apenas "Outro", então não deve ter resposta na v1.03

pesquisa <- pesquisa %>%
  mutate(e05 = case_when(rowSums(across(v1.02.1:v1.02.98), na.rm = TRUE) == 1 & 
                           (v1.02.8 == 1 | v1.02.90 == 1 | v1.02.98 == 1) & 
                           !is.na(v1.03) ~ 1, 
                         T ~ 0)) %>%
  mutate(v1.03 = case_when(e05 == 1 ~ NA, T ~ v1.03))

pesquisa %>% count(e05)

pesquisa %>% filter(e05 %in% 1) %>% select(v1.02:v1.12.2) %>% View()

check <- pesquisa %>%
  mutate(v1.03.1 = ifelse(v1.03 %in% "Ingressar numa carreira/área específica", 1, 0),
         v1.03.2 = ifelse(v1.03 %in% "Ser mais competitivo no mercado de trabalho", 1, 0),
         v1.03.3 = ifelse(v1.03 %in% "Melhorar o desempenho no trabalho que exercia quando iniciou o curso", 1, 0),
         v1.03.4 = ifelse(v1.03 %in% "Mudar de trabalho", 1, 0),
         v1.03.5 = ifelse(v1.03 %in% "Ser promovido", 1, 0),
         v1.03.6 = ifelse(v1.03 %in% "Conseguir um trabalho com carteira assinada", 1, 0),
         v1.03.7 = ifelse(v1.03 %in% "Trabalhar por conta própria/montar um negócio próprio", 1, 0)) %>%
  select(cpf, v1.02.1, v1.02.2, v1.02.3, v1.02.4, v1.02.5, v1.02.6, v1.02.7,
         v1.03.1, v1.03.2, v1.03.3, v1.03.4, v1.03.5, v1.03.6, v1.03.7) %>%
  pivot_longer(!cpf,
               names_to = c(".value", "rep"),
               names_pattern = "v1\\.(\\d{2})\\.(\\d+)") %>%
  rename(v1.02 = `02`, v1.03 = `03`) %>%
  mutate(v1.02 = replace_na(v1.02, 0)) %>% 
  mutate(erro = ifelse(v1.03 == 1 & v1.02 == 0, 1, 0))

check %>% count(erro)

pesquisa %>% 
  left_join(check %>% 
              filter(erro %in% 1) %>% 
              select(cpf, erro), 
            by = "cpf") %>% 
  filter(erro %in% 1) %>% 
  select(v1.01:v1.12.2) %>% 
  View()

pesquisa <- pesquisa %>% 
  left_join(check %>% 
              filter(erro %in% 1) %>% 
              select(cpf, erro), 
            by = "cpf") %>% 
  mutate(v1.03 = case_when(erro %in% 1 ~ NA, T ~ v1.03)) %>% 
  select(-erro)

rm(check)

###############################################################################.
# 2.6 Erro - 06 -----------------------------------------------------------
# Se marcou APENAS Outros na v1.02, entao, v1.03 e v1.04 em branco

# Verifica casos com erro

pesquisa <- pesquisa %>%
  mutate(e06 = case_when(rowSums(across(v1.02.1:v1.02.98), na.rm = TRUE) == 1 & 
                           (v1.02.90 == 1 | v1.02.98 == 1) &
                           (!is.na(v1.03) | rowSums(across(v1.04.1:v1.04.8)) > 0) ~ 1, T ~ 0))

pesquisa %>% count(e06)

pesquisa %>% filter(e06 == 1) %>% select(v1.02.1:v1.04.8) %>% View()

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v1.03:v1.04.8),
                ~case_when(e06 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e06 == 1) %>% select(e06, v1.02.1:v1.12.2) %>% View()

check <- pesquisa %>%
  select(cpf, v1.02.1, v1.02.2, v1.02.3, v1.02.4, v1.02.5, v1.02.6, v1.02.7,
         v1.04.1, v1.04.2, v1.04.3, v1.04.4, v1.04.5, v1.04.6, v1.04.7) %>%
  pivot_longer(!cpf,
               names_to = c(".value", "rep"),
               names_pattern = "v1\\.(\\d{2})\\.(\\d+)") %>%
  rename(v1.02 = `02`, v1.04 = `04`) %>%
  mutate(v1.04 = ifelse(is.na(v1.04), 0, v1.04)) %>%
  mutate(v1.02 = replace_na(v1.02, 0)) %>% 
  mutate(erro = ifelse(v1.04 == 1 & v1.02 == 0, 1, 0))

check %>% count(erro)

rm(check)

###############################################################################.
# 2.7 Erro - 07 -----------------------------------------------------------
# Se marcou v1.04.8 = 1, então v1.04.1 a v1.04.7 deve ser 0 ou NA.

pesquisa <- pesquisa %>%
  mutate(e07 = ifelse(rowSums(across(v1.04.1:v1.04.7), na.rm = TRUE) > 0 & 
                        v1.04.8 %in% 1, 1, 0))

pesquisa %>% count(e07)

pesquisa %>% count(v1.04, v1.04.1, v1.04.2, v1.04.3, v1.04.4,
                   v1.04.5, v1.04.6, v1.04.7, v1.04.8) %>% View()

###############################################################################.
# Aplicando tratamento nas variáveis de v1.03 a v1.04.8 -----------------------
# Se marcado apenas o "Outro" na v1.02, então v1.03:v1.04.8 tem que ser NA

pesquisa <- pesquisa %>%
  mutate(across(c(v1.03:v1.04.8),
                ~case_when(aprendizagem %in% "Não" & 
                             rowSums(across(v1.02.1:v1.02.7), na.rm = T) %in% 0 &
                             (v1.02.90 %in% 1 | v1.02.98 %in% 1) ~ NA,
                           T ~ .x)))

###############################################################################.
# Aplicando tratamento nas variáveis de v1.04.1 a v1.04.8 -----------------------
# Se questionário terminado & v1.02.1:v1.02.7 == 1 & v1.07.1:v1.07.7 !NA, imputar valor na v1.04.1:v1.04.7

pesquisa <- pesquisa %>%
  mutate(v1.04.1 = ifelse(sit.quest %in% "Terminado" & v1.02.1 == 1 & v1.04.1 == 0 & !is.na(v1.07.1) & v1.04.8 == 0, 1, v1.04.1),
         v1.04.2 = ifelse(sit.quest %in% "Terminado" & v1.02.2 == 1 & v1.04.2 == 0 & !is.na(v1.07.2) & v1.04.8 == 0, 1, v1.04.2),
         v1.04.3 = ifelse(sit.quest %in% "Terminado" & v1.02.3 == 1 & v1.04.3 == 0 & !is.na(v1.07.3) & v1.04.8 == 0, 1, v1.04.3),
         v1.04.4 = ifelse(sit.quest %in% "Terminado" & v1.02.4 == 1 & v1.04.4 == 0 & !is.na(v1.07.4) & v1.04.8 == 0, 1, v1.04.4),
         v1.04.5 = ifelse(sit.quest %in% "Terminado" & v1.02.5 == 1 & v1.04.5 == 0 & !is.na(v1.07.5) & v1.04.8 == 0, 1, v1.04.5),
         v1.04.6 = ifelse(sit.quest %in% "Terminado" & v1.02.6 == 1 & v1.04.6 == 0 & !is.na(v1.07.6) & v1.04.8 == 0, 1, v1.04.6),
         v1.04.7 = ifelse(sit.quest %in% "Terminado" & v1.02.7 == 1 & v1.04.7 == 0 & !is.na(v1.07.7) & v1.04.8 == 0, 1, v1.04.7))


###############################################################################.
# 2.8 Erro - 08 -----------------------------------------------------------
# Respostas da v1.07.x tem q está na v1.04

# Verifica casos com erro
pesquisa <- pesquisa %>%
  mutate(e08 = case_when(!is.na(v1.07.1) & !v1.04.1 %in% "1" ~ 1,
                         !is.na(v1.07.2) & !v1.04.2 %in% "1" ~ 1,
                         !is.na(v1.07.3) & !v1.04.3 %in% "1" ~ 1,
                         !is.na(v1.07.4) & !v1.04.4 %in% "1" ~ 1,
                         !is.na(v1.07.5) & !v1.04.5 %in% "1" ~ 1,
                         !is.na(v1.07.6) & !v1.04.6 %in% "1" & v1.05 %in% "Sim" ~ 1,
                         !is.na(v1.07.7) & !v1.04.7 %in% "1" ~ 1,
                         .default = 0))

pesquisa %>% count(e08)

pesquisa %>% filter(e08 == 1) %>% select(sit.quest, v1.02.1:v1.04.8, v1.05, v1.07.1:v1.07.7) %>% View()

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(v1.07.1 = ifelse(!is.na(v1.07.1) & !v1.04.1 %in% "1", NA, v1.07.1),
         v1.07.2 = ifelse(!is.na(v1.07.2) & !v1.04.2 %in% "1", NA, v1.07.2),
         v1.07.3 = ifelse(!is.na(v1.07.3) & !v1.04.3 %in% "1", NA, v1.07.3),
         v1.07.4 = ifelse(!is.na(v1.07.4) & !v1.04.4 %in% "1", NA, v1.07.4),
         v1.07.5 = ifelse(!is.na(v1.07.5) & !v1.04.5 %in% "1", NA, v1.07.5),
         v1.07.6 = ifelse(!is.na(v1.07.6) & !v1.04.6 %in% "1", NA, v1.07.6),
         v1.07.7 = ifelse(!is.na(v1.07.7) & !v1.04.7 %in% "1", NA, v1.07.7))

pesquisa %>% filter(e08 == 1) %>% select(sit.quest, v1.02.1:v1.04.8, v1.07.1:v1.07.7) %>% View()

###############################################################################.
# 2.9 Erro - 09 -----------------------------------------------------------
# v1.04.8 = 1, então NA em v1.07.1 a v1.07.7

# Verifica casos com erro
pesquisa <- pesquisa %>%
  mutate(e09 = case_when((!is.na(v1.07.1) | !is.na(v1.07.2) | !is.na(v1.07.3) | !is.na(v1.07.4) |
                            !is.na(v1.07.5) | !is.na(v1.07.7)) & v1.04.8 %in% "1" ~ 1,
                         !is.na(v1.07.6) & v1.05 %in% "Sim" & v1.04.8 %in% "1" ~ 1,
                         .default = 0))

pesquisa %>% count(e09)

###############################################################################.
# 2.10 Erro - 10 -----------------------------------------------------------
# v1.05 != "Sim" & !is.na(v1.07.8)

# Verifica casos com erro
pesquisa <- pesquisa %>%
  mutate(e10 = case_when(!is.na(v1.07.8) & !v1.05 %in% "Sim" ~ 1,
                         .default = 0))

pesquisa %>% count(e10)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(v1.07.8 = ifelse(e10 %in% "1", NA, v1.07.8))

pesquisa %>% filter(e10 == 1) %>% select(v1.05, v1.07.8) %>% View()

###############################################################################.
# 2.11 Erro - 11 -----------------------------------------------------------
# v1.06 != "Sim" & !is.na(v1.07.9)

# Verifica casos com erro
pesquisa <- pesquisa %>%
  mutate(e11 = case_when(!is.na(v1.07.9) & !v1.06 %in% "Sim" ~ 1,
                         .default = 0))

pesquisa %>% count(e11)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(v1.07.9 = ifelse(e11 %in% "1", NA, v1.07.9))

pesquisa %>% filter(e11 == 1) %>% select(e11, v1.06, v1.07.9) %>% View()

###############################################################################.
# Aplicando tratamento nas variáveis de v1.01 a v1.07.9 -----------------------
# Se aprendizagem %in% "Sim", estas variáveis viram NA

pesquisa <- pesquisa %>%
  mutate(across(c(v1.01:v1.07.9),
                ~case_when(aprendizagem %in% "Sim" ~ NA,
                           T ~ .x)))

###############################################################################.
# Imputação - v1.08 (Principal objetivo profissional - Aprendizagem) ----------
# Se rowSums(v1.08.1 a v1.08.5) = 1, não responderia a v1.09, pois o objetivo
# assinalido na v1.08 seria imputado na v1.09

pesquisa %>% filter(aprendizagem %in% "Sim") %>% count(v1.09)

pesquisa <- pesquisa %>%
  mutate(input.v1.09 = rowSums(across(v1.08.1:v1.08.5), na.rm = T)) %>%
  mutate(v1.09 = ifelse(input.v1.09 == 1 & v1.08.1 == 1, "Aprender uma profissão",
                        ifelse(input.v1.09 == 1 & v1.08.2 == 1, "Ter uma renda fixa",
                               ifelse(input.v1.09 == 1 & v1.08.3 == 1, "Adquirir experiência profissional",
                                      ifelse(input.v1.09 == 1 & v1.08.4 == 1, "Conseguir o 1º emprego com carteira assinada",
                                             ifelse(input.v1.09 == 1 & v1.08.5 == 1, "Conciliar trabalho e estudo", v1.09)))))) %>%
  select(-input.v1.09)

pesquisa %>% filter(aprendizagem %in% "Sim") %>% count(v1.09)

###############################################################################.
# 2.12 Erro - 12 -----------------------------------------------------------
# Se v1.08 for somente "Outro", não deve ter resposta na v1.09
# Além disso, checar se informou o objetivo principal na v1.09 mas ela não foi marcada na v1.08

pesquisa <- pesquisa %>%
  mutate(e12 = ifelse(rowSums(across(v1.08.1:v1.08.90), na.rm = TRUE) %in% "1" & v1.08.90 %in% "1" & !is.na(v1.09), 1, 0)) %>%
  mutate(v1.09 = if_else(e12 == 1, NA, v1.09))

pesquisa %>% count(e12)

check <- pesquisa %>%
  filter(aprendizagem %in% "Sim") %>%
  mutate(v1.09.1 = ifelse(v1.09 %in% "Aprender uma profissão", 1, 0),
         v1.09.2 = ifelse(v1.09 %in% "Ter uma renda fixa", 1, 0),
         v1.09.3 = ifelse(v1.09 %in% "Adquirir experiência profissional", 1, 0),
         v1.09.4 = ifelse(v1.09 %in% "Conseguir o 1º emprego com carteira assinada", 1, 0),
         v1.09.5 = ifelse(v1.09 %in% "Conciliar trabalho e estudo", 1, 0)) %>%
  select(cpf, v1.08.1:v1.08.5, v1.09.1:v1.09.5) %>%
  pivot_longer(!cpf,
               names_to = c(".value", "rep"),
               names_pattern = "v1\\.(\\d{2})\\.(\\d+)") %>%
  rename(v1.08 = `08`, v1.09 = `09`) %>%
  mutate(erro = ifelse(v1.09 == 1 & v1.08 == 0, 1, 0))

check %>% count(erro)

rm(check)

###############################################################################.
# 2.13 Erro - 13 ----------------------------------------------------------
# Só pode responder v1.11 quem marcou Nao va v1.10

pesquisa <- pesquisa %>%
  mutate(e13 = case_when(!v1.10 %in% "Não" & !is.na(v1.11) ~ 1,
                         .default = 0))

pesquisa %>% count(e13)

###############################################################################.
# 2.14 Erro - 14 ----------------------------------------------------------
# v1.12 não pode ser NA para quem terminou o questionário e respondeu
# v1.02.8 != 1

# Verifica casos com erro
pesquisa <- pesquisa %>%
  mutate(e14 = case_when(aprendizagem %in% "Não" & v1.02.8 %in% 0 & (is.na(v1.12.1) | is.na(v1.12.2)) &
                           sit.quest %in% "Terminado" & termo == "Sim" ~ 1,
                         .default = 0))

pesquisa %>% count(e14)

###############################################################################.
# Aplicando tratamento nas variáveis de v1.08 a v1.11 ---------------------
# Se aprendizagem %in% "Não", estas variáveis viram NA

pesquisa <- pesquisa %>%
  mutate(across(c(v1.08:v1.11),
                ~case_when(aprendizagem %in% "Não" ~ NA,
                           T ~ .x)))

###############################################################################.
# 2.15 Erro - 15 ----------------------------------------------------------
# Se a questão v2.01 == "Sim" E
# Não pode responder as questões: v2.02, v2.03, v2.03a, v2.04, v2.05, v2.06,
# v2.07, v2.08 e v2.09

# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(e15 = ifelse(!v2.01 %in% "Não" &
                        (!is.na(v2.02)|!is.na(v2.03)|!is.na(v2.03a)|!is.na(v2.04)|!is.na(v2.05)|
                           !is.na(v2.06)|!is.na(v2.07)|!is.na(v2.08)|!is.na(v2.09)), 1, 0))

pesquisa %>% count(e15)

pesquisa <- pesquisa %>%
  mutate(across(c(v2.02:v2.09),
                ~case_when(e15 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e15 == 1) %>% select(v2.01:v2.09) %>% View()

###############################################################################.
# 2.16 Erro - 16 ----------------------------------------------------------
# Se a questão v2.02 = "Não" e Aprendizagem = "Não",
# Não pode responder a(s) seguinte(s) questão(ões): v2.03, v2.03a, v2.04 e v2.06

pesquisa <- pesquisa %>%
  mutate(e16 = ifelse(v2.02 == "Não" & aprendizagem == "Não" &
                        (!is.na(v2.03)|!is.na(v2.03a)|!is.na(v2.04)|!is.na(v2.06)), 1, 0))

pesquisa %>% count(e16)

# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v2.03, v2.03a, v2.04, v2.06),
                ~case_when(e16 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e16 == 1) %>% select(v2.01:v2.09) %>% View()

###############################################################################.
# 2.17 Erro - 17 ----------------------------------------------------------
# Se a questão v2.02 = "Não" e Aprendizagem = "Sim",
# Não pode responder a(s) seguinte(s) questão(ões): v2.03, v2.03a, v2.04 e v2.05

pesquisa <- pesquisa %>%
  mutate(e17 = ifelse(v2.02 == "Não" & aprendizagem == "Sim" &
                        (!is.na(v2.03)|!is.na(v2.03a)|!is.na(v2.04)|!is.na(v2.05)), 1, 0))

pesquisa %>% count(e17)

# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v2.03, v2.03a, v2.04, v2.05),
                ~case_when(e17 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e17 == 1) %>% select(v2.01:v2.06) %>% View()

###############################################################################.
# 2.18 Erro - 18 ----------------------------------------------------------
# Não trabalhou por estar afastado nos últimos 7 dias
# Não pode responder a(s) seguinte(s) questão(ões): v2.03a, v2.04, v2.05,
# v2.06, v2.07, v2.08, v2.09

pesquisa <- pesquisa %>%
  mutate(e18 = ifelse(v2.02 == "Sim" &
                        (v2.03 %in% c("Férias, folga ou jornada de trabalho variável",
                                      "Licença maternidade ou paternidade",
                                      "Licença remunerada por motivo de saúde ou por ter se acidentado",
                                      "Outro tipo de licença remunerada (estudo, casamento, licença prêmio etc.)") |
                           is.na(v2.03)) &
                        (!is.na(v2.03a)|!is.na(v2.04)|!is.na(v2.05)|!is.na(v2.06)|!is.na(v2.07)|!is.na(v2.08)|
                           !is.na(v2.09)), 1, 0))

pesquisa %>% count(e18)

# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v2.03a:v2.09),
                ~case_when(e18 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e18 == 1) %>% select(sit.quest, v2.01:comentario) %>% View()

###############################################################################.
# 2.19 Erro - 19 ----------------------------------------------------------
# v2.03 != Outro
# Não pode responder a(s) seguinte(s) questão(ões): v2.03a

pesquisa <- pesquisa %>%
  mutate(e19 = ifelse(v2.02 == "Sim" & !v2.03 %in% "Outro motivo" & (!is.na(v2.03a)), 1, 0))

pesquisa %>% count(e19)

###############################################################################.
# 2.20 Erro - 20 ----------------------------------------------------------
# v2.04 == "Sim" & aprendizagem == "Não"
# Não pode responder a(s) seguinte(s) questão(ões): v2.06

pesquisa <- pesquisa %>%
  mutate(e20 = ifelse(v2.04 == "Sim" & aprendizagem == "Não" & (!is.na(v2.06)), 1, 0))

pesquisa %>% count(e20)

###############################################################################.
# 2.21 Erro - 21 ----------------------------------------------------------
# v2.04 == "Sim" & aprendizagem == "Sim"
# Não pode responder a(s) seguinte(s) questão(ões): v2.05

pesquisa <- pesquisa %>%
  mutate(e21 = ifelse(v2.04 == "Sim" & aprendizagem == "Sim" & (!is.na(v2.05)), 1, 0))

pesquisa %>% count(e21)

###############################################################################.
# 2.22 Erro - 22 ----------------------------------------------------------
# v2.04 == "Não"
# Não pode responder a(s) seguinte(s) questão(ões): v2.05, 2.06, 2.07, 2.08 e 2.09

pesquisa <- pesquisa %>%
  mutate(e22 = ifelse(v2.04 %in% "Não" & (!is.na(v2.05)|!is.na(v2.06)|!is.na(v2.07)|!is.na(v2.08)|!is.na(v2.09)), 1, 0))

pesquisa %>% count(e22)

###############################################################################.
# 2.23 Erro - 23 ----------------------------------------------------------
# v2.07 == "Não"
# Não pode responder a(s) seguinte(s) questão(ões): v2.08

pesquisa <- pesquisa %>%
  mutate(e23 = case_when(v2.07 %in% "Não" & !is.na(v2.08) ~ 1,
                         .default = 0),
         v2.08 = if_else(e23 == 1, NA, v2.08))

pesquisa %>% count(e23)

pesquisa %>% filter(e23 == 1) %>%
  select(id.pesquisa, sit.quest, termo, aprendizagem, v2.07, v2.08, v2.09)


###############################################################################.
# 2.24 Erro - 24 ----------------------------------------------------------
# v2.07 == "Sim"
# Não pode responder a(s) seguinte(s) questão(ões): v2.09

pesquisa <- pesquisa %>%
  mutate(e24 = case_when(v2.07 %in% "Sim" & !is.na(v2.09) ~ 1,
                         .default = 0),
         v2.09 = if_else(e24 == 1, NA, v2.09))

pesquisa %>% count(e24)

pesquisa %>% filter(e24 == 1) %>%
  select(id.pesquisa, sit.quest, termo, aprendizagem, v2.07, v2.08, v2.09)


###############################################################################.
# 2.25 Erro - 25 ----------------------------------------------------------
# Se v2.08 != "NA" ou v2.09 != "NA"
# Não pode responder o bloco 3 a(s) seguinte(s) questão(ões):
# v3.01, v3.02, v3.03, v3.04, v3.04a, v3.05, v3.06, v3.07, v3.08, v3.09,
# v3.10, v3.11, v3.11a, v3.12, v3.13

pesquisa <- pesquisa %>%
  mutate(e25 = ifelse((!is.na(v2.08)|!is.na(v2.09)) &
                        (!is.na(v3.01)|!is.na(v3.02)|!is.na(v3.03)|!is.na(v3.04)|!is.na(v3.04a)|
                           !is.na(v3.05)|!is.na(v3.06)|!is.na(v3.07)|!is.na(v3.08)|!is.na(v3.09)|
                           !is.na(v3.10)|!is.na(v3.11)|!is.na(v3.12)), 1, 0))

pesquisa %>% count(e25)

#Corrige registros com erros
# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.01:v3.12),
                ~case_when(e25 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e25 == 1) %>% select(v2.01:v3.12) %>% View()

###############################################################################.
# 2.25a Erro - 25a ----------------------------------------------------------
# Se v2.08 != "NA" ou v2.09 != "NA"
# Não pode responder o bloco 3 a(s) seguinte(s) questão(ões):
# v3.01, v3.02, v3.03, v3.04, v3.04a, v3.05, v3.06, v3.07, v3.08, v3.09,
# v3.10, v3.11, v3.11a, v3.12, v3.13

pesquisa <- pesquisa %>%
  mutate(e25a = ifelse(v2.07 %in% "Não" & (is.na(v2.08)|is.na(v2.09)) &
                         (!is.na(v3.01)|!is.na(v3.02)|!is.na(v3.03)|!is.na(v3.04)|!is.na(v3.04a)|
                            !is.na(v3.05)|!is.na(v3.06)|!is.na(v3.07)|!is.na(v3.08)|!is.na(v3.09)|
                            !is.na(v3.10)|!is.na(v3.11)|!is.na(v3.12)), 1, 0))

pesquisa %>% count(e25a)

#Corrige registros com erros
# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.01:v3.12),
                ~case_when(e25a == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e25a == 1) %>% select(sit.quest, v2.01:v3.12) %>% View()

###############################################################################.
# 2.26 Erro - 26 ----------------------------------------------------------
# Se v2.01 == "NA"
# Não pode responder o bloco 3 a(s) seguinte(s) questão(ões):
# v3.01, v3.02, v3.03, v3.04, v3.04a, v3.05, v3.06, v3.07, v3.08, v3.09,
# v3.10, v3.11, v3.11a, v3.12, v3.13

pesquisa <- pesquisa %>%
  mutate(e26 = ifelse((is.na(v2.01)) &
                        (!is.na(v3.01)|!is.na(v3.02)|!is.na(v3.03)|!is.na(v3.04)|!is.na(v3.04a)|
                           !is.na(v3.05)|!is.na(v3.06)|!is.na(v3.07)|!is.na(v3.08)|!is.na(v3.09)|
                           !is.na(v3.10)|!is.na(v3.11)|!is.na(v3.12)), 1, 0))

pesquisa %>% count(e26)

###############################################################################.
# 2.27 Erro - 27 ----------------------------------------------------------
# aprendizagem == "Sim"
# Não pode responder a(s) seguinte(s) questão(ões): v3.02

pesquisa <- pesquisa %>%
  mutate(e27 = case_when(aprendizagem == "Sim" & (!is.na(v3.02)) ~ 1,
                         .default = 0))

pesquisa %>% count(e27)

##############################################################################.
# 2.28 Erro - 28 ----------------------------------------------------------
# v3.04 == "Empregado(a) com carteira assinada"
#  Não pode responder a(s) seguinte(s) questão(ões): v3.06, v3.07, v3.08, v3.09

pesquisa <- pesquisa %>%
  mutate(e28 = ifelse(v3.04 %in% "Empregado(a) com carteira assinada" &
                        (!is.na(v3.04a)|!is.na(v3.06)|!is.na(v3.07)|!is.na(v3.08)|!is.na(v3.09)), 1, 0))

pesquisa %>% count(e28)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.04a, v3.06:v3.09),
                ~case_when(e28 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e28 == 1) %>% select(sit.quest, v3.04:v3.12) %>% View()

###############################################################################.
# 2.29 Erro - 29 ----------------------------------------------------------
# v3.04 == ("Empregado(a) sem carteira assinada" |
#           "Empregador(a) (tem de possuir pelo menos 2 empregados)" |
#           "Profissional universitário autônomo (médico, dentista, advogado, contador etc.)")
# Não pode responder a(s) seguinte(s) questão(ões): v3.04a, v3.05, v3.06, v3.07, v3.08

pesquisa <- pesquisa %>%
  mutate(e29 = ifelse((v3.04 %in% c("Empregado(a) sem carteira assinada",
                                    "Empregador(a) (tem de possuir pelo menos 2 empregados)",
                                    "Profissional universitário autônomo (médico, dentista, advogado, contador etc.)")) &
                        (!is.na(v3.04a)|!is.na(v3.05)|!is.na(v3.06)|!is.na(v3.07)|!is.na(v3.08)), 1, 0))

pesquisa %>% count(e29)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.04a:v3.08),
                ~case_when(e29 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e29 == 1) %>% select(sit.quest, v3.04:v3.12) %>% View()

###############################################################################.
# 2.30 Erro - 30 ----------------------------------------------------------
# v3.04 == "Empregado(a) do setor público(a)/Militar (inclusive CLT,
# contrato temporário, comissionado)"
# Não pode responder a(s) seguinte(s) questão(ões): v3.05, v3.07, v3.08, v3.09

pesquisa <- pesquisa %>%
  mutate(e30 = ifelse(v3.04 %in% "Empregado(a) do setor público(a)/Militar (inclusive CLT, contrato temporário, comissionado)" &
                        (!is.na(v3.04a)|!is.na(v3.05)|!is.na(v3.07)|!is.na(v3.08)|!is.na(v3.09)), 1, 0))

pesquisa %>% count(e30)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.04a, v3.05, v3.07, v3.08, v3.09),
                ~case_when(e30 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e30 == 1) %>% select(sit.quest, v3.04:v3.12) %>% View()

###############################################################################.
# 2.31 Erro - 31 ----------------------------------------------------------
# v3.04 == "Conta-própria/ Autônomo/ PJ"
#  Não pode responder a(s) seguinte(s) questão(ões): v3.05, v3.06

pesquisa <- pesquisa %>%
  mutate(e31 = ifelse((v3.04 %in% "Conta-própria/ Autônomo/ PJ") &
                        (!is.na(v3.04a)|!is.na(v3.05)|!is.na(v3.06)|!is.na(v3.08)), 1, 0))

pesquisa %>% count(e31)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.04a, v3.05, v3.06, v3.08),
                ~case_when(e31 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e31 == 1) %>% select(sit.quest, v3.04:v3.12) %>% View()

###############################################################################.
# 2.32 Erro - 32 ----------------------------------------------------------
# v3.04 == "Trabalhador doméstico (doméstica, jardineiro, passadeira, inclusive diarista)"
# Não pode responder a(s) seguinte(s) questão(ões): v3.05, v3.06, v3.07

pesquisa <- pesquisa %>%
  mutate(e32 = ifelse(v3.04 %in% "Trabalhador doméstico (doméstica, jardineiro, passadeira, inclusive diaristas)" &
                        (!is.na(v3.04a)|!is.na(v3.05)|!is.na(v3.06)|!is.na(v3.07)), 1, 0))

pesquisa %>% count(e32)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.04a, v3.05, v3.06, v3.07),
                ~case_when(e32 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e32 == 1) %>% select(sit.quest, v3.04:v3.12) %>% View()

################################################################################.
# 2.33 Erro - 33 ----------------------------------------------------------
# v3.04 == "Aprendiz"
# Não pode responder a(s) seguinte(s) questão(ões): v3.05, v3.06, v3.07, v3.08, v3.09

pesquisa <- pesquisa %>%
  mutate(e33 = ifelse(v3.04 %in% "Aprendiz" &
                        (!is.na(v3.04a)|!is.na(v3.05)|!is.na(v3.06)|!is.na(v3.07)|!is.na(v3.08)|!is.na(v3.09)), 1, 0))

pesquisa %>% count(e33)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.04a:v3.09),
                ~case_when(e33 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e33 == 1) %>% select(sit.quest, v3.04:v3.12) %>% View()

###############################################################################.
# 2.34 Erro - 34 ----------------------------------------------------------
# v3.04 == "Outro"
# Não pode responder a(s) seguinte(s) questão(ões): v3.05, v3.06, v3.07, v3.08

pesquisa <- pesquisa %>%
  mutate(e34 = ifelse((v3.04 == "Outro") &
                        (!is.na(v3.05)|!is.na(v3.06)|!is.na(v3.07)|!is.na(v3.08)), 1, 0))

pesquisa %>% count(e34)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.05:v3.08),
                ~case_when(e34 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e34 == 1) %>% select(sit.quest, v3.04:v3.09) %>% View()

###############################################################################.
# 2.35 Erro - 35 ---------------------------------------------------------
# v3.05 != "NA"
# Não pode responder a(s) seguinte(s) questão(ões): v3.06, v3.07, v3.08, v3.09

pesquisa <- pesquisa %>%
  mutate(e35 = if_else(!is.na(v3.05) &
                         (!is.na(v3.06)|!is.na(v3.07)|!is.na(v3.08)|!is.na(v3.09)), 1, 0))

pesquisa %>% count(e35)

###############################################################################.
# 2.36 Erro - 36 ----------------------------------------------------------
# v3.06 != "NA"
# Não pode responder a(s) seguinte(s) questão(ões): v3.07, v3.08, v3.09

pesquisa <- pesquisa %>%
  mutate(e36 = ifelse(!is.na(v3.06) &
                        (!is.na(v3.07)|!is.na(v3.08)|!is.na(v3.09)), 1, 0))

pesquisa %>% count(e36)

###############################################################################.
# 2.37 Erro - 37 ----------------------------------------------------------
# : v3.07 == "Sim"
#  Não pode responder a(s) seguinte(s) questão(ões): v3.08, v3.09

pesquisa <- pesquisa %>%
  mutate(e37 = ifelse(v3.07 %in% "Sim" & (!is.na(v3.08)|!is.na(v3.09)), 1, 0))

pesquisa %>% count(e37)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v3.08:v3.09),
                ~case_when(e37 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e37 == 1) %>% select(sit.quest, v3.04:v3.09)

###############################################################################.
# 2.38 Erro - 38 ----------------------------------------------------------
# v3.07 == "Não" | "Não sei responder"
# Não pode responder a(s) seguinte(s) questão(ões): v3.08

pesquisa <- pesquisa %>%
  mutate(e38 = ifelse((!v3.07 %in% c(NA, "Sim")) & (!is.na(v3.08)), 1, 0))

pesquisa %>% count(e38)

###############################################################################.
# 2.39 Erro - 39 ----------------------------------------------------------
# v3.08 == "Sim"
# Não pode responder a(s) seguinte(s) questão(ões): v3.09

pesquisa <- pesquisa %>%
  mutate(e39 = ifelse(v3.08 %in% "Sim" & (!is.na(v3.09)), 1, 0))

pesquisa %>% count(e39)

###############################################################################.
# 2.40 Erro - 40 ----------------------------------------------------------
# v4.02 == "Até fundamental incompleto" |
# v4.02 == "Fundamental completo" |
# v4.02 == "Prefiro não responder/ Não sei"
# Não pode responder a(s) seguinte(s) questão(ões): v4.03

# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(e40 = ifelse(v4.02 %in% c("Até fundamental incompleto", 
                                   "Fundamental completo", 
                                   "Prefiro não responder/Não sei") &
                        (!is.na(v4.03)), 1, 0))

pesquisa %>% count(e40)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v4.03),
                ~case_when(e40 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e40 == 1) %>% select(sit.quest, v4.01:v4.03) %>% View()

###############################################################################.
# 2.41 Erro - 41 ----------------------------------------------------------
# Modalidade == "Educação Profissional Técnica de Nível Médio"
# Não pode ser v4.02%in%c(1,2,3)
# Modalidade == "Educação Superior"
# Não pode ser v4.02%in%c(1,2,3,4,5)

pesquisa <- pesquisa %>%
  mutate(e41 = ifelse(Modalidade %in% "Educação Profissional Técnica de Nível Médio" &
                        v4.02 %in% c("Até fundamental incompleto",
                                     "Fundamental completo",
                                     "Médio incompleto")|
                        Modalidade %in% "Educação Superior" &
                        v4.02 %in% c("Até fundamental incompleto",
                                     "Fundamental completo",
                                     "Médio incompleto",
                                     "Médio completo",
                                     "Superior incompleto"), 1, 0))

pesquisa %>% count(e41)

###############################################################################.
# 2.42 Erro - 42 ----------------------------------------------------------
# Modalidade == "Educação Profissional Técnica de Nível Médio"
# Não pode ser !is.na(v4.03)

pesquisa <- pesquisa %>%
  mutate(e42 = ifelse(Modalidade == "Educação Profissional Técnica de Nível Médio" &
                        !is.na(v4.03), 1, 0))

pesquisa %>% count(e42)

###############################################################################.
# 2.43 Erro - 43 ----------------------------------------------------------
# esc_pos_grad == "Não" & curso_tec == "Não" & is.na(v4.02) & sit.quest == "Terminado"
# Não pode ser !is.na(v4.03)

pesquisa <- pesquisa %>%
  mutate(e43 = ifelse(esc_pos_grad == "Não" & 
                        curso_tec == "Não" & 
                        is.na(v4.02) & sit.quest == "Terminado" & !is.na(v4.03), 1, 0))

pesquisa %>% count(e43)

###############################################################################.
# 2.45 Erro - 45 ----------------------------------------------------------
# A renda do domicílio não pode ser menor que a renda do trabalho

# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(e45 = case_when(v4.07 %in% 1 & v3.12 %in% "Menos de R$ 1.518,00" & v4.08 %in% "Sem rendimento" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 1.518 a 3.035,99" & v4.08 %in% "Sem rendimento" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 1.518 a 3.035,99" & v4.08 %in% "Menos de R$ 1.518,00" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 3.036 a 4.553,99" & v4.08 %in% "Sem rendimento" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 3.036 a 4.553,99" & v4.08 %in% "Menos de R$ 1.518,00" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 3.036 a 4.553,99" & v4.08 %in% "De 1.518 a 3.035,99" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 4.554 a 7.589,99" & v4.08 %in% "Sem rendimento" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 4.554 a 7.589,99" & v4.08 %in% "Menos de R$ 1.518,00" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 4.554 a 7.589,99" & v4.08 %in% "De 1.518 a 3.035,99" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 4.554 a 7.589,99" & v4.08 %in% "De 3.036 a 4.553,99" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 7.590 a 15.179,99" & v4.08 %in% "Sem rendimento" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 7.590 a 15.179,99" & v4.08 %in% "Menos de R$ 1.518,00" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 7.590 a 15.179,99" & v4.08 %in% "De 1.518 a 3.035,99" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 7.590 a 15.179,99" & v4.08 %in% "De 3.036 a 4.553,99" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "De 7.590 a 15.179,99" & v4.08 %in% "De 4.554 a 7.589,99" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "R$ 15.180,00 ou mais" & v4.08 %in% "Sem rendimento" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "R$ 15.180,00 ou mais" & v4.08 %in% "Menos de R$ 1.518,00" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "R$ 15.180,00 ou mais" & v4.08 %in% "De 1.518 a 3.035,99" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "R$ 15.180,00 ou mais" & v4.08 %in% "De 3.036 a 4.553,99" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "R$ 15.180,00 ou mais" & v4.08 %in% "De 4.554 a 7.589,99" ~ 1,
                         v4.07 %in% 1 & v3.12 %in% "R$ 15.180,00 ou mais" & v4.08 %in% "De 7.590 a 15.179,99" ~ 1,
                         T ~ 0))

pesquisa %>% count(e45)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(v4.08 = ifelse(e45 == 1, v3.12, v4.08))

pesquisa %>% filter(e45 %in% 1) %>% count(v3.12, v4.08) %>% View()

###############################################################################.
# 2.46 Erro - 46 ----------------------------------------------------------
# v4.09 %in% ("Não", "Prefiro não responder/Não sei"), v4.10 e v4.11 tem que ser NA

# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(e46 = ifelse((is.na(v4.09) | v4.09 %in% c("Prefiro não responder/Não sei")) &
                        (!is.na(v4.10)|!is.na(v4.11)), 1, 0))

pesquisa %>% count(e46)

#Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(across(c(v4.10:v4.11),
                ~case_when(e46 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e46 == 1) %>% select(sit.quest, v4.09:v4.11) %>% View()

###############################################################################.
# 2.47 Erro - 47 ----------------------------------------------------------
# v4.10.99 %in% ("Prefiro não responder/Não sei"), v4.10.1 a v4.10.4 tem que ser 0

# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(e47 = ifelse(v4.10.99 %in% 1 &
                        rowSums(across(v4.10.1:v4.10.4), na.rm = TRUE) > 0, 1, 0))

pesquisa %>% count(e47)

###############################################################################.
# 2.48 Erro - 48 ----------------------------------------------------------
# v4.11.99 %in% ("Prefiro não responder/Não sei"), v4.11.1 a v4.11.4 tem que ser 0

# Corrige registros com erros
pesquisa <- pesquisa %>%
  mutate(e48 = ifelse(v4.11.99 %in% 1 &
                        rowSums(across(v4.11.1:v4.11.4), na.rm = TRUE) > 0, 1, 0))

pesquisa %>% count(e48)

###############################################################################.
# 3. Criação da sit.ocup e imputação de dados -------------------------------

###############################################################################.
# 3.1 Criação da sit.ocup ------------------------------------

pesquisa <- pesquisa %>% 
  mutate(ocupado = ifelse(v2.01 %in% "Sim" |
                            (v2.02 %in% "Sim" & (v2.03 %in% "Férias, folga ou jornada de trabalho variável"|
                                                   v2.03 %in% "Licença maternidade ou paternidade"|
                                                   v2.03 %in% "Licença remunerada por motivo de saúde ou por ter se acidentado"|
                                                   v2.03 %in% "Outro tipo de licença remunerada (estudo, casamento, licença prêmio etc.)")) |
                            (v2.02 %in% "Sim" & (v2.03 %in% "Afastamento do próprio negócio/empresa, sem ser remunerado por instituto de previdência"|
                                                   v2.03 %in% "Fatores ocasionais (má condição climática, paralisação nos serviços de transporte, greve etc.)"|
                                                   v2.03 %in% "Outro motivo") & v2.04 %in% "Não"), 1, 0),
         desocupado = ifelse(v2.07 %in% "Sim" & v2.08 %in% "Sim", 1, 0),
         inativo    = ifelse(v2.07 %in% "Não" | (v2.07 %in% "Sim" & v2.08 %in% "Não"), 1, 0),
         sit.ocup   = ifelse(ocupado == 1, "Ocupado", 
                             ifelse(desocupado == 1, "Desocupado",
                                    ifelse(inativo == 1, "Inativo", NA))))

# Fazendo algumas checagens na classificação

pesquisa %>% count(sit.ocup)
pesquisa %>% count(termo, sit.quest, sit.ocup)

# Mantendo apenas as respostas válidas
pesquisa <- pesquisa %>% 
  filter(!is.na(sit.ocup))

###############################################################################.
# 3.2 Imputação - v4.02 (Escolaridade) ------------------------------------
# esc_pos_grad == "Sim" & !is.na(sit.ocup) & is.na(v4.02)

# Imputa os registros de escolaridade
pesquisa <- pesquisa %>%
  mutate(input.v4.02 = ifelse(esc_pos_grad == "Sim" & !is.na(sit.ocup) & is.na(v4.02) & !is.na(v4.03), 1, 0),
         across(v4.02,
                ~case_when(input.v4.02 == 1 ~ "Pós-graduação completa",
                           .default = .x)))

###############################################################################.
# 3.3 Imputação - v4.03 (Curso técnico) -----------------------------------
# curso_tec == "Sim" & !is.na(sit.ocup) & !is.na(v4.02) & is.na(v4.03)

# Imputa os registros de escolaridade
pesquisa <- pesquisa %>%
  mutate(input.v4.03 = ifelse(curso_tec == "Sim" & !is.na(sit.ocup) & (!v4.02 %in% c(NA, "Prefiro não responder/Não sei")) &
                                is.na(v4.03), 1, 0),
         across(v4.03,
                ~case_when(input.v4.03 == 1 ~ "Sim, já concluí",
                           .default = .x)))

###############################################################################.
# 4. Verificação dos Fluxos (Suplemento de IA) -------------------------------

###############################################################################.
# 4.1 Erro - 49 ----------------------------------------------------------
# Se s.01 %in% ("Prefiro não responder/Não sei" ou NA), o bloco não deve ser respondido
pesquisa <- pesquisa %>% 
  mutate(e49 = case_when((is.na(s.01) | s.01 == "Prefiro não responder/Não sei") & 
                           (!is.na(s.02)|!is.na(s.03)|!is.na(s.04)|!is.na(s.05)|
                              !is.na(s.06)|!is.na(s.06)|!is.na(s.07)|
                              !is.na(s.08)|!is.na(s.09)|!is.na(s.10)|
                              !is.na(s.11)|!is.na(s.12)|!is.na(s.13)|
                              !is.na(s.14.1)|!is.na(s.14.2)|!is.na(s.15)|
                              !is.na(s.16)|!is.na(s.17)|!is.na(s.18)) ~ 1, T ~ 0))

pesquisa %>% count(e49)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(across(c(s.02:s.18),
                ~case_when(e49 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e49 %in% 1) %>% select(s.01:s.18) %>% View()

###############################################################################.
# 4.2 Erro - 50 ----------------------------------------------------------
# Se s.01 %in% ("Sim"), s.02 tem que ser NA
pesquisa <- pesquisa %>% 
  mutate(e50 = case_when(s.01 %in% "Sim" & 
                           (!is.na(s.02)|!is.na(s.02a)) ~ 1, T ~ 0))

pesquisa %>% count(e50)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(across(c(s.02:s.02a),
                ~case_when(e50 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e50 %in% 1) %>% select(s.01:s.18) %>% View()

###############################################################################.
# 4.3 Erro - 51 ----------------------------------------------------------
# Se s.01 %in% ("Não"), s.03 até s.17 tem que ser NA
pesquisa <- pesquisa %>% 
  mutate(e51 = case_when(s.01 %in% "Não" & 
                           (!is.na(s.03)|!is.na(s.04)|!is.na(s.05)|
                              !is.na(s.06)|!is.na(s.06)|!is.na(s.07)|
                              !is.na(s.08)|!is.na(s.09)|!is.na(s.10)|
                              !is.na(s.11)|!is.na(s.12)|!is.na(s.13)|
                              !is.na(s.14.1)|!is.na(s.14.2)|!is.na(s.15)|
                              !is.na(s.16)|!is.na(s.17)) ~ 1, T ~ 0))

pesquisa %>% count(e51)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(across(c(s.03:s.17),
                ~case_when(e51 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e51 %in% 1) %>% select(s.01:s.18) %>% View()

###############################################################################.
# 4.4 Erro - 52 ----------------------------------------------------------
# Se s.02 %in% ("Prefiro não responder/Não sei"), s.02.1 a s.02.6 tem que ser 0
pesquisa <- pesquisa %>% 
  mutate(e52 = case_when(s.01 %in% "Não" & rowSums(across(c(s.02.1:s.02.90), ~ .x %in% 1)) > 0 & s.02.99 %in% 1 ~ 1, 
                         T ~ 0))

pesquisa %>% count(e52)

###############################################################################.
# 4.5 Erro - 53 ----------------------------------------------------------
# Se s.03.3 !NA 1 somente se sit.ocup %in% "Ocupado"
pesquisa <- pesquisa %>% 
  mutate(e53 = case_when(!sit.ocup %in% "Ocupado" & s.01 %in% "Sim" & s.03.3 %in% 1 ~ 1, T ~ 0))

pesquisa %>% count(e53)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(across(c(s.03.3),
                ~case_when(e53 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e53 %in% 1) %>% select(sit.ocup, s.03.1, s.03.2, s.03.3, s.03.4, s.03.99) %>% View()

pesquisa <- pesquisa %>% 
  mutate(s.03.3 = case_when(!sit.ocup %in% "Ocupado" & s.01 %in% "Sim" ~ NA, T ~ s.03.3))

###############################################################################.
# 4.6 Erro - 54 ----------------------------------------------------------
# Se s.03.3 %in% 1, s.04 a s.07 tem que ser NA
pesquisa <- pesquisa %>% 
  mutate(e54 = case_when(s.03.3 %in% 1 &
                           (!is.na(s.04)|!is.na(s.05)|
                              !is.na(s.06)|!is.na(s.06)|!is.na(s.07)) ~ 1, T ~ 0))

pesquisa %>% count(e54)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(across(c(s.04:s.07),
                ~case_when(e54 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e54 %in% 1) %>% select(s.03.1:s.07) %>% View()

###############################################################################.
# 4.7 Erro - 55 ----------------------------------------------------------
# Se s.03.3 %in% 0 & !sit.ocup %in% Ocupado, s.04 NA
pesquisa <- pesquisa %>% 
  mutate(e55 = case_when(!sit.ocup %in% "Ocupado" & s.01 %in% "Sim" &
                           (!is.na(s.04)) ~ 1, T ~ 0))

pesquisa %>% count(e55)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(across(c(s.04:s.04a),
                ~case_when(e55 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e55 %in% 1) %>% select(sit.ocup, s.01, s.04:s.04a) %>% View()

###############################################################################.
# 4.8 Erro - 56 ----------------------------------------------------------
# Se v3.04 diferente de Carteira assinada ou setor público e s.04.4 diferente de NA
pesquisa <- pesquisa %>% 
  mutate(e56 = case_when(!v3.04 %in% c("Empregado(a) com carteira assinada",
                                       "Empregado(a) do setor público(a)/Militar (inclusive CLT, contrato temporário, comissionado)") & 
                           s.01 %in% "Sim" &
                           (!is.na(s.04.4)) ~ 1, T ~ 0))

pesquisa %>% count(e56)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(across(c(s.04.4),
                ~case_when(e56 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e56 %in% 1) %>% select(sit.ocup, v3.04, s.01, s.04.4) %>% View()

###############################################################################.
# 4.9 Erro - 57 ----------------------------------------------------------
# Se s.03.3 %in% 1 & !is.na(s.07), s.08 a s.16 tem que ser NA
pesquisa <- pesquisa %>% 
  mutate(e57 = case_when(!s.03.3 %in% 1 & !is.na(s.07) & 
                           (!is.na(s.08)|!is.na(s.09)|!is.na(s.10)|
                              !is.na(s.11)|!is.na(s.12)|!is.na(s.13)|
                              !is.na(s.14.1)|!is.na(s.14.2)|!is.na(s.15)|
                              !is.na(s.16)) ~ 1, T ~ 0))

pesquisa %>% count(e57)

# Corrigindo os casos com erros
pesquisa <- pesquisa %>%
  mutate(across(c(s.08:s.16),
                ~case_when(e57 == 1 ~ NA,
                           T ~ .x)))

pesquisa %>% filter(e57 %in% 1) %>% select(s.03.3, s.07, s.08:s.16) %>% View()

###############################################################################.
# 4.10 Erro - 58 ----------------------------------------------------------
# Se v3.04 diferente de Carteira assinada ou setor público e s.12 for "Orientação da empresa/de um superior"
pesquisa <- pesquisa %>% 
  mutate(e58 = case_when(!v3.04 %in% c("Empregado(a) com carteira assinada",
                                       "Empregado(a) do setor público(a)/Militar (inclusive CLT, contrato temporário, comissionado)") & 
                           s.01 %in% "Sim" &
                           s.12 %in% "Orientação da empresa/de um superior" ~ 1, T ~ 0))

pesquisa %>% count(e58)

###############################################################################.
# 4.11 Erro - 59 ----------------------------------------------------------
# Se v3.04 diferente de Carteira assinada ou setor público e s.13 diferente de NA
pesquisa <- pesquisa %>% 
  mutate(e59 = case_when(!v3.04 %in% c("Empregado(a) com carteira assinada",
                                       "Empregado(a) do setor público(a)/Militar (inclusive CLT, contrato temporário, comissionado)") & 
                           s.01 %in% "Sim" &
                           !is.na(s.13) ~ 1, T ~ 0))

pesquisa %>% count(e59)

###############################################################################.
# 4.12 Erro - 60 ----------------------------------------------------------
# Se v3.04 diferente de Carteira assinada ou setor público e s.15 a s.15a diferente de NA
pesquisa <- pesquisa %>% 
  mutate(e60 = case_when(!v3.04 %in% c("Empregado(a) com carteira assinada",
                                       "Empregado(a) do setor público(a)/Militar (inclusive CLT, contrato temporário, comissionado)") & 
                           s.01 %in% "Sim" &
                           (!is.na(s.15)) ~ 1, T ~ 0))

pesquisa %>% count(e60)

###############################################################################.
# 4.13 Erro - 61 ----------------------------------------------------------
# Se v3.04 diferente de Carteira assinada ou setor público e s.16 a s.16a diferente de NA
pesquisa <- pesquisa %>% 
  mutate(e61 = case_when(!v3.04 %in% c("Empregado(a) com carteira assinada",
                                       "Empregado(a) do setor público(a)/Militar (inclusive CLT, contrato temporário, comissionado)") & 
                           s.01 %in% "Sim" &
                           (!is.na(s.16)) ~ 1, T ~ 0))

pesquisa %>% count(e61)

###############################################################################.
# 4.14 Erro - 62 ----------------------------------------------------------
# Se v3.04 diferente de Carteira assinada ou setor público e s.17 ser "Em treinamento(s) da empresa"
pesquisa <- pesquisa %>% 
  mutate(e62 = case_when(!v3.04 %in% c("Empregado(a) com carteira assinada",
                                       "Empregado(a) do setor público(a)/Militar (inclusive CLT, contrato temporário, comissionado)") & 
                           s.01 %in% "Sim" &
                           s.17 %in% "Em treinamento(s) da empresa" ~ 1, T ~ 0))

pesquisa %>% count(e62)

###############################################################################.
# Compendio de tabelas com frequencias simples dos erros -------------------

erros <- pesquisa %>%
  add_count(name = "Total") %>%
  select(starts_with(c("e0", "e1", "e2", "e3", "e4", "e5", "e6")), Total) %>%
  pivot_longer(cols = starts_with("e"),
               names_to = "erro",
               values_to = "qtd_erro") %>%
  dplyr::group_by(erro) %>%
  dplyr::summarize(total_erro = sum(qtd_erro),
                   Respostas = mean(Total)) %>%
  mutate(perc_erro = round((total_erro/Respostas)*100,1))

writexl::write_xlsx(erros, "01. PNAES/{ano}/5. Bases/2. Pesquisa/4. Consistência/tab_ERROS_PNAES_{ano}.xlsx")

rm(erros)

# Retirando da base de pesquisa as variáveis dos erros ("e..")
pesquisa <- pesquisa %>%
  select(-c(starts_with(c("e0", "e1", "e2", "e3", "e4", "e5", "e6")), 
            ajuste.v1.02a, ajuste.v1.08a, ajuste.v2.03a, ajuste.v3.04a,
            input.v4.02, input.v4.03,
            DR2, regiao2, nucleo2, cod.mun.res, 
            Modalidade, TipoCurso, Eixo, Segmento, mod.recurso))

# Fazendo os últimos ajustes

pesquisa <- pesquisa %>%
  mutate(v1.02.1 = ifelse(curso_ocup %in% "Não", NA, v1.02.1),
         v1.02.3 = ifelse(!v1.01 %in% "Sim", NA, v1.02.3),
         v1.02.5 = ifelse(!v1.01 %in% "Sim", NA, v1.02.5),
         v1.02.4 = ifelse(!v1.01 %in% "Sim" | curso_ocup %in% "Sim", NA, v1.02.4),
         v1.04.1 = ifelse(!v1.02.1 %in% "1", NA, v1.04.1),
         v1.04.2 = ifelse(!v1.02.2 %in% "1", NA, v1.04.2),
         v1.04.3 = ifelse(!v1.02.3 %in% "1", NA, v1.04.3),
         v1.04.4 = ifelse(!v1.02.4 %in% "1", NA, v1.04.4),
         v1.04.5 = ifelse(!v1.02.5 %in% "1", NA, v1.04.5),
         v1.04.6 = ifelse(!v1.02.6 %in% "1", NA, v1.04.6),
         v1.04.7 = ifelse(!v1.02.7 %in% "1", NA, v1.04.7))

###############################################################################.
# 5. Salvando as bases consistidas -------------------------------

saveRDS(pesquisa, "01. PNAES/{ano}/5. Bases/2. Pesquisa/3. rds/2. Base.consistida.rds")

rm(pesquisa, pop.pesq)

