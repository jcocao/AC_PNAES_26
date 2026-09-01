box::use(
  dplyr[...]
)



#' @export
brasil <- readRDS("data/br_uf_shape.Rds")



dados_populacao <- readRDS("data/dados_p.rds") %>% 
  rename(DR = DR2) 

#' @export
dados_populacao <- dados_populacao %>% 
  bind_rows(
    # Cria uma única linha "BR", somando pop_a e pop_p de todos os
    # estados e semestres (sem manter a quebra por semestre)
    dados_populacao %>%
      ungroup() %>% 
      summarise(
        DR = "BR",
        semestre = NA,
        pop_a = sum(pop_a, na.rm = TRUE),
        pop_p = sum(pop_p, na.rm = TRUE)
      ) %>%
      mutate(tx = pop_p / pop_a)
  )

#' @export
dados_primarios <- readRDS("data/dados.rds") %>% 
  mutate(valido = ifelse(!is.na(sit.ocup), "valido", "invalido"))

#' @export
cor_p <- "#282957"

#' @export
cor_s <- "#8853c3"

#' @export
cor_s1 <- "#7bd0f0"

#' @export
cor_s2 <- "#e4e0e0"

#' @export
cor_tabela <- "#7982E5"

#' @export
palheta_mapa <- c("#000D66", "#4B6AA9", "#7AD0F0", "#B5D8E8", "#E4DFDF")