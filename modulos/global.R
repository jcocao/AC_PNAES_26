box::use(
  shiny[h3,
        HTML,
        tags],
  dplyr[...]
)



#' @export
brasil <- readRDS("data/br_uf_shape.Rds")


#' @export
dados_populacao <- readRDS("data/dados_p.rds") 

#' @export
dados_populacao <- dados_populacao %>% 
  rename(DR = DR2) %>%
  bind_rows(
    dados_populacao %>%
      group_by(semestre) %>%
      summarise(
        DR = "BR",
        pop_a = sum(pop_a, na.rm = TRUE),
        pop_p = sum(pop_p, na.rm = TRUE),
        tx = pop_p / pop_a,
        .groups = "drop"
      )
  ) %>% 
  mutate(validos = "validos")
  


#' @export
dados_primarios <- readRDS("data/dados.rds") 
  # %>% 
  # bind_rows(
  #   # BR por semestre
  #   dados %>%
  #     distinct(semestre, DR, Total) %>%
  #     group_by(semestre) %>%
  #     summarise(
  #       DR = "BR",
  #       Total = sum(Total, na.rm = TRUE),
  #       .groups = "drop"
  #     ),
  #   
  #   # BR geral
  #   dados %>%
  #     distinct(semestre, DR, Total) %>%
  #     summarise(
  #       semestre = NA_real_,
  #       DR = "BR",
  #       Total = sum(Total, na.rm = TRUE),
  #       .groups = "drop"
  #     )
  # )
  




# dados_filtrado <- dados %>% 
#   group_by(semestre, DR) %>%
#   summarise(
#     total_acessos = n(),
#     tempo_medio = mean(`tempo`, na.rm = TRUE),
#     tempo_mediano = median(`tempo`, na.rm = TRUE),
#     .groups = "drop"
#   )


#' @export
pop1 <- 400000

#' @export
titulo_mapa <- tags$div(
  h3("Taxa de resposta (%), por Departamento Regional, PNAES 2024", 
     style = " color: black;
    font-weight: bold;
    font-size: 28px;")
)  

#' @export
cor_p <- "#282957"

#' @export
cor_s <- "#8853c3"

#' @export
cor_s1 <- "#7bd0f0"

#' @export
cor_s2 <- "#e4e0e0"

#' @export
cor_extra <- "#A7CBDC"

#'@export
cor_tabela <- "#7982E5"

#' @export
palheta_mapa <- c("#000D66", "#4B6AA9", "#7AD0F0", "#B5D8E8", "#E4DFDF")

#' @export
preenchimento_valuebox <- "#e4e0e0"

#' @export
preenchimento_card <- c("#010E67", "#2647B2", "#4D85E5", "#7BD0F0")