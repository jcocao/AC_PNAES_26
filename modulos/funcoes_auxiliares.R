box::use(
  shiny[moduleServer, NS],
  dplyr[...],
  stats[median]
)


#' @export
formatar_numero <- function(x, digitos = 2, ndigitos = 2, percent = F) {
  saida <- x %>%
    format(big.mark = ".",
           decimal.mark = ",",
           nsmall = ndigitos,
           digits = digitos)
  
  if(percent) {
    saida <- x*100
    
    saida <- saida  %>%
      format(big.mark = ".",
             decimal.mark = ",",
             nsmall = ndigitos,
             digits = digitos) %>% 
      paste0("%")
  }
  
  return(saida)
}

#' @export
calcular_estatistica_coluna <- function(data, coluna, estatistica = "media") {
  
  coluna_sym <- if(is.character(coluna)) {
    rlang::sym(coluna)
  } else {
    rlang::ensym(coluna)
  }
  
  saida <- data %>%
    summarise(
      estatistica = case_when(
        tolower(estatistica) %in% c("media", "mean") ~ mean(!!coluna_sym, na.rm = TRUE),
        tolower(estatistica) %in% c("mediana", "median") ~ median(!!coluna_sym, na.rm = TRUE),
        TRUE ~ NA_real_
      )
    ) %>%
    pull(estatistica)
  
  return(saida)
}

