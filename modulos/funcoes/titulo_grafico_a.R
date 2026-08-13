box::use(
  dplyr[count, mutate, summarise, `%>%`, pull]
)
#
#' @export
transformar_titulo <- function(dados){
  
  
  saida <- dados %>%
    mutate(dia = as.Date(dt.conclusao)) %>%
    count(dia, name = "qtd_dia") %>%
    summarise(media = mean(qtd_dia)) %>%
    pull(media)
  
  return(saida)
}