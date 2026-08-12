box::use(
  dplyr[`%>%`, group_by, mutate, n, summarise, ungroup]
)

#' @export
tabela_Geracao <- function(dados){
  
  saida <- dados %>%
    mutate(Data = cut.POSIXt(dt.conclusao,
                             breaks = "days")) %>%
    group_by(Data) %>%
    summarise(Acessos = n(),
              `Válidos` = sum(valido == "valido")) %>%
    mutate(Data = format(as.Date(Data), "%d/%b")) %>% 
    ungroup()
  
  return(saida)
}