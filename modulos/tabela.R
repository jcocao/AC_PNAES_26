box::use(
  shiny[moduleServer, NS, reactive, req, strong, span, em, HTML, br, tags],
  bslib[card_header, card_body,
        tooltip],
  dplyr[...],
  tidyr[starts_with],
  reactable[...],
  htmltools[div]
)


box::use(
  ./global[cor_tabela, 
           opcoes],
  ./funcoes/funcoes_auxiliares[formatar_numero],
  ./carregar_dados[dados_populacao]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  reactableOutput(ns("tbl_dr"),
                  width = "100%")
  
  
}

#' @export
server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    
    output$tbl_dr <- renderReactable({

      flat <- unlist(opcoes)
      trad <- data.frame(Nomes = names(flat),
                         DR = unname(flat),
                         stringsAsFactors = FALSE)
      trad$Nomes <- sub("^.*?\\.", "", trad$Nomes)
      
      pop_pesquisa <- dados_populacao %>% summarise(pop_p = sum(pop_p), .by = "DR")
      
      dados_t <- dados() %>%
        left_join(trad, by = c("DR")) %>%
        left_join(pop_pesquisa, by = "DR") %>%
        filter(!is.na(valido)) %>%
        mutate(DR = Nomes,
               .keep = "unused") %>%
        group_by(DR) %>%
        filter(DR != "SG") %>% 
        summarise(Valido = sum(valido == "valido"),
                  Total = unique(pop_p),
                  Taxa = (Valido/Total))
      
      
      
      reactable(dados_t,
                pagination = FALSE,
                filterable = FALSE,
                highlight = TRUE,
                bordered = TRUE,
                striped = FALSE,
                height = 750,
                defaultColDef = colDef(format = colFormat(separators = TRUE,
                                                          locales = "pt-BR")),
                theme = reactableTheme(
                  borderColor = "#010e67",
                  color = "black",
                  highlightColor = "#e4e0e0",
                  headerStyle = list(
                    color = "white",
                    fontWeight = "bold",
                    backgroundColor = cor_tabela,
                    fontSize = "18px"
                  )
                ),
                columns = list(
                  ead = colDef(
                    show = FALSE
                  ),
                  DR = colDef(
                    name = "Departamento Regional"
                  ),
                  Valido = colDef(
                    filterable = FALSE,
                    name = "Total de questionários válidos",
                    align = "center",
                    format = colFormat(separators = TRUE,
                                       percent = FALSE,
                                       digits = 0,
                                       locales = "pt-BR"),
                    style = list(
                      fontSize = "16px"
                    )
                  ),
                  Total = colDef(
                    name = "População de Pesquisa",
                    align = "center",
                    format = colFormat(separators = TRUE,
                                       percent = FALSE,
                                       digits = 0,
                                       locales = "pt-BR")
                  ),
                  Taxa = colDef(
                    name = "Taxa de resposta (%)",
                    filterable = FALSE,
                    format = colFormat(separators = TRUE,
                                       percent = TRUE,
                                       digits = 2,
                                       locales = "pt-BR"),
                    align = "center",
                    style = list(
                      fontSize = "16px"
                    )
                  )
                )
                
      )
      
      
      
    })
    
  })
}

