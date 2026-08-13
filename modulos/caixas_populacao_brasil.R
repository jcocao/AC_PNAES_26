box::use(
  bslib[value_box,
        value_box_theme],
  bsicons[bs_icon],
  shiny[NS,
        moduleServer,
        textOutput,
        renderText,
        reactive],
  dplyr[...]
)

box::use(
  funcoes/funcoes_auxiliares[formatar_numero],
  funcoes/extras[...]
)

#' @export
ui <- function(id) {
  
  ns <- NS(id)
  
  list(
    value_box(
      title = "População Alvo do Brasil:",
      value = textOutput(ns("PopulacaoAlvo")),
      showcase = bs_icon("people-fill",
                         size="0.6em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    value_box(
      title = "Questionários válidos do Brasil:",
      value = textOutput(ns("QuestionariosValidos")),
      showcase = bs_icon("clipboard-check-fill",
                         size="0.6em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    value_box(
      title = "Taxa de resposta do Brasil:",
      value = textOutput(ns("TaxaDeResposta")),
      showcase = bs_icon("percent",
                         size="0.6em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    )
  )
}

#' @export
server <- function(id, dados_brasil_filtrado, populacao_brasil_filtrado) {
  moduleServer(id, function(input, output, session) {
    
    
    popbrasil <- reactive({
      saida <- populacao_brasil_filtrado() %>% 
        summarise(pop_a = sum(pop_a, na.rm = T)) %>% 
        pull(pop_a)
      
      return(saida)
    })
    
    validos_brasil <- reactive({
      
      saida <- dados_brasil_filtrado() %>%
        filter(!is.na(valido)) %>% 
        filter(valido==1) %>% 
        nrow()
      
      return(saida)
    })
    
    output$PopulacaoAlvo <- renderText({
      saida <- popbrasil() %>% 
        formatar_numero(ndigitos = 0)
      
      return(saida)
    })
    
    output$QuestionariosValidos <- renderText({
      
      saida <- validos_brasil() %>% 
        formatar_numero()
      
      return(saida)
    })
    
    output$TaxaDeResposta <- renderText({
      
      valor <- validos_brasil()
      numerador <- popbrasil()
      saida <- valor/numerador
      
      saida <- formatar_numero(saida, percent = T,
                               digitos = 1, 
                               ndigitos = 2)
      
      return(saida)
    })
    
  })
}
