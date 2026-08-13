box::use(
  bslib[value_box,
        layout_column_wrap],
  bsicons[bs_icon],
  shiny[NS,
        moduleServer,
        selectizeInput,
        textOutput,
        renderText,
        reactive,
        req],
  dplyr[`%>%`,
        slice,
        pull,
        filter]
)

box::use(
  ./carregar_dados[opcoes],
  funcoes/funcoes_auxiliares[formatar_numero],
  funcoes/extras[...]
)

#' @export
ui <-  function(id) {
  ns <- NS(id)
  
  layout_column_wrap(
    width = "400px",
    value_box(
      title = "População Alvo:",
      value = textOutput(ns("PopulacaoAlvo")),
      showcase = bs_icon("people-fill",
                         size="0.6em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    value_box(
      title = "Total de Acessos:",
      value = textOutput(ns("TotalAcessos")),
      showcase = bs_icon("clipboard-data",
                         size="0.6em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    value_box(
      title = "População Alvo com contato",
      value = textOutput(ns("PopulacaoAlvoContato")),
      showcase = bs_icon("person-check-fill",
                         size="0.6em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    value_box(
      title = "Taxa de cobertura",
      value = textOutput(ns("TaxaCobertura")),
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

server <- function(id, populacao_filtrada,dados_filtrado) {
  moduleServer(id, function(input, output, session) {
    
    output$TotalAcessos <- renderText({
      dados <- dados_filtrado()
      req(nrow(dados) > 0)
      saida <- nrow(dados_filtrado())
      
      saida <- formatar_numero(saida)
      
      return(saida)
    })
    
    
    output$PopulacaoAlvo <- renderText({
      
      dados <- populacao_filtrada()
      
      req(nrow(dados) > 0)
      dados %>%
        slice(1) %>%
        pull(pop_a) %>% 
        formatar_numero(ndigitos = 0)
    })
    
    output$PopulacaoAlvoContato <- renderText({
      dados <- populacao_filtrada()
      req(nrow(dados) > 0)
      dados %>%
        slice(1) %>%
        pull(pop_p) %>% 
        formatar_numero(ndigitos = 0)
    })
    
    output$TaxaCobertura <- renderText({
      dados <- populacao_filtrada()
      req(nrow(dados) > 0)
      dados %>%
        slice(1) %>%
        pull(tx) %>% formatar_numero(percent = T, 
                                     digitos = 1, 
                                     ndigitos = 1)
    })
    
    return(reactive(input$DR))
  })
}