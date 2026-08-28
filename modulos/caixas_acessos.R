box::use(
  bslib[value_box,
        layout_column_wrap],
  bsicons[bs_icon],
  shiny[NS,
        moduleServer,
        textOutput,
        renderText, 
        req],
  htmltools[div]
)

box::use(
    funcoes/funcoes_auxiliares[...],
    funcoes/extras[...]
)




#' @export
ui <- function(id) {
  
  ns <- NS(id)
  layout_column_wrap(
    width = 1,
    value_box(
      title = "Total de Acessos:",
      value = textOutput(ns("TotalAcessos")),
      showcase = bs_icon("clipboard-data",
                         size="0.6em"),
      showcase_layout = "top right",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    value_box(
      title = "Tempo médio de resposta:",
      value = textOutput(ns("TempoMedio")),
      showcase = bs_icon("stopwatch-fill",
                         size="0.9em"),
      showcase_layout = "top right",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    value_box(
      title = "Tempo mediano de resposta:",
      value = textOutput(ns("TempoMediano")),
      showcase = bs_icon("stopwatch",
                         size="0.9em"),
      showcase_layout = "top right",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    )
  )

  }

#' @export
server <- function(id, dados_filtrado) {
  moduleServer(id, function(input, output, session) {
    
    output$TotalAcessos <- renderText({
      dados <- dados_filtrado()
      req(nrow(dados) > 0)
      saida <- nrow(dados_filtrado())
      
      saida <- formatar_numero(saida)
      
      return(saida)
    })
    
    output$TempoMedio <- renderText({
      dados <- dados_filtrado()
      req(nrow(dados) > 0)
      
      x <- dados_filtrado()
      
      if(nrow(x) > 0){
        saida <- calcular_estatistica_coluna(x,
                                             "tempo",
                                             "media") |>
          formatar_numero(
            digitos = 1, 
            ndigitos = 1) |> 
          paste("mins")
      }
      
      if(nrow(x) == 0) saida <- "0"
      
      return(saida)
    })
    
    output$TempoMediano <- renderText({
      dados <- dados_filtrado()
      req(nrow(dados) > 0)
      
      x <- dados_filtrado()
      if(nrow(x) > 0){
        saida <- calcular_estatistica_coluna(x,
                                             "tempo",
                                             "mediana") |>
          formatar_numero(
            digitos = 1, 
            ndigitos = 1) |> 
          paste("mins")
      }
      
      if(nrow(x) == 0) saida <- "0"
      
      return(saida)
    })
    
  })
}
