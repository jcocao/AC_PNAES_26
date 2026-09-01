box::use(
  shiny[...],
  bslib[card_header,card_body],
  dplyr[`%>%`,
        filter,
        pull,
        mutate],
  echarts4r[...],
  htmlwidgets[JS],
  glue[glue],
  stringr[str_detect]
  
)

box::use(
  grafico_tempo_t = funcoes/grafico_tempo_a[tabela_Geracao],
  titulo_grafico_t = funcoes/titulo_grafico_a[transformar_titulo]
)

box::use(
  ./global[cor_p, cor_s1]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  echarts4rOutput(outputId = ns("chart_tempo_1"))
  

}

#' @export
server <- function(id, dados, filtro) {
  moduleServer(id, function(input, output, session) {
    
    output$chart_tempo_1 <- renderEcharts4r({
      
      if(length(filtro()) < 1){
        linha <- "."
      } else {
        if(filtro() == "BR"){
          linha <- "."
        }else{
          linha <- filtro()
        }
      }
      
      
      dados_aqui <-   dados() %>%
        filter(str_detect(DR, linha))
      
      titulo <- transformar_titulo(dados_aqui) %>% round()
      titulo <- paste0("Média de acessos por dia: ", titulo)
      
      if(nrow(dados_aqui) > 0){
        
        tabela <- tabela_Geracao(dados_aqui)
        
        cores <- tabela %>%
          mutate(cores = ifelse(str_detect(Data, "abr|mai|Apr|May") == 1, cor_p, cor_s1)) %>% 
          pull(cores)
        
        grafico <- tabela %>%
          #dplyr::select(-semestre) %>% 
          e_chart(Data) %>%
          e_bar(Acessos, colorBy = "data")  %>%
          e_grid(
            left = "7%",
            right = "7%",
            top = "19%",
            bottom = "12%"
          ) %>%
          e_legend(show = FALSE) %>%
          e_tooltip(valueFormatter =  JS('function(value) {
        var fmt = new Intl.NumberFormat("pt-BR",
        {style:"decimal",
        minimumFractionDigits:0,
        maximumFractionDigits:0});
        return fmt.format(value);
      }')) %>%
          #e_theme_custom(glue('{{"color":["{cor_p}"]}}')) %>%
          e_y_axis(formatter = JS('function(value) {
        var fmt = new Intl.NumberFormat("pt-BR",
        {style:"decimal",
        minimumFractionDigits:0,
        maximumFractionDigits:0});
        return fmt.format(value);
      }'),
                   axisLabel = list(fontSize = 14)) %>%
          e_title(text = "Total de acessos por dia, PNAES 2026",
                  textStyle = list(fontSize = 18,
                                   fontStyle = "normal"),
                  subtext = titulo, 
                  subtextStyle = list(fontSize = 14,
                                      fontStyle = "italic")) %>% 
          e_show_loading(text = "Carregando",
                         color = cor_s1,
                         text_color = "#000",
                         mask_color = "rgba(255, 255, 255, 1)")%>%
          e_color(cores)
      }
      
      if(nrow(dados_aqui) == 0){
        x <- data.frame(Sale = 1, modelo = "A", stringsAsFactors = F)
        
        grafico <- e_charts(x,
                            modelo) %>%
          e_bar(Sale,
                animation = T) %>%
          e_legend(show = FALSE) %>%
          e_color("transparent") %>%
          e_labels(position = "inside",
                   formatter = "DR sem acessos no momento",
                   fontSize = 30,
                   color = "black") %>%
          e_x_axis(show = FALSE) %>%
          e_y_axis(show = FALSE) %>% 
          e_show_loading(text = "Carregando",
                         color = cor_s1,
                         text_color = "#000",
                         mask_color = "rgba(255, 255, 255, 1)")
      }
      
      return(grafico)
      
    })
    
    
  })
}