box::use(
  shiny[moduleServer, NS, renderPlot, plotOutput, strong],
  bslib[card_header,card_body],
  dplyr[tibble, `%>%`, count, n, summarise,
        group_by, mutate, filter, case_when],
  echarts4r[...],
  stringr[str_detect],
  htmlwidgets[JS]
  
)

box::use(
  ./global[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  echarts4rOutput(outputId = ns("grafico_dr_1"))
  
  
  
}

#' @export
server <- function(id, dados, filtro) {
  moduleServer(id, function(input, output, session) {
    
    
    output$grafico_dr_1 <- renderEcharts4r({
      if(length(filtro()) < 1){
        linha <- "."
        check <- F
      } else {
        if(filtro() == "BR"){
          linha <- "."
          check <- F
        }else{
          linha <- filtro()
          check <- T
        }
      }
      
      titulo <- case_when(linha == "AC" ~ "Acre",
                          linha == "AL" ~ "Alagoas",
                          linha == "AM" ~ "Amazonas",
                          linha == "AP" ~ "Amapá",
                          linha == "BA" ~ "Bahia",
                          linha == "CE" ~ "Ceará",
                          linha == "DF" ~ "Distrito Federal",
                          linha == "ES" ~ "Espírito Santo",
                          linha == "GO" ~ "Goiás",
                          linha == "MA" ~ "Maranhão",
                          linha == "MG" ~ "Minas Gerais",
                          linha == "MS" ~ "Mato Grosso do Sul",
                          linha == "MT" ~ "Mato Grosso",
                          linha == "PA" ~ "Pará",
                          linha == "PB" ~ "Paraíba",
                          linha == "PE" ~ "Pernambuco",
                          linha == "PI" ~ "Piaúi",
                          linha == "PR" ~ "Paraná",
                          linha == "RJ" ~ "Rio de Janeiro",
                          linha == "RN" ~ "Rio Grande do Norte",
                          linha == "RO" ~ "Rondônia",
                          linha == "RR" ~ "Roraima",
                          linha == "RS" ~ "Rio Grande do Sul",
                          linha == "SC" ~ "Santa Catarina",
                          linha == "SE" ~ "Sergipe",
                          linha == "SP" ~ "São Paulo",
                          linha == "TO" ~ "Tocantins",
                          .default = "Brasil")
      
      dados_aqui <- dados() %>%
        filter(str_detect(DR, linha)) %>%
        count(tp.aparelho, name = "Quantidade", sort = T)
      
      
      if(nrow(dados_aqui) > 0){
        grafico <- dados_aqui %>%
          e_charts(tp.aparelho) %>%
          e_pie(Quantidade,
                percentPrecision = 2,
                center = c("50%", "50%"),
                itemStyle = list(borderColor = "rgba(0, 0, 0, 0.30)"),
                labelLine = list(show = TRUE,
                                 length = 5,
                                 length2 = 5,
                                 shadowColor = 'rgba(0, 0, 0, 100)',
                                 shadowBlur = 2)) %>%
          e_color(c(cor_p,
                    cor_s,
                    cor_s2)) %>% 
          e_tooltip(valueFormatter = JS("function(value) {
          saida = value.toString().replace(',', '.');
          return saida
        }")) %>%
          e_labels(formatter = JS("function(params) {
          saida = params.percent.toFixed(2).toString().replace('.', ',');
          return saida + '%'
        }"),
                   position = "outside",
                   fontSize = 16) %>%
          e_legend(orient = 'vertical',
                   left = "0%",
                   top = "bottom",
                   itemStyle = list(borderColor =  "rgba(0, 0, 0, 1)",
                                    borderWidth =  0.5),
                   selectedMode = FALSE) %>%
          e_title(text = "Distribuição dos acessos, por tipo de aparelho utilizado,\nPNAES 2026",
                  #subtext = titulo,
                  textStyle = list(fontSize = 18,
                                   fontStyle = "normal")) %>% 
          e_show_loading(text = "Carregando",
                         color = cor_s1,
                         text_color = "#000",
                         mask_color = "rgba(255, 255, 255, 1)")
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