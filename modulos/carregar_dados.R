box::use(
  dplyr[...],
  AzureAuth[get_azure_token],
  Microsoft365R[get_sharepoint_site]
)

#' @export
opcoes <- list(Norte = c(Acre = "AC", 
                         `Amapá` = "AP",
                         Amazonas = "AM", 
                         `Pará` = "PA", 
                         `Rondônia` = "RO",
                         Roraima = "RR",
                         Tocantins = "TO"),
               Nordeste = c(Alagoas = "AL",
                            Bahia = "BA",
                            `Ceará` = "CE", 
                            `Maranhão` = "MA",
                            `Paraíba` = "PB", 
                            Pernambuco = "PE",
                            `Piauí` = "PI",
                            `Rio Grande do Norte` = "RN", 
                            Sergipe = "SE"),
               `Centro-Oeste` = c(`Distrito Federal` = "DF", 
                                  `Goiás` = "GO",
                                  `Mato Grosso` = "MT",
                                  `Mato Grosso do Sul` = "MS"),
               Sudeste = c(`Espírito Santo` = "ES",
                           `Minas Gerais` = "MG", 
                           `Rio de Janeiro` = "RJ",
                           `São Paulo` = "SP"),
               Sul = c(`Paraná` = "PR", 
                       `Rio Grande do Sul` = "RS",
                       `Santa Catarina` = "SC"),
               `Departamento Nacional` = list(`Senac Gastronomia` = "SG"))



#' # SharePoint — Configurações ----------------------------------------------

PASTA_SHAREPOINT <- "00 - Area de Influencia/pnaes"
ARQ_DADOS <- "dados.Rds"
ARQ_DADOS_P <- "dados_p.Rds"

token_sp <- get_azure_token(
  resource = "https://graph.microsoft.com",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  password = Sys.getenv("secret_id"),
  auth_type = "client_credentials"
)

site <- get_sharepoint_site(
  site_url = "https://senacnacional.sharepoint.com/sites/GerProspecAvalEducacional",
  token = token_sp
)
drive <- site$get_drive("Documentos")


#' @export
dados_sharepoint <- function() reactivePoll(
  intervalMillis = 60 * 1000,
  session = NULL,

  checkFunc = function() {
    drive$get_item_properties(file.path(PASTA_SHAREPOINT,
                                        ARQ_DADOS))$fileSystemInfo$lastModifiedDateTime
  },

  valueFunc = function() {
    baixar_rds_sharepoint(drive,
                          file.path(PASTA_SHAREPOINT,
                                    ARQ_DADOS)
    )

  }
)
