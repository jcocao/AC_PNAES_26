box::use(
  shiny[reactivePoll],
  dplyr[...],
  AzureAuth[get_azure_token],
  Microsoft365R[get_sharepoint_site]
)

# SharePoint — Configurações ----------------------------------------------

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

baixar_rds_sharepoint <- function(drive, caminho_remoto) {
  tmp <- tempfile(fileext = ".Rds")
  on.exit(unlink(tmp), add = TRUE)
  drive$download_file(
    src = caminho_remoto,
    dest = tmp,
    overwrite = TRUE
  )
  readRDS(tmp)
}

preparar_dados_populacao <- function(dados) {
  dados <- dados |>
    rename(DR = DR2)

  dados |>
    bind_rows(
      dados |>
        ungroup() |>
        summarise(
          DR = "BR",
          semestre = NA,
          pop_a = sum(pop_a, na.rm = TRUE),
          pop_p = sum(pop_p, na.rm = TRUE)
        ) |>
        mutate(tx = pop_p / pop_a)
    )
}

#' @export
brasil <- readRDS("data/br_uf_shape.Rds")

#' @export
dados_populacao <- preparar_dados_populacao(
  baixar_rds_sharepoint(
    drive,
    file.path(PASTA_SHAREPOINT, ARQ_DADOS_P)
  )
)

#' @export
dados_sharepoint <- function() {
  reactivePoll(
    intervalMillis = 60 * 1000,
    session = NULL,
    checkFunc = function() {
      drive$get_item_properties(
        file.path(PASTA_SHAREPOINT, ARQ_DADOS)
      )$fileSystemInfo$lastModifiedDateTime
    },
    valueFunc = function() {
      baixar_rds_sharepoint(
        drive,
        file.path(PASTA_SHAREPOINT, ARQ_DADOS)
      ) |>
        mutate(
          valido = ifelse(!is.na(sit.ocup), "valido", "invalido")
        )
    }
  )
}
