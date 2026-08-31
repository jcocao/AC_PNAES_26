box::use(
  dplyr[...],
  tidyr[...],
  purrr[keep_at, map],
  stringr[str_replace, str_replace_all, str_pad],
  lubridate[as_datetime, date, hour, minute, second, month, day],
  httr2[
    request,
    req_method,
    req_headers,
    req_body_raw,
    req_perform,
    resp_body_json,
    req_auth_bearer_token,
    req_url_query
  ],
  AzureAuth[get_azure_token],
  Microsoft365R[get_sharepoint_site],
  readr[read_csv]
)

ano <- "2026"
INTERVALO_SEGUNDOS <- 60L

# SharePoint ------------------------------------------------------------------

PASTA_SHAREPOINT <- "00 - Area de Influencia/pnaes"
ARQ_POP <- "pop2.Rds"
ARQ_POP_2 <- "dados_p.Rds"
ARQ_DADOS <- "dados.Rds"
ARQ_LOG <- "log_pnaes.txt"

# Log -------------------------------------------------------------------------

log_erros <- character(0)

registrar_log <- function(mensagem, drive = NULL) {
  
  linha <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ",
    mensagem
  )
  
  log_erros <<- c(log_erros, linha)
  
  message(linha)
  
  if (is.null(drive)) {
    return(invisible(linha))
  }
  
  tryCatch({
    
    tmp <- tempfile(fileext = ".txt")
    
    on.exit(
      unlink(tmp),
      add = TRUE
    )
    
    caminho_log <- file.path(
      PASTA_SHAREPOINT,
      ARQ_LOG
    )
    
    log_existente <- character(0)
    
    tryCatch({
      
      drive$download_file(
        src = caminho_log,
        dest = tmp,
        overwrite = TRUE
      )
      
      log_existente <- readLines(
        tmp,
        warn = FALSE,
        encoding = "UTF-8"
      )
      
    }, error = function(e) {
      log_existente <<- character(0)
    })
    
    log_completo <- unique(
      c(
        log_existente,
        log_erros
      )
    )
    
    writeLines(
      log_completo,
      con = tmp,
      useBytes = TRUE
    )
    
    drive$upload_file(
      src = tmp,
      dest = caminho_log
    )
    
  }, error = function(e) {
    
    message(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " | ERRO AO SALVAR LOG NO SHAREPOINT | ",
      conditionMessage(e)
    )
    
  })
  
  invisible(linha)
}

# Autenticação Microsoft / SharePoint ----------------------------------------

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

# Funções SharePoint ----------------------------------------------------------

baixar_rds_sharepoint <- function(drive, caminho_remoto) {
  
  tmp <- tempfile(fileext = ".Rds")
  
  on.exit(
    unlink(tmp),
    add = TRUE
  )
  
  drive$download_file(
    src = caminho_remoto,
    dest = tmp,
    overwrite = TRUE
  )
  
  readRDS(tmp)
}

enviar_rds_sharepoint <- function(objeto, drive, caminho_remoto) {
  
  tmp <- tempfile(fileext = ".Rds")
  
  on.exit(
    unlink(tmp),
    add = TRUE
  )
  
  saveRDS(
    objeto,
    tmp
  )
  
  drive$upload_file(
    src = tmp,
    dest = caminho_remoto
  )
}

# Token Sphinx ----------------------------------------------------------------

obter_token_sphinx <- function() {
  
  request(
    "https://pesquisa.senac.br/SphinxAuth/connect/token"
  ) |>
    req_method("POST") |>
    req_headers(
      `Content-Type` = "application/x-www-form-urlencoded"
    ) |>
    req_body_raw(
      paste0(
        "username=senac&token=",
        Sys.getenv("sphinx_token"),
        "&lang=pt-BR&grant_type=personal_token&client_id=sphinxapiclient"
      )
    ) |>
    req_perform() |>
    resp_body_json() |>
    keep_at("access_token") |>
    unlist()
}

# Contagem da pesquisa --------------------------------------------------------

obter_contagem_pesquisa <- function(token) {
  
  contagem <- request(
    paste0(
      "https://pesquisa.senac.br/sphinxapi/api/v4.1/survey/pnaes26"
      
    )
  ) |>
    req_auth_bearer_token(token) |>
    req_headers(
      Accept = "application/json"
    ) |>
    req_perform() |>
    resp_body_json() |>
    keep_at("recordsCount") |>
    unlist()
  
  as.integer(contagem[[1]])
}

# Funções auxiliares ----------------------------------------------------------

funcao_troca <- function(variavel, dic) {
  
  tam <- seq_along(dic)
  
  for (i in tam) {
    variavel <- str_replace(
      variavel,
      dic[i],
      as.character(i)
    )
  }
  
  return(variavel)
}

# Carregamento inicial --------------------------------------------------------

lotes <- drive$load_dataframe(
  file.path(
    PASTA_SHAREPOINT,
    "lotes.csv"
  ),
  delim = ";",
  col_types = "c",
  show_col_types = FALSE
)

pop <- baixar_rds_sharepoint(
  drive,
  file.path(
    PASTA_SHAREPOINT,
    ARQ_POP
  )
)

# Atualização dos dados -------------------------------------------------------

atualizar_dados <- function(
    drive,
    token,
    ano,
    mailing,
    lotes,
    pop) {
  
  tryCatch({
    
    pesquisa <- request(
      paste0(
        "https://pesquisa.senac.br/sphinxapi/api/v4.1/survey/pnaes26",
        "/data"
      )
    ) |>
      req_auth_bearer_token(token) |>
      req_headers(
        Accept = "application/json"
      ) |>
      req_url_query(
        variables = paste(c("termo",
                            "cpf",
                            "q2_01",
                            "q2_02",
                            "q2_03",
                            "q2_04",
                            "q2_07",
                            "q2_08",
                            "ORIGINE_SAISIE",
                            "DATE_SAISIE",
                            "DATE_ENREG",
                            "DATE_MODIF",
                            "TEMPS_SAISIE",
                            "APPAREIL_SAISIE",
                            "PROGRESSION",
                            "DERNIERE_QUESTION_SAISIE"), 
                          collapse = ";")) |>
      req_perform() |>
      resp_body_json() |>
      map(unlist) |>
      bind_rows()
    
    pesquisa <- pesquisa |>
      select(
        termo,
        cpf,
        q2_01,
        q2_02,
        q2_03,
        q2_04,
        q2_07,
        q2_08,
        origem = ORIGINE_SAISIE,
        dt.entrada = DATE_SAISIE,
        dt.conclusao = DATE_ENREG,
        dt.modif = DATE_MODIF,
        tempo = TEMPS_SAISIE,
        tp.aparelho = APPAREIL_SAISIE,
        sit.quest = PROGRESSION,
        ultima_resp = DERNIERE_QUESTION_SAISIE
      ) |>
      left_join(
        lotes,
        by = "cpf"
      )
    
    pesquisa <- pesquisa |>
      mutate(ocupado = ifelse(q2_01 %in% "Sim" |
                                (q2_02 %in% "Sim" & (q2_03 %in% "Férias, folga ou jornada de trabalho variável"|
                                                       q2_03 %in% "Licença maternidade ou paternidade"|
                                                       q2_03 %in% "Licença remunerada por motivo de saúde ou por ter se acidentado"|
                                                       q2_03 %in% "Outro tipo de licença remunerada (estudo, casamento, licença prêmio etc.)")) |
                                (q2_02 %in% "Sim" & (q2_03 %in% "Afastamento do próprio negócio/empresa, sem ser remunerado por instituto de previdência"|
                                                       q2_03 %in% "Fatores ocasionais (má condição climática, paralisação nos serviços de transporte, greve etc.)"|
                                                       q2_03 %in% "Outro motivo") & q2_04 %in% "Não"), 1, 0),
             desocupado = ifelse(q2_07 %in% "Sim" & q2_08 %in% "Sim", 1, 0),
             inativo    = ifelse(q2_07 %in% "Não" | (q2_07 %in% "Sim" & q2_08 %in% "Não"), 1, 0),
             sit.ocup   = ifelse(ocupado == 1, "Ocupado", 
                                 ifelse(desocupado == 1, "Desocupado",
                                        ifelse(inativo == 1, "Inativo", NA))),
             dt.entrada = as_datetime(dt.entrada,
                                      format = "%m/%d/%Y %I:%M:%S %p"),
             dt.conclusao = as_datetime(dt.conclusao,
                                        format = "%m/%d/%Y %I:%M:%S %p"),
             check2 = dt.entrada,
             hora.ini = paste(str_pad(hour(dt.entrada),
                                      width = 2,
                                      side = "left",
                                      pad = "0"),
                              str_pad(minute(dt.entrada),
                                      width = 2,
                                      side = "left",
                                      pad = "0"),
                              str_pad(second(dt.entrada),
                                      width = 2,
                                      side = "left",
                                      pad = "0"),
                              sep = ":"),
             hora.fim = paste(str_pad(hour(dt.conclusao),
                                      width = 2,
                                      side = "left",
                                      pad = "0"),
                              str_pad(minute(dt.conclusao),
                                      width = 2,
                                      side = "left",
                                      pad = "0"),
                              str_pad(second(dt.conclusao),
                                      width = 2,
                                      side = "left",
                                      pad = "0"),
                              sep = ":"),
             dt.ini = date(dt.entrada),
             dt.fim = date(dt.conclusao),
             dia = day(dt.entrada),
             dt.h = paste0(str_pad(month(dt.entrada),
                                   width = 2,
                                   side = "left",
                                   pad = "0"),
                           "/",
                           str_pad(day(dt.entrada),
                                   width = 2,
                                   side = "left",
                                   pad = "0"),
                           "-",
                           str_pad(hour(dt.entrada),
                                   width = 2,
                                   side = "left",
                                   pad = "0")
             ),
             tempo = round(as.numeric(tempo) / 60, 2),
             finalizado = ifelse(termo == "Sim" & sit.quest == "Terminado",
                                 1,
                                 0),
             incompleto = ifelse(termo == "Sim" & sit.quest == "Em andamento",
                                 1,
                                 0),
             id.pesquisa = row_number(),
             cpf = as.numeric(cpf),
             pesquisa = 1) |>
      select(-c(check2,
                q2_01,
                q2_03,
                q2_04,
                q2_02,
                q2_07,
                q2_08))
    
    pesquisa <- left_join(pesquisa, 
                          pop,
                          by = "cpf")
    
    pesquisa <- pesquisa |>
      mutate(across(where(is.character),
                    ~ na_if(., "")))
    
    painel <- pesquisa |>
      select(cpf,
             dia,
             tempo,
             dt.conclusao,
             DR = DR2,
             cod.unidade,
             tp.aparelho,
             valido,
             Total) |>
      as_tibble() |>
      mutate(DR2 = case_when(DR == "AC" ~ "12",
                             DR == "AL" ~ "27",
                             DR == "AM" ~ "13",
                             DR == "AP" ~ "16",
                             DR == "BA" ~ "29",
                             DR == "CE" ~ "23",
                             DR == "DF" ~ "53",
                             DR == "ES" ~ "32",
                             DR == "GO" ~ "52",
                             DR == "MA" ~ "21",
                             DR == "MG" ~ "31",
                             DR == "MS" ~ "50",
                             DR == "MT" ~ "51",
                             DR == "PA" ~ "15",
                             DR == "PB" ~ "25",
                             DR == "PE" ~ "26",
                             DR == "PI" ~ "22",
                             DR == "PR" ~ "41",
                             DR == "RJ" ~ "33",
                             DR == "RN" ~ "24",
                             DR == "RO" ~ "11",
                             DR == "RR" ~ "14",
                             DR == "RS" ~ "43",
                             DR == "SC" ~ "42",
                             DR == "SE" ~ "28",
                             DR == "SP" ~ "35",
                             DR == "TO" ~ "17",
                             .default = NA_character_),
             cod.unidade = paste(DR,
                                 cod.unidade,
                                 sep = "-"))
    
    
    
    enviar_rds_sharepoint(
      painel,
      drive,
      file.path(
        PASTA_SHAREPOINT,
        ARQ_DADOS
      )
    )
    
    enviar_rds_sharepoint(
      pesquisa,
      drive,
      file.path(
        PASTA_SHAREPOINT,
        ARQ_EXTRA
      )
    )
    
    n_linhas <- nrow(painel)
    
    cat(
      "Última atualização: ",
      format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " | linhas atualizadas: ",
      n_linhas,
      "\n",
      sep = ""
    )
    
    invisible(TRUE)
    
  }, error = function(e) {
    
    mensagem <- paste0(
      "ERRO em atualizar_dados | ",
      class(e)[1],
      " | ",
      conditionMessage(e)
    )
    
    registrar_log(
      mensagem,
      drive
    )
    
    message(
      "Atualização não concluída. ",
      "A rotina continuará no próximo ciclo."
    )
    
    invisible(FALSE)
  })
}

# Loop de monitoramento -------------------------------------------------------

ultima_contagem <- -1L

repeat {
  
  tryCatch({
    
    token <- obter_token_sphinx()
    
    contagem <- obter_contagem_pesquisa(
      token
    )
    
    if (is.na(contagem)) {
      
      mensagem <- paste0(
        "Contagem inválida; nova tentativa em ",
        INTERVALO_SEGUNDOS,
        " segundos."
      )
      
      message(mensagem)
      
      registrar_log(
        mensagem,
        drive
      )
      
      Sys.sleep(
        INTERVALO_SEGUNDOS
      )
      
    } else if (contagem > ultima_contagem) {
      
      sucesso <- atualizar_dados(
        drive,
        token,
        ano,
        mailing,
        lotes,
        pop
      )
      
      if (sucesso) {
        
        ultima_contagem <- contagem
        
        message(
          "Atualização concluída com sucesso. ",
          "recordsCount: ",
          contagem
        )
        
        gc()
        
      } else {
        
        message(
          "A atualização falhou. ",
          "ultima_contagem não será alterada. ",
          "A rotina tentará novamente no próximo ciclo."
        )
      }
      
      Sys.sleep(
        INTERVALO_SEGUNDOS
      )
      
    } else {
      
      Sys.sleep(
        INTERVALO_SEGUNDOS
      )
    }
    
  }, error = function(e) {
    
    mensagem <- paste0(
      "ERRO no loop principal | ",
      class(e)[1],
      " | ",
      conditionMessage(e)
    )
    
    registrar_log(
      mensagem,
      drive
    )
    
    message(
      "Erro capturado. ",
      "A rotina continuará em ",
      INTERVALO_SEGUNDOS,
      " segundos."
    )
    
    Sys.sleep(
      INTERVALO_SEGUNDOS
    )
  })
  
  if (Sys.Date() >= as.Date("2026-08-25")) {
    break
  }
}
