library(dplyr)
library(httr)
library(readr)
library(RSelenium)
library(rvest)
library(stringr)

options(OutDec = ",")

is.error <- function(x) inherits(x, "try-error")

extrai_bens_candidato_de_html <- function(bens_html, xpath = "//*[@data-title]") {
  bens_vec <- bens_html %>%
    read_html() %>%
    xml_find_all(xpath) %>%
    html_text() %>%
    str_trim()

  # Linhas impares tem a descricao do bem e linhas pares o seu valor
  bens_df <- data_frame(
    bem = bens_vec[seq_along(bens_vec) %% 2 == 1],
    valor = bens_vec[seq_along(bens_vec) %% 2 == 0]) %>%
  mutate(valor = parse_number(valor, locale = locale(decimal_mark = ",")))

  return(bens_df)
}

extrai_bens_candidato_selenium <- function(candidato_link, selenium_client) {
  nome_candidato <- candidato_link$getElementText()[[1]]
  candidato_link$clickElement()
  
  xpath_lista_bens <- "//*[contains(text(), 'Lista de Bens Declarados')]"
  err <- try({
    lista_bens_link <- selenium_client$findElement("xpath", xpath_lista_bens)
    Sys.sleep(1) # aguarda mais 1 segundo
    lista_bens_link$clickElement()
  }, silent = TRUE)

    if (!is.error(err)) {
    xpath_bens <- "//*[@data-title]"
    selenium_client$findElement("xpath", xpath_bens)
    bens_html <- selenium_client$getPageSource()[[1]]
    bens_df <- extrai_bens_candidato_de_html(bens_html) %>%
      mutate(nome_candidato) %>%
      select(3, 1, 2)
  } else {
    bens_df <- data_frame()
  }
  return(bens_df)
}

extrai_bens_candidatos <- function(url_candidatos) {
  selenium_driver <- rsDriver(verbose = FALSE, check = FALSE)
  selenium_client <- selenium_driver[["client"]]
  selenium_client$setImplicitWaitTimeout(30000)
  
  bens_df <- data_frame()
  n_candidatos <- Inf
  i <- 1

  while (i <= n_candidatos) {
    selenium_client$navigate(url_candidatos)
    
    xpath_candidatos = "//*[contains(@class, 'visible-lg')]//a"
    candidatos_links <- selenium_client$findElements("xpath", xpath_candidatos)
    candidato_link <- candidatos_links[[i]]
    Sys.sleep(1) # aguarda 1 segundo
    bens_cand <- extrai_bens_candidato_selenium(candidato_link, selenium_client)
    bens_df <- bind_rows(bens_df, bens_cand)
    n_candidatos <- length(candidatos_links)
    i <- i + 1
  }
  
  selenium_client$close()
  selenium_client$closeServer()
  
  return(bens_df)
}

url_cands_presidente <- "http://divulgacandcontas.tse.jus.br/divulga/#/estados/2018/2022802018/BR/candidatos"
bens_presidentes <- extrai_bens_candidatos(url_cands_presidente)
