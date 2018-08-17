library(dplyr)
library(httr)
library(readr)
library(RSelenium)
library(rvest)
library(stringr)

options(OutDec= ",")

baixa_html_bens_candidato <- function(url, xpath_bens = "//*[@data-title]") {
  rs_dr <- rsDriver(verbose = FALSE, check = FALSE)
  rem_dr <- rs_dr[["client"]]
  rem_dr$navigate(url)
  
  # Aguarda ate 30 segundos para carregar tabela (ate surgir item do xpath_bens)
  rem_dr$setImplicitWaitTimeout(30000)
  rem_dr$findElement("xpath", xpath_bens)
  
  page_source <- rem_dr$getPageSource()
  
  rem_dr$close()
  rem_dr$closeServer()
  
  return(page_source[[1]])
}

extrai_bens_candidato <- function(url) {
  bens_html <- baixa_html_bens_candidato(url)  %>%
    read_html()
  
  bens_vec <- bens_html %>%
    xml_find_all('//*[@data-title]') %>%
    html_text() %>%
    str_trim()
  
  bens_df <- data_frame(url, bem = bens_vec[seq_along(bens_vec) %% 2 == 1],
                        valor = bens_vec[seq_along(bens_vec) %% 2 == 0]) %>%
    mutate(valor = parse_number(valor, locale = locale(decimal_mark = ",")))
  
  return(bens_df)
}

# Teste com Lula
url <- "http://divulgacandcontas.tse.jus.br/divulga/#/candidato/2018/2022802018/BR/280000625869/bens"
bens <- extrai_bens_candidato(url)
