library(dplyr)
library(httr)
library(readr)
library(RSelenium)
library(rvest)
library(stringr)

options(OutDec= ",")

extrai_bens_candidato <- function(url) {
  rs_dr <- rsDriver(verbose = FALSE, check = FALSE)
  rem_dr <- rs_dr[["client"]]
  rem_dr$navigate(url)
  
  rem_dr$setImplicitWaitTimeout(10000)
  rem_dr$findElement("xpath", "//*[@data-title]")
  
  page_source <- rem_dr$getPageSource()
  
  rem_dr$close()
  rem_dr$closeServer()
  
  bens_html <- read_html(page_source[[1]])
  
  bens_raw <- bens_html %>%
    xml_find_all('//*[@data-title]') %>%
    html_text() %>%
    str_trim()
  
  bens_df <- data_frame(url,
                        bem = bens_raw[seq_along(bens_raw) %% 2 == 1],
                        valor = bens_raw[seq_along(bens_raw) %% 2 == 0]) %>%
    mutate(valor = parse_number(valor, locale = locale(decimal_mark = ",")))
  
  
  return(bens_df)
}

# Teste com Lula
url <- "http://divulgacandcontas.tse.jus.br/divulga/#/candidato/2018/2022802018/BR/280000625869/bens"
bens <- extrai_bens_candidato(url)
