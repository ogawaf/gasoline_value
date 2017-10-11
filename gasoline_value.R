library(rvest)
library(magrittr)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
# -----------------------
# ファイルのダウンロード
# -----------------------
enecho_url <- "http://www.enecho.meti.go.jp/statistics/petroleum_and_lpgas/pl007/results.html"
file_url <-
  read_html(enecho_url) %>% 
  rvest::html_nodes(xpath = "//ul[@class='ulist']/li/a") %>% 
  html_attr("href") %>% 
  .[1]
  
file_url <- paste0("http://www.enecho.meti.go.jp/statistics/petroleum_and_lpgas/pl007/",
                   file_url)  
tmp <- tempfile(fileext = ".xls")
download.file(url = file_url, destfile = tmp, mode = "wb")

# -----------------------
# ダウンロードファイルの読込
# -----------------------
gasoline <-
  readxl::read_xls(path = tmp, sheet = "レギュラー", col_types = "numeric") %>% 
  filter(!is.na(調査日))

names(gasoline) %<>% 
  stringr::str_replace_all(pattern = "[:space:]", "")

gasoline$調査日 <-
  gasoline$調査日%>% as.Date(origin = "1900-01-01")


latest_survey_date <- 
  gasoline %>% arrange(desc(調査日)) %>% slice(1) %>% magrittr::extract2("調査日")

# レギュラーガソリンの価格推移
ggplot(gasoline) +
  geom_path(aes(x = 調査日, y = 全国)) +
  labs(title = paste0("最新調査日：", latest_survey_date))
