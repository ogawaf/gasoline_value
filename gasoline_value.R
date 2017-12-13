library(rvest)
library(magrittr)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
theme_set(theme_bw())
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
#tmp <- tempfile(fileext = ".xls")
tmp <- paste0(getwd(), "/gasoline_prices.xls")
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
ggplot(gasoline,aes(x = 調査日, y = 全国)) +
  geom_path() +
  scale_x_date(date_breaks = "2 year") +
  labs(title = paste0("これまでのガソリン価格の推移   最新調査日：", latest_survey_date)) +
  theme(axis.text.x = element_text(angle = 30, vjust = .5))
ggsave("change_in_gasoline_prices.png")

# 直近一か月のガソリン価格の推移
gasoline %>% 
  arrange(desc(調査日)) %>% 
  slice(1:52) %>% 
  ggplot(aes(x = 調査日, y = 全国)) +
  geom_path() +
  geom_point() +
  scale_x_date(date_breaks = "4 week") +
  labs(title = paste0("直近52週(約1年)のガソリン価格の推移  最新調査日：", latest_survey_date))  +
  theme(axis.text.x = element_text(angle = 30, vjust = .5))
ggsave("change_in_gasoline_prices(latest_1year).png")

gasoline %>% 
  mutate(inc_rate_zenkoku = 全国/dplyr::lag(全国)) %>% 
  arrange(desc(調査日)) %>% 
  slice(1:52) %>% 
  ggplot(aes(x = 調査日, y = inc_rate_zenkoku)) +
  geom_path() +
  geom_point() +
  geom_abline(slope = 0, intercept = 1, linetype = 2) + 
  scale_x_date(date_breaks = "4 week") +
  labs(title = paste0("直近52週(約1年)のガソリン価格の推移  最新調査日：", latest_survey_date))  +
  theme(axis.text.x = element_text(angle = 30, vjust = .5))
ggsave("increase_rate_in_gasoline_prices(latest_1year).png")
