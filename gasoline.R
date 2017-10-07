library(tidyverse)
library(readr)
library(lubridate)


gasoline <-
  openxlsx::read.xlsx("gasoline.xlsx", sheet = "レギュラー") %>% 
    mutate(調査日 = as.Date(調査日, origin = "1899-12-30"))
names(gasoline) <-
  names(gasoline) %>% 
  stringr::str_replace_all(pattern = "\\.", replacement = "")

# レギュラーガソリン価格の推移
ggplot(gasoline)+
  geom_path(aes(x = 調査日, y = `全国`))+
  labs(title = paste("ガソリン価格  最新調査日：",
                     gasoline %>% arrange(desc(調査日)) %>% select(調査日) %>% 
                       slice(1) %>% unlist %>% as_date),
       y = "ガソリン価格(全国)")
ggsave("ガソリン価格の推移.png")

p <-
  gasoline %>% 
  gather(全国:九州沖縄局, key = area, value = value) %>% 
  filter(grepl("局$", `area`)) %>% 
  ggplot() +
  geom_line(aes(x = 調査日, y = value, color = area), size = 1)
p
plotly::ggplotly(p)


# 日経平均株価の推移
nikkei <- 
  read_csv("nikkei_stock_average_monthly_jp.csv",
           col_types = cols(), locale = locale(encoding="CP932"))

nikkei %>% mutate(date = ymd(データ日付)) %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymax = 高値, ymin = 安値), alpha = 0.5) +
  labs(y = "日経株価") +
  labs(title = "日経平均株価の推移(max = 高値, min = 安値)",
       y = "日経平均株価(円)")
ggsave("日経平均株価の推移.png")









