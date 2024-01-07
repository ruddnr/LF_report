pacman::p_load(kwlib, plotly, ggrepel, trelliscopejs, gt)
source("03.script/get_port_return.R")

port <- read_xlsx("02.port/LF_2312.xlsx") %>% 
  mutate(td = ymd(td))

read_rtn_sheet <- function(path, sheet_name) {
  read_xlsx(path, sheet = sheet_name) %>% 
    pivot_longer(-td, names_to = "ticker", values_to = "rtn") %>% 
    mutate(currency = str_extract(sheet_name, "usd|krw"),
           td = ymd(td),
           rtn = rtn / 100) %>% 
    mutate(wday = wday(td)) %>% 
    filter(!wday %in% c(1, 7)) %>% 
    select(-wday)
}

read_rtn_dat <- function(path, rtn_type = c("stock", "bm")) {
  excel_sheets(path) %>% 
    `[`(str_detect(., rtn_type)) %>% 
    map_dfr(
      .f = ~read_rtn_sheet(path, sheet_name = .x)
    )
}

# read_rtn_sheet("01.data/rtn.xlsx", sheet = "rtn_bm_usd")

rtn_file_path <- "01.data/rtn.xlsx"

daily_rtn_stock <- read_rtn_dat(rtn_file_path, "stock")
daily_rtn_bm <- read_rtn_dat(rtn_file_path, "bm") %>% 
  mutate(ticker = str_remove(ticker, "-US"))

daily_rtn_bm_usd <- daily_rtn_bm %>% 
  filter(currency == "usd") %>% 
  select(td, ticker, rtn)

daily_rtn_bm_krw <- daily_rtn_bm %>% 
  filter(currency == "krw") %>% 
  select(td, ticker, rtn)

port_rtn_usd <- get_port_return(port, daily_rtn_stock %>% filter(currency == "usd"))

port_rtn_usd2 <- port_rtn_usd %>% 
  pluck("port_return") %>%
  select(td, rtn = rtn_with_cost) %>% 
  mutate(ticker = "port")

plot_dat_usd <- 
  bind_rows(
    port_rtn_usd2,
    daily_rtn_bm_usd %>% 
      filter(td >= min(port_rtn_usd2$td))
  ) %>% 
  mutate(rtn_cum = cumprod(rtn + 1) - 1, .by = ticker)
  
g <- plot_dat_usd %>% 
  ggplot(aes(td, rtn_cum, color = ticker)) +
  geom_line() +
  scale_y_continuous(labels = label_percent())+
  labs(x = "", y = "", title = "Cumulative Return - USD")

ggplotly(g)

# 포트 수익 테이블
port_rtn_tbl <- plot_dat_usd %>% 
  filter(td >= "2024-01-01") %>% 
  mutate(
    rtn_1w = slide_dbl(rtn, ~prod(. + 1) - 1, .before = 4, .complete = TRUE),
    rtn_max = slide_dbl(rtn, ~prod(. + 1) - 1, .before = Inf, .complete = TRUE),
    .by = ticker
  ) %>% 
  filter(td == max(td)) %>% 
  select(ticker, rtn_1d = rtn, rtn_1w, rtn_max)

port_rtn_tbl %>% 
  gt() %>% 
  tab_header(
    title = md("**Performance**"),
    subtitle = "As of 2024-01-05"
  ) %>% 
  fmt_percent(
    columns = where(is.numeric)
  ) %>% 
  cols_label(
    ticker = md("**Type**"),
    rtn_1d = md("**1 Day**"),
    rtn_1w = md("**1 Week**"),
    rtn_max = md("**MTD**")
  ) %>% 
  sub_missing(missing_text = "")

port %>% 
  filter(td == "2023-10-31") %>% 
  left_join(
    port_rtn_usd %>% 
      pluck("port_weight_daily") %>% 
      filter(term == "2023-10-31") %>% 
      summarise(
        rtn_cum = last(rtn_cum),
        .by = ticker
      ),
    by = c("ticker")
  ) %>% 
  mutate(contrib = weight * (rtn_cum - 1)) %>% 
  mutate(contrib_theme = sum(contrib), .by = theme) %>% 
  mutate(ticker = fct_reorder(ticker, contrib_theme)) %>% 
  mutate(ticker = fct_reorder(ticker, contrib)) %>% 
  ggplot(aes(contrib, ticker, label = percent(contrib, accuracy = .1), fill = theme)) + 
  geom_col() + 
  scale_x_continuous(labels = label_percent(accuracy = .1)) +
  geom_label_repel(point.size = NA, fill = "white")
  

indv_rtn_tbl <- port_rtn_usd %>% 
  pluck("port_weight_daily") %>% 
  filter(term == "2023-10-31") %>% 
  # select(td, ticker, rtn, weight) %>% 
  mutate(
    rtn_1w = slide_dbl(rtn, ~prod(. + 1) - 1, .before = 4, .complete = TRUE),
    rtn_max = slide_dbl(rtn, ~prod(. + 1) - 1, .before = Inf, .complete = TRUE),
    .by = ticker
  ) %>% 
  filter(td == max(td)) %>% 
  select(term, ticker, weight, rtn_1d = rtn, rtn_1w, rtn_max)

indv_rtn_tbl2 <- port %>% 
  semi_join(indv_rtn_tbl, by = c("td" = "term")) %>% 
  select(theme, ticker, name) %>% 
  left_join(indv_rtn_tbl) %>% 
  mutate(n_ticker = n(), .by = ticker) %>% 
  mutate(weight = weight / n_ticker) %>% 
  select(-c(term, n_ticker))

indv_rtn_tbl2 %>% 
  gt(
    groupname_col = "theme",
  ) %>% 
  tab_header(
    title = md("**Performance**"),
    subtitle = "As of 2024-01-05"
  ) %>% 
  fmt_percent(
    columns = where(is.numeric)
  ) %>% 
  tab_spanner(
    label = "Performance",
    columns = c(contains("rtn"))
  ) %>% 
  cols_label(
    ticker = md("**Ticker**"),
    name = md("**Name**"),
    weight = md("**Weight**"),
    rtn_1d = md("**1 Day**"),
    rtn_1w = md("**1 Week**"),
    rtn_max = md("**MTD**")
  ) %>% 
  sub_missing(missing_text = "")
# 
# daily_rtn_stock %>% 
#   filter(currency == "usd") %>% 
#   semi_join(
#     port %>% filter(td == "2024-01-02"),
#     by = "ticker"
#   ) %>% 
#   mutate(rtn = cumprod(rtn + 1) -1, .by = ticker) %>% 
#   ggplot(aes(td, rtn)) +
#   geom_line() +
#   scale_y_continuous(labels = label_percent()) +
#   facet_trelliscope(~ticker, ncol = 3, scales = "free_y")





# 개별종목 highcharts
library(highcharter)

library(dy)
