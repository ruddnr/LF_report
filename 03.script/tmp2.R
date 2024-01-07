pacman::p_load(kwlib, plotly, ggrepel, trelliscopejs, gt)
source("03.script/get_port_return.R")

port <- read_xlsx("02.port/LF_2312.xlsx") %>% 
  mutate(td = ymd(td))

read_single_sheet <- function(path, sheet_name) {
  read_xlsx(path, sheet = sheet_name) %>% 
    pivot_longer(-td, names_to = "ticker", values_to = sheet_name) %>% 
    mutate(td = ymd(td))
}

filter_weekdays <- function(dat) {
  dat %>% 
    mutate(wday = wday(td)) %>% 
    filter(!wday %in% c(1, 7)) %>% 
    select(-wday)
}

read_excel_file <- function(path, sheet_vec) {
  sheet_vec %>% 
    map(
      .f = ~read_single_sheet(path, sheet_name = .x)
    ) %>% 
    reduce(left_join, by = c("td", "ticker")) 
}

path <- "01.data/data.xlsx"

sheet_vec <- excel_sheets(path)

daily_dat <- read_excel_file(path, c("open", "high", "low", "close", "rtn_stock_usd")) %>% 
  filter_weekdays() %>% 
  rename(rtn = rtn_stock_usd) %>% 
  mutate(rtn = rtn / 100)

daily_rtn_bm <- read_excel_file(path, "rtn_bm_usd") %>% 
  filter_weekdays() %>% 
  rename(rtn = rtn_bm_usd) %>% 
  mutate(rtn = rtn / 100) %>% 
  mutate(ticker = str_remove(ticker, "-US"))

financials_dat <- read_xlsx(path, sheet = "financials")

port_rtn <- get_port_return(port, daily_dat)

port_rtn2 <- port_rtn %>% 
  pluck("port_return") %>%
  select(td, rtn = rtn_with_cost) %>% 
  mutate(ticker = "port")

plot_dat<- 
  bind_rows(
    port_rtn2,
    daily_rtn_bm %>% 
      filter(td >= min(port_rtn2$td))
  ) %>% 
  mutate(rtn_cum = cumprod(rtn + 1) - 1, .by = ticker)
  
g <- plot_dat %>% 
  ggplot(aes(td, rtn_cum, color = ticker)) +
  geom_line() +
  scale_y_continuous(labels = label_percent())+
  labs(x = "", y = "", title = "Cumulative Return - USD")

ggplotly(g)

# dygraphs
library(dygraphs)
plot_dat_tmp <- plot_dat %>% 
  select(td, ticker, rtn_cum) %>% 
  pivot_wider(names_from = ticker, values_from = rtn_cum) %>% 
  mutate(diff_SPY = port - SPY, diff_QQQ = port - QQQ)

plot_dat_tmp %>% 
  select(td, port, SPY, QQQ) %>% 
  dygraph(main = "Portfolio Return", group = "port_cum_rtn") %>%
  dyRangeSelector() %>% 
  dyAxis(
    "y", 
    valueFormatter = "function(v){return (v*100).toFixed(1) + '%'}",
    axisLabelFormatter = "function(v){return (v*100).toFixed(0) + '%'}"
  ) 

plot_dat_tmp %>% 
  select(td, contains("diff")) %>% 
  dygraph(main = "Portfolio Return Diff", group = "port_cum_rtn") %>%
  dyAxis(
    "y", 
    valueFormatter = "function(v){return (v*100).toFixed(1) + '%'}",
    axisLabelFormatter = "function(v){return (v*100).toFixed(0) + '%'}"
  ) %>% 
  dyOptions(fillGraph = TRUE, fillAlpha = 0.4)




  
  dyLegend(width = 600)
  

# highchater
library(highcharter)
plot_dat %>% 
  select(td, ticker, rtn_cum) %>% 
  hchart("spline", hcaes(td, rtn_cum, color = ticker, group = ticker)) %>% 
  hc_title(
    text = "Portfolio Return"
  ) %>% 
  hc_subtitle(
    text = "This is an intereseting subtitle to give
    context for the chart or some interesting fact"
  ) %>% 
  hc_tooltip(
    shared = TRUE
  )

tmp <- plot_dat %>% 
  select(td, ticker, rtn_cum) %>% 
  pivot_wider(names_from = ticker, values_from = rtn_cum) %>% 
  mutate(diff_SPY = port - SPY, diff_QQQ = port - QQQ) 

highchart() %>% 
  hc_add_series(tmp, type = "spline", hcaes(x = td, y = "port"), name = "PORT") %>% 
  hc_add_series(tmp, type = "spline", hcaes(x = td, y = "SPY"), name = "SPY") %>% 
  hc_add_series(tmp, type = "spline", hcaes(x = td, y = "QQQ"), name = "QQQ") %>% 
  hc_tooltip(
    shared = TRUE
  ) %>% 
  hc_plotOptions(
    series = list(
      marker = 
        list(enabled = FALSE)
    )
  ) %>% 
  hc_yAxis(formatter = "function(v){return (v*100).toFixed(1) + '%'}")


# 포트 수익 테이블
target_rebal <- max(port$td)

port_rtn_tbl <- plot_dat %>% 
  filter(td > target_rebal) %>% 
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
library(dygraphs)


