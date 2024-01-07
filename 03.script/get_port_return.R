#' 포트폴리오 일간 수익률 계산 함수
#'
#' 포트폴리오 비중에 대해 거래수수료를 감안한 일간 수익률 계산
#' 포트비중의 날짜 종가로
#'
#' @param port_weight 포트비중 시계열. td(날짜), ticker(티커명), weight(비중) 열을
#'  가지고 있어야 함.
#' @param rtn_tbl 종목 일간 수익률. 종목에 대한 일간수익률 데이터는
#' td, ticker, rtn(수익률) 열을 가지고 있어야 함.
#' @param trd_cost 거래비용. 디폴트는 30bp
#' @param adjust_rebal_date 리밸런싱 일자 조정. 디폴트 값 1일 경우, 포트 비중에
#' 적힌 날짜의 종가로 매수하여, 그 다음날 수익률이 적용된다고 가정.
#' 예를들면 비중 데이터의 날짜가 10월 31일이라면 해당 비중으로 11월 1일 수익률부터 적용됨.
#' 데이터 가용시점을 고려한 보수적인 리밸런싱을 가정할 경우 날짜를 더 미룰 수 있음.
#' @return 일별 포트 수익률과 비용감안 포트 수익률
#' @import dtplyr
#' @export
get_port_return <- function(port_weight, rtn_tbl, trd_cost = 0.003, adjust_rebal_date = 1, losscut = 0.30) {

  port_weight <- sum_port_weight(port_weight)

  term_tbl <- get_term_tbl(port_weight = port_weight,
                           rtn_tbl = rtn_tbl,
                           adjust_rebal_date = adjust_rebal_date)

  port_weight_daily <- get_port_weight_daily(port_weight = port_weight,
                                             rtn_tbl = rtn_tbl,
                                             term_tbl = term_tbl,
                                             losscut)

  port_return <- calc_port_return(port_weight_daily = port_weight_daily, trd_cost = trd_cost)
  
  res <- list(
    port_weight_daily = port_weight_daily %>% filter(weight != 0),
    port_return = port_return
  )

  return(res)
}


#' @export
get_term_tbl <- function(port_weight, rtn_tbl, adjust_rebal_date = 1) {

  term_tbl <- tibble(
    td = unique(rtn_tbl$td)
  ) %>%
    filter(td >= min(port_weight$td)) %>%
    left_join(port_weight %>% distinct(td) %>% rename(term = td), by = join_by(closest(td >= term))) %>%
    fill(term, .direction = "down") %>%
    mutate(rebal = td == min(td), .by = term) %>%
    mutate(rebal = if_else(td == min(td), FALSE, rebal)) %>%
    mutate(td = lead(td, adjust_rebal_date)) %>%
    na.omit()

  return(term_tbl)
}

#' @export
sum_port_weight <- function(port_weight) {
  port_weight %>%
    summarise(weight = sum(weight), .by = c(td, ticker))
}


#' @export
# calc_drifting_weight <- function(dat, losscut) {
#   dat %>%
#     dtplyr::lazy_dt() %>%
#     mutate(weight = replace_na(weight, 0)) %>%
#     group_by(term, ticker) %>%
#     mutate(rtn_20d = rollapply(rtn + 1, 20, prod, align = "right", fill = NA),
#            rtn_20d_lag = lag(rtn_20d),
#            losscut_flag = if_else(rtn_20d_lag <= 1 - losscut, 1, NA_real_)) %>% 
#     fill(losscut_flag, .direction = "down") %>% 
#     mutate(losscut_flag = replace_na(losscut_flag, 0),
#            rtn_old = rtn,
#            rtn = if_else(losscut_flag == 1, 0, rtn),
#            rtn_cum = cumprod(1 + rtn)) %>% 
#     mutate(weight2 = rtn_cum * weight) %>%
#     mutate(weight2 = lag(weight2)) %>%
#     mutate(weight = if_else(is.na(weight2), weight, weight2)) %>%
#     ungroup() %>%
#     select(-weight2) %>%
# 
#     # 비중 노멀라이즈
#     group_by(td) %>%
#     mutate(weight = weight / sum(weight)) %>%
#     ungroup() %>%
#     collect()
# }


#' @export
calc_drifting_weight <- function(dat, losscut) {
  dat %>%
    dtplyr::lazy_dt() %>%
    mutate(weight = replace_na(weight, 0)) %>%
    group_by(term, ticker) %>%
    mutate(rtn_cum = cumprod(rtn + 1),
           rtn_max = cummax(rtn_cum),
           # rtn_rel_to_max = rtn_cum_lag / rtn_max,
           rtn_rel_to_max = rtn_max - rtn_cum,
           losscut_flag = if_else((rtn_cum <= 1-losscut) | (rtn_rel_to_max >= 0.25 & rtn_rel_to_max >= losscut), 1, NA_real_),
           # losscut_flag = if_else((rtn_rel_to_max >= 0.25 & rtn_rel_to_max >= losscut), 1, NA_real_),
           # 손절 신호는 다음으로 이연해야 함
           losscut_flag = lag(losscut_flag)
           ) %>% 
    fill(losscut_flag, .direction = "down") %>% 
    mutate(losscut_flag = replace_na(losscut_flag, 0),
           rtn_old = rtn,
           rtn = if_else(losscut_flag == 1, 0, rtn),
           rtn_cum = cumprod(1 + rtn)) %>% 
    mutate(weight2 = rtn_cum * weight) %>%
    mutate(weight2 = lag(weight2)) %>%
    mutate(weight = if_else(is.na(weight2), weight, weight2)) %>%
    ungroup() %>%
    select(-weight2) %>%
    
    # 비중 노멀라이즈
    group_by(td) %>%
    mutate(weight = weight / sum(weight)) %>%
    ungroup() %>%
    collect()
}


#' @export
get_port_weight_daily <- function(port_weight, rtn_tbl, term_tbl, losscut) {
  rtn_tbl %>%
    filter(ticker %in% port_weight$ticker) %>%
    filter(td >= min(term_tbl$td), td <= max(term_tbl$td)) %>%
    left_join(term_tbl, by = "td") %>%
    left_join(
      port_weight %>% rename(term = td),
      by = c("term", "ticker")
    ) %>%
    arrange(td) %>%
    calc_drifting_weight(losscut) %>%

    dtplyr::lazy_dt() %>%
    group_by(ticker) %>%
    mutate(diff = if_else(rebal == TRUE, weight - lag(weight), 0)) %>%
    # 분석초기 weight_chg 0으로 취급
    mutate(diff = replace_na(diff, 0)) %>%
    ungroup() %>%
    collect()
}


#' @export
calc_port_return <- function(port_weight_daily, trd_cost) {
  port_weight_daily %>%
    lazy_dt() %>%
    group_by(td) %>%
    summarise(
      rtn = sum(rtn * weight, na.rm = TRUE),
      diff = sum(abs(diff), na.rm  = TRUE) / 2,
      .groups = "drop"
    ) %>%
    mutate(rtn_with_cost = rtn - diff * trd_cost) %>%
    collect()
}
