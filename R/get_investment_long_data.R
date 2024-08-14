#' Given a long list of investment transactions in a data frame of transactions,
#' returns the position and value of each (account, security, symbol, date) on
#' dates going from the earliest date in data frame to latest day in data frame
#' 
#' For transactions with non-empty symbols, an attenpt to retrieve prices with 
#' tidyquant::tq_get is made.
#' If that fails, price is inferred from last buy/sell transaction from 
#' cash value and number of shares traded
#' 
#' Additional pricing data can be added through hard_coded_asset_price_df.
#' This is particularly useful for money market accounts.
#' 
#' For dates where prices for (security, symbol) are not available, 
#' last available price is used
#' 
#' @param investment_data_df a data frame containing cash transactions
#' @param hard_coded_asset_price_df a data frame containing (security, symbol, date, price) data
#' @return a data frame containing positions and balances at the dates of each 
#' (account, security, symbol, date)
#' @author Guilherme V Rocha
#' @importFrom zoo as.Date
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom tidyr complete
#' @importFrom tidyquant tq_get
#' @importFrom dplyr anti_join
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join
#' @export
get_investment_long_data = function(investment_data_df, hard_coded_asset_price_df = NULL){
  
  date_vec = zoo::as.Date(seq(as.numeric(min(investment_data_df$date)), as.numeric(today())))
  
  # Compute cash in investment accounts at each date
  unaligned_investment_cash_delta_df = 
    investment_data_df %>% 
    dplyr::group_by(account, date) %>% 
    dplyr::summarise(.groups = "drop", 
                     delta.shares = sum(amount)) %>%
    dplyr::filter(abs(delta.shares) > 1e-6) %>%
    as.data.frame() 
  
  aligned_investment_cash_delta_df = 
    unaligned_investment_cash_delta_df %>% 
    #tidyr::complete(account, date, fill = list(delta.shares = 0)) %>%
    dplyr::distinct(account) %>%
    merge(data.frame(date = date_vec), by = character(0)) %>%
    dplyr::left_join(unaligned_investment_cash_delta_df, by = c("account", "date")) %>%
    dplyr::mutate(delta.shares = mapply(ifelse, is.na(delta.shares), 0, delta.shares)) %>%
    dplyr::mutate(security = "CASH ON HAND", symbol = "") %>%
    dplyr::select(account, security, symbol, date, delta.shares) %>%
    as.data.frame()
  
  #---
  # Compute investment position at each date
  unaligned_transacted_shares_df = 
    investment_data_df %>% 
    dplyr::filter(shares != 0) %>%
    dplyr::select(account, security, symbol, date, shares) %>%
    dplyr::group_by(account, security, symbol, date) %>%
    dplyr::summarize(.groups = "drop", delta.shares = sum(shares)) %>%
    dplyr::arrange(account, security, symbol, date) %>%
    rbind(aligned_investment_cash_delta_df) %>%
    as.data.frame()
  
  aligned_transacted_shares_df = 
    unaligned_transacted_shares_df %>%
    tidyr::complete(tidyr::nesting(account, security, symbol), date, fill = list(delta.shares = 0)) 
  
  position_by_date_df = 
    aligned_transacted_shares_df %>%
    dplyr::arrange(account, security, symbol, date) %>%
    dplyr::group_by(account, security, symbol) %>%
    dplyr::mutate(n.shares = cumsum(delta.shares), 
                  n.shares = mapply(ifelse, 
                                    n.shares < 1e-6,
                                    0,
                                    n.shares)) %>%
    dplyr::select(-delta.shares) %>%
    as.data.frame()
  
  #---
  # Get asset prices from market data
  security_symbol_df = 
    position_by_date_df %>%
    dplyr::distinct(security, symbol)
  
  tq_get_gapped_price_df = 
    tidyquant::tq_get(setdiff(unique(position_by_date_df$symbol), ""),
                      from = min(position_by_date_df$date),
                      to   = Sys.time() %>% format("%Y-%m-%d"),
                      get  = "stock.prices") %>%
    dplyr::select(price_date = date, 
                  symbol, 
                  close, 
                  adjusted) %>% 
    dplyr::left_join(security_symbol_df, by = "symbol") %>%
    dplyr::select(security, symbol, dplyr::everything()) %>%
    as.data.frame()
  
  #---
  # Get asset prices from transactions
  full_inferred_price_df =
    investment_data_df %>%
    filter(abs(shares) > 1e-6,
           abs(amount) > 1e-6) %>%
    dplyr::mutate(raw_inferred_price = (abs(amount)+abs(comm.fee))/abs(shares)) %>% 
    dplyr::select(account, security, symbol, date, raw_inferred_price) %>%
    dplyr::arrange(account, security, symbol, date) %>% 
    dplyr::group_by(security, symbol, date) %>%
    dplyr::summarise(.groups = "drop", 
                     inferred_price     = median(raw_inferred_price), 
                     min_inferred_price = min(raw_inferred_price), 
                     max_inferred_price = max(raw_inferred_price)) %>% 
    as.data.frame() %>%
    dplyr::arrange(security, symbol, date)
  
  inferred_price_df = 
    full_inferred_price_df %>%
    dplyr::anti_join(tq_get_gapped_price_df, by = c("security", "symbol")) %>%
    dplyr::rename(price_date = date, 
                  close = inferred_price) %>%
    dplyr::mutate(adjusted = NA) %>%
    dplyr::select(security, 
                  symbol, 
                  price_date, 
                  close, 
                  adjusted)
  
  #---
  gapped_price_df = 
    rbind(tq_get_gapped_price_df %>% dplyr::mutate(source = "market:tq_get"), 
          inferred_price_df %>% dplyr::mutate(source = "transaction"))
  
  if(!is.null(hard_coded_asset_price_df)){
    gapped_price_df = 
      gapped_price_df %>%
      rbind(hard_coded_asset_price_df %>% 
              dplyr::mutate(price_date = min(c(tq_get_gapped_price_df$price_date, 
                                               inferred_price_df$price_date))))
  }
  
  price_df = 
    gapped_price_df %>% 
    dplyr::distinct(security, symbol) %>% 
    merge(data.frame(price_date = zoo::as.Date(min(gapped_price_df$price_date):max(gapped_price_df$price_date))), 
          by = character(0)) %>%
    dplyr::left_join(gapped_price_df, 
                     by = c("security", "symbol", "price_date")) %>%
    plyr::ddply(c("security", "symbol"), 
                function(z){
                  res = 
                    z %>%
                    dplyr::arrange(price_date) %>%
                    dplyr::mutate(source        = zoo::na.locf(source, na.rm = F), 
                                  last_close    = zoo::na.locf(close, na.rm = F), 
                                  last_adjusted = zoo::na.locf(adjusted, na.rm = F)) 
                  return(res)
                }) %>%
    dplyr::filter(!is.na(last_close)) %>%
    dplyr::rename(date = price_date)
  
  priced_position_df = 
    dplyr::inner_join(position_by_date_df, 
                      price_df, 
                      by = c("security", "symbol", "date")) %>%
    dplyr::mutate(value = last_close * n.shares) %>%
    as.data.frame()
  
  res = list(price_df = price_df, 
             priced_position_df = priced_position_df)
  return(res)
}

