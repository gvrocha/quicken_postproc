#---
#' Fills in a data frame data_df of balances with the dates listed in date_vec
#' Last balance is carried forward
#' @param data_df a data frame containing non-investment transactions
#' @param date_vec a vector of dates at which balances are reported
#' @return a data frame with balances for each account on dates listed in date_vec
#' @importFrom zoo as.Date
#' @importFrom zoo na.locf
#' @importFrom plyr ddply
#' @importFrom dplyr semi_join
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export
get_balance_at_date = function(data_df, date_vec){
  account_vec = data_df$account %>% unique() %>% sort()
  date_vec    = zoo::as.Date(date_vec)
  
  res = 
    data.frame(account = account_vec) %>%
    plyr::ddply(colnames(.), 
                function(z){
                  these_data = 
                    data_df %>%
                    dplyr::semi_join(z, by = "account")
                  missing_dates = zoo::as.Date(setdiff(date_vec, these_data$date))
                  if(length(missing_dates) > 0){
                    inres = 
                      these_data %>%
                      dplyr::rename(balance.0 = balance) %>%
                      rbind(data.frame(account = unique(z$account), 
                                       date = missing_dates, 
                                       balance.0 = NA)) %>%
                      dplyr::arrange(account, date) %>%
                      dplyr::mutate(balance = zoo::na.locf(balance.0, na.rm = F)) %>%
                      dplyr::select(-balance.0)
                  }else{
                    inres = these_data
                  }
                  inres = 
                    inres %>% 
                    dplyr::filter(date %in% date_vec)
                  return(inres)
                })
  return(res)
}
