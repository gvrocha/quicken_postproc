#' From data read from Quicken infers which accounts are investment accounts
#' and split transactions into investment, noninvestment accounts
#' 
#' @param unfiltered_transaction_data a data frame containing transactions as read from Quicken csv
#' @return A list with many data frames
#' @author Guilherme V Rocha
#' @export
split_investment_data = function(unfiltered_transaction_data){
  #---
  all_account_df = unfiltered_transaction_data %>% dplyr::distinct(account) %>% dplyr::arrange(account)
  all_date_df = unfiltered_transaction_data %>% dplyr::distinct(date) %>% dplyr::arrange(date)
  
  investment_account_df = 
    unfiltered_transaction_data %>%
    dplyr::filter(grepl("^Investment", category)) %>%
    dplyr::distinct(account) %>% 
    dplyr::arrange(account)
  
  #---
  noninvestment_account_df = 
    all_account_df %>%
    dplyr::anti_join(investment_account_df, 
                     by = "account")
  
  investment_data_df = 
    unfiltered_transaction_data %>% 
    dplyr::semi_join(investment_account_df, 
                     by = "account")
  
  cash_movement_df = 
    unfiltered_transaction_data %>% 
    dplyr::semi_join(noninvestment_account_df, 
                     by = "account")
  
  res = list(investment_account_df = investment_account_df, 
             noninvestment_account_df = noninvestment_account_df, 
             investment_movement_df = investment_data_df, 
             cash_movement_df = cash_movement_df)
  return(res)
}
