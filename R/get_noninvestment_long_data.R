#' Given a long list of transactions in a data frame containing only cash
#' transactions, returns the balances for each account
#' @param noninvestment_movement_df a data frame containing cash transactions
#' @return a data frame containing balances at the dates of each (account, date)
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom tidyr complete
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @author Guilherme V Rocha
#' @export
get_noninvestment_long_data = function(noninvestment_movement_df){
  unaligned_noninvestment_transactions_df = 
    noninvestment_movement_df %>% 
    dplyr::filter(amount != 0) %>%
    dplyr::select(account, date, amount) %>%
    dplyr::arrange(account, date) %>%
    as.data.frame()
  
  aligned_noninvestment_transactions_df = 
    unaligned_noninvestment_transactions_df %>% 
    tidyr::complete(account, date, fill = list(amount = 0)) %>%
    as.data.frame()
  
  noninvestment_balance_by_date_df = 
    aligned_noninvestment_transactions_df %>%
    dplyr::group_by(account, date) %>%
    dplyr::summarize(amount = sum(amount)) %>%
    dplyr::group_by(account) %>%
    dplyr::mutate(balance = cumsum(amount)) %>%
    dplyr::select(-amount) %>%
    as.data.frame()
  
  return(noninvestment_balance_by_date_df)
}
