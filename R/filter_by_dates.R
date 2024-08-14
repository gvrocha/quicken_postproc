#' Filters a data frame read from Quicken by dates
#' 
#' @param df a data frame containing Quicken transactions 
#' (such as the one returned by read_transaction_file)
#' @param start_date the initial date to keep (larger than OR equal to)
#' @param end_date the final date to keep (smaller than OR equal to)
#' @return A data frame containing the transactions from Quicken file
#' @author Guilherme V Rocha
#' @importFrom dplyr filter
#' @export
filter_by_dates = function(df, start_date = "1500/01/01", end_date = "3000/01/01"){
  res = 
    df %>%
    dplyr::filter(date >= as.Date(start_date, "%Y/%m/%d")) %>%
    dplyr::filter(date <= as.Date(end_date, "%Y/%m/%d"))
  return(res)
}
