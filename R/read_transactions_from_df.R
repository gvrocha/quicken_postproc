#' Given data read from a Quicken file directly into a data frame
#' preprocesses data to esnsure quantities are numerical, column names are
#' more R friendly, etc
#' 
#' @param df a data frame as read from a Quicken exported csv file
#' @return A data frame containing the transactions from Quicken file
#' @author Guilherme V Rocha
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @export
read_transactions_from_df = function(df){
  res = 
    df %>% 
    dplyr::mutate(amount     = as.numeric(gsub(",", "", Amount, fixed = T)),
                  date       = as.Date(Date, "%m/%d/%Y"), 
                  inv.amt    = as.numeric(gsub(",", "", Invest.Amt, fixed = T)), 
                  shares.out = as.numeric(gsub(",", "", Shares.Out, fixed = T)),
                  shares.in  = as.numeric(gsub(",", "", Shares.In,  fixed = T)), 
                  shares     = as.numeric(gsub(",", "", Shares,     fixed = T)),
                  comm.fee   = as.numeric(gsub(",", "", Comm.Fee,   fixed = T))) %>%
    dplyr::mutate(inv.amt    = mapply(ifelse, is.na(inv.amt),    0, inv.amt), 
                  shares.out = mapply(ifelse, is.na(shares.out), 0, shares.out),
                  shares.in  = mapply(ifelse, is.na(shares.in),  0, shares.in),
                  shares     = mapply(ifelse, is.na(shares),     0, shares), 
                  comm.fee   = mapply(ifelse, is.na(comm.fee),     0, comm.fee)) %>%
    dplyr::select(date,
                  account = Account,
                  category = Category, 
                  amount,
                  payee  = Payee.Security, 
                  type   = Type,
                  tags   = Tags, 
                  inv.amt,
                  shares.in, 
                  shares.out, 
                  shares, 
                  dplyr::everything()) %>%
    dplyr::mutate(year  = as.numeric(format(date, "%Y")),
                  month = as.numeric(format(date, "%m")),
                  day   = as.numeric(format(date, "%d")),
                  coarse_category = sapply(category, function(x){return(unlist(strsplit(x, ":", fixed = T))[1])}),
                  fine_category   = sapply(category, function(x){return(unlist(strsplit(x, ":", fixed = T))[2])}), 
                  row.id = seq_along(date)) %>%
    dplyr::select(row.id, 
                  date,
                  year,
                  month,
                  day,
                  account,
                  category,
                  coarse_category,
                  fine_category,
                  amount,
                  payee,
                  type,
                  tags,
                  shares.out,
                  shares.in,
                  inv.amt,
                  dplyr::everything())
  
  return(res)
}
