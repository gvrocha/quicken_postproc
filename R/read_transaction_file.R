#' Read a csv file exported by Quicken (assumed not to contain all realized 
#' transactions but no scheduled transactions)
#' 
#' @param infile the filename containing transactions exported by Quicken
#' @return A data frame containing the transactions from Quicken file
#' @author Guilherme V Rocha
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @export
read_transaction_file = function(infile){
  res = 
    read.csv(infile, 
             skip = 6, 
             stringsAsFactors = F) %>% 
    dplyr::mutate(amount = as.numeric(gsub(",", "", Amount, fixed = T)),
                  date   = as.Date(Date, "%m/%d/%Y"), 
                  shares.out_str = gsub(",", "", Shares.Out, fixed = T),
                  shares.in_str  = gsub(",", "", Shares.In, fixed = T),  
                  shares.out = as.numeric(shares.out_str),
                  shares.in  = as.numeric(shares.in_str)) %>%
    dplyr::select(date,
                  account = Account,
                  category = Category, 
                  amount,
                  payee  = Payee.Security, 
                  type   = Type,
                  tags   = Tags, 
                  shares.out_str,
                  shares.in_str, 
                  shares.out,
                  shares.in, 
                  inv.amt    = Invest.Amt) %>%
    dplyr::mutate(year  = as.numeric(format(date, "%Y")),
                  month = as.numeric(format(date, "%m")),
                  day   = as.numeric(format(date, "%d")),
                  coarse_category = sapply(category, function(x){return(unlist(strsplit(x, ":", fixed = T))[1])}),
                  fine_category   = sapply(category, function(x){return(unlist(strsplit(x, ":", fixed = T))[2])}), 
                  row.id = seq_along(date)) %>%
    dplyr::select(row.id, dplyr::everything())
  return(res)
}
