# quickenpostproc

## To install

```
devtools::install_github("gvrocha/quickenpostproc", force=TRUE, build_vignettes = TRUE)
```

## Account name prefixes:

I assume that different account types have  prefix indicating the type of account it is.   
This makes it easier to group transactions later (for instance, for getting total expected sales proceeds from real estage, do RES-MTG-ELE accounts).   

For instance, I might name a checking account at Wells Fargo as ``CHK: WF 1234``.   

```
CHK	Checking
SAV	Savings
BRK	Brokerage
CRC	Credit card
SET	Settlement
ZCR	Credits
CAS	Cash

RES	Real estate
MTG	Mortgage
ELE	Expected liquidation expense
PRP	Property

GCR	Gift cards
BUF	Buffers
DEP	Depositos
```
