library(quantmod)
library(jsonlite)
library(lubridate)

options("getSymbols.warning4.0"=FALSE)

# PARAMETERS ----
# Prevent getting the same day twice
cur_hour <- as.numeric(format(as.POSIXct(Sys.time(),format="%H:%M:%S"),"%H"))
if(cur_hour > 9) {
  get_cur_date <- TRUE
} else {
  get_cur_date <- FALSE
}

symbols_str <- "AGG
VTI
%5EVIX
SPXL
SPXU
SSO
SPUU
SDS
MUB
SPLB
SPTL
VNQ
USRT
QQQ
IPO
LGLV
IVV
VINIX
VIIIX
FXAIX
TIEIX
VSMPX
VBTLX
VBTIX
SPTM
STIP
SPAB
SSSYX
GBTC
VFIAX"
symbols <- strsplit(symbols_str, "\n")[[1]]

from_date <- "2023-01-01"

# GET DATA ----
close_df <- data.frame(symbol=symbols, close=NA, close_date=NA, stringsAsFactors=FALSE)
  
for(i in 1:length(symbols)) {
  #i <- 2
  symbol <- symbols[i]
  cat("Symbol: ", symbol, "\n")
  
  dat <- NULL
  dat <- tryCatch({
      # Get historic data
      getSymbols(Symbols=symbol, from=from_date, auto.assign=TRUE)

      get(symbol)
    }, error=function(e){})
  if(is.null(dat)) next() # if dat is still NULL go to next ticker
  
  dat_close <- Cl(dat)
  close_df$close[close_df$symbol == symbol] <- as.numeric(dat_close[nrow(dat_close), 1])
  close_df$close_date[close_df$symbol == symbol] <- rownames(as.data.frame(dat_close))[nrow(dat_close)]
}

# RESULTS ----
last_date <- close_df$close_date[close_df$symbol == "AGG"]
run_time <- lubridate::now(tz="America/New_York")
lst <- list(run_time=run_time, 
            date=last_date, 
            result_df=close_df)

# WRITE RESULTS ----
json <- toJSON(lst, digits=2, auto_unbox=TRUE, rownames=FALSE)
write(json, "quotes.json")

cat(dir())
cat(readLines("quotes.json"))

