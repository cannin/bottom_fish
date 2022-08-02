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

symbols <- c("AGG", "GBTC", "BITO", "SPBC", "IPO", "MUB", "QQQ", "RJA", "PDBC", "SSO", "SFYF", "TIP", "USRT", "VTI", "VXUS", "VT", "BNDW", "RLY", "AOK", "AOA")
offset <- 30
hi_rsi <- 65
lo_rsi <- 43

# GET DATA ----
results <- list()
close_df <- data.frame(symbol=symbols, close=NA, close_date=NA, stringsAsFactors=FALSE)
  
for(i in 1:length(symbols)) {
  #i <- 2
  symbol <- symbols[i]
  cat("Symbol: ", symbol, "\n")
  
  dat <- NULL
  dat <- tryCatch({
      # Get historic data
      getSymbols(Symbols=symbol, from=Sys.Date()-offset, auto.assign=TRUE)
      
      # Get current values
      org_cols <- c("Trade Time", "Open", "High", "Low", "Last", "Volume")
      mod_cols <- c("Trade Time", "Open", "High", "Low", "Close", "Volume")
      
      if(get_cur_date) {
        q1 <- getQuote(symbol, src="yahoo", what=standardQuote())
        q2 <- q1[, org_cols]
        colnames(q2) <- mod_cols
        
        # Convert to xts
        xts_quote <- xts(q2[, -1], as.Date(q2[, 1])) # use Date for indexClass
        xts_quote$Adjusted <- xts_quote[, 'Close'] # add an Adjusted column
        
        assign(symbol, rbind(get(symbol), xts_quote))      
      }
      
      get(symbol)
    }, error=function(e){})
  if(is.null(dat)) next() # if dat is still NULL go to next ticker
  
  dat_close <- Cl(dat)
  rsi <- RSI(dat_close)
  rsi$rsi <- round(rsi$rsi, 1)
  rsi_df <- as.data.frame(rsi)
  
  close_df$close[close_df$symbol == symbol] <- as.numeric(dat_close[nrow(dat_close)-1, 1])
  close_df$close_date[close_df$symbol == symbol] <- rownames(as.data.frame(dat_close))[nrow(dat_close)-1]

  tmp_results <- data.frame(date=rownames(rsi_df), rsi=rsi_df$rsi, close=dat_close, stringsAsFactors=FALSE)
  colnames(tmp_results) <- c("date", "rsi", "close")
  
  results[[symbol]] <- tmp_results
} 

all_results <- do.call(cbind, results)
cols <- paste0(symbols, ".rsi")

no_na_results <- all_results[!is.na(all_results[,2]),]
no_na_results_x <- no_na_results

i2 <- colnames(no_na_results)[grepl("rsi", colnames(no_na_results))]
x3 <- no_na_results[, c("VTI.date", i2)]

# RESULTS ----
s3 <- x3

for(i in 2:(2+length(symbols)-1)) {
  s3[s3[,i] < hi_rsi & s3[,i] > lo_rsi, i] <- NA
}

s4 <- s3[nrow(s3), 2:(2+length(symbols)-1)]
i3 <- which(!is.na(s4))

vals <- unlist(s4[i3])
names(vals) <- gsub(".rsi", "", names(vals))
vals_df <- data.frame(symbol=gsub(".rsi", "", names(vals)), rsi=vals, stringsAsFactors=FALSE)
vals_df <- merge(vals_df, close_df, all.x=TRUE)

last_date <- s3$VTI.date[nrow(s3)]

vals_str <- paste(names(vals), vals, collapse="_", sep=":")
vals_str <- paste0(vals_str, "_", last_date)

#run_time <- substr(capture.output(as.POSIXct(Sys.time(), tz="America/New_York")), 6, 28)
run_time <- lubridate::now(tz="America/New_York")
lst <- list(run_time=run_time, date=last_date, hi_rsi=hi_rsi, lo_rsi=lo_rsi, result_str=vals_str, result_df=vals_df)

# WRITE RESULTS ----
json <- toJSON(lst, digits=2, auto_unbox=TRUE, rownames=FALSE)
write(json, "rsi.json")

cat(dir())
cat(readLines("rsi.json"))

