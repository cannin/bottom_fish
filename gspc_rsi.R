library(quantmod)
library(jsonlite)

symbols <- c("gspc"="^GSPC", "vti"="VTI")

for(i in 1:length(symbols)) {
  #i <- 1
  symbol <- unname(symbols[i])
  name <- names(symbols)[i]
  
  dat <- getSymbols(symbol, auto.assign = FALSE, from = "2020-01-02")
  cl <- Cl(dat)
  rsi <- RSI(cl)
  
  tmp <- as.data.frame(rsi)
  df <- data.frame(date=rownames(tmp), rsi=tmp$rsi, close=as.vector(cl))
  df <- tail(df, 20)
  
  write.table(df, paste0(name, ".txt"), sep="\t", quote=FALSE, row.names = FALSE)
  
  json <- toJSON(df, digits=2)
  write(json, paste0(name, ".json"))
}

cat(dir())