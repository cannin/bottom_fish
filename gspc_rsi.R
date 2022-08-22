library(quantmod)
library(jsonlite)

symbols <- c("gspc"="^GSPC", "vti"="VTI")

for(i in 1:length(symbols)) {
  #i <- 2
  symbol <- unname(symbols[i])
  name <- names(symbols)[i]
  
  dat <- getSymbols(symbol, auto.assign = FALSE, from = "2020-01-02")
  cl <- Cl(dat)
  rsi <- RSI(cl)
  
  tmp <- as.data.frame(rsi)
  df <- data.frame(date=rownames(tmp), rsi=round(tmp$rsi, 2), close=round(as.vector(cl), 2))
  df$close_target <- round(df$close*0.99, 2)
  df <- tail(df, 20)
  
  t1 <- c(as.numeric(cl$VTI.Close), df$close_target[nrow(df)])
  t1 <- c(as.numeric(cl$VTI.Close), df$close_target[nrow(df)])
  t2 <- RSI(t1)
  df$rsi_target <- c(rep(NA, 19), t2[length(t2)])
  
  write.table(df, paste0(name, ".txt"), sep="\t", quote=FALSE, row.names = FALSE)
  
  json <- toJSON(df, digits=2)
  write(json, paste0(name, ".json"))
}

cat(dir())