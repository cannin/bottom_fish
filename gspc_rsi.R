library(quantmod)
library(jsonlite)

symbols <- c("gspc"="^GSPC", "vti"="VTI", "sso"="SSO")

for(i in 1:length(symbols)) {
  #i <- 2
  symbol <- unname(symbols[i])
  name <- names(symbols)[i]
  
  dat <- getSymbols(symbol, auto.assign = FALSE, from = "2023-01-02")
  cl <- Cl(dat)
  rsi <- RSI(cl)
  
  tmp <- as.data.frame(rsi)
  df_all <- data.frame(date=rownames(tmp), rsi=round(tmp$rsi, 2), close=round(as.vector(cl), 2))
  df_all$close_target <- round(df_all$close*0.99, 2)
  df_all$close_target_3 <- round(df_all$close*0.97, 2)
  df_all$close_target_5 <- round(df_all$close*0.95, 2)
  df <- tail(df_all, 4)
  
  t4 <- tail(df_all$close_target, 1)
  t3 <- tail(df_all$close[!is.na(df_all$rsi) & df_all$rsi >= 70], 1)
  diff_gain <- round(100*((t4-t3)/t3), 2)
  df$diff_gain_70 <- c(rep(NA, 3), diff_gain)  
  
  t2 <- RSI(c(as.numeric(cl[,1]), df$close_target[nrow(df)]))
  df$rsi_target <- c(rep(NA, 3), t2[length(t2)])   
  
  t2 <- RSI(c(as.numeric(cl[,1]), df$close_target_3[nrow(df)]))
  df$rsi_target_3 <- c(rep(NA, 3), t2[length(t2)])   

  t2 <- RSI(c(as.numeric(cl[,1]), df$close_target_5[nrow(df)]))
  df$rsi_target_5 <- c(rep(NA, 3), t2[length(t2)])
  
  write.table(df, paste0(name, ".txt"), sep="\t", quote=FALSE, row.names = FALSE)
  
  json <- toJSON(df, digits=2)
  write(json, paste0(name, ".json"))
}

cat(dir())