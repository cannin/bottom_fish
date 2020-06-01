library(quantmod)

dat <- getSymbols("^GSPC", auto.assign = FALSE, from = "2020-01-02")
fpr <- Cl(dat)
rsi <- RSI(fpr)

tmp <- as.data.frame(rsi)
df <- data.frame(date=rownames(tmp), rsi=tmp$rsi)

write.table(df, "gspc.txt", sep="\t", quote=FALSE, row.names = FALSE)
