library(quantmod)
library(jsonlite)

dat <- getSymbols("^GSPC", auto.assign = FALSE, from = "2020-01-02")
cl <- Cl(dat)
rsi <- RSI(cl)

tmp <- as.data.frame(rsi)
df <- data.frame(date=rownames(tmp), rsi=tmp$rsi, close=as.vector(cl))
df <- tail(df, 20)

write.table(df, "gspc.txt", sep="\t", quote=FALSE, row.names = FALSE)

json <- toJSON(df, digits=2)
write(json, "gspc.json")

cat(dir())