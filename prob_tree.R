library(magrittr)
library(quantmod)
library(GetoptLong)
library(jsonlite)

# PARAMETERS ----
start_date <- "2006-01-03"
start_date <- "1980-01-02"
end_date <- as.character(Sys.Date())

symbol <- "^GSPC"

days_offset <- 10 

min_chng <- -8
max_chng <- min_chng + 1
min_chng <- 1 # Lowest
max_chng <- 2

q_all <- "up_dn_up_up$" # q_cond will always be +1 (up_*)

# DOWNLOAD DATA ----
# Download and save file 
tmp_dates <- c(as.Date(start_date), as.Date(end_date)-1)
dat_filename <- paste0("sp500_", paste(format(tmp_dates, format="%Y%m%d"), collapse = "_"), ".rds")
dat_filename

if(!file.exists(dat_filename)) {
  dat <- getSymbols(symbol, auto.assign=FALSE, from=start_date, to=end_date)
  dat_df <- as.data.frame(dat)
  tmp_dates <- as.Date(c(rownames(dat_df)[1], rownames(dat_df)[nrow(dat_df)]))
  saveRDS(dat_df, dat_filename)
}

# Load file
files <- dir(".", pattern="sp500.*rds", full.names=TRUE)
df <- file.info(files)
tmp_filename <- rownames(df)[which.max(df$mtime)]
tmp_filename
sp500 <- readRDS(tmp_filename) %>% as.data.frame
#sp500 <- readRDS("sp500_19800102_20200629.rds") %>% as.data.frame

start_idx <- which(rownames(sp500) == "2006-01-03")
start_idx <- 1
end_idx <- nrow(sp500)

dat <- sp500[start_idx:end_idx, ]

#days_offset <- 10 
n_na <- length(days_offset:nrow(dat))

tmp_dat <- data.frame(date=character(n_na), open=numeric(n_na), 
                      close1=numeric(n_na), close2=numeric(n_na), close3=numeric(n_na), close4=numeric(n_na), close5=numeric(n_na), 
                      diff0=numeric(n_na), diff1=numeric(n_na), diff2=numeric(n_na), diff3=numeric(n_na), diff4=numeric(n_na), diff5=numeric(n_na), 
                      stringsAsFactors = FALSE)
for(i in days_offset:(nrow(dat))) {
  tmp_dat$date[i-days_offset-1] <- rownames(dat)[i]
  # cat("I: ", i, " ", tmp_dat$date[i-days_offset-1], " ", rownames(dat)[i], "\n")

  tmp_dat$open[i-days_offset-1] <- dat$GSPC.Close[i]
  tmp_dat$close1[i-days_offset-1] <- dat$GSPC.Close[i-1]
  tmp_dat$close2[i-days_offset-1] <- dat$GSPC.Close[i-2]
  tmp_dat$close3[i-days_offset-1] <- dat$GSPC.Close[i-3]
  tmp_dat$close4[i-days_offset-1] <- dat$GSPC.Close[i-4]
  tmp_dat$close5[i-days_offset-1] <- dat$GSPC.Close[i-5]

  # Just yesterday
  tmp_dat$diff0[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-0]-dat$GSPC.Close[i-1])/dat$GSPC.Close[i-1], 3)  
  
  # Excluding yesterday, trailing
  tmp_dat$diff1[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-2])/dat$GSPC.Close[i-2], 3)
  tmp_dat$diff2[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-3])/dat$GSPC.Close[i-3], 3)
  tmp_dat$diff3[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-4])/dat$GSPC.Close[i-4], 3)
  tmp_dat$diff4[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-5])/dat$GSPC.Close[i-5], 3)
  tmp_dat$diff5[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-6])/dat$GSPC.Close[i-6], 3)
}

# BAD HACK: FIXME (BUT BETTER)
#tmp_dat <- head(tmp_dat, (nrow(tmp_dat)-days_offset))
tmp_idx <- which(grepl(format(Sys.Date(), "%Y"), tmp_dat$date))
tmp_idx <- tmp_idx[length(tmp_idx)]  
tmp_dat <- tmp_dat[1:tmp_idx,]

# OTHERS ----
# min_chng <- -8
# max_chng <- min_chng + 1
# min_chng <- 1 # Lowest
# max_chng <- 2

labels_1 <- expand.grid(a=c("dn","up"), stringsAsFactors = FALSE)
opts_1 <- expand.grid(a=c("<",">="))
results_1 <- list() 

for(i in 1:nrow(opts_1)) {
  #i <- 4
  tmpl_1 <- qq(paste0("which(tmp_dat$open @{opts_1[", i, ",1]} tmp_dat$close1)"))
  tmpl_1
  
  tmp_label <- labels_1[i,] %>% paste(., collapse = "_")
  results_1[[tmp_label]] <- eval(parse(text=tmpl_1)) %>% length 
  results_1[[paste0("idx_", tmp_label)]] <- eval(parse(text=tmpl_1)) 
}

results_1
results_1$up/(results_1$dn + results_1$up)
tmp_dat[c(results_1$idx_up, results_1$idx_dn), ] %>% nrow

labels_2 <- expand.grid(a=c("dn","up"), b=c("dn","up"), stringsAsFactors = FALSE)
opts_2 <- expand.grid(a=c("<",">="), b=c("<",">="))
results_2 <- list() 

for(i in 1:nrow(opts_2)) {
  #i <- 4
  tmpl_2 <- qq(paste0("which(tmp_dat$diff1 < @{max_chng} & tmp_dat$diff1 > @{min_chng} & tmp_dat$open @{opts_2[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_2[", i, ",2]} tmp_dat$close2)"))
  tmpl_2
  
  tmp_label <- labels_2[i,] %>% paste(., collapse = "_")
  results_2[[tmp_label]] <- eval(parse(text=tmpl_2)) %>% length  
  results_2[[paste0("idx_", tmp_label)]] <- eval(parse(text=tmpl_2)) 
}

results_2[names(results_2)[grepl("^up", names(results_2))]] %>% unlist %>% sum / results_2[names(results_2)[!grepl("^idx", names(results_2))]] %>% unlist %>% sum 
results_2[names(results_2)[!grepl("^idx", names(results_2))]] %>% unlist %>% sum
tmp_dat[unlist(results_2[names(results_2)[grepl("^idx", names(results_2))]]), ] %>% nrow

labels_3 <- expand.grid(a=c("dn","up"), b=c("dn","up"), c=c("dn","up"), stringsAsFactors = FALSE)
opts_3 <- expand.grid(a=c("<",">="), b=c("<",">="), c=c("<",">="))
results_3 <- list() 

for(i in 1:nrow(opts_3)) {
  #i <- 4
  tmpl_3 <- qq(paste0("which(tmp_dat$diff2 < @{max_chng} & tmp_dat$diff2 > @{min_chng} & tmp_dat$open @{opts_3[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_3[", i, ",2]} tmp_dat$close2 & tmp_dat$close2 @{opts_3[", i, ",3]} tmp_dat$close3)"))
  tmpl_3
  
  tmp_label <- labels_3[i,] %>% paste(., collapse = "_")
  results_3[[tmp_label]] <- eval(parse(text=tmpl_3)) %>% length  
  results_3[[paste0("idx_", tmp_label)]] <- eval(parse(text=tmpl_3)) 
}

results_3[names(results_3)[grepl("^up", names(results_3))]] %>% unlist %>% sum / results_3[names(results_3)[!grepl("^idx", names(results_3))]] %>% unlist %>% sum 
results_3[names(results_3)[!grepl("^idx", names(results_3))]] %>% unlist %>% sum
tmp_dat[unlist(results_3[names(results_3)[grepl("^idx", names(results_3))]]), ] %>% nrow

labels_4 <- expand.grid(a=c("dn","up"), b=c("dn","up"), c=c("dn","up"), d=c("dn","up"), stringsAsFactors = FALSE)
opts_4 <- expand.grid(a=c("<",">="), b=c("<",">="), c=c("<",">="), d=c("<",">="))
results_4 <- list() 

for(i in 1:nrow(opts_4)) {
  #i <- 4
  tmpl_4 <- qq(paste0("which(tmp_dat$diff3 < @{max_chng} & tmp_dat$diff3 > @{min_chng} & tmp_dat$open @{opts_4[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_4[", i, ",2]} tmp_dat$close2 & tmp_dat$close2 @{opts_4[", i, ",3]} tmp_dat$close3 & tmp_dat$close3 @{opts_4[", i, ",4]} tmp_dat$close4)"))
  tmpl_4
  
  tmp_label <- labels_4[i,] %>% paste(., collapse = "_")
  results_4[[tmp_label]] <- eval(parse(text=tmpl_4)) %>% length  
  results_4[[paste0("idx_", tmp_label)]] <- eval(parse(text=tmpl_4)) 
}

results_4[names(results_4)[grepl("^up", names(results_4))]] %>% unlist %>% sum / results_4 %>% unlist %>% sum 

labels_5 <- expand.grid(a=c("dn","up"), b=c("dn","up"), c=c("dn","up"), d=c("dn","up"), e=c("dn","up"), stringsAsFactors = FALSE)
opts_5 <- expand.grid(a=c("<",">="), b=c("<",">="), c=c("<",">="), d=c("<",">="), e=c("<",">="))
results_5 <- list() 

for(i in 1:nrow(opts_5)) {
  #i <- 4
  tmpl_5 <- qq(paste0("which(tmp_dat$diff4 < @{max_chng} & tmp_dat$diff4 > @{min_chng} & tmp_dat$open @{opts_5[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_5[", i, ",2]} tmp_dat$close2 & tmp_dat$close2 @{opts_5[", i, ",3]} tmp_dat$close3 & tmp_dat$close3 @{opts_5[", i, ",4]} tmp_dat$close4 & tmp_dat$close4 @{opts_5[", i, ",5]} tmp_dat$close5)"))
  tmpl_5
  
  tmp_label <- labels_5[i,] %>% paste(., collapse = "_")
  results_5[[tmp_label]] <- eval(parse(text=tmpl_5)) %>% length  
  results_5[[paste0("idx_", tmp_label)]] <- eval(parse(text=tmpl_5)) 
}

results_5[names(results_5)[grepl("^up", names(results_5))]] %>% unlist %>% sum / results_5 %>% unlist %>% sum 

min_chng
# results_1[names(results_1)[grepl("^up", names(results_1))]] %>% unlist %>% sum / results_1[names(results_1)[!grepl("^idx", names(results_1))]] %>% unlist %>% sum
# results_1[names(results_1)[!grepl("^idx", names(results_1))]] %>% unlist %>% sum

results_2[names(results_2)[grepl("^up", names(results_2))]] %>% unlist %>% sum / results_2[names(results_2)[!grepl("^idx", names(results_2))]] %>% unlist %>% sum 
results_2[names(results_2)[!grepl("^idx", names(results_2))]] %>% unlist %>% sum

results_3[names(results_3)[grepl("^up", names(results_3))]] %>% unlist %>% sum / results_3[names(results_3)[!grepl("^idx", names(results_3))]] %>% unlist %>% sum 
results_3[names(results_3)[!grepl("^idx", names(results_3))]] %>% unlist %>% sum

results_4[names(results_4)[grepl("^up", names(results_4))]] %>% unlist %>% sum / results_4[names(results_4)[!grepl("^idx", names(results_4))]] %>% unlist %>% sum 
results_4[names(results_4)[!grepl("^idx", names(results_4))]] %>% unlist %>% sum

results_5[names(results_5)[grepl("^up", names(results_5))]] %>% unlist %>% sum / results_5[names(results_5)[!grepl("^idx", names(results_5))]] %>% unlist %>% sum 
results_5[names(results_5)[!grepl("^idx", names(results_5))]] %>% unlist %>% sum

# Search
#lst <- results_5
#q_all <- "up_dn_up_up$"
var_name <- paste0("results_", length(strsplit(q_cond, "_")[[1]]))
lst <- get(var_name)
q_all <- q_all
q_cond <- paste0("up_", q_all)

idx_cond <- which(grepl(q_cond, names(lst)) & !grepl("^idx", names(lst)))
idx_all <- which(grepl(q_all, names(lst)) & !grepl("^idx", names(lst)))
idx_idx <- which(grepl(q_all, names(lst)) & grepl("^idx", names(lst)))
var_name
q_cond
q_all
lst[idx_cond] %>% unlist %>% sum / lst[idx_all] %>% unlist %>% sum 
lst[idx_all]
lst[idx_cond] %>% unlist %>% sum
lst[idx_all] %>% unlist %>% sum
tmp_dat[unname(unlist(lst[idx_idx])),] %>% nrow
tmp_results <- tmp_dat[unname(unlist(lst[idx_idx])),]
min_chng
max_chng
tmp_results$diff0[which(tmp_results$diff0 >= 0)] %>% summary
tmp_results$diff0[which(tmp_results$diff0 < 0)] %>% summary

pred_up <- lst[idx_cond] %>% unlist %>% sum / lst[idx_all] %>% unlist %>% sum
pred_up_prcnt <- tmp_results$diff0[which(tmp_results$diff0 >= 0)] %>% median
tmp_lst <- list(pred_date=Sys.Date(), pred_time=Sys.time(), cond=q_cond, pred_up=pred_up, pred_up_prcnt=pred_up_prcnt)
tmp_json <- toJSON(tmp_lst, auto_unbox=TRUE)
writeLines(tmp_json, "gspc_pred.json")

# EXAMPLE 
# 2020-04-16	2799.55	2783.36	2846.06	2761.63	2789.82	2749.98	0.582	-2.203	0.787	-0.232	1.214	4.661
# results_4 found Apr 9-15, dn_up_dn followed by up on 16

