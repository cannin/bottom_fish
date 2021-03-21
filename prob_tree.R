library(magrittr)
library(quantmod)
library(GetoptLong)
library(jsonlite)

# FIXME: MOVE THIS ----
library(tidyverse)

#curl -s https://money.cnn.com/data/fear-and-greed/ | ~/go/bin/pup 'div#needleChart ul li:first-child text{}' | ggrep -oP '\d+'

fear_greed <- xml2::read_html("https://money.cnn.com/data/fear-and-greed/") %>%
  rvest::html_node("div#needleChart ul li:first-child") %>%
  rvest::html_text() %>% 
  readr::parse_number()
# FIXME: MOVE THIS ----

# PARAMETERS ----
start_date <- "2006-01-03"
start_date <- "1980-01-02"
end_date <- as.character(Sys.Date())

symbol <- "^GSPC"

days_offset <- 10

min_chng <- -8
max_chng <- min_chng + 1
min_chng <- 0 # Lowest
max_chng <- 1

q_all <- "dn_up_dn_up$" # q_cond will always be +1 (up_*)
use_auto <- 0 # Use 0 for production, 1 for testing (NOTE: IF NOT 0 THEN CHANGE RESULTS MANUALLY)
max_days_auto <- 4

# LOAD DATA ----
# Download and save file 
tmp_dates <- c(as.Date(start_date), as.Date(end_date))
dat_filename <- paste0("sp500_", paste(format(tmp_dates, format="%Y%m%d"), collapse = "_"), ".rds")
dat_filename

if(!file.exists(dat_filename)) {
  dat <- getSymbols(symbol, auto.assign=FALSE, from=start_date, to=end_date)
  dat_df <- as.data.frame(dat)
  
  cur_day_dat <- getQuote(symbol)
  tmp_df <- data.frame(GSPC.Open=cur_day_dat$Open, GSPC.High=cur_day_dat$High, GSPC.Low=cur_day_dat$Low, GSPC.Close=cur_day_dat$Last, GSPC.Volume=cur_day_dat$Volume, GSPC.Adjusted=NA)
  dat_df <- rbind(dat_df, tmp_df)
  rownames(dat_df)[nrow(dat_df)] <- as.character(Sys.Date())

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

# FILTER DATA ----
start_idx <- which(rownames(sp500) == "2006-01-03")
start_idx <- 1
end_idx <- nrow(sp500)

dat <- sp500[start_idx:end_idx, ]

#days_offset <- 10 
n_na <- length(days_offset:nrow(dat))

tmp_dat <- data.frame(date=character(n_na), open=numeric(n_na), 
                      close0=numeric(n_na), close1=numeric(n_na), close2=numeric(n_na), close3=numeric(n_na), close4=numeric(n_na), 
                      close5=numeric(n_na), close6=numeric(n_na), close7=numeric(n_na), close8=numeric(n_na),
                      diff01=numeric(n_na), diff02=numeric(n_na), diff03=numeric(n_na), diff04=numeric(n_na), diff05=numeric(n_na), 
                      diff06=numeric(n_na), diff07=numeric(n_na), diff08=numeric(n_na), 
                      diff11=numeric(n_na), diff12=numeric(n_na), diff13=numeric(n_na), diff14=numeric(n_na), diff15=numeric(n_na), 
                      diff16=numeric(n_na), diff17=numeric(n_na), diff18=numeric(n_na), 
                      stringsAsFactors = FALSE)
for(i in days_offset:(nrow(dat))) {
  # i <- 15
  tmp_dat$date[i-days_offset-1] <- rownames(dat)[i]
  #cat("I: ", i, " ", tmp_dat$date[i-days_offset-1], " ", rownames(dat)[i], "\n")
  #cat("I: ", i, "\n")

  tmp_dat$open[i-days_offset-1] <- dat$GSPC.Open[i-0]
  tmp_dat$close0[i-days_offset-1] <- dat$GSPC.Close[i-0]
  tmp_dat$close1[i-days_offset-1] <- dat$GSPC.Close[i-1]
  tmp_dat$close2[i-days_offset-1] <- dat$GSPC.Close[i-2]
  tmp_dat$close3[i-days_offset-1] <- dat$GSPC.Close[i-3]
  tmp_dat$close4[i-days_offset-1] <- dat$GSPC.Close[i-4]
  tmp_dat$close5[i-days_offset-1] <- dat$GSPC.Close[i-5]
  tmp_dat$close6[i-days_offset-1] <- dat$GSPC.Close[i-6]
  tmp_dat$close7[i-days_offset-1] <- dat$GSPC.Close[i-7]
  tmp_dat$close8[i-days_offset-1] <- dat$GSPC.Close[i-8]

  # Just yesterday
  tmp_dat$diff01[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-0]-dat$GSPC.Close[i-1])/dat$GSPC.Close[i-1], 3)  
  tmp_dat$diff02[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-0]-dat$GSPC.Close[i-2])/dat$GSPC.Close[i-2], 3)
  tmp_dat$diff03[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-0]-dat$GSPC.Close[i-3])/dat$GSPC.Close[i-3], 3)
  tmp_dat$diff04[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-0]-dat$GSPC.Close[i-4])/dat$GSPC.Close[i-4], 3)
  tmp_dat$diff05[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-0]-dat$GSPC.Close[i-5])/dat$GSPC.Close[i-5], 3)
  tmp_dat$diff06[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-0]-dat$GSPC.Close[i-6])/dat$GSPC.Close[i-6], 3)
  tmp_dat$diff07[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-0]-dat$GSPC.Close[i-7])/dat$GSPC.Close[i-7], 3)
  tmp_dat$diff08[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-0]-dat$GSPC.Close[i-8])/dat$GSPC.Close[i-8], 3)
  
  # Excluding yesterday, trailing
  tmp_dat$diff11[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-2])/dat$GSPC.Close[i-2], 3)
  tmp_dat$diff12[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-3])/dat$GSPC.Close[i-3], 3)
  tmp_dat$diff13[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-4])/dat$GSPC.Close[i-4], 3)
  tmp_dat$diff14[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-5])/dat$GSPC.Close[i-5], 3)
  tmp_dat$diff15[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-6])/dat$GSPC.Close[i-6], 3)
  tmp_dat$diff16[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-7])/dat$GSPC.Close[i-7], 3)
  tmp_dat$diff17[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-8])/dat$GSPC.Close[i-8], 3)
  tmp_dat$diff18[i-days_offset-1] <- round(100*(dat$GSPC.Close[i-1]-dat$GSPC.Close[i-9])/dat$GSPC.Close[i-9], 3)
}

# BAD HACK: FIXME (BUT BETTER)
# NOTE: 2021-03-06 NOT SURE WHAT PROBLEM WAS FIXED; Old date entry? 
#tmp_dat <- head(tmp_dat, (nrow(tmp_dat)-days_offset))
tmp_idx <- which(grepl(format(Sys.Date(), "%Y"), tmp_dat$date))
tmp_idx <- tmp_idx[length(tmp_idx)]  
tmp_dat <- tmp_dat[1:tmp_idx,]

cat("LAST: ", unlist(tmp_dat[nrow(tmp_dat),]), "\n")

# Get signs for day changes
# Previous day to current: 0: no change, 1: positive change, -1: negative change
signPastDays <- (sign(tmp_dat[nrow(tmp_dat):(nrow(tmp_dat)-10), "diff01"]))
#signPastDays <- c(1,  1,  1,  1, -1,  1,  1)

# Get streak
tmpStreakDay <- 1
# Start with day 2, subtract 1 to account for the first day
while(signPastDays[2+tmpStreakDay-1] == signPastDays[1]) {
  tmpStreakDay <- tmpStreakDay + 1
}

# Get signs of the days in the streak
curSignPastDays <- signPastDays[1:tmpStreakDay]

# Make querys and get the min/max intervals of change
# Signed query
q_all_sign_auto0 <- sapply(1:length(curSignPastDays), function(i) { ifelse(curSignPastDays[i] > 0, "up", "dn") }) %>% paste(., collapse="_") %>% paste0(., "$")
m0 <- tmp_dat[tmp_idx, paste0("diff0", tmpStreakDay)] # the last date entry
min_chng_sign_auto0 <- floor(m0)
max_chng_sign_auto0 <- ceiling(m0)

# Unsigned query 
q_all_auto0 <- sapply(1:max_days_auto, function(i) { ifelse((sign(tmp_dat[tmp_idx, paste0("diff0", i)]) > 0), "up", "dn") }) %>% paste(., collapse="_") %>% paste0(., "$")
q_all_auto1 <- sapply(1:max_days_auto, function(i) { ifelse((sign(tmp_dat[tmp_idx, paste0("diff1", i)]) > 0), "up", "dn") }) %>% paste(., collapse="_") %>% paste0(., "$")
m0 <- tmp_dat[tmp_idx, paste0("diff0", max_days_auto)]
min_chng_auto0 <- floor(m0)
max_chng_auto0 <- ceiling(m0)
m1 <- tmp_dat[tmp_idx, paste0("diff1", max_days_auto)]
min_chng_auto1 <- floor(m1)
max_chng_auto1 <- ceiling(m1)

# Set parameters
if(use_auto == 0) {
  min_chng <- min_chng_auto0
  max_chng <- max_chng_auto0
  q_all <- q_all_auto0
  days_checked <- max_days_auto
  
  min_chng_sign <- min_chng_sign_auto0
  max_chng_sign <- max_chng_sign_auto0
  q_all_sign <- q_all_sign_auto0
  days_checked_sign <- tmpStreakDay
} else if(use_auto == 1) {
  min_chng <- min_chng_auto1
  max_chng <- max_chng_auto1
  q_all <- q_all_auto1
  days_checked <- max_days_auto
  
  min_chng_sign <- NA
  max_chng_sign <- NA
  q_all_sign <- NA
  days_checked_sign <- tmpStreakDay
}

# base looks back max_days_auto vs sign looks back only for the streak
parameters <- list(base=list(min_chng=min_chng, max_chng=max_chng, q_all=q_all, days_checked=days_checked), 
                   sign=list(min_chng=min_chng_sign, max_chng=max_chng_sign, q_all=q_all_sign, days_checked=days_checked_sign))
all_results <- list()

for(j in 1:length(parameters)) {
  #i <- 2
  cat("J: ", j, "\n")
  name <- names(parameters)[j]
  min_chng <- parameters[[j]]$min_chng
  max_chng <- parameters[[j]]$max_chng
  q_all <- parameters[[j]]$q_all
  
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
    tmpl_1 <- qq(paste0("which(tmp_dat$close0 @{opts_1[", i, ",1]} tmp_dat$close1)"))
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
    tmpl_2 <- qq(paste0("which(tmp_dat$diff11 < @{max_chng} & tmp_dat$diff11 > @{min_chng} & tmp_dat$close0 @{opts_2[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_2[", i, ",2]} tmp_dat$close2)"))
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
    tmpl_3 <- qq(paste0("which(tmp_dat$diff12 < @{max_chng} & tmp_dat$diff12 > @{min_chng} & tmp_dat$close0 @{opts_3[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_3[", i, ",2]} tmp_dat$close2 & tmp_dat$close2 @{opts_3[", i, ",3]} tmp_dat$close3)"))
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
    tmpl_4 <- qq(paste0("which(tmp_dat$diff13 < @{max_chng} & tmp_dat$diff13 > @{min_chng} & tmp_dat$close0 @{opts_4[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_4[", i, ",2]} tmp_dat$close2 & tmp_dat$close2 @{opts_4[", i, ",3]} tmp_dat$close3 & tmp_dat$close3 @{opts_4[", i, ",4]} tmp_dat$close4)"))
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
    tmpl_5 <- qq(paste0("which(tmp_dat$diff14 < @{max_chng} & tmp_dat$diff14 > @{min_chng} & tmp_dat$close0 @{opts_5[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_5[", i, ",2]} tmp_dat$close2 & tmp_dat$close2 @{opts_5[", i, ",3]} tmp_dat$close3 & tmp_dat$close3 @{opts_5[", i, ",4]} tmp_dat$close4 & tmp_dat$close4 @{opts_5[", i, ",5]} tmp_dat$close5)"))
    tmpl_5
    
    tmp_label <- labels_5[i,] %>% paste(., collapse = "_")
    results_5[[tmp_label]] <- eval(parse(text=tmpl_5)) %>% length  
    results_5[[paste0("idx_", tmp_label)]] <- eval(parse(text=tmpl_5)) 
  }
  
  results_5[names(results_5)[grepl("^up", names(results_5))]] %>% unlist %>% sum / results_5 %>% unlist %>% sum 
  
  labels_6 <- expand.grid(a=c("dn","up"), b=c("dn","up"), c=c("dn","up"), d=c("dn","up"), e=c("dn","up"), f=c("dn","up"), stringsAsFactors = FALSE)
  opts_6 <- expand.grid(a=c("<",">="), b=c("<",">="), c=c("<",">="), d=c("<",">="), e=c("<",">="), f=c("<",">="))
  results_6 <- list() 
  
  for(i in 1:nrow(opts_6)) {
    #i <- 4
    tmpl_6 <- qq(paste0("which(tmp_dat$diff15 < @{max_chng} & tmp_dat$diff15 > @{min_chng} & tmp_dat$close0 @{opts_6[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_6[", i, ",2]} tmp_dat$close2 & tmp_dat$close2 @{opts_6[", i, ",3]} tmp_dat$close3 & tmp_dat$close3 @{opts_6[", i, ",4]} tmp_dat$close4 & tmp_dat$close4 @{opts_6[", i, ",5]} tmp_dat$close5 & tmp_dat$close5 @{opts_6[", i, ",6]} tmp_dat$close6)"))
    tmpl_6
    
    tmp_label <- labels_6[i,] %>% paste(., collapse = "_")
    results_6[[tmp_label]] <- eval(parse(text=tmpl_6)) %>% length  
    results_6[[paste0("idx_", tmp_label)]] <- eval(parse(text=tmpl_6)) 
  }
  
  results_6[names(results_6)[grepl("^up", names(results_6))]] %>% unlist %>% sum / results_6 %>% unlist %>% sum 
  
  labels_7 <- expand.grid(a=c("dn","up"), b=c("dn","up"), c=c("dn","up"), d=c("dn","up"), e=c("dn","up"), f=c("dn","up"), g=c("dn","up"), stringsAsFactors = FALSE)
  opts_7 <- expand.grid(a=c("<",">="), b=c("<",">="), c=c("<",">="), d=c("<",">="), e=c("<",">="), f=c("<",">="), g=c("<",">="))
  results_7 <- list() 
  
  for(i in 1:nrow(opts_7)) {
    #i <- 4
    tmpl_7 <- qq(paste0("which(tmp_dat$diff16 < @{max_chng} & tmp_dat$diff16 > @{min_chng} & tmp_dat$close0 @{opts_7[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_7[", i, ",2]} tmp_dat$close2 & tmp_dat$close2 @{opts_7[", i, ",3]} tmp_dat$close3 & tmp_dat$close3 @{opts_7[", i, ",4]} tmp_dat$close4 & tmp_dat$close4 @{opts_7[", i, ",5]} tmp_dat$close5 & tmp_dat$close5 @{opts_7[", i, ",6]} tmp_dat$close6 & tmp_dat$close6 @{opts_7[", i, ",7]} tmp_dat$close7)"))
    tmpl_7
    
    tmp_label <- labels_7[i,] %>% paste(., collapse = "_")
    results_7[[tmp_label]] <- eval(parse(text=tmpl_7)) %>% length  
    results_7[[paste0("idx_", tmp_label)]] <- eval(parse(text=tmpl_7)) 
  }
  
  results_7[names(results_7)[grepl("^up", names(results_7))]] %>% unlist %>% sum / results_7 %>% unlist %>% sum 
  
  labels_8 <- expand.grid(a=c("dn","up"), b=c("dn","up"), c=c("dn","up"), d=c("dn","up"), e=c("dn","up"), f=c("dn","up"), g=c("dn","up"), h=c("dn","up"), stringsAsFactors = FALSE)
  opts_8 <- expand.grid(a=c("<",">="), b=c("<",">="), c=c("<",">="), d=c("<",">="), e=c("<",">="), f=c("<",">="), g=c("<",">="), h=c("<",">="))
  results_8 <- list()

  # for(i in 1:nrow(opts_8)) {
  #   #i <- 4
  #   tmpl_8 <- qq(paste0("which(tmp_dat$diff17 < @{max_chng} & tmp_dat$diff17 > @{min_chng} & tmp_dat$close0 @{opts_8[", i, ",1]} tmp_dat$close1 & tmp_dat$close1 @{opts_8[", i, ",2]} tmp_dat$close2 & tmp_dat$close2 @{opts_8[", i, ",3]} tmp_dat$close3 & tmp_dat$close3 @{opts_8[", i, ",4]} tmp_dat$close4 & tmp_dat$close4 @{opts_8[", i, ",5]} tmp_dat$close5 & tmp_dat$close5 @{opts_8[", i, ",6]} tmp_dat$close6 & tmp_dat$close6 @{opts_8[", i, ",7]} tmp_dat$close7) & tmp_dat$close7 @{opts_8[", i, ",8]} tmp_dat$close8)"))
  #   tmpl_8
  # 
  #   tmp_label <- labels_8[i,] %>% paste(., collapse = "_")
  #   results_8[[tmp_label]] <- eval(parse(text=tmpl_8)) %>% length
  #   results_8[[paste0("idx_", tmp_label)]] <- eval(parse(text=tmpl_8))
  # }
  # 
  # results_8[names(results_8)[grepl("^up", names(results_8))]] %>% unlist %>% sum / results_8 %>% unlist %>% sum
  
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
  
  results_6[names(results_6)[grepl("^up", names(results_6))]] %>% unlist %>% sum / results_6[names(results_6)[!grepl("^idx", names(results_6))]] %>% unlist %>% sum 
  results_6[names(results_6)[!grepl("^idx", names(results_6))]] %>% unlist %>% sum
  
  results_7[names(results_7)[grepl("^up", names(results_7))]] %>% unlist %>% sum / results_7[names(results_7)[!grepl("^idx", names(results_7))]] %>% unlist %>% sum 
  results_7[names(results_7)[!grepl("^idx", names(results_7))]] %>% unlist %>% sum
  
  # results_8[names(results_8)[grepl("^up", names(results_8))]] %>% unlist %>% sum / results_8[names(results_8)[!grepl("^idx", names(results_8))]] %>% unlist %>% sum
  # results_8[names(results_8)[!grepl("^idx", names(results_8))]] %>% unlist %>% sum

  # SEARCH ----
  ## Extract into lst the results that match up to the condition (streak of days)
  #lst <- results_5
  #q_all <- "up_dn_up_up$"
  q_cond <- paste0("up_", q_all)
  var_name <- paste0("results_", length(strsplit(q_cond, "_")[[1]]))
  lst <- get(var_name)
  q_all <- q_all # not needed, but to have the entry
  
  # Grab either the condition or the index entry
  ## q_cond is with the extra current day
  idx_cond <- which(grepl(q_cond, names(lst)) & !grepl("^idx", names(lst))) 
  ## q_all is the original condition; idx will match for up AND down versions of q_cond
  idx_all <- which(grepl(q_all, names(lst)) & !grepl("^idx", names(lst))) 
  idx_idx <- which(grepl(q_all, names(lst)) &  grepl("^idx", names(lst)))
  var_name
  q_cond
  q_all
  pred_up <- lst[idx_cond] %>% unlist %>% sum / lst[idx_all] %>% unlist %>% sum 
  pred_up
  lst[idx_all]
  cond_cnt <- lst[idx_cond] %>% unlist %>% sum
  all_cnt <- lst[idx_all] %>% unlist %>% sum
  cond_cnt
  all_cnt
  # Days with X+q_cond; NOTE: do not look for original values here
  tmp_results <- tmp_dat[unname(unlist(lst[idx_idx])),]
  min_chng
  max_chng
  tmp_results$diff01[which(tmp_results$diff01 >= 0)] %>% summary
  tmp_results$diff01[which(tmp_results$diff01 < 0)] %>% summary
  
  pred_up_prcnt <- tmp_results$diff01[which(tmp_results$diff01 >= 0)] %>% median
  
  if(cond_cnt > 0 && all_cnt > 0) {
    t1 <- binom.test(cond_cnt, all_cnt, conf.level=0.9)
    low_conf_90 <- t1$conf.int[1]
  } else {
    low_conf_90 <- 0
  }
  
  tmp_results <- list(pred_up=pred_up, 
                      pred_up_prcnt=pred_up_prcnt,
                      cond_cnt=cond_cnt,
                      all_cnt=all_cnt,
                      low_conf_90=low_conf_90,
                      cond=q_cond,
                      min_chng=min_chng,
                      max_chng=max_chng,
                      days_checked=days_checked)
  
  cat("OUT: ", as.character(toJSON(tmp_results, auto_unbox=TRUE)), "\n")
  
  all_results[[name]] <- tmp_results
}

results <- list(last_close_date=tmp_dat$date[nrow(tmp_dat)],
                last_close_price=tmp_dat$close0[nrow(tmp_dat)],
                pred_date=Sys.Date(), 
                pred_time=Sys.time(), 
                max_days_auto_prices=paste(round(tmp_dat$close0[nrow(tmp_dat):(nrow(tmp_dat)-max_days_auto+1)], 2), collapse="|"),
                
                pred_up=all_results[["base"]]$pred_up, 
                pred_up_prcnt=all_results[["base"]]$pred_up_prcnt,
                cond_cnt=all_results[["base"]]$cond_cnt,
                all_cnt=all_results[["base"]]$all_cnt,
                low_conf_90=all_results[["base"]]$low_conf_90, 
                cond=all_results[["base"]]$cond,
                min_chng=all_results[["base"]]$min_chng,
                max_chng=all_results[["base"]]$max_chng,
                days_checked=all_results[["base"]]$days_checked,
                
                pred_up_sign=all_results[["sign"]]$pred_up, 
                pred_up_prcnt_sign=all_results[["sign"]]$pred_up_prcnt,
                cond_cnt_sign=all_results[["sign"]]$cond_cnt,
                all_cnt_sign=all_results[["sign"]]$all_cnt,
                low_conf_90=all_results[["sign"]]$low_conf_90, 
                cond_sign=all_results[["sign"]]$cond, 
                cond_prices_sign=all_results[["base"]]$cond_prices,
                min_chng_sign=all_results[["sign"]]$min_chng,
                max_chng_sign=all_results[["sign"]]$max_chng,
                days_checked=all_results[["sign"]]$days_checked,
                
                fear_greed=fear_greed
                )

tmp_json <- toJSON(results, auto_unbox=TRUE)
tmp_json
writeLines(tmp_json, "gspc_pred.json")

# EXAMPLE 
# 2020-04-16	2799.55	2783.36	2846.06	2761.63	2789.82	2749.98	0.582	-2.203	0.787	-0.232	1.214	4.661
# results_4 found Apr 9-15, dn_up_dn followed by up on 16

