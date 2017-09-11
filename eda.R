library(data.table)
library(Hmisc)
library(corrplot)
library(tidyverse)

# kaggle-zillow competition
rm(list=ls());gc()
if(getwd()!="/home/rstudio/kaggle_zillow")setwd("./kaggle_zillow")
source("./rscript/settings.R")
data_path <- "./data/"
load(paste0(data_path, "raw_data.Rdata"))
load(paste0(data_path, "data.Rdata"))


# Is id_parcel PK in prpt?
nrow(prpt_raw) # 총 2985217개의 parcel
prpt_raw %>% distinct(id_parcel) %>% nrow() == nrow(prpt_raw) # parcel_id=PK

# key correspondency : trans parcel은 다 prpt에 존재
sum(!(trans$id_parcel %in% prpt_raw$id_parcel)) # 0

# id_parcel in trans
# 중복거래 id_parcel 총 125건(2회 거래 124건, 3회 거래 1건)
nrow(trans_raw) # 총 거래건 90275
trans_raw %>% distinct(id_parcel) %>% nrow() # unique 거래건 90150
sum(duplicated(trans_raw$id_parcel)) # id_parcel 중복 125회
temp <- trans_raw %>%
  group_by(id_parcel) %>% 
  summarise(n=n()) %>% 
  filter(n>=2) %>% 
  mutate(dupl_tran = n-1)
sum(temp$dupl_tran) # 125건
temp %>% filter(dupl_tran==1) %>% nrow() # 2회 거래 123건. (1*123)
temp %>% filter(dupl_tran==2) %>% nrow() # 3회 거래 1건. (2*1)


# census, tract, block
# BLOCKID:  15-character code that is the concatenation of fields consisting of the 
# 2-character state FIPS code, the 3-character county FIPS code, 
# the 6-character census tract code, and the 4-character tabulation block code.
prpt %>% group_by(region_state) %>% tally() # All california(06)
prpt %>% group_by(region_county) %>% tally() # LA(037), Orange(059), Ventura(111)
(temp2 <- prpt %>% 
  group_by(length=str_length(census_tnb_raw)) %>% 
  tally()) # 8~15 length string
prpt %>% 
  select(census_tnb_raw) %>% 
  filter(str_length(census_tnb_raw)==15) %>% 
  head()


# distribution of date_transaction by each month
# after 10-15, transactions are in the test set(public LB)
trans_raw %>% count(year = str_sub(date_transaction, 1, 4))  # all 2016
trans_raw %>% 
  mutate(month = str_sub(date_transaction, 6, 7)) %>% 
  count(month) %>% # transaction count by month
  ggplot(aes(x=month, y=n)) + 
  geom_bar(stat="identity", fill="red") +
  geom_vline(aes(xintercept = 08), linetype="longdash", size=0.8) +
  labs(title="2016 Transactions by month") +
  theme(plot.title = element_text(hjust=0.5, size=20))


# Missing value "" in features.
num_blank_string  <- prpt_raw %>% summarise_all( funs(sum(.=="", na.rm=T)) )
(num_blank_string_cols <- (num_blank_string %>% gather() %>% filter(value>0))[[1]])
i=1 # i=1:5
prpt_raw %>% select(num_blank_string_cols[i]) %>% distinct() %>% nrow() # levels check


# NAs in features
# (with null string handled data, prpt, not using prpt_raw)
na_ratio <- prpt %>% 
  summarise_all( funs(sum(is.na(.))/length(.) )) %>% 
  gather(key="feature", value="missing_pct")
na_ratio %>%  
  ggplot(aes(x=reorder(feature, -missing_pct), y=missing_pct)) +
  geom_bar(stat="identity", fill="red") +
  geom_hline(yintercept = 0.75, size=0.6, linetype="dashed") +
  geom_hline(yintercept = 0.5, size=0.6, linetype="dashed") +
  geom_hline(yintercept = 0.25, size=0.6, linetype="dashed") +
  coord_flip()
(better_variables <- (na_ratio %>% filter(missing_pct<=0.5))[[1]])

  
# distribution of logerror
trans_raw %>% 
  ggplot(aes(x=logerror)) +
  geom_histogram(bins=500, fill="red") +
  scale_x_continuous(limits = c(-0.5, 0.5))


# distribution of logerror_abs
trans_raw %>% 
  mutate(logerror_abs = abs(logerror)) %>% 
  ggplot(aes(x=logerror_abs)) +
  geom_histogram(bins=500, fill="red") +
  scale_x_continuous(limits=c(0, 0.5))


# distribution of logerror by months
trans_raw %>% 
  group_by(month = str_sub(date_transaction, 6, 7)) %>% 
  summarise(mean_logerror = mean(logerror)) %>% 
  ggplot(aes(x=month, y=mean_logerror, group=1)) +
  geom_point(size=3) + geom_line(size=1)


# distribution of absolute logerror by months
trans %>% group_by(ym = parse_date_time(str_sub(date_transaction, 1, 7), orders="ym")) %>% 
  summarise(mean_logerror_abs = mean(logerror_abs)) %>% 
  ggplot(aes(x=ym, y=mean_logerror_abs)) +
  geom_point(size=3) + geom_line(size=1)


# memory
rm(prpt_raw, trans_raw);gc()

# correlation with absolute error
total <- prpt %>% select(better_variables)















