library(data.table)
library(Hmisc)
library(tidyverse)


# kaggle-zillow competition
rm(list=ls());gc()
if(getwd()!="/home/rstudio/kaggle_zillow")setwd("./kaggle_zillow")
source("./rscript/settings.R")
data_path <- "./data/"


# file importing -----
  prpt_raw  <- fread(paste0(data_path, "properties_2016.csv"))
  trans_raw  <- fread(paste0(data_path, "train_2016_v2.csv"))

  setkey(prpt_raw, parcelid)
  setkey(trans_raw, parcelid)

# renaming -----
colnames(prpt_raw)
new_names = c("id_parcel", # 1
              "type_aircon", # 2
              "type_architectural", #3
              "area_basement_finished", # 4
              "num_bathroom", #5
              "num_bedroom", #6
              "type_building_framing", #7
              "type_building_quality", #8
              "num_bathroom_calc", #9
              "type_deck", #10
              "area_floor1_finished", #11
              "area_total_finished", #12
              "area_living_finished", #13
              "area_living_perimeter_finished", #14
              "area_total", #15
              "area_floor1_finished2", #16
              "area_basement_total", #17
              "num_fireplace", #19
              "num_fullbath", #20
              "num_garage", #21
              "area_garage", #22
              "flag_tub", #23
              "type_heating", #24
              "area_lot", #27
              "num_pool", #28
              "area_pool", #29
              "flag_pool_tub1", #30
              "flag_pool_tub2", #31
              "flag_pool_tub3", #32
              "zoning_landuse_county", #33
              "zoning_landuse", #34
              "zoning_landuse_desc", #35
              "census_tnb_raw", #36
              "region_county", #37
              "region_city", #38
              "region_neighborhood", #39
              "region_zip", #40
              "num_room", #41
              "type_story", #42
              "num_3qbath", #43
              "type_construction", #44
              "num_unit", #45
              "area_patio", #46
              "area_shed", #47
              "year_built", #48
              "num_stories", #49
              "flag_fireplace", #50
              "tax_building", #51
              "tax_total", #52
              "tax_year", #53
              "tax_land", #54
              "tax_amount", #55
              "tax_delinquency", #56
              "tax_delinquency_year", #57
              "census_tnb" # 58
)
old_names <- c("parcelid", # 1
               "airconditioningtypeid", # 2
               "architecturalstyletypeid", #3
               "basementsqft", # 4
               "bathroomcnt", #5
               "bedroomcnt", #6
               "buildingclasstypeid", #7
               "buildingqualitytypeid", #8
               "calculatedbathnbr", #9
               "decktypeid", #10
               "finishedfloor1squarefeet", #11
               "calculatedfinishedsquarefeet", #12
               "finishedsquarefeet12", #13
               "finishedsquarefeet13", #14
               "finishedsquarefeet15", #15
               "finishedsquarefeet50", #16
               "finishedsquarefeet6", #17
               "fireplacecnt", #19
               "fullbathcnt", #20
               "garagecarcnt", #21
               "garagetotalsqft", #22
               "hashottuborspa", #23
               "heatingorsystemtypeid", #24
               "lotsizesquarefeet", #27
               "poolcnt", #28
               "poolsizesum", #29
               "pooltypeid10", #30
               "pooltypeid2", #31
               "pooltypeid7", #32
               "propertycountylandusecode", #33
               "propertylandusetypeid", #34
               "propertyzoningdesc", #35
               "rawcensustractandblock", #36
               "regionidcounty", #37
               "regionidcity", #38
               "regionidneighborhood", #39
               "regionidzip", #40
               "roomcnt", #41
               "storytypeid", #42
               "threequarterbathnbr", #43
               "typeconstructiontypeid", #44
               "unitcnt", #45
               "yardbuildingsqft17", #46
               "yardbuildingsqft26", #47
               "yearbuilt", #48
               "numberofstories", #49
               "fireplaceflag", #50
               "structuretaxvaluedollarcnt", #51
               "taxvaluedollarcnt", #52
               "assessmentyear", #53
               "landtaxvaluedollarcnt", #54
               "taxamount", #55
               "taxdelinquencyflag", #56
               "taxdelinquencyyear", #57
               "censustractandblock" # 58
)
setnames(prpt_raw, old_names, new_names)
rm(old_names, new_names)

setnames(trans_raw, "parcelid", "id_parcel")
setnames(trans_raw, "transactiondate", "date_transaction")

# save
save(prpt_raw, trans_raw,
     file=paste0(data_path, "raw_data.Rdata"))


# data reengineering -----
prpt <- prpt_raw
trans <- trans_raw

# type setting and reengineering
trans[, date_transaction := as.Date(date_transaction)]
trans[, logerror_abs := abs(logerror)]


# 5개 null string 변수 ("flag_tub", "zoning_landuse_county", 
# "zoning_landuse_desc", "flag_fireplace", "tax_delinquency"
# 2레벨(true, "")인 것 false처리
prpt <- prpt %>% 
  mutate_at( vars(flag_tub, flag_fireplace, tax_delinquency),
             funs(ifelse(.=="", "False", .))) %>% 
  mutate_at( vars(zoning_landuse_county, zoning_landuse_desc),
             funs(ifelse(is.na(.), "NA", .)))


# trans에 날짜 분리하여 저장.
trans <- trans %>% 
  mutate(year_month = parse_date_time(str_sub(date_transaction, 1, 7), orders="ym"),
         month = as.factor(str_sub(date_transaction, 6, 7)),
         dow = as.factor(wday(str_sub(date_transaction, 1, 10)))
         )

# split census tract and block raw
prpt <- prpt %>% 
  mutate(census_tnb_raw = as.character(census_tnb_raw),
         census_state = str_sub(census_tnb_raw, 1, 1),
         census_county = str_sub(census_tnb_raw, 2, 4),
         census_tract_and_block = str_sub(census_tnb_raw, 5, -1),
         census_tnb_raw = NULL,
         census_tnb = NULL)


save(prpt, trans,
     file = paste0("./data/data.Rdata"))
