## Importing packages

library(tidyverse) # metapackage with lots of helpful functions
library(jsonlite)
library(stringr)
library(feather)

# To see the files added to the current kernel. 
list.files(path = "../input/ga-customer-revenue-prediction/")

#Briefly examine the dataset
file_in_tr <- file("../input/ga-customer-revenue-prediction/train_v2.csv","r")
chunk_size <- 1000 
x <- read.csv(file_in_tr, nrows=chunk_size)
str(x)

file_in_te <- file("../input/ga-customer-revenue-prediction/test_v2.csv","r")
y <- read.csv(file_in_te, nrows=chunk_size)
str(y)

#Assign correct data types and appropriate column names 
col_types <- cols(
  channelGrouping = col_character(),
  customDimensions = col_character(),
  date = col_datetime(), # Parses YYYYMMDD
  device = col_character(),
  fullVisitorId = col_character(),
  geoNetwork = col_character(),
  hits = col_skip(), # Skip, huge amount of data with no useful information!
  socialEngagementType = col_skip(), # Skip as always "Not Socially Engaged"
  totals = col_character(),
  trafficSource = col_character(),
  visitId = col_integer(), 
  visitNumber = col_integer(),
  visitStartTime = col_integer()
)

## Reading in files
train <- read_csv("../input/ga-customer-revenue-prediction/train_v2.csv", col_types = col_types) 
test <- read_csv("../input/ga-customer-revenue-prediction/test_v2.csv",  col_types = col_types)

#which columns includes JSON, JSON-like data
head(train)
head(test)
#JSON columns are "device", "geoNetwork", "totals", "trafficSource", "customDimensions"

#need this function for JSON to R data frame conversion as customeDiemensions JSON format is [{}]. Other JSON columns are {}
unsnake <- . %>%
  str_replace_all(c("\\[\\]" = "[{}]", # empty element must contain dictionary
                    "^\\[|\\]$" = "", # remove initial and final brackets
                    "(\\[|\\{|, |: |', )'" = "\\1\"", # open single- to double-quote (on key or value)
                    "'(\\]|\\}|: |, )" = '\"\\1')) # close quote

#Flattening JSON data
tr_device <- paste("[", paste(train$device, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_geoNetwork <- paste("[", paste(train$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_totals <- paste("[", paste(train$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_trafficSource <- paste("[", paste(train$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

tr_customDimensions <- paste("[", paste(unsnake(train$customDimensions), collapse = ","), "]") %>% fromJSON(flatten = T)

te_device <- paste("[", paste(test$device, collapse = ","), "]") %>% fromJSON(flatten = T)
te_geoNetwork <- paste("[", paste(test$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
te_totals <- paste("[", paste(test$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
te_trafficSource <- paste("[", paste(test$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

te_customDimensions <- paste("[", paste(unsnake(test$customDimensions), collapse = ","), "]") %>% fromJSON(flatten = T)

#list each JSON attribute sub-columns
names(tr_device)
names(tr_geoNetwork)
names(tr_totals)
names(tr_trafficSource)
names(tr_customDimensions)

#device sub-columns: browser, operatingSystem, isMobile, deviceCategory
#geoNetwork: continent, subContinent, country, region, metro, city, networkDomain
#totals: hits, transactions, transactionRevenue, totalTransactionRevenue, pagViews, sessionQualityDim,
#timeOnSite
#trafficSource: campaign, source, medium, keyword, adContent, adwordsClickInfo.page, adwordsClockInfo.slot
#adwordsClickInfo.gclId, adwordsClickInfo.adNetworkType
#customDimensions: value

#Check to see if the training and test sets have the same column names
setequal(names(tr_device), names(te_device))
setequal(names(tr_geoNetwork), names(te_geoNetwork))
setequal(names(tr_totals), names(te_totals))
setequal(names(tr_trafficSource), names(te_trafficSource))
setequal(names(tr_customDimensions), names(te_customDimensions))

#Apparently, in v2 version of dataset, both train and test have transactionRevenue column!
#names(tr_totals)
#names(te_totals)
#tr_trafficSource contains an extra column as well - campaignCode
#It actually has only one non-NA value, so this column can safely be dropped later
table(tr_trafficSource$campaignCode, exclude = NULL)
names(tr_trafficSource)
names(te_trafficSource)


#Combine to make the full training and test sets. campaignCode in train is dropped.
flat.train <- train %>%
  cbind(tr_device, tr_geoNetwork, tr_totals, tr_trafficSource, tr_customDimensions) %>%
  select(-device, -geoNetwork, -totals, -trafficSource, -customDimensions, -campaignCode)

flat.test <- test %>%
  cbind(te_device, te_geoNetwork, te_totals, te_trafficSource, te_customDimensions) %>%
  select(-device, -geoNetwork, -totals, -trafficSource, -customDimensions)

#Number of columns in the new training and test sets. 
ncol(flat.train)
ncol(flat.test)

#Remove temporary tr_ and te_ sets
rm(tr_device); rm(tr_geoNetwork); rm(tr_totals); rm(tr_trafficSource); rm(tr_customDimensions)
rm(te_device); rm(te_geoNetwork); rm(te_totals); rm(te_trafficSource); rm(te_customDimensions)

#We remove columns with not available data or zero varience here. However,
#some attributes are analyzed later but the removal takes place here
#to avoid useless data.
flat.train.red <- flat.train %>%
  select(-browserVersion, -browserSize, -operatingSystemVersion, -mobileDeviceInfo, -mobileDeviceBranding, -mobileDeviceModel, 
         -mobileInputSelector, -mobileDeviceMarketingName, -flashVersion, -language, -screenColors, 
         -screenResolution, -cityId,-latitude, -longitude, -networkLocation, -visits, -bounces, -newVisits, 
         -isTrueDirect, -adwordsClickInfo.criteriaParameters, -adwordsClickInfo.isVideoAd, -index)

str(flat.train.red)

flat.test.red <- flat.test %>%
  select(-browserVersion, -browserSize, -operatingSystemVersion, -mobileDeviceInfo, -mobileDeviceBranding, -mobileDeviceModel, 
         -mobileInputSelector, -mobileDeviceMarketingName, -flashVersion, -language, -screenColors, 
         -screenResolution, -cityId,-latitude, -longitude, -networkLocation, -visits, -bounces, -newVisits, 
         -isTrueDirect, -adwordsClickInfo.criteriaParameters, -adwordsClickInfo.isVideoAd, -index)

str(flat.test.red)

## Saving data
write.csv(flat.train.red, "train_flat.csv", row.names = F)
write.csv(flat.test.red, "test_flat.csv", row.names = F)

#univariate analysis
#browserVersion and browserSize, operatingSystemVersion, mobileDeviceBranding, 
#mobileDeviceModel, mobileInputSelector have only "not available in demo dataset" 
#value and they can be dropped.
str(as.factor(flat.train$browserVersion))
str(as.factor(flat.test$browserVersion))

str(as.factor(flat.train$browserSize))
str(as.factor(flat.test$browserSize))

str(as.factor(flat.train$operatingSystem))
str(as.factor(flat.test$operatingSystem))

str(as.factor(flat.train$operatingSystemVersion))
str(as.factor(flat.test$operatingSystemVersion))

str(as.factor(flat.train$isMobilen))
str(as.factor(flat.test$isMobile))

str(as.factor(flat.train$mobileDeviceBranding))
str(as.factor(flat.test$mobileDeviceBranding))

str(as.factor(flat.train$mobileDeviceModel))
str(as.factor(flat.test$mobileDeviceModel))

str(as.factor(flat.train$mobileInputSelector))
str(as.factor(flat.test$mobileInputSelector))

#drop mobileDeviceInfo, only one level "not available in demo dataset"
str(as.factor(flat.train$mobileDeviceInfo))
str(as.factor(flat.test$mobileDeviceInfo))
#drop mobileDeviceMarketingName, only one level "not available in demo dataset"
str(as.factor(flat.train$mobileDeviceMarketingName))
str(as.factor(flat.test$mobileDeviceMarketingName))
#flashVersion, only one level "not available in demo dataset"
str(as.factor(flat.train$flashVersion))
str(as.factor(flat.test$flashVersion))
#language, only one level "not available in demo dataset" 
str(as.factor(flat.train$language))
str(as.factor(flat.test$language))
#screenColors, only one level "not available in demo dataset"
str(as.factor(flat.train$screenColors))
str(as.factor(flat.test$screenColors))
#screenResolution, only one level "not available in demo dataset"
str(as.factor(flat.train$screenResolution))
str(as.factor(flat.test$screenResolution))
#cityId, 1 level "not available in demo dataset, drop
str(as.factor(flat.train$cityId))
str(as.factor(flat.test$cityId))
#latitude, 1 level "not available in demo dataset"
str(as.factor(flat.train$latitude))
str(as.factor(flat.test$latitude))
#longitude, 1 level "not available in demo dataset", drop
str(as.factor(flat.train$longitude))
str(as.factor(flat.test$longitude))
#networkLocation,  1 level "not available in demo dataset", drop 
str(as.factor(flat.train$networkLocation))
str(as.factor(flat.test$networkLocation))
#visits, 1 level "1"                            
str(as.factor(flat.train$visits))
#hits                             
str(as.factor(flat.train$hits))
#pageviews                          
str(as.factor(flat.train$pageviews))
#bounces,  1 level "1"                            
str(as.factor(flat.train$bounces))
str(as.factor(flat.test$bounces))
#newVisits, 1 level "1"                         
str(as.factor(flat.train$newVisits))
str(as.factor(flat.test$newVisits))
#sessionQualityDim                 
str(as.factor(flat.train$sessionQualityDim))
#timeOnSite  
str(as.factor(flat.train$timeOnSite))
#campaign
str(as.factor(flat.train$campaign))
#referralPath 
str(as.factor(flat.train$referralPath))
#isTrueDirect, 1 level "TRUE", drop                       
str(as.factor(flat.train$isTrueDirect))
str(as.factor(flat.test$isTrueDirect))
#adContent                         
str(as.factor(flat.train$adContent))

#adwordsClickInfo.criteriaParameters, 1 level "not available in demo dataset" drop
str(as.factor(flat.train$adwordsClickInfo.criteriaParameters))
str(as.factor(flat.test$adwordsClickInfo.criteriaParameters))
#adwordsClickInfo.page
str(as.factor(flat.train$adwordsClickInfo.page))
#adwordsClickInfo.slot              
str(as.factor(flat.train$adwordsClickInfo.slot))
#adwordsClickInfo.gclId             
str(as.factor(flat.train$adwordsClickInfo.gclId))
#adwordsClickInfo.adNetworkType    
str(as.factor(flat.train$adwordsClickInfo.adNetworkType))
#adwordsClickInfo.isVideoAd, 1 level "FALSE" drop         
str(as.factor(flat.train$adwordsClickInfo.isVideoAd))
str(as.factor(flat.test$adwordsClickInfo.isVideoAd))
#index, 1 level "4", drop                       
str(as.factor(flat.train$index))
str(as.factor(flat.test$index))

#train and test datasets have the same columns
setequal(names(flat.train.red), names(flat.test.red))

#channelGrouping, 8 level, no NAs
table(flat.train.red$channelGrouping, exclude = NULL)
str(as.factor(flat.test.red$channelGrouping))


#char type attributes, referralPath does not seem to have useful/meaningful information. A lot of URLs, to be dropped
table(flat.train.red$referralPath, exclude = NULL)

#analysis of remaining attributes: not completed yet
table(flat.train.red$networkDomain, exclude = NULL)

str(as.factor(flat.train.red$source))

head(as.double(flat.train.red$transactionRevenue))
boxplot(as.double(flat.train.red$transactionRevenue))
dfrev <- aggregate(as.double(flat.train.red$transactionRevenue), by=list(Category=flat.train.red$fullVisitorId), FUN=sum)
table(dfrev, exclude = NULL)

summary(flat.train.red$visitNumber)
dfvis <- aggregate(flat.train.red$visitNumber, by=list(Category=flat.train.red$fullVisitorId), FUN=sum)
summary(dfvis)
hist(dfvis$x)
plot(dfvis$Category, dfvis$x)

max(flat.train.red$visitStartTime)
str(as.POSIXct(flat.train.red$visitStartTime))

plot(flat.train.red$fullVisitorId, flat.train.red$transactionRevenue)

max(flat.test.red$date)
summary(flat.train.red$visitNumber)