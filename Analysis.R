
## Libraries required

library(dplyr)
library(tidyr)

## Getting And Cleaning Data

### Download Data

# download.file("https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/download/train.csv.zip","train.csv.zip")
# download.file("https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/download/test.csv.zip","test.csv.zip")
# 
# download.file("https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/download/bids.csv.zip","bids.csv.zip")
# download.file("https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/download/sampleSubmission.csv", "sampleSubmission.csv")

### Load Data 

train <- read.csv("train.csv")
test <- read.csv("test.csv")
bids <- read.csv("bids.csv")

sampleSubmission <- read.csv("sampleSubmission.csv")

### Merge Data

train$bidder_id <- as.character(train$bidder_id)
test$bidder_id <- as.character(test$bidder_id)
bids$bidder_id <- as.character(bids$bidder_id)

train$outcome <- as.factor(train$outcome)

train <- train[train$bidder_id %in% bids$bidder_id, ]
test <- test[test$bidder_id %in% bids$bidder_id, ]

colSums(is.na(bids))

colSums(is.na(train))

colSums(is.na(test))

### Replace non available countries with NA 

bids[ bids$country == "", "country" ] <- NA

bid_train <- merge(bids,train, by= "bidder_id")


## Expolatory Data Analysis and Feature Selection

### Order data frame by bidder id and time 

bid_train <- bid_train[order(bid_train$bidder_id, bid_train$time),]
train <- train[order(train$bidder_id),]

### mean number of bidders

mean_nb_bidders <- summarise(group_by(bid_train,outcome), nb_bidders = n_distinct(bidder_id)) %>%
    select(-outcome)

### mean number of bids per bidder

mean_nb_bids <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_bids = n_distinct(bid_id)),outcome), nb_bids = mean(nb_bids)) %>%
    select(-outcome)

### mean number of auctions per bidder

mean_nb_auctions <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_auctions = n_distinct(auction)),outcome), nb_auctions = mean(nb_auctions)) %>%
    select(-outcome)

### mean number of devices per bidder

mean_nb_devices <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_devices = n_distinct(device)),outcome), nb_devices = mean(nb_devices)) %>%
    select(-outcome)

### mean number of countries per bidder

mean_nb_countries <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_countries = n_distinct(country)),outcome), nb_countries = mean(nb_countries)) %>%
    select(-outcome)


### mean number of ips per bidder

mean_nb_ips <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_ips = n_distinct(ip)),outcome), nb_ips = mean(nb_ips)) %>%
    select(-outcome)

### mean number of bids per auction per bidder

mean_bids_per_auction <- 
    group_by(bid_train,outcome,bidder_id,auction) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_auction = mean(mean_bids)) %>%
    select(-outcome)

### mean number of bids per device per bidder


mean_bids_per_device <- 
    group_by(bid_train,outcome,bidder_id,device) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_device = mean(mean_bids)) %>%
    select(-outcome)


### mean number of bids per country per bidder

mean_bids_per_country <- 
    group_by(bid_train,outcome,bidder_id,country) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_country = mean(mean_bids)) %>%
    select(-outcome)


### mean number of bids per url per bidder

mean_bids_per_url <- 
    group_by(bid_train,outcome,bidder_id,url) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_url = mean(mean_bids)) %>%
    select(-outcome)

### mean number of bids per url per ip

mean_bids_per_ip <- 
    group_by(bid_train,outcome,bidder_id,ip) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_ip = mean(mean_bids)) %>%
    select(-outcome)

### mean time difference between 2 bids

mean_resp <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id), resp = mean(diff(time))), outcome), mean_resp = mean(resp, na.rm = TRUE)) %>%
    select(-outcome)


features <- cbind(mean_nb_bidders,mean_nb_bids,mean_nb_auctions,mean_nb_devices,mean_nb_countries,mean_nb_ips, mean_bids_per_auction, mean_bids_per_country, mean_bids_per_device, mean_bids_per_ip, mean_bids_per_url)

features <- t(features)

colnames(features) <- c("Human","Robot")


## Creating the training data

bidder_id <- train$bidder_id

outcome <- train$outcome

nb_bids <- group_by(bid_train,bidder_id) %>%
    summarize(nb_bids = n_distinct(bid_id)) %>%
    select(nb_bids)
 
nb_auctions <- group_by(bid_train,bidder_id) %>%
    summarize(nb_auctions = n_distinct(auction)) %>%
    select(nb_auctions)
          
nb_merchandise <- group_by(bid_train,bidder_id) %>%         
    summarize(nb_merchandise = n_distinct(merchandise)) %>%
    select(nb_merchandise)

nb_devices <- group_by(bid_train,bidder_id) %>%
    summarize(nb_devices = n_distinct(device)) %>%
    select(nb_devices)

nb_countries <- group_by(bid_train,bidder_id) %>%
    summarize(nb_countries = n_distinct(country)) %>%
    select(nb_countries)

nb_ips <- group_by(bid_train,bidder_id) %>%
    summarize(nb_ips = n_distinct(ip)) %>%
    select(nb_ips)

nb_urls <- group_by(bid_train,bidder_id) %>%
    summarize(nb_urls = n_distinct(url)) %>%
    select(nb_urls)

bid_inter_time <- group_by(bid_train,bidder_id) %>%
    summarise(inter_time = mean(diff(time))) %>%
    select(inter_time)

bids_per_auction <- group_by(bid_train,bidder_id,auction) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_auction = mean(nb_bids)) %>% select(bids_per_auction)

bids_per_country <- group_by(bid_train,bidder_id,country) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_country = mean(nb_bids)) %>% select(bids_per_country)

bids_per_device <- group_by(bid_train,bidder_id,device) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_device = mean(nb_bids)) %>% select(bids_per_device)

bids_per_ip <- group_by(bid_train,bidder_id,ip) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_ip = mean(nb_bids)) %>% select(bids_per_ip)

bids_per_url <- group_by(bid_train,bidder_id,url) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_url = mean(nb_bids)) %>% select(bids_per_url)

training <- data.frame(nb_bids,nb_auctions,nb_devices,nb_countries,
                       nb_ips,bid_inter_time,bids_per_auction,bids_per_country,bids_per_device,
                       bids_per_ip,bids_per_url,outcome)

### Shuffle training dataset

training <- training[sample(seq_len(nrow(training))),]


