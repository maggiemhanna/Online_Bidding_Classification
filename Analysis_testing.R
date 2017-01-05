bid_test <- merge(bids,test, by= "bidder_id")
bid_test <- bid_test[order(bid_test$bidder_id, bid_test$time),]
test <- test[order(test$bidder_id),]

## Creating the testing data

bidder_id_t <- group_by(bid_test,bidder_id) %>%
    summarize(nb_bids = n_distinct(bid_id)) %>%
    select(bidder_id)

nb_bids_t <- group_by(bid_test,bidder_id) %>%
    summarize(nb_bids = n_distinct(bid_id)) %>%
    select(nb_bids)

nb_auctions_t <- group_by(bid_test,bidder_id) %>%
    summarize(nb_auctions = n_distinct(auction)) %>%
    select(nb_auctions)

nb_merchandise_t <- group_by(bid_test,bidder_id) %>%         
    summarize(nb_merchandise = n_distinct(merchandise)) %>%
    select(nb_merchandise)

nb_devices_t <- group_by(bid_test,bidder_id) %>%
    summarize(nb_devices = n_distinct(device)) %>%
    select(nb_devices)

nb_countries_t <- group_by(bid_test,bidder_id) %>%
    summarize(nb_countries = n_distinct(country)) %>%
    select(nb_countries)

nb_ips_t <- group_by(bid_test,bidder_id) %>%
    summarize(nb_ips = n_distinct(ip)) %>%
    select(nb_ips)

nb_urls_t <- group_by(bid_test,bidder_id) %>%
    summarize(nb_urls = n_distinct(url)) %>%
    select(nb_urls)

bid_inter_time_t <- group_by(bid_test,bidder_id) %>%
    summarise(inter_time = mean(diff(time))) %>%
    select(inter_time)

bids_per_auction_t <- group_by(bid_test,bidder_id,auction) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_auction = mean(nb_bids)) %>% select(bids_per_auction)

bids_per_country_t <- group_by(bid_test,bidder_id,country) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_country = mean(nb_bids)) %>% select(bids_per_country)

bids_per_device_t <- group_by(bid_test,bidder_id,device) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_device = mean(nb_bids)) %>% select(bids_per_device)

bids_per_ip_t <- group_by(bid_test,bidder_id,ip) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_ip = mean(nb_bids)) %>% select(bids_per_ip)

bids_per_url_t <- group_by(bid_test,bidder_id,url) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(bidder_id) %>%
    summarise(bids_per_url = mean(nb_bids)) %>% select(bids_per_url)


### Testing database

testing <- data.frame(nb_bids_t,nb_auctions_t,nb_devices_t,nb_countries_t,
                       nb_ips_t,bid_inter_time_t,bids_per_auction_t,bids_per_country_t,bids_per_device_t,
                       bids_per_ip_t,bids_per_url_t)

### Shuffle testing dataset

testing <- testing[sample(seq_len(nrow(testing))),]

