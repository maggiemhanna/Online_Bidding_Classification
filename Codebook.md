# Online Bidding: Human or Robot
Maggie Mhanna  
03/01/2017  

In this project, I'll be chasing down robots for an online auction site. The goal is to identify online auction bids that are placed by "robots", helping the site owners easily flag these users for removal from their site to prevent unfair auction activity. 

## Libraries required


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyr)
library(knitr)
library(easyGgplot2)
```

```
## Loading required package: ggplot2
```

```r
library(devtools)
library(pander)
```

## Getting And Cleaning Data

### Downloading Data


```r
download.file("https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/download/train.csv.zip","train.csv.zip")
download.file("https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/download/test.csv.zip","test.csv.zip")

download.file("https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/download/bids.csv.zip","bids.csv.zip")
download.file("https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/download/sampleSubmission.csv", "sampleSubmission.csv")
```

### Loading Data 


```r
train <- read.csv("train.csv")
test <- read.csv("test.csv")
bids <- read.csv("bids.csv")
sampleSubmission <- read.csv("sampleSubmission.csv")
```

### Understanding the Data

The data is primarily composed of two tables. One is a simple list of all bidders and the second one contains all the bids made by these bidders. 

### Merge Data

I will be combining the two datasets on Bidder ID. At the beginning, I need to remove all training and test samples that are not in the bid data frame.


```r
train$bidder_id <- as.character(train$bidder_id)
test$bidder_id <- as.character(test$bidder_id)
bids$bidder_id <- as.character(bids$bidder_id)

train$outcome <- as.factor(train$outcome)

train <- train[train$bidder_id %in% bids$bidder_id, ]
test <- test[test$bidder_id %in% bids$bidder_id, ]
```

I can verify if any of the data in the columns is missing 


```r
colSums(is.na(bids))
```

```
##      bid_id   bidder_id     auction merchandise      device        time 
##           0           0           0           0           0           0 
##     country          ip         url 
##           0           0           0
```

```r
colSums(is.na(train))
```

```
##       bidder_id payment_account         address         outcome 
##               0               0               0               0
```

```r
colSums(is.na(test))
```

```
##       bidder_id payment_account         address 
##               0               0               0
```

Replace non available countries with NA 


```r
bids[ bids$country == "", "country" ] <- NA
```

To do any kind of prediction and apply machine learning, I need to summarize the data on Bidder ID level. This is because bidder and not bids can be bots. Hence, target variable is defined on bidder level.


```r
bid_train <- merge(bids,train, by= "bidder_id")
```

 
## Expolatory Data Analysis and Feature Selection

Finding variables which are able to distinguish humans from robots is the first thing I will do before building any kind of model. In order to do so, what I simply do is find the mean value of each parameter for both humans and Robots and see if they look different. There are many other ways to do the same thing, however this is the simplest of them all the understand. 

### Order data frames by bidder id and time 


```r
bid_train <- bid_train[order(bid_train$bidder_id, bid_train$time),]
train <- train[order(train$bidder_id),]
```

### mean number of bidders


```r
mean_nb_bidders <- summarise(group_by(bid_train,outcome), nb_bidders = n_distinct(bidder_id)) %>%
    select(-outcome)
```

### mean number of bids per bidder


```r
mean_nb_bids <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_bids = n_distinct(bid_id)),outcome), nb_bids = mean(nb_bids)) %>%
    select(-outcome)
```

### mean number of auctions per bidder


```r
mean_nb_auctions <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_auctions = n_distinct(auction)),outcome), nb_auctions = mean(nb_auctions)) %>%
    select(-outcome)
```

### mean number of devices per bidder


```r
mean_nb_devices <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_devices = n_distinct(device)),outcome), nb_devices = mean(nb_devices)) %>%
    select(-outcome)
```

### mean number of countries per bidder


```r
mean_nb_countries <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_countries = n_distinct(country)),outcome), nb_countries = mean(nb_countries)) %>%
    select(-outcome)
```

### mean number of ips per bidder


```r
mean_nb_ips <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id),nb_ips = n_distinct(ip)),outcome), nb_ips = mean(nb_ips)) %>%
    select(-outcome)
```

### mean number of bids per auction per bidder


```r
mean_bids_per_auction <- 
    group_by(bid_train,outcome,bidder_id,auction) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_auction = mean(mean_bids)) %>%
    select(-outcome)
```

### mean number of bids per device per bidder


```r
mean_bids_per_device <- 
    group_by(bid_train,outcome,bidder_id,device) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_device = mean(mean_bids)) %>%
    select(-outcome)
```

### mean number of bids per country per bidder

```r
mean_bids_per_country <- 
    group_by(bid_train,outcome,bidder_id,country) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_country = mean(mean_bids)) %>%
    select(-outcome)
```

### mean number of bids per url per bidder


```r
mean_bids_per_url <- 
    group_by(bid_train,outcome,bidder_id,url) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_url = mean(mean_bids)) %>%
    select(-outcome)
```

### mean number of bids per url per ip


```r
mean_bids_per_ip <- 
    group_by(bid_train,outcome,bidder_id,ip) %>% 
    summarise(nb_bids = n_distinct(bid_id)) %>% 
    group_by(outcome,bidder_id) %>%
    summarise(mean_bids = mean(nb_bids)) %>% 
    group_by(outcome) %>%
    summarise(bids_per_ip = mean(mean_bids)) %>%
    select(-outcome)
```

### mean time difference between 2 bids


```r
mean_resp <- summarise(group_by(summarise(group_by(bid_train,outcome,bidder_id), resp = mean(diff(time))), outcome), mean_resp = mean(resp, na.rm = TRUE)) %>%
    select(-outcome)
```



```r
features <- cbind(mean_nb_bidders,mean_nb_bids,mean_nb_auctions,mean_nb_devices,mean_nb_countries,mean_nb_ips, mean_bids_per_auction, mean_bids_per_country, mean_bids_per_device, mean_bids_per_ip, mean_bids_per_url, mean_resp)

features <- t(features)

colnames(features) <- c("Human","Robot")

pander(features, style='grid')
```



+------------------------+-----------+-----------+
|         &nbsp;         |   Human   |   Robot   |
+========================+===========+===========+
|     **nb_bidders**     |   1881    |    103    |
+------------------------+-----------+-----------+
|      **nb_bids**       |   1414    |   4004    |
+------------------------+-----------+-----------+
|    **nb_auctions**     |   58.07   |    145    |
+------------------------+-----------+-----------+
|     **nb_devices**     |   73.95   |   163.6   |
+------------------------+-----------+-----------+
|    **nb_countries**    |   12.68   |   26.48   |
+------------------------+-----------+-----------+
|       **nb_ips**       |   581.3   |   2388    |
+------------------------+-----------+-----------+
|  **bids_per_auction**  |   6.442   |   23.15   |
+------------------------+-----------+-----------+
|  **bids_per_country**  |   40.47   |    181    |
+------------------------+-----------+-----------+
|  **bids_per_device**   |   12.36   |   100.4   |
+------------------------+-----------+-----------+
|    **bids_per_ip**     |   8.693   |   60.39   |
+------------------------+-----------+-----------+
|    **bids_per_url**    |   15.63   |   116.8   |
+------------------------+-----------+-----------+
|     **mean_resp**      | 3.395e+12 | 5.332e+10 |
+------------------------+-----------+-----------+


As you can see from the table, most of the variables come out to be significant. 

## Creating the training data

The features I have created can be summarised below:

1. Total bids for each bidder

2. Total auctions for each bidder

3. Total countries for each bidder

4. Total urls for each bidder

5. Total devices for each bidder

6. Total merchanise for each bidder

7. Total ips for each bidder

8. Mean difference of time between two consecutive bids

9. Mean number bids per auction for each bidder

10. Mean number bids per country for each bidder

11. Mean number bids per device for each bidder

12. Mean number bids per ip for each bidder

13. Mean number bids per url for each bidder



```r
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
```

Now we can look at the variables to see if they look as differentiators. For example:


```r
plot(density(training[training$outcome==0,"nb_bids"]), xlim=range(0:5000), main="average number of bids", xlab = "nb_bids", ylab = "density", yaxt='n', lwd=2)
par(new=TRUE)
plot(density(training[training$outcome==1,"nb_bids"]), xlim=range(0:5000), main="average number of bids", xlab = "nb_bids", ylab = "density", yaxt='n',col="red", lwd=2)
legend(x="topright", c("Human","Robot"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
```

![](Codebook_files/figure-html/unnamed-chunk-23-1.png)<!-- -->


```r
plot(density(training[training$outcome==0,"bids_per_auction"]), xlim=range(0:50), main="average number of bids per auction", xlab = "bids_per_auction", ylab = "density", yaxt='n', lwd=2)
par(new=TRUE)
plot(density(training[training$outcome==1,"bids_per_auction"]), xlim=range(0:50), main="average number of bids per auction", xlab = "bids_per_auction", ylab = "density", yaxt='n',col="red", lwd=2)
legend(x="topright", c("Human","Robot"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
```

![](Codebook_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

### Shuffle training dataset


```r
options(width = 160)

training <- training[sample(seq_len(nrow(training))),]

pander(training[1:20,], style='grid')
```



+------------+-----------+---------------+--------------+----------------+
|   &nbsp;   |  nb_bids  |  nb_auctions  |  nb_devices  |  nb_countries  |
+============+===========+===============+==============+================+
|  **1632**  |    32     |      23       |      19      |       6        |
+------------+-----------+---------------+--------------+----------------+
|  **986**   |     6     |       5       |      5       |       4        |
+------------+-----------+---------------+--------------+----------------+
|  **667**   |     1     |       1       |      1       |       1        |
+------------+-----------+---------------+--------------+----------------+
|  **1070**  |     1     |       1       |      1       |       1        |
+------------+-----------+---------------+--------------+----------------+
|  **649**   |     1     |       1       |      1       |       1        |
+------------+-----------+---------------+--------------+----------------+
|   **73**   |   3005    |      279      |     235      |       11       |
+------------+-----------+---------------+--------------+----------------+
|  **1806**  |    175    |      50       |      48      |       2        |
+------------+-----------+---------------+--------------+----------------+
|  **307**   |    159    |      43       |      78      |       22       |
+------------+-----------+---------------+--------------+----------------+
|  **1101**  |   2073    |      111      |     255      |       21       |
+------------+-----------+---------------+--------------+----------------+
|  **614**   |   11819   |      676      |     849      |      119       |
+------------+-----------+---------------+--------------+----------------+
|  **269**   |    86     |      35       |      32      |       8        |
+------------+-----------+---------------+--------------+----------------+
|  **729**   |    28     |      18       |      19      |       4        |
+------------+-----------+---------------+--------------+----------------+
|  **437**   |     1     |       1       |      1       |       1        |
+------------+-----------+---------------+--------------+----------------+
|  **937**   |   2655    |      302      |     404      |       78       |
+------------+-----------+---------------+--------------+----------------+
|  **1378**  |   1279    |      303      |     283      |       43       |
+------------+-----------+---------------+--------------+----------------+
|  **1617**  |     4     |       4       |      4       |       2        |
+------------+-----------+---------------+--------------+----------------+
|  **1512**  |     3     |       3       |      3       |       2        |
+------------+-----------+---------------+--------------+----------------+
|  **1395**  |     1     |       1       |      1       |       1        |
+------------+-----------+---------------+--------------+----------------+
|  **1178**  |    483    |       8       |     167      |       62       |
+------------+-----------+---------------+--------------+----------------+
|  **1205**  |     5     |       4       |      4       |       2        |
+------------+-----------+---------------+--------------+----------------+

Table: Table continues below

 

+------------+----------+--------------+--------------------+
|   &nbsp;   |  nb_ips  |  inter_time  |  bids_per_auction  |
+============+==========+==============+====================+
|  **1632**  |    26    |  2.407e+12   |       1.391        |
+------------+----------+--------------+--------------------+
|  **986**   |    6     |  1.317e+13   |        1.2         |
+------------+----------+--------------+--------------------+
|  **667**   |    1     |      NA      |         1          |
+------------+----------+--------------+--------------------+
|  **1070**  |    1     |      NA      |         1          |
+------------+----------+--------------+--------------------+
|  **649**   |    1     |      NA      |         1          |
+------------+----------+--------------+--------------------+
|   **73**   |   1034   |  2.573e+10   |       10.77        |
+------------+----------+--------------+--------------------+
|  **1806**  |   100    |  4.225e+11   |        3.5         |
+------------+----------+--------------+--------------------+
|  **307**   |   125    |  8.602e+10   |       3.698        |
+------------+----------+--------------+--------------------+
|  **1101**  |   1100   |  6.584e+09   |       18.68        |
+------------+----------+--------------+--------------------+
|  **614**   |   5660   |  6.541e+09   |       17.48        |
+------------+----------+--------------+--------------------+
|  **269**   |    66    |  9.079e+11   |       2.457        |
+------------+----------+--------------+--------------------+
|  **729**   |    20    |  4.632e+11   |       1.556        |
+------------+----------+--------------+--------------------+
|  **437**   |    1     |      NA      |         1          |
+------------+----------+--------------+--------------------+
|  **937**   |   1648   |  2.912e+10   |       8.791        |
+------------+----------+--------------+--------------------+
|  **1378**  |   778    |  1.065e+10   |       4.221        |
+------------+----------+--------------+--------------------+
|  **1617**  |    4     |  2.273e+13   |         1          |
+------------+----------+--------------+--------------------+
|  **1512**  |    3     |   4.4e+12    |         1          |
+------------+----------+--------------+--------------------+
|  **1395**  |    1     |      NA      |         1          |
+------------+----------+--------------+--------------------+
|  **1178**  |   366    |  1.601e+11   |       60.38        |
+------------+----------+--------------+--------------------+
|  **1205**  |    5     |  3.078e+12   |        1.25        |
+------------+----------+--------------+--------------------+

Table: Table continues below

 

+------------+--------------------+-------------------+---------------+
|   &nbsp;   |  bids_per_country  |  bids_per_device  |  bids_per_ip  |
+============+====================+===================+===============+
|  **1632**  |       5.333        |       1.684       |     1.231     |
+------------+--------------------+-------------------+---------------+
|  **986**   |        1.5         |        1.2        |       1       |
+------------+--------------------+-------------------+---------------+
|  **667**   |         1          |         1         |       1       |
+------------+--------------------+-------------------+---------------+
|  **1070**  |         1          |         1         |       1       |
+------------+--------------------+-------------------+---------------+
|  **649**   |         1          |         1         |       1       |
+------------+--------------------+-------------------+---------------+
|   **73**   |       273.2        |       12.79       |     2.906     |
+------------+--------------------+-------------------+---------------+
|  **1806**  |        87.5        |       3.646       |     1.75      |
+------------+--------------------+-------------------+---------------+
|  **307**   |       7.227        |       2.038       |     1.272     |
+------------+--------------------+-------------------+---------------+
|  **1101**  |       98.71        |       8.129       |     1.885     |
+------------+--------------------+-------------------+---------------+
|  **614**   |       99.32        |       13.92       |     2.088     |
+------------+--------------------+-------------------+---------------+
|  **269**   |       10.75        |       2.688       |     1.303     |
+------------+--------------------+-------------------+---------------+
|  **729**   |         7          |       1.474       |      1.4      |
+------------+--------------------+-------------------+---------------+
|  **437**   |         1          |         1         |       1       |
+------------+--------------------+-------------------+---------------+
|  **937**   |       34.04        |       6.572       |     1.611     |
+------------+--------------------+-------------------+---------------+
|  **1378**  |       29.74        |       4.519       |     1.644     |
+------------+--------------------+-------------------+---------------+
|  **1617**  |         2          |         1         |       1       |
+------------+--------------------+-------------------+---------------+
|  **1512**  |        1.5         |         1         |       1       |
+------------+--------------------+-------------------+---------------+
|  **1395**  |         1          |         1         |       1       |
+------------+--------------------+-------------------+---------------+
|  **1178**  |        7.79        |       2.892       |     1.32      |
+------------+--------------------+-------------------+---------------+
|  **1205**  |        2.5         |       1.25        |       1       |
+------------+--------------------+-------------------+---------------+

Table: Table continues below

 

+------------+----------------+-----------+
|   &nbsp;   |  bids_per_url  |  outcome  |
+============+================+===========+
|  **1632**  |       2        |     0     |
+------------+----------------+-----------+
|  **986**   |       6        |     0     |
+------------+----------------+-----------+
|  **667**   |       1        |     0     |
+------------+----------------+-----------+
|  **1070**  |       1        |     0     |
+------------+----------------+-----------+
|  **649**   |       1        |     0     |
+------------+----------------+-----------+
|   **73**   |     3.193      |     0     |
+------------+----------------+-----------+
|  **1806**  |     13.46      |     0     |
+------------+----------------+-----------+
|  **307**   |     2.338      |     0     |
+------------+----------------+-----------+
|  **1101**  |     9.052      |     0     |
+------------+----------------+-----------+
|  **614**   |     1.576      |     0     |
+------------+----------------+-----------+
|  **269**   |     12.29      |     0     |
+------------+----------------+-----------+
|  **729**   |     1.333      |     0     |
+------------+----------------+-----------+
|  **437**   |       1        |     0     |
+------------+----------------+-----------+
|  **937**   |     18.83      |     0     |
+------------+----------------+-----------+
|  **1378**  |     2.543      |     0     |
+------------+----------------+-----------+
|  **1617**  |     1.333      |     0     |
+------------+----------------+-----------+
|  **1512**  |       1        |     0     |
+------------+----------------+-----------+
|  **1395**  |       1        |     0     |
+------------+----------------+-----------+
|  **1178**  |      161       |     0     |
+------------+----------------+-----------+
|  **1205**  |      2.5       |     0     |
+------------+----------------+-----------+

### Repeat for Testing 


```r
source("Analysis_testing.R")
```


## Classification

The test problem used here is a binary classification. I will be using the caret package in R as it provides an excellent interface into hundreds of different machine learning algorithms and useful tools for evaluating and comparing models.

The test harness is comprised of three key elements:
    
* The dataset we will use to train models.
* The test options used to evaluate a model (e.g. resampling method).
* The metric we are interested in measuring and comparing.


```r
library(caret)
```

```
## Loading required package: lattice
```

### Parallel Processing

R supports parallel computations with the core parallel package. What the doParallel package does is provide a backend while utilizing the core parallel package. The caret package is used for developing and testing machine learning models in R. This package as well as others like plyr support multicore CPU speedups if a parallel backend is registered before the supported instructions are called.


```r
library(doParallel)
```

```
## Loading required package: foreach
```

```
## Loading required package: iterators
```

```
## Loading required package: parallel
```

```r
cl <- makeCluster(detectCores())
registerDoParallel(cl)
```


### Cross Validation Options

Here, We usually randomly split the data into K distinct blocks of roughly equal size.

* We leave out the first block of data and fit a model.
* This model is used to predict the held-out block
* We continue this process until we’ve predicted all K held–out blocks

The final performance is based on the hold-out predictions K is usually taken to be 5 or 10 and leave one out cross–validation has each sample as a block Repeated K–fold CV creates multiple versions of the folds and aggregates the results.

In this case study I will use 10-fold cross validation with 3 repeats.


```r
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
```

### Test Metric

There are many possible evaluation metrics to chose from. Caret provides a good selection and you can use your own if needed. Some good test metrics to use for classification include:
    
* Accuracy
* Kappa
* ROC

The evaluation metric is specified the call to the train() function for a given model, so I will define the metric now for use with all of the model training later.


```r
metric <- "Accuracy"
```

### Model Building

There are three concerns when selecting models to spot check:
    
* What models to actually choose.
* How to configure their arguments.
* Preprocessing of the data for the algorithm.

### Algorithm

It is important to have a good mix of algorithm representations (lines, trees, instances, etc.) as well as algorithms for learning those representations.

Almost all machine learning algorithms are parameterized, requiring that you specify their arguments.

One aspect of the caret package in R is that it helps with tuning algorithm parameters. It can also estimate good defaults (via the automatic tuning functionality and the tunelength argument to the train() function).

### Data Preprocessing

Some algorithms perform a whole lot better with some basic data preprocessing. Fortunately, the train() function in caret lets you specify preprocessing of the data to perform prior to training. The transforms you need are provided to the preProcess argument as a list and are executed on the data sequentially

The most useful transform is to scale and center the data via. For example:
    

```r
preProcess=c("center", "scale")
```
### Training 



```r
# Logistic Regression

set.seed(seed)
fit.glm <- train(outcome~., data=training, method="glm", metric=metric, trControl=control, na.action=na.exclude)
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
# SVM Radial

set.seed(seed)
fit.svmRadial <- train(outcome~., data=training, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE, na.action=na.exclude)
```

```
## Loading required package: kernlab
```

```
## 
## Attaching package: 'kernlab'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     alpha
```

```r
# Random Forest

set.seed(seed)
fit.rf <- train(outcome~., data=training, method="rf", metric=metric, trControl=control, na.action=na.exclude)
```

```
## Loading required package: randomForest
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
# Stochastic Gradient Boosting (Generalized Boosted Modeling)

set.seed(seed)
fit.gbm <- train(outcome~., data=training, method="gbm", metric=metric, trControl=control, verbose=FALSE, na.action=na.exclude)
```

```
## Loading required package: gbm
```

```
## Loading required package: survival
```

```
## 
## Attaching package: 'survival'
```

```
## The following object is masked from 'package:caret':
## 
##     cluster
```

```
## Loading required package: splines
```

```
## Loaded gbm 2.1.1
```

```
## Loading required package: plyr
```

```
## --------------------------------------------------------------------------------------------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## --------------------------------------------------------------------------------------------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise, summarize
```

### Model Selection


```r
results <- resamples(list(logistic=fit.glm, svm=fit.svmRadial,rf=fit.rf, gbm=fit.gbm))

summary(results)
```

```
## 
## Call:
## summary.resamples(object = results)
## 
## Models: logistic, svm, rf, gbm 
## Number of resamples: 30 
## 
## Accuracy 
##            Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
## logistic 0.9053  0.9346 0.9405 0.9394  0.9464 0.9524    0
## svm      0.9345  0.9408 0.9464 0.9451  0.9464 0.9527    0
## rf       0.9231  0.9522 0.9525 0.9542  0.9586 0.9760    0
## gbm      0.9226  0.9464 0.9554 0.9519  0.9585 0.9702    0
## 
## Kappa 
##              Min.   1st Qu.  Median    Mean 3rd Qu.   Max. NA's
## logistic -0.04644 -0.008157 0.06497 0.08836  0.1616 0.3198    0
## svm      -0.01094  0.000000 0.08643 0.10580  0.1729 0.3199    0
## rf        0.09774  0.319800 0.40850 0.40400  0.4767 0.7028    0
## gbm       0.11270  0.267500 0.40920 0.38580  0.4959 0.6529    0
```

It is also useful to review the results using a few different visualization techniques to get an idea of the mean and spread of accuracies.


```r
# boxplot comparison
bwplot(results)
```

![](Codebook_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

```r
# Dot-plot comparison
dotplot(results)
```

![](Codebook_files/figure-html/unnamed-chunk-33-2.png)<!-- -->

### Prediction on Testing Model

We choose to predict using the gbm algorithm 

```r
outcome_probs <- predict(fit.gbm, newdata = testing, type = "prob",na.action = na.pass)
outcome_class <- predict(fit.gbm, newdata = testing, type = "raw",na.action = na.pass)

Submission <- cbind(bidder_id_t, predicted_outcome = factor(outcome_class, label=c("Human","Robot")))

write.csv(Submission,'Submission.csv')
```

The predicted outcome of the first 200 bidders in the test dataset


```r
pander(Submission[1:200,], style='grid')
```



+---------------------------------------+---------------------+
|               bidder_id               |  predicted_outcome  |
+=======================================+=====================+
| 002d229ffb247009810828f648afc2ef593rb |        Robot        |
+---------------------------------------+---------------------+
| 003180b29c6a5f8f1d84a6b7b6f7be57tjj1o |        Human        |
+---------------------------------------+---------------------+
| 00486a11dff552c4bd7696265724ff81yeo9v |        Human        |
+---------------------------------------+---------------------+
| 0051aef3fdeacdadba664b9b3b07e04e4coc6 |        Human        |
+---------------------------------------+---------------------+
| 0053b78cde37c4384a20d2da9aa4272aym4pb |        Human        |
+---------------------------------------+---------------------+
| 0061edfc5b07ff3d70d693883a38d370oy4fs |        Human        |
+---------------------------------------+---------------------+
| 00862324eb508ca5202b6d4e5f1a80fc3t3lp |        Human        |
+---------------------------------------+---------------------+
| 009479273c288b1dd096dc3087653499lrx3c |        Human        |
+---------------------------------------+---------------------+
| 009cee781e8414f7fb55b2f92157e9dbu0y6o |        Human        |
+---------------------------------------+---------------------+
| 00a79ebd15f0b24a0a3b5794457cd8ed7dng1 |        Human        |
+---------------------------------------+---------------------+
| 00b519ec8ed5e370328451379bb708a306eoj |        Human        |
+---------------------------------------+---------------------+
| 00ceb1b66fc6c08f2e1b7937b5bc7f870qn5k |        Human        |
+---------------------------------------+---------------------+
| 00dd948c3a88f7b68f1952dbeeac68ffb6qoc |        Human        |
+---------------------------------------+---------------------+
| 00e0f614d9dd32dd27f6080f472d2934emlos |        Human        |
+---------------------------------------+---------------------+
| 010e5ed5eb439e0daf1870a0b426e32cztbdt |        Human        |
+---------------------------------------+---------------------+
| 0113d101ec6aabd354adac645a1ec3e82ln88 |        Human        |
+---------------------------------------+---------------------+
| 0165dcd38f54e83aafb7bb3d95cd9369800km |        Human        |
+---------------------------------------+---------------------+
| 016e3c511caf01a845c9c976bbf355a6m1nns |        Human        |
+---------------------------------------+---------------------+
| 0176025cc599cb59f825d592b8ef3ee3p5aqv |        Human        |
+---------------------------------------+---------------------+
| 018c9ecc065880c95e21c0291a3b478dj1c0a |        Human        |
+---------------------------------------+---------------------+
| 0196137a75792cfa9e1ffae0c968f8e5h5eqq |        Human        |
+---------------------------------------+---------------------+
| 019cf2d366df756c092c91e26f406acdozha7 |        Human        |
+---------------------------------------+---------------------+
| 01a2f136b87e5c544e2f2c59295bea7ebwv8y |        Human        |
+---------------------------------------+---------------------+
| 01c729a32b5f51f088884b558cd2cfd1xz1qe |        Human        |
+---------------------------------------+---------------------+
| 01d5348d4d4472288c0c458d8630504165r4k |        Human        |
+---------------------------------------+---------------------+
| 01e4994f0a29fa76c7595fe3929bec777acv0 |        Human        |
+---------------------------------------+---------------------+
| 01ecfeb5d9946423c7fb088a84fe77c3smw4c |        Human        |
+---------------------------------------+---------------------+
| 0204227c6c8042ec8d8cc1bacb0d44b1csycb |        Human        |
+---------------------------------------+---------------------+
| 02127cccff7c56391882f1d58dd960fchwfua |        Human        |
+---------------------------------------+---------------------+
| 023d51d2bcf7d962c766ff570fa7a21ds3c96 |        Human        |
+---------------------------------------+---------------------+
| 024b9e2d822424b09ddf86b2a9cf8614a0uwy |        Human        |
+---------------------------------------+---------------------+
| 024d6a4a489d4411f795d04b6617d1f47qvrs |        Human        |
+---------------------------------------+---------------------+
| 02518a364d87661eed1453e251933862yvmjj |        Human        |
+---------------------------------------+---------------------+
| 02615aa755c78f1812d7f772f4ed3d8anist8 |        Human        |
+---------------------------------------+---------------------+
| 02857858772bd4320632bf6a51e02f6ejw6au |        Human        |
+---------------------------------------+---------------------+
| 0294102fd936228343828f48c7f2b270dklg3 |        Human        |
+---------------------------------------+---------------------+
| 02a28b9de21bcb744bc2a7dedc008f91pbcpc |        Human        |
+---------------------------------------+---------------------+
| 02acbacd7a029481f44d5a2e2d3fb0b6xe0gx |        Human        |
+---------------------------------------+---------------------+
| 02caba35d87c40ffecdb1d998d909957dx7uo |        Human        |
+---------------------------------------+---------------------+
| 02db55f07b252ecff0aa4cba93880a44bv4eb |        Human        |
+---------------------------------------+---------------------+
| 02de68074ee2edccf6a00dfbd3213269w7k7i |        Human        |
+---------------------------------------+---------------------+
| 02fbc26f080655008337619a08cb7c04voa3c |        Human        |
+---------------------------------------+---------------------+
| 0306d10ab30a2f5cda41ae7ed77b8d31oehvv |        Human        |
+---------------------------------------+---------------------+
| 030fe7527c9394a74f25462cc2ed5af4myezo |        Robot        |
+---------------------------------------+---------------------+
| 036abb8a6ba8171c3e41e16eec64e977ez1jd |        Human        |
+---------------------------------------+---------------------+
| 03757ddf8a00f5c600241f3ce956ad7bfv7tw |        Human        |
+---------------------------------------+---------------------+
| 038d5c12a210df3818eb8dc34ebedac2ldr62 |        Human        |
+---------------------------------------+---------------------+
| 03a5ac97839b407f5637ce534a0de583ygwx8 |        Human        |
+---------------------------------------+---------------------+
| 03b57948c18680b55b6e9ba10eeac42659hsj |        Human        |
+---------------------------------------+---------------------+
| 03cac5cc349396386e833a24543980cckfhyu |        Human        |
+---------------------------------------+---------------------+
| 03dd77a9a964371144e72f66e791bbe2gkphv |        Human        |
+---------------------------------------+---------------------+
| 03e81f2bdfcf6f376b1deb163127eeaedmsqn |        Human        |
+---------------------------------------+---------------------+
| 03ead6957061c72543b8bc35f6b23852j0a0h |        Human        |
+---------------------------------------+---------------------+
| 042d1cd843d886c1bd8968e18bfa1b5b8t55o |        Human        |
+---------------------------------------+---------------------+
| 0430923026291636fff20fa1707fd34f0nayx |        Human        |
+---------------------------------------+---------------------+
| 0457e2bcdd0a06bd9c8dc3ea9b3c1a7419mgs |        Human        |
+---------------------------------------+---------------------+
| 046897d78512c75febd7cd493f408632lqm40 |        Human        |
+---------------------------------------+---------------------+
| 04ab1fd6ed917467e808fbe3c6a126ebyusca |        Human        |
+---------------------------------------+---------------------+
| 04be4b7846b32bb6445928b4cb3d16af5xwzq |        Human        |
+---------------------------------------+---------------------+
| 04bffaa5e353b8a37a66767e3d9b531din3rc |        Human        |
+---------------------------------------+---------------------+
| 04db33ea12bbed34ea9907f908ad2ca2gnh2s |        Human        |
+---------------------------------------+---------------------+
| 04ef306fce8e66b25ecf6a78800e9776s4stl |        Human        |
+---------------------------------------+---------------------+
| 04f0778852b2671aab928de427ce7a8ck7vd3 |        Human        |
+---------------------------------------+---------------------+
| 04f53d51a9cac9391b1d35c109294c48apax1 |        Human        |
+---------------------------------------+---------------------+
| 04fc5cae5e63ce4437449827e58b2614058v2 |        Human        |
+---------------------------------------+---------------------+
| 05068af5ba4645de987789f5a3248cd1a9nft |        Human        |
+---------------------------------------+---------------------+
| 051586c3297f5b992000048f3704550fxja6g |        Human        |
+---------------------------------------+---------------------+
| 05357eefa29d2c28f59f9e346ec64cebylh3z |        Human        |
+---------------------------------------+---------------------+
| 05457a5a02ff30f21d0d16d6e987c7cdn9poc |        Human        |
+---------------------------------------+---------------------+
| 0550568ad77e8e41f2b188be324c10b8xw912 |        Human        |
+---------------------------------------+---------------------+
| 05606b484938640c4309fd9d286fea3fifen9 |        Human        |
+---------------------------------------+---------------------+
| 0565d7d345ac29442b26cd9ffe2b3b841wwnb |        Human        |
+---------------------------------------+---------------------+
| 057071d595b08bd00caa8e3a91eb954bhgt1q |        Human        |
+---------------------------------------+---------------------+
| 0578bad1a458d60f9ba0e810f7058389lz4wj |        Human        |
+---------------------------------------+---------------------+
| 057a0760c7d5306786fe54af9d49ddac796rf |        Human        |
+---------------------------------------+---------------------+
| 058931834bffe72041a6f046e3c7dbbepzv1e |        Human        |
+---------------------------------------+---------------------+
| 0590c434e36245c7983845752e73ebbdgyugm |        Human        |
+---------------------------------------+---------------------+
| 05b75a11dd918ab9d9dff644219e6727vqzz5 |        Human        |
+---------------------------------------+---------------------+
| 05bd6eb40889047958e8b96332cc1975vib58 |        Human        |
+---------------------------------------+---------------------+
| 05ccfd8ca96643c468ce64c50dd01078ux33u |        Human        |
+---------------------------------------+---------------------+
| 05ce81fa868e12e99ab0390d9579747b0tntg |        Human        |
+---------------------------------------+---------------------+
| 05de85f05fbb6a6bf86ff6b33be7678ek3flf |        Human        |
+---------------------------------------+---------------------+
| 05e5905428f5718a5cecfbde859dc689mcex9 |        Human        |
+---------------------------------------+---------------------+
| 05f327ba853313444da03bbc41c1cf2ac8cbo |        Human        |
+---------------------------------------+---------------------+
| 05f5792216f9a5bfe91ecd0dbccc3cdceuwmf |        Human        |
+---------------------------------------+---------------------+
| 05f9e773506d32bb059045cc9bf31b4187sga |        Human        |
+---------------------------------------+---------------------+
| 06052ab145f8a3ddc6ad8433ef11da8dputmu |        Human        |
+---------------------------------------+---------------------+
| 0619a3ec37e6d49be8b9a2dd094bc8d4skgew |        Human        |
+---------------------------------------+---------------------+
| 06237b976cf6ae2d3e3bea5e2d3dade4krnvu |        Human        |
+---------------------------------------+---------------------+
| 064711ab52b5f3b7deac4c0465a7ee42h1uot |        Human        |
+---------------------------------------+---------------------+
| 06586694c6bba2ccaa8e13354701bb30em5ro |        Human        |
+---------------------------------------+---------------------+
| 068b3d3f48528027025cd31ba92c0d1bpkjdq |        Human        |
+---------------------------------------+---------------------+
| 0695b948daa6f8bffb8758d201c27517k50ao |        Human        |
+---------------------------------------+---------------------+
| 06a984e49706c47d079c33a6e038ee9a3moip |        Human        |
+---------------------------------------+---------------------+
| 06af48f3b9799398a569506d7cbfaa747k49t |        Human        |
+---------------------------------------+---------------------+
| 06b9a5226231e5c03cd00c074dce0bacjod7v |        Human        |
+---------------------------------------+---------------------+
| 06bbde5ca852af9ed6f2afdc6ee5bc3bko1h1 |        Human        |
+---------------------------------------+---------------------+
| 06cb735854688d6d03c39fe1f8fe84563y4m1 |        Human        |
+---------------------------------------+---------------------+
| 06dbfffbca4f34157f8a575fa535d9ad3xfwm |        Human        |
+---------------------------------------+---------------------+
| 06ea8e569d68b4767725e060d3a72938iqim6 |        Human        |
+---------------------------------------+---------------------+
| 06ed8a1726d2a3d03c195cb219b2bbb9wbmz8 |        Human        |
+---------------------------------------+---------------------+
| 06ff2d37e247d28cac0e2a944e39cd6budsbx |        Human        |
+---------------------------------------+---------------------+
| 070841d4774fef5fb50eca78a7156715q2hfi |        Human        |
+---------------------------------------+---------------------+
| 070c705dab050bebc8c9b379045d8ae8lx62r |        Human        |
+---------------------------------------+---------------------+
| 070d7aa0b0e7754e8c58194a6e2c8fc2js081 |        Human        |
+---------------------------------------+---------------------+
| 070f01355c418cfca4d58db04a5d5af5wuk9n |        Human        |
+---------------------------------------+---------------------+
| 071300a7c3385788c00573d1c4bf150csf3cg |        Human        |
+---------------------------------------+---------------------+
| 07201f701766a7ea89b36b4e7b984beap4zdc |        Human        |
+---------------------------------------+---------------------+
| 074993754af952e8d582e9ff04a2bd5bh2s05 |        Human        |
+---------------------------------------+---------------------+
| 0750cce1071adff96c9e8d417482cf762jjmb |        Human        |
+---------------------------------------+---------------------+
| 0751502caf4afd959c99955c80edacc85u11v |        Human        |
+---------------------------------------+---------------------+
| 0763e7371b763087182eb37930074ff8mj7fw |        Human        |
+---------------------------------------+---------------------+
| 0764308c5a1d35bf76c32d943eff07a50tp9e |        Human        |
+---------------------------------------+---------------------+
| 0774742ee8690973faf76efd53e66b435sakj |        Human        |
+---------------------------------------+---------------------+
| 07965f31105b6ef1ab30efedbfcd66707lwrm |        Human        |
+---------------------------------------+---------------------+
| 07a1eeb3bc5be386c1eb67f4bf69ad56wvkcc |        Human        |
+---------------------------------------+---------------------+
| 07a5dbfbf2629ef7018abb660fe0e795xmrw3 |        Human        |
+---------------------------------------+---------------------+
| 07d23b0f422ef8851ad105ad659fa6492n2su |        Human        |
+---------------------------------------+---------------------+
| 07d4faa935f7e83e3b8c08c34baf74a59lwb6 |        Human        |
+---------------------------------------+---------------------+
| 080042810884fa0937e9ddc89ca18a28jhz48 |        Human        |
+---------------------------------------+---------------------+
| 08078a2526666deea62ca7c065bf22c4h7i8s |        Human        |
+---------------------------------------+---------------------+
| 080a1139f5575159bdca6044ae568550d8f12 |        Human        |
+---------------------------------------+---------------------+
| 08144977af383cb9f1d3b51ade19f35frjmwm |        Human        |
+---------------------------------------+---------------------+
| 082659fb2b29eb5ae7c241393ebb76bb8wq4y |        Human        |
+---------------------------------------+---------------------+
| 083bc8dabba83902d98f0dd986388319sryc2 |        Human        |
+---------------------------------------+---------------------+
| 084a25ac6da2d0997becd19eeda619c8po6d2 |        Human        |
+---------------------------------------+---------------------+
| 085829afe1af4c5e81563458bb288088y992x |        Human        |
+---------------------------------------+---------------------+
| 0865d1e207fa3728a7379c415c903133rr6ce |        Human        |
+---------------------------------------+---------------------+
| 08728c61fc683905eec522c104030551y5qqh |        Human        |
+---------------------------------------+---------------------+
| 0875b86dd85b2cfc8727d6ae019832b870s6s |        Human        |
+---------------------------------------+---------------------+
| 0893eebbf5c7e431be85d18a4aad99a2mc32v |        Human        |
+---------------------------------------+---------------------+
| 08c389a8a55b13d0423b5c0dba3a9b6clh1gr |        Human        |
+---------------------------------------+---------------------+
| 08e14af6fbdb7aa4f3e38dcef26df601mz63o |        Human        |
+---------------------------------------+---------------------+
| 08e32f3d08db3edea0fea2e8c6634c8c4iq14 |        Human        |
+---------------------------------------+---------------------+
| 08e89d1f541baac1ed7a0f16fcd46474ob5u9 |        Human        |
+---------------------------------------+---------------------+
| 08f13a984962c44d88e10f188ffc7c38yqn9o |        Human        |
+---------------------------------------+---------------------+
| 08f2b08040d62a4dc55d9f65539b6090um72f |        Human        |
+---------------------------------------+---------------------+
| 08f929bbb45f6101b61eeade665542bdj6ioy |        Human        |
+---------------------------------------+---------------------+
| 08fdd6af0ffe017623d5dd2f3b509f598dk01 |        Human        |
+---------------------------------------+---------------------+
| 0903c5009f3a06db04d5e40a83e04480ukuvm |        Human        |
+---------------------------------------+---------------------+
| 090b3859aad1ed0dfbe857cee2ef6aac8w0kp |        Human        |
+---------------------------------------+---------------------+
| 090c13aa3b9f8ff7227bfb74308e9b2e128jf |        Human        |
+---------------------------------------+---------------------+
| 091415979b1bf178f6c358bca7a3d30dkct2r |        Human        |
+---------------------------------------+---------------------+
| 09141d643a0021782bd040bf51cbdef4c17t5 |        Human        |
+---------------------------------------+---------------------+
| 0918faabadfe856a25ebf1ff96b2cfaf7ez6a |        Human        |
+---------------------------------------+---------------------+
| 0925bab07c4d3600f2a56dc4468d243ahcuo6 |        Human        |
+---------------------------------------+---------------------+
| 092ceb77862ed0c120af013fe90093e64qknp |        Human        |
+---------------------------------------+---------------------+
| 0933e1844ef295b026b5e6ff6f4c0960jhzqr |        Human        |
+---------------------------------------+---------------------+
| 094ca167823888700c962d06c82b261bdkp2a |        Human        |
+---------------------------------------+---------------------+
| 0956ac7abf361527a71a2c24a3f29b80ytlot |        Human        |
+---------------------------------------+---------------------+
| 09790d0fc669de383256c7171ffd5143abd3f |        Human        |
+---------------------------------------+---------------------+
| 097ac653dca3fe0ba83d24ebd1fd23b28ztxe |        Human        |
+---------------------------------------+---------------------+
| 0983ad44219dfddd5a501810f655233dur2r8 |        Human        |
+---------------------------------------+---------------------+
| 0985bc13264dfe1af0abc65f3c0b8a96c8sl9 |        Human        |
+---------------------------------------+---------------------+
| 09886a694696deb1bd6c3f12c6acb08bg66sz |        Human        |
+---------------------------------------+---------------------+
| 098d2373ee8e8f83c5e597f210f082e2gx9uj |        Human        |
+---------------------------------------+---------------------+
| 09a5fc9ba18647d388f46f0cb134cadd838er |        Human        |
+---------------------------------------+---------------------+
| 09aa1a84c7c7ba807ab6b2ea7fa2aa6dorm2y |        Robot        |
+---------------------------------------+---------------------+
| 09ae6f2cf493017750379a2f5763ed53wfnni |        Human        |
+---------------------------------------+---------------------+
| 09ba0a844a8683d61e18f183ca23be7435vfd |        Human        |
+---------------------------------------+---------------------+
| 09d99aa44268dd5c999628733fe24b7f34cag |        Human        |
+---------------------------------------+---------------------+
| 09f0c22f61c5a7f643357a2e606b71677a2fa |        Human        |
+---------------------------------------+---------------------+
| 0a08cba9576abdd7f401ca52a50c328coo7j3 |        Human        |
+---------------------------------------+---------------------+
| 0a0e1973ea3cba47f33b0166e9abbb4c5ndpa |        Human        |
+---------------------------------------+---------------------+
| 0a19c41f6163cd4dd020318ea1a14fb4uapty |        Human        |
+---------------------------------------+---------------------+
| 0a3591872765dc366dab4ce259874395kh6mj |        Human        |
+---------------------------------------+---------------------+
| 0a39721ba697f341aae5915f25a440050er1r |        Human        |
+---------------------------------------+---------------------+
| 0a4733c94d22a38664cff64d6859ec18y6db1 |        Human        |
+---------------------------------------+---------------------+
| 0a584f77759fefad47820d54ffa21a24dxqqu |        Human        |
+---------------------------------------+---------------------+
| 0a798636852c5948e899311731439855kisj2 |        Human        |
+---------------------------------------+---------------------+
| 0a807dffdef748a472534f0b42df4cefi0913 |        Human        |
+---------------------------------------+---------------------+
| 0a8622c7bc8526de762f3b59c91ecbbcvpua2 |        Human        |
+---------------------------------------+---------------------+
| 0a86a3b6549af71433eff4e733d5a19aqi2qn |        Human        |
+---------------------------------------+---------------------+
| 0a926caf34d9c1d78c7e5af4ba05f065m4asu |        Human        |
+---------------------------------------+---------------------+
| 0a9752168b19c91ec99b943a40af4c1a0b8s8 |        Human        |
+---------------------------------------+---------------------+
| 0aba70072c70a28a41ac341bb9c41136t38g1 |        Human        |
+---------------------------------------+---------------------+
| 0ac6f29b2269feea7d94aec032b6353adh38a |        Human        |
+---------------------------------------+---------------------+
| 0ad5956336240ebaa75932cf95c85a9by36tn |        Human        |
+---------------------------------------+---------------------+
| 0adb524d88ff4c0c6da7ed87599b34800pl1i |        Human        |
+---------------------------------------+---------------------+
| 0b1feb6c1cfe82dacf7634565f96ac48y5muk |        Human        |
+---------------------------------------+---------------------+
| 0b2f7f319c7727d3fb98cf01b4bdc5c6f9u83 |        Human        |
+---------------------------------------+---------------------+
| 0b37699cfd1395db5454973168a5340bmp9ko |        Human        |
+---------------------------------------+---------------------+
| 0b4f65b31803376d777d1d935a44fcfa6d5ge |        Human        |
+---------------------------------------+---------------------+
| 0b5f65d28922ebbcf2ee9becd5f7dd08snxnr |        Human        |
+---------------------------------------+---------------------+
| 0b6c8845a6a8032ed0856636bbecd906v1ywr |        Robot        |
+---------------------------------------+---------------------+
| 0b7b89e4e36908ddc32dfd2b46fe52dbp6by5 |        Human        |
+---------------------------------------+---------------------+
| 0b839e3756b868b511319ff582235851nzeb7 |        Human        |
+---------------------------------------+---------------------+
| 0b9ad98f40b4d4f3a9e5a1dd32185d18uy668 |        Human        |
+---------------------------------------+---------------------+
| 0b9d45948078268c6394a9c51bf2db08cbijp |        Human        |
+---------------------------------------+---------------------+
| 0ba0e2c0a33c032cf705d7eadde0bab5j4j9e |        Human        |
+---------------------------------------+---------------------+
| 0bca7f1039919c1572cedda5224434f9g5j2f |        Human        |
+---------------------------------------+---------------------+
| 0bf21b162b2eddf37e92bf708a79b94b5e0st |        Human        |
+---------------------------------------+---------------------+
| 0bf40de3a66bd8a70777ece6756c6882jg7s6 |        Human        |
+---------------------------------------+---------------------+
| 0bf6ae3f3085a90b43aa86e3c1eaff695w2jj |        Human        |
+---------------------------------------+---------------------+
| 0c1385603d821c3a5d91f5c291dfec2a0f7rv |        Human        |
+---------------------------------------+---------------------+
| 0c19bfcb25b818fb085f7b890f1c3fd0opg88 |        Human        |
+---------------------------------------+---------------------+
| 0c2ab4132564259a0b02c5bf57b49763n2bzh |        Human        |
+---------------------------------------+---------------------+
| 0c3e5de44a27de84eff9f81ed777e8a7bxjyk |        Human        |
+---------------------------------------+---------------------+
| 0c44f546ed40a8ccfc2e985c6d1c624fjtj11 |        Human        |
+---------------------------------------+---------------------+
| 0c47587c4459baed81d0b707eff6b4bff5tmx |        Robot        |
+---------------------------------------+---------------------+

```r
stopCluster(cl)
```

## Room for Improvement

The analysis done can be improved in several ways

* Many other features can be taken into consideration

* Other classification algorithms should be considered and compared to see which algorithm is better using the test metric

* ROC should be considered instead of accuracy because classification is asymmetric and should be plotted for each algorithm to check which performs better on the cross validation 


