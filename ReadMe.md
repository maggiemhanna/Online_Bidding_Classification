This is my solution for the Kaggle competition - Facebook Recruiting IV:
Human or Robot?

[Facebook Recruiting IV: Human or
Robot?](https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/leaderboard/private)

There are two datasets in this competition. One is a bidder dataset that
includes a list of bidder information, including their id, payment
account, and address. The other is a bid dataset that includes 7.6
million bids on different auctions. The datasets can be downloaded from

[Datasets](https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/data)

The solution is done in three seperate R scripts

-   Analysis.R

-   Analysis\_testing.R

-   Classification.R

Project Summary
---------------

The following are the steps done

1.  Merge the train and the bid datasets by bidder\_id to create the
    training dataset
2.  Check what could be the possible features
3.  Create the Tidy training and testing datasets based on the chosen
    features
4.  Choose the appropraite classification algorithm based on the ROC
    test metric
5.  Make the appropraite prediction of the outcome of the testing
    dataset

Additional Information
----------------------

You can find additional information about the variables, data and
transformations in the CodeBook.md file.
