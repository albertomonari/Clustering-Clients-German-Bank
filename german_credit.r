#######################################################
#########           Clustering                #########
#######################################################


# Import the dataset


bank_customers <- read.csv("german_credit_data.csv")

# Useful packages
#install.packages('plyr')
#install.packages('ggplot2')
#install.packages('cluster')
#install.packages('lattice')
#install.packages('graphics')
#install.packages('grid')
#install.packages('gridExtra')

# Useful packages are loaded
library (plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)
library(dplyr)


### DISCOVERY ###
# The ultimate goal of this project is to cluster the clients of a German bank in order to provide the bank 
# with important information about its clients and to try to answer the business question: 
# "How should the bank personalise its investment products for its customers?".

# The original dataset contains 1000 observations (people asking for a credit in a bank) with 10 variables for each entry:
# - the first column is only a unique identifier of the line; the lines are numbered from 1 to 1000
# - Age (numeric)
# - Sex (text: male, female)
# - Job (numeric: 0 - unskilled and non-resident, 1 - unskilled and resident, 2 - skilled, 3 - highly skilled)
# - Housing (text: own, rent, or free)
# - Saving accounts (text - little, moderate, quite rich, rich)
# - Checking account (numeric, in DM - Deutsch Mark)
# - Credit amount (numeric, in DM)
# - Duration (numeric, in month)
# - Purpose (text: car, furniture/equipment, radio/TV, domestic appliances, repairs, education, business, vacation/others)

summary(bank_customers)


### DATA PREPARATION ###

# Remove X column because it is useless
bank_customers <- bank_customers[-1]

# Remove lines with missing values
bank_customers <- na.omit(bank_customers)
# We move from 1000 observations to 522 observations (observation = customer)

# We create a duplicate of the original dataset, which we will not touch during the process so as to 
# add the group of each client to the original dataset
bank_customers_original_dataset <- bank_customers

# Visualizing data
# Plotting Age
ggplot(bank_customers, aes(Age)) + geom_histogram(binwidth=5, colour="black", fill="red") +
  labs(x= "Age",y= "Number of customers", title = "Histogram Age") +
  stat_bin(binwidth=5, geom='text', color='white', aes(label=..count..),
           position=position_stack(vjust = 0.5))

# The majority of clients are in the 20 to 40 age range

# Plotting Sex
ggplot(bank_customers, aes(Sex) ) + geom_bar(aes(fill = as.factor(bank_customers$Sex))) + 
  theme(legend.title = element_blank()) +
  labs(x= "Sex",y= "Number of customers" , title = "Barplot Sex") +  
  stat_count( geom='text', color='white', aes(label=..count..),
  position=position_stack(vjust = 0.5)) 

# The ratio between males and females is about 2 : 1

# Plotting Job
ggplot(bank_customers, aes(Job) ) + geom_bar(aes(fill = as.factor(bank_customers$Job))) + 
  scale_fill_discrete(labels=c( "unskilled and non-resident","unskilled and resident", "skilled", "highly skilled")) + 
  theme(legend.title = element_blank()) +
  labs(x= "Level of Job",y= "Number of customers" , title = "Barplot Job") +
  stat_bin(binwidth=1, geom='text', color='white', aes(label=..count..),
           position=position_stack(vjust = 0.5))
# The majority of clients are characterised by a job identified as skilled

# Plotting Housing
ggplot(bank_customers, aes(Housing) ) + geom_bar(aes(fill = as.factor(bank_customers$Housing))) + 
  theme(legend.title = element_blank()) +
  labs(x= "Housing",y= "Number of customers" , title = "Barplot Housing") +
  stat_count( geom='text', color='white', aes(label=..count..),
              position=position_stack(vjust = 0.5)) 
# Most customers own their own house of residence

# Plotting Saving accounts
ggplot(bank_customers, aes(Saving.accounts) ) + geom_bar(aes(fill = as.factor(bank_customers$Saving.accounts))) +
  theme(legend.title = element_blank()) +
  labs(x= "Saving Accounts",y= "Number of customers" , title = "Barplot Saving Accounts") +
  stat_count(geom='text', color='white', aes(label=..count..),
           position=position_stack(vjust = 0.5))
# The vast majority of clients have little money invested

# Plotting Checking accounts
ggplot(bank_customers, aes(Checking.account) ) + geom_bar(aes(fill = as.factor(bank_customers$Checking.account))) + 
  theme(legend.title = element_blank()) +
  labs(x= "Checking Account",y= "Number of customers" , title = "Barplot Checking Account") +
  stat_count(geom='text', color='white', aes(label=..count..),
             position=position_stack(vjust = 0.5))
# The vast majority of clients have little or moderate money on hand (this was to be expected from the fact 
# that these clients are applying for a loan from the bank)

# Plotting Credit amount
ggplot(bank_customers, aes(Credit.amount)) + geom_histogram(binwidth=1000, colour="black", fill="blue") +
  labs(x= "Credit Amount",y= "Number of customers" , title = "Histogram Credit Amount") +
  stat_bin(binwidth=1000, geom='text', color='white', aes(label=..count..),
             position=position_stack(vjust = 0.5))

# The credit amount is mainly between 500 and 4500 euros

# Plotting Duration
ggplot(bank_customers, aes(Duration)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Duration in Months",y= "Number of customers" , title = "Histogram Duration") +
  stat_bin(binwidth=4, geom='text', color='black', aes(label=..count..),
           position=position_stack(vjust = 0.5))
# The duration of the loans does not follow a precise distribution, but does not exceed 72 months.

# Plotting Purpose
ggplot(bank_customers, aes(Purpose) ) + geom_bar(aes(fill = as.factor(bank_customers$Purpose))) + 
  theme(legend.title = element_blank()) +
  labs(x= "Purpose of Loan",y= "Number of customers" , title = "Barplot Loan Purpose")+
  stat_count(geom='text', color='white', aes(label=..count..),
             position=position_stack(vjust = 0.5))
# The purpose of the loan is mainly to purchase tangible goods such as cars, furniture or TV or radio.


# The standard k-means algorithm is not applicable to categorical data
# As in this case categorical data is important to apply clustering, we transform categorical data into numerical data
unique(bank_customers$Sex)
# Sex is a categorical variable and it can assume the values "male" and "female"
bank_customers$Sex <- ifelse(bank_customers$Sex == "male", 1, 
                             ifelse(bank_customers$Sex == "female",2,3))
# Sex: "male" = 1, "female" = 2

unique(bank_customers$Housing)
# Housing is a categorical variable and it can assume the values "own", "free" and "rent"
bank_customers$Housing <-ifelse(bank_customers$Housing == "own", 1, 
                               ifelse(bank_customers$Housing == "free", 2,
                                      ifelse(bank_customers$Housing == "rent", 3, 4)))
# Housing: "own" = 1, "free" = 2, "rent" = 3

unique(bank_customers$Saving.accounts)
# Saving accounts is a categorical variable and it can assume the values "little", "moderate", "quite rich" and "rich"
bank_customers$Saving.accounts <- ifelse(bank_customers$Saving.accounts == "little", 1, 
                                         ifelse(bank_customers$Saving.accounts == "moderate", 2,
                                                ifelse(bank_customers$Saving.accounts == "quite rich",3, 
                                                       ifelse(bank_customers$Saving.accounts == "rich", 4, 5))))
# Saving.accounts: "little" = 1, "moderate" = 2, "quite rich" = 3, "rich" = 4

unique(bank_customers$Checking.account)
# Saving accounts is a categorical variable and it can assume the values "moderate", "little" and "rich"
bank_customers$Checking.account <- ifelse(bank_customers$Checking.account == "little", 1, 
                                          ifelse(bank_customers$Checking.account == "moderate", 2, 
                                                 ifelse(bank_customers$Checking.account == "rich", 3, 4)))
# Checking_accounts: "little" = 1, "moderate" = 2, "rich" = 3

unique(bank_customers$Purpose)
# Purpose is a categorical variable and it can assume the values "radio/TV", "furniture/equipment", "car", "business",
# "domestic appliances", "repairs", "vacation/others" and "education"
bank_customers$Purpose <- ifelse(bank_customers$Purpose == "business", 1, 
                                 ifelse(bank_customers$Purpose == "car", 2, 
                                        ifelse(bank_customers$Purpose == "domestic appliances", 3, 
                                               ifelse(bank_customers$Purpose == "education", 4, 
                                                      ifelse(bank_customers$Purpose == "furniture/equipment", 5, 
                                                             ifelse(bank_customers$Purpose == "radio/TV", 6, 
                                                                    ifelse(bank_customers$Purpose == "repairs", 7, 
                                                                           ifelse(bank_customers$Purpose == "vacation/others", 8, 9))))))))
# Purpose: "business" = 1, "car" = 2, "domestic appliances" = 3, "education" = 4,  "furniture/equipment" = 5, 
# "radio/TV" = 6, "repairs" = 7, "vacation/others" = 8

str(bank_customers)

# To check unique values
sapply(bank_customers, function(x) length(unique(x)))


# Standardize variables
bank_customers_scaled <- scale(bank_customers)

# To determine an appropriate value for k, the k-means algorithm is used to identify clusters for k = 1, 2, .. . , 30

wss <- numeric(30)
for(k in 1:30){
  wss[k]<-sum(kmeans(bank_customers_scaled, centers=k, nstart = 25, iter.max = 50)$withinss)
}


#  The option *nstart=25* specifies that the k-means algorithm will be repeated 25 times, each starting with k random initial centroids.

#  The Within Sum of Squares metric (wss) is plotted against the respective number of centroids

plot(1:30, wss, type="b", xlab="Number of Clusters" , ylab="Within Sum of Squares" )


# A substantial reduction is occurring when k = 4 and k = 5
# Let's try both cases: with 5 groups and with 4 groups. 
# Let's set a seed before continuing, so that we have the same results every time we launch the k-means algorithm
set.seed(92277)
km<-kmeans(bank_customers_scaled, 5)
km

km$size
km$centers
km$cluster


set.seed(92277)

km<-kmeans(bank_customers_scaled, 4)
km

km$size
km$centers
km$cluster

# Since we don't have many variables, it is easier to identify 4 groups, so we decided to keep 4 groups instead of 5.

# Add the cluster column to the dataset
bank_customers$cluster <- factor(km$cluster)
bank_customers_original_dataset$cluster <- factor(km$cluster)

head(bank_customers_original_dataset)

# Compute min, mean and max for quantitative variables for each group
library(dplyr)
bank_customers_quantitative <- bank_customers %>%
  select(Age, Credit.amount, Duration, cluster)

groups_charateristics <- bank_customers_quantitative %>%
  group_by(cluster) %>%
  summarize(min.Age = round(min(Age)),
            mean.Age = round(mean(Age)),
            max.Age = round(max(Age)),
            min.Credit.amount = round(min(Credit.amount)),
            mean.Credit.amount = round(mean(Credit.amount)),
            max.Credit.amount = round(max(Credit.amount)),
            min.Duration = round(min(Duration)),
            mean.Duration = round(mean(Duration)),
            max.Duration = round(max(Duration)))
groups_charateristics


# Compute the most frequent type of client for each cluster
bank_customers_original_dataset %>% group_by(cluster) %>% 
                                    count(Sex, Housing, Saving.accounts, Checking.account, Purpose) %>%
                                    top_n(1)

# Compute the most frequent Sex for each cluster
bank_customers_original_dataset %>% group_by(cluster) %>% 
  count(Sex) %>%
  top_n(1)

# Compute the most frequent Housing for each cluster
bank_customers_original_dataset %>% group_by(cluster) %>% 
  count(Housing) %>%
  top_n(1)

# Compute the most frequent Saving account for each cluster
bank_customers_original_dataset %>% group_by(cluster) %>% 
  count(Saving.accounts) %>%
  top_n(1)

# Compute the most frequent Checking account for each cluster
bank_customers_original_dataset %>% group_by(cluster) %>% 
  count(Checking.account) %>%
  top_n(1)

# Compute the most frequent Purpose for each cluster
bank_customers_original_dataset %>% group_by(cluster) %>% 
  count(Purpose) %>%
  top_n(1)

# Plot showing the correlation between Customer Age and Duration divided by clusters
g1=ggplot(data=bank_customers, aes(x=bank_customers$Age, y=bank_customers$Duration, color=cluster)) + geom_point() + 
   labs(x= "Customer Age",y= "Duration" , title = "Correlation Age / Duration")
g1
g2=ggplot(data=bank_customers, aes(x=bank_customers$Age, y=bank_customers$Credit.amount, color=cluster)) + geom_point() +
   labs(x= "Customer Age",y= "Credit Amount" , title = "Correlation Age / Credit amount")
g2
g3=ggplot(data=bank_customers, aes(x=bank_customers$Credit.amount, y=bank_customers$Duration, color=cluster)) + geom_point() +
  labs(x= "Credit Amount",y= "Duration" , title = "Correlation Credit Amount / Duration")
g3


# Cluster 1: older customers, low mean credit amount, low mean duration, only male, with their own house,
#            they want to buy a new car, they have a little saving account, they have a little checking account

# Cluster 2: older customers, very high mean credit amount, high mean duration, mostly male, with their own house,
#            they want to buy a new car, they have a little saving account, they have a moderate checking account

# Cluster 3: younger customers, low mean credit amount, low mean duration, half male half female, they live in a rented house,
#            they want to buy a new car, they have a little saving account, they have a little checking account

# Cluster 4: middle age customers, very low mean credit amount, low mean duration, only male, with their own house,
#            they want to buy a new radio or a new TV, they have a little saving account, they have a moderate checking account         


# Responding to our business issue: how should the bank personalize its investment products for its customers? 
# With our clustering we are now able to inform the bank on the relative risk for each type of customer, 
# and thus enable it to adapt its offer depending on the customer profile. 


# Customers from cluster 4 seem to present the less credit risk: 
# even with little saving or checking account, their credit amount is very low for a short term, 
# and their ownership status brings a certain guarantee to the bank. 

# Customers from clusters 1 & 3 both have low amount and low duration credits, but only people from cluster 1 are homeowners. 
# Thus customers from cluster 3 seem a bit less reliable than the ones from cluster 1. 

# Finally customers from cluster 2 appear to present the more credit risk, 
# with a very high credit amount and duration, for a little saving account and moderate checking account.

