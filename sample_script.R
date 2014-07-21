require(plyr)
require(ggplot2)
require(ggvis)

setwd('/Users/ginaschmalzle/Documents/Craig_R')

# This is the number of shoes I theoreticallyy bought 
# last year divided by the total number of shoes in 
# the store (multiply by 100 and you get the 
# percentage of shoes I bought compared
# to the total number in the store).
R <- 0.01
# Assign the total number of iterations for sampled datasets
it <- 200
# Assign number of shoes
nshoe1 <- 1000            # Number of shoes in the store in year 1
meanprice1 <- 100         # Mean price of shoes in year 1
pricesd1 <- 50            # Std of price in shoes in year 1

nshoe2 <- 1400            # Number of shoes in the store in year 2
meanprice2 <- 120         # Mean price of shoes in year 2          
pricesd2 <- 40            # Std of price in shoes in year 2
#############################################################################
# Some functions that help with this problem
#############################################################################
# Create dummy dataframes
# The dummy dataframe has a number of shoes (n), mean price, and Stdev. of price
makedata <- function (numberofshoes, dm, sdv){
 # Assign number of shoes 
 df <- data.frame(shoes = seq(1:numberofshoes))
 # Assign random # of bucks for each shoe
 df$bucks <-  rnorm(n = numberofshoes, mean = dm, sd = sdv)
 return (df)
}

# Functions for make a sample dataframe
sampleme <- function(dataframe, samplerate){
  # Generate a subsample of shoe numbers, then take the associated
  # bucks and stick them into sdf.
  sdf <- data.frame(shoes=sample(1:nrow(dataframe), size = (samplerate*nrow(dataframe))))
  sdf <- merge(sdf,dataframe,all.x=TRUE)
  return (sdf)
}

# Take samples and store them in one big dataframe that contains the iteration number
storesamples<-function(iteration, df, sr){
  for (iter in 1:iteration){
    sdf <- sampleme(dataframe = df, samplerate=sr)
    sdf$index <- iter
    ifelse(iter == 1, allsdf <-sdf, allsdf <-rbind(allsdf,sdf))
  } 
  return(allsdf)
}

# Figure out how many shoes I can buy
shoesIcanbuy <- function(dataframe,mypurse){
  numofshoepairs <- 0
  while (mypurse > 0)  {
    Shoe.pair<-dataframe[sample(nrow(dataframe),1),] # Pick a random pair of shoes
    if (mypurse >= Shoe.pair$bucks){                 # As long as I have enough money in my purse
      mypurse<-mypurse-Shoe.pair$bucks               # Buy a pair of shoes and subtract them from my budget
      numofshoepairs <- numofshoepairs + 1           # Record the number of shoes I bought
    } 
    else {
      break
    }
  }
  return(numofshoepairs)                             # Return the number of shoes I bought
}

# Figure out how many shoes in the store I can buy given last year's budget.
how_many_shoes_in_store_I_bought <- function(dataframe, summarya, it){
  numofshoepairs <- array()                                   # Declare an array
  for (i in 1:nrow(summarya)) {                               # Use each row in summarya as my starting budget
    mypurse<-summarya[i,2]
    for (j in 1:(2*it)){                                          # Figure out how many shoes I bought with each starting budget
      numofshoepairs[j] <- shoesIcanbuy(dataframe, mypurse)
    }
    numofshoepairs.df<-data.frame(Shoes=numofshoepairs)
    ifelse(i==1, numofshoepairs.masterdf<-numofshoepairs.df, 
           numofshoepairs.masterdf<-rbind(numofshoepairs.masterdf,numofshoepairs.df))
  }
  return(numofshoepairs.masterdf)
}

##############################################
# END OF FUNCTIONS
##############################################

# Now for the questions...
##############################################
# QUESTION 1
##############################################
# In year 1, there were nshoe1 number of shoes in the store, that had a mean price of
# meanprice1, and a price standard deviation of pricesd1.  How much money do I 
# spend if I purchase 1% (R = 0.01) of the store's inventory?
##############################################

# First, make a fake dataset 
# makedata takes three inputs: number of shoes, mean, and sd)
# shoesinstore1 is a dataframe
shoesinstore1 <- makedata(nshoe1, meanprice1, pricesd1)

# moneyIspent collects R (in this case 1%) shoes of the store's inventory
# and figures out how much money I spent in each case.
moneyIspent <- storesamples(it,shoesinstore1,R)

## Summarize output
# summarya provides the mean, median, 1st and 3rd quartiles, and min and max money 
# I spent given the samples in moneyIspent.
summarya <- ddply(moneyIspent, .(index), summarize, Totalbucks = floor(sum(bucks)))

# Plot a histogram of the money I spent
(ggplot(summarya, aes(x=Totalbucks))
 + geom_histogram()
)

#Print a summary of the amount of money I could spend. 
summary(summarya$Totalbucks)

##############################################
# QUESTION 2
##############################################
# In year 2, I was given the same amount of money I spent in year 1 as my budget.  
# What percentage of the store's inventory can I buy in year 2, 
# given the amount of money I spent in year 1?  
# Hint:  I will take the sampled money I spent last year and figure out 
# how many shoes I can buy with that money.
##############################################

# Make another dataset for year 2 
shoesinstore2 <- makedata(nshoe2, meanprice2, pricesd2)
# Collect information on how many shoes I bought, and the corresponding
# Percentage of how many shoes I bought in the store.
numofshoepairs.masterdf <- how_many_shoes_in_store_I_bought(shoesinstore2,summarya,it)
# Calculate a percent by taking the number of shoes I bought and dividing it by the corresponding 
# Number of shoes in the store, and multiplying by 100.
numofshoepairs.masterdf$Percent<-(numofshoepairs.masterdf$Shoes/nrow(shoesinstore2))*100

# Plot a histogram of the percentage of shoes in the store I bought 
(ggplot(numofshoepairs.masterdf, aes(x=Percent))
 + geom_histogram(aes(y=..density..), fill="gray", color="black", binwidth = .1)
 + theme_bw()
 #+ geom_density(color="red")
 + geom_vline(x=mean(numofshoepairs.masterdf$Percent), color="blue")
)


# Print summary of the percentage of shoes I bought from the store
summary(numofshoepairs.masterdf$Percent)

# Print summary of the number of shoes I bought given my budget.
summary(numofshoepairs.masterdf$Shoes)
