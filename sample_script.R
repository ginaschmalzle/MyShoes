require(plyr)
require(ggplot2)

setwd('/Users/ginaschmalzle/Documents/Craig_R')

# This is the number of shoes I theoreticallyy bought 
# last year divided by the total number of shoes in 
# the store (multiply by 100 and you get the 
# percentage of shoes I bought compared
# to the total number in the store).
R <- 0.01
# Assign the total number of iterations for sampled datasets
it <- 100
# Assign year 2 budget 
bd = 1000
# Assign number of shoes
nshoe1 <- 1000               # Number of shoes in the store in year 1
meanprice1 <- 100         # Mean price of shoes in year 1
pricesd1 <- 50            # Std of price in shoes in year 1

nshoe2 <- 1400               # Number of shoes in the store in year 2
meanprice2 <- 120            # Mean price of shoes in year 2          
pricesd2 <- 40            # Std of price in shoes in year 2
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
  sdf <- data.frame(shoes=sample(dataframe$shoes, size = (samplerate*nrow(dataframe))))
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
    Shoe.pair<-dataframe[sample(nrow(dataframe),1),]
    if (mypurse >= Shoe.pair$bucks){
      mypurse<-mypurse-Shoe.pair$bucks
      numofshoepairs <- numofshoepairs + 1 
    }  
  }
  return(numofshoepairs)
}

how_many_shoes_in_store_I_bought <- function(dataframe, summarya){
  numofshoepairs <- array()
  for (i in 1:nrow(summarya)) {
    mypurse<-summarya[i,3]
    for (j in 1:100){
      numofshoepairs[j] <- shoesIcanbuy(dataframe, mypurse)
    }
    numofshoepairs.df<-data.frame(Shoes=numofshoepairs)
    ifelse(i==1, numofshoepairs.masterdf<-numofshoepairs.df, 
           numofshoepairs.masterdf<-rbind(numofshoepairs.masterdf,numofshoepairs.df))
  }
  return(numofshoepairs.masterdf)
}
############  END PART !
#########START PART 2 - Given a total day budget of bd bucks, 
#                       and given the number of bucks in summarya, what
#                       sampling fraction can be expected from df2 data?
#steps 
#0 find your budget in bucks= BD-summarya$bucks in each interation
# sum the number of bucks in df2
#1. start with row 1 in summarya
#2. select a pair of shoes from df2
#3. select bucks in step 2 and subtract from budget
#4. add 1 to the number of shoes already selected and save
#5. evaluate new budget against 0; if <= 0 stop else goto step 2
#Step 0:


# make data takes three inputs: number of shoes, mean, and sd)
df1 <- makedata(nshoe1, meanprice1, pricesd1)
df2 <- makedata(nshoe2, meanprice2, pricesd2)
a <- storesamples(it,df1,R)

## Summarize output
summarya <- ddply(a, .(index), summarize, Totalbucks = floor(sum(bucks)))
(ggplot(summarya, aes(x=Totalbucks))
 + geom_histogram()
)
summary(summarya$Totalbucks)
###Summarize the outcome as a rate
#summarya$Available.bucks <- bd-summarya$Totalbucks
#numofshoepairs.masterdf <- how_many_shoes_in_store_I_bought(df2,summarya)
#numofshoepairs.masterdf$Rate<-numofshoepairs.masterdf$Shoes/nrow(df2)
#(ggplot(numofshoepairs.masterdf, aes(x=Rate))
# + geom_histogram(aes(y=..density..), fill="gray", color="black")
# + theme_bw()
# + geom_density(color="red")
# + geom_vline(x=mean(numofshoepairs.masterdf$Rate), color="blue")
#)
#summary(numofshoepairs.masterdf$Rate)
#summary(numofshoepairs.masterdf$Shoes)
