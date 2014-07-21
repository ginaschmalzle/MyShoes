require(plyr)
require(ggplot2)

setwd('/Users/ginaschmalzle/Documents/Craig_R')

# Assign sampling rate to 0.15
R <- 0.16
# Assign the total number of iterations for sampled datasets
it <- 100
# Assign daily budget 
bd = 1000
# Assign number of shoes
nshoe1 <- 1500
dailymean1 <- 3
dailysd1 <- 1

nshoe2 <- 4000
dailymean2 <- 3
dailysd2 <- 3
#############################################################################
# Create a dummy dataframe
# Create a 'bucks' column
#CREATE DATA TASK- want a dataframe with two parts;
# each part has a number of shoes N, mean bucks D, and Stdev. of bucks.
# we want a function to set these parameters in each part.
#

makedata <- function (numberofshoes, dm, sdv){
 # Assign number of shoes 
 df <- data.frame(shoes = seq(1:numberofshoes))
 # Assign random # of bucks for each shoe
 df$bucks <-  rnorm(n = numberofshoes, mean = dm, sd = sdv)
 return (df)
}

# Functions for sampling and creating tables
# Make a sample dataframe
sampleme <- function(dataframe, samplerate){
  # Generate a subsample of shoe numbers, then take the associated
  # bucks and stick them into sdf.
  sdf <- data.frame(shoes=sample(dataframe$shoes, size = (samplerate*nrow(dataframe))))
  sdf <- merge(sdf,dataframe,all.x=TRUE)
  return (sdf)
}

storesamples<-function(iteration, df, sr){
  for (iter in 1:iteration){
    sdf <- sampleme(dataframe = df, samplerate=sr)
    sdf$index <- iter
    ifelse(iter == 1, allsdf <-sdf, allsdf <-rbind(allsdf,sdf))
  } 
  return(allsdf)
}


shoesIcanbuy <- function(dataframe,mypurse){
  numofshoepairs <- 0
  while (mypurse > 0)  {
    Shoe.pair<-dataframe[sample(nrow(dataframe),1),]
    mypurse<-mypurse-Shoe.pair$bucks
    numofshoepairs <- numofshoepairs + 1 
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
df1 <- makedata(nshoe1, dailymean1, dailysd1)
df2 <- makedata(nshoe2, dailymean2, dailysd2)
a <- storesamples(it,df1,R)

## Summarize output
summarya <- ddply(a, .(index), summarize, Totalbucks = floor(sum(bucks)))
#(ggplot(summarya, aes(x=Totalbucks))
# + geom_histogram()
#)
summary(summarya$Totalbucks)
###Summarize the outcome as a rate
summarya$Available.bucks <- bd-summarya$Totalbucks
numofshoepairs.masterdf <- how_many_shoes_in_store_I_bought(df2,summarya)
numofshoepairs.masterdf$Rate<-numofshoepairs.masterdf$Shoes/nrow(df2)
(ggplot(numofshoepairs.masterdf, aes(x=Rate))
 + geom_histogram(aes(y=..density..), fill="gray", color="black")
 + theme_bw()
 + geom_density(color="red")
 + geom_vline(x=mean(numofshoepairs.masterdf$Rate), color="blue")
)
summary(numofshoepairs.masterdf$Rate)
summary(numofshoepairs.masterdf$Shoes)
