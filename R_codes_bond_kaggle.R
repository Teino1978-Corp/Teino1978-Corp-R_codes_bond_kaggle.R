############################################################################################
####################### READING THE RELEVANT FILES #########################################

train.data <- read.csv("train_sample.csv")

## Set seed for reproducing the random numbers
set.seed(10001)

## This is to confirm the selection of trade_price_last{1-10}

for(i in seq(from=18, to=61, by=5))
{
  print(names(train.data[i]))
}

## This is to confirm the selection of curve_based_last_price {1-10} 

for(i in seq(from = 16, to= 61 , by = 5))
{
  print(names(train.data[i]))
}

##################### Dealing with NA's in the past trade prices and past curve prices #####

# We have replaced the NA's with the next day's price minus a random normal variable that 
# is generated with mean = difference between all the non-NA prices between the 2 days
# and similarly computed the standard deviation of the random variable that is then 
# substracted.(This follows from the efficient market hypothesis(EMH))

# Approximating the Last trade prices {1-10} 

for(i in seq(from=18, to=61, by=5))
{
 train.data[,i][is.na(train.data[,i])] <- as.integer(train.data[,i-5] - rnorm(n=1,
                            mean = mean(train.data[,i-5] - train.data[,i], na.rm = T),
                              sd = sqrt(var(train.data[,i-5] - train.data[,i], na.rm = T))))
   
  }
  

# Similarly approximating the Last curve based prices

for(i in seq(from = 16, to= 61 , by = 5))
{
  train.data[,i][is.na(train.data[,i])] <- as.integer(train.data[,i-5] - rnorm(n=1,
                            mean = mean(train.data[,i-5] - train.data[,i], na.rm = T),
                              sd = sqrt(var(train.data[,i-5] - train.data[,i], na.rm = T))))
}


# Creating new variable as an exponential weighted average of the differences b/w the 
# curve based and the trade price

idx <- seq(from=13, to=61, by=5)
names(train.data[idx])     ## Checking if I have the right variables
idx.2 <- seq(from = 16, to= 61 , by = 5)
names(train.data[idx.2])   ## Checking if I have the right variables

train.data$exp.diff <- rep(0, length(train.data[,1]))

for(i in 1:length(idx)) ## This loop calculates the exponentially weighted sum of the differences 
{                        # b/w trade price and the curve based price and stores it in "exp.diff"
  train.data$exp.diff <- train.data$exp.diff + (0.95^i) * (train.data[, idx[i]] - train.data[, idx.2[i]])  
}