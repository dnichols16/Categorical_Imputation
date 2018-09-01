
set.seed(125)
#**************************************
cats <- rbinom(10000,1,.68)
cats_all <- cats #save the original intact data - the counterfactuals
missings <- round(runif(6000, 1, 10000))
cats[missings] <- NA
sum(is.na(cats)) # number of NA's in cats
#cats now represents original data that
#we would be presented with with missing values
(tab1 <- xtabs(~cats))
#the proportion of 1's to 0's in the data that we know
(prob <- tab1[[2]]/(tab1[[1]] + tab1[[2]]))
#impute the missing values
cats <- if_else(is.na(cats), rbinom(1,1,prob), cats)
toll <- as.numeric(0) #initialize a vector
#compare the conterfactual to the imputed data
for(i in seq_along(cats_all)) {
  toll[[i]] <- if_else(cats[[i]] == cats_all[[i]], 1, 0) 
}
# number of misses after imputation
sum(toll == 0)
acc  <- 1 - (sum(toll == 0)/length(cats_all))

#***************************************************
set.seed(123)
options(error=recover)
n = 10000
prop = 0.68
missing = 6000
accuracy <- function(n, prop, missing) {
  set.seed(125)
  cats <- rbinom(n,1L,prop)
  cats_all <- cats
  missings <- round(runif(missing, 1, n))
  cats[missings] <- NA
  tab1 <- xtabs(~cats)
  prob <- tab1[[2]]/(tab1[[1]] + tab1[[2]])
  cats <- if_else(is.na(cats), rbinom(1,1,prob), cats)
  toll <- as.numeric(0)
  for(i in seq_along(cats_all)) {
    toll[[i]] <- if_else(cats[[i]] == cats_all[[i]], 1, 0) 
  }
  acc  <- 1 - (sum(toll == 0)/length(cats_all))
}

#**************************************************
options(scipen = 99)
(accuracy(10000, .5, 5000))
(accuracy(10000, .6, 5000))
(accuracy(10000, .7, 5000))
(accuracy(10000, .8, 5000))
(accuracy(10000, .9, 5000))

n = 10000
prop = 0.68
missing = 6000
args <- list("n" = n, "prop" = prop, "missing" = missing)
args
map(args, accuracy)
#**********************
n <- rep(10000L, 100)
prop <- rep(0.68, 100)
missing <- round(seq(500, 10000, length.out = 100))
#args <- list(n, prop, missing)
df <- data_frame(n = n, prop = prop, missing = missing)
df <- df %>% mutate(args = as.numeric(0))

acc <- as.numeric(0)
for(i in 1:100) {
  acc[i] <- accuracy(df[i,1], df[i,2], df[i,3])
}
  
df[1,2]  
df[2,3]  
  

