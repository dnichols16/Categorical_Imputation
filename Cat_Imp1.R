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
#************************************************
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
  return(acc)
}
#***************************************
n <- rep(10000, 100)
prop <- rep(0.68, 100)
missing <- as.integer(round(seq(500, 10000, length.out = 100)))
df <- data.frame(n = n, prop = prop, missing = missing)
#***************************************
df <- df %>% mutate(acc = accuracy(n, prop, missing))




















