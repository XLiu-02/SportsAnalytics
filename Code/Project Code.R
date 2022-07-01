library(robotstxt)
library(rvest)
library(stringr)
library(plyr)
library(tidyverse)
library(reshape2)
#############################
###### data scraping ########
#############################
## Original Website
base_url <- "https://www.basketball-reference.com"
player_url <- "https://www.basketball-reference.com//players/h/hardeja01.html"
paths_allowed(player_url)
paths_allowed(base_url)

######### basic stat #########
## file paths
pages <- read_html(file.path(player_url)) %>%
  html_nodes("#per_game th a") %>%
  html_attr("href") 
## Get full file paths for all game logs
fullpages <- read_html(file.path(player_url)) %>%
  html_nodes("#per_game th a") %>%
  html_attr("href") %>%
  file.path(base_url, .)

## create a placeholder for each regular season's game logs
regular<- list()
#function
for(i in 1:length(fullpages)){
  gamelog.temp <- read_html(fullpages[i]) %>%
    html_nodes("#pgl_basic") %>%
    html_table(header=T)
  
    gamelog <- gamelog.temp[[1]]
    
    gamelog <- subset(gamelog, Rk != "Rk")
    gamelog <- gamelog[,-c(2,6)]
    colnames(gamelog)[6] <- "Outcome"
  
    regular[[i]] <- gamelog
}
## in each element and stacks them on top of each other
gamelogs.regular <- rbind.fill(regular)
gamelogs.regular$Season <- "regular"

## get playoffs game logs
playoffs_url <- "https://www.basketball-reference.com/players/h/hardeja01/gamelog-playoffs"
paths_allowed(playoffs_url)

playoffs <- read_html(playoffs_url) %>%
  html_nodes("#pgl_basic_playoffs") %>%
  html_table(header=T)

## Pull all playoffs data
gamelogs.playoffs<- playoffs[[1]]

## clean up
gamelogs.playoffs<- subset(gamelogs.playoffs, Rk != "Rk" & Rk !="")
gamelogs.playoffs <- gamelogs.playoffs[,-c(2,4,6,8)]
colnames(gamelogs.playoffs)[c(2,5)] <- c("Date","Outcome")
gamelogs.playoffs$Season <- "playoffs"

## Combine two datasets
gamelogs.regular <- gamelogs.regular[,-3]
gamelogs.basic <- rbind(gamelogs.regular, gamelogs.playoffs)

## export basic file for shiny dashboard
# write.csv(Harden_stat,'/Users/xiaoyingziliu/Desktop/6341 Sports/gamelogs_basic.csv', row.names = FALSE)

########### advanced stat ##########
fullpages2 <- read_html(file.path(player_url)) %>%
  html_nodes("#per_game th a") %>%
  html_attr("href") %>%
  file.path(base_url, .) %>%
  str_replace("gamelog","gamelog-advanced")

regular2<- list()
#function
for(i in 1:length(fullpages2)){
  gamelog.temp <- read_html(fullpages2[i]) %>%
    html_nodes("#pgl_advanced") %>%
    html_table(header=T)
  
  gamelog <- gamelog.temp[[1]]
  
  gamelog <- subset(gamelog, Rk != "Rk")
  gamelog <- gamelog[,-c(2,6)]
  colnames(gamelog)[6] <- "Outcome"
  
  regular2[[i]] <- gamelog
}
## in each element and stacks them on top of each other
gamelogs.ad.regular <- rbind.fill(regular2)
gamelogs.ad.regular$Season <- "regular"

## get playoffs game logs
advanced <- read_html(advanced_url) %>%
  html_nodes("#pgl_advanced_playoffs") %>%
  html_table(header=T)

## Pull all playoffs data
gamelogs.ad.playoff<- advanced[[1]]

## clean up
gamelogs.ad.playoff<- subset(gamelogs.ad.playoff, Rk != "Rk" & Rk !="")
gamelogs.ad.playoff <- gamelogs.ad.playoff[,-c(2,4,6,8)]
colnames(gamelogs.ad.playoff)[c(2,5)] <- c("Date","Outcome")
gamelogs.ad.playoff$Season <- "playoffs"

## Combine two datasets
gamelogs.ad.regular <- gamelogs.ad.regular[,-3]  # remove age
gamelogs.ad.regular <- gamelogs.ad.regular[,-21]   # remove BPM
gamelogs.advanced <- rbind(gamelogs.ad.regular, gamelogs.ad.playoff)

######## combine basic and advanced #######
gamelogs.all <-  inner_join(gamelogs.basic, gamelogs.advanced)


###############################
####### data wrangling ########
###############################
str(gamelogs.all)
gamelogs.all <- gamelogs.all[,c(1:27,29:40,28)]
# covert cols to numeric
gamelogs.all[,c(8:39)] <- sapply(gamelogs.all[,c(8:39)], as.character)
gamelogs.all[,c(8:39)] <- sapply(gamelogs.all[,c(8:39)], as.numeric)
# replace NA with 0
gamelogs.all[,c(8:39)][is.na(gamelogs.all[,c(8:39)])] <- 0

# change variable names： 3P,3PA,3P_percent,+.-
colnames(gamelogs.all)[c(11,12,13)] <- c("ThreeP","ThreePA","ThreeP_percent")
colnames(gamelogs.all)[27] <- c("Plus.Minus")

# Replace all % with _percent
colnames(gamelogs.all) <- colnames(gamelogs.all) %>%
  str_replace("[%]", "_percent")

# convert the character values in the MP column to actual minutes played
game_min <- as.numeric(sub(":.*", "", gamelogs.all$MP))
game_sec <- as.numeric(sub(".*:", "", gamelogs.all$MP))
gamelogs.all$MP <- game_min+game_sec/60
gamelogs.all$MP[is.na(gamelogs.all$MP)] <- 0
gamelogs.all <- gamelogs.all %>% 
  mutate_at(vars(MP), list(~ round(., 2)))

# GS col:replace missing values with None
gamelogs.all$GS <- as.factor(gamelogs.all$GS)
levels(gamelogs.all$GS) <- list("0"="0","1"="1")
level_GS <- levels(gamelogs.all$GS)
level_GS[length(level_GS) + 1] <- "None"
gamelogs.all$GS <- factor(gamelogs.all$GS, levels = level_GS)
gamelogs.all$GS[is.na(gamelogs.all$GS)] <- 'None'


##### feature engineering #####

# create WL variable as outcome variable
gamelogs.all$WL <- ifelse(grepl("W", gamelogs.all$Outcome), "W", "L")
gamelogs.all <- gamelogs.all[,-5]

# add Year variable
gamelogs.all$Year <- str_sub(gamelogs.all$Date,1,4)

# manually add team points, All_star
gamelogs.all <- read.csv("/Users/xiaoyingziliu/Desktop/6341 Sports/gamelogs_all.csv")
# convert Year, All_star to factor col
gamelogs.all$All_star <- as.factor(gamelogs.all$All_star)
gamelogs.all$Year<- as.factor(gamelogs.all$Year)



##########################
######## Modeling ########
##########################
library(recipes)
library(caret)
library(ranger)
library(vip)
library(rsample)

dim(gamelogs.all)
use.index <- c(3:7,9,10,12,13,15:43)

######## Splitting #########
##Orginal response distribution
table(gamelogs.all$WL) %>% prop.table()

##Stratified sampling with the rsample package
set.seed(123)
split_strat  <- initial_split(gamelogs.all, prop = 0.8, 
                              strata = "WL")
train_strat  <- training(split_strat)
test_strat   <- testing(split_strat)
##Consistent response ratio between train & test
table(train_strat$WL) %>% prop.table()
table(test_strat$WL) %>% prop.table()

###### recipes ######
# Step 1 & 2: recipe and ingredients
nba.recipe <- recipe(WL ~ ., data = train_strat[,use.index]) %>%
  step_YeoJohnson(all_numeric()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Step 3: provide data
prepare.nba <- prep(nba.recipe, training = train_strat[,use.index])

# Step 4: bake
bake.nba.train <- bake(prepare.nba, new_data=train_strat[,use.index])
bake.nba.test <- bake(prepare.nba, new_data=test_strat[,use.index])

y_train <- bake.nba.train$WL
x_train <- bake.nba.train %>% select(-WL)
x_test <- bake.nba.test %>% select(-WL)


######## Base model - logistic regression #######
base_nba <- train(
  x= x_train,
  y=y_train,
  method = "glm",
  family = 'binomial',
  trControl = trainControl(method = "cv", number = 10),
  metric = 'Accuracy'
)
# Training data Accuracy
train.predict <- predict(base_nba,x_train)
confusionMatrix(data=train.predict, reference = train_strat$WL)

# predict on test data
test.predict <- predict(base_nba, x_test)
# Confusion Matrix
confusionMatrix(data=test.predict, reference = test_strat$WL)

####### elastic net ######
enGrid <- expand.grid(alpha = seq(0, 1, 0.1), lambda = 10^seq(2, -2, length=25))

en_fit <- train(
  x= x_train,
  y= y_train,
  method = "glmnet",
  tuneGrid = enGrid,
  family = 'binomial',
  trControl = trainControl(method = "cv", number = 10),
  metric = 'Accuracy',
  set.seed=123
)

en_fit$bestTune
# training set
train.predict.en <- predict(en_fit,x_train)
confusionMatrix(data=train.predict.en, reference = train_strat$WL)

# predict on test data
test.predict.en <- predict(en_fit, x_test)
confusionMatrix(data=test.predict.en, reference = test_strat$WL)

###### Random forest ######
## Hyperparameter Tuning
# define hypergrid
hyper_grid <- expand.grid(
  mtry = floor(length(use.index) * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),
  sample.fraction = c(.5, .75, 1) 
)

for(i in seq_len(nrow(hyper_grid))) {
  ## with hyperparameter combination
  fit_rf <- ranger(
    x= x_train,
    y= y_train,
    num.trees       = 10*length(use.index),
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    verbose         = FALSE,
    seed            = 123
  )
}
fit_rf
# final model 
ranger.final <- ranger( x= x_train,
                        y= y_train,
                        num.trees = 380,
                        mtry = 15,
                        min.node.size   = 10,
                        replace  = FALSE,
                        seed = 123,
                        importance = 'impurity'
)

# traning set
ranger.final$confusion.matrix

# predict on the test data
test.predict.rf <- predict(ranger.final, x_test)
# Confusion matrix
confusionMatrix(data=test.predict.rf$predictions, reference = test_strat$WL)

####### XGBoost #######
library(xgboost)
x_train.matrix <- model.matrix(WL~., data=bake.nba.train)
x_test.matrix <- model.matrix(WL~., data=bake.nba.test)
Y <-  ifelse(y_train == "W",1,0)

hyper_grid2 <- expand.grid(
  eta=c(0.3, 0.1, 0.05),
  max_depth = c(1, 3, 5),
  min_child_weight = c(5, 10, 15),
  subsample = c(0.5, 0.75, 1),
  colsample_bytree = c(0.5,0.75,1),
  colsample_bynode = 1,
  gamma = c(0, 1, 10),
  lambda = c(0, 0.1, 1),
  alpha = c(0, 0.1, 1)
)


for(i in seq_len(nrow(hyper_grid2))) {
  set.seed(123)
  ## with hyperparameter combination
  fit_xbg <- xgb.cv(
    data = x_train.matrix,
    label=Y,
    nrounds = 100,
    nfold = 10,
    early_stopping_rounds = 20, 
    objective = "binary:logistic",
    verbose = 0,
    params = list( 
      eta = hyper_grid2$eta[i], 
      max_depth = hyper_grid2$max_depth[i],
      min_child_weight = hyper_grid2$min_child_weight[i],
      subsample = hyper_grid2$subsample[i],
      colsample_bytree = hyper_grid2$colsample_bytree[i],
      colsample_bynode = 1,
      gamma = hyper_grid2$gamma[i], 
      lambda = hyper_grid2$lambda[i], 
      alpha = hyper_grid2$alpha[i]
    ) 
  )
}

# parameters
params <- list(
  eta = 0.05,
  max_depth = 5, 
  min_child_weight = 15,
  subsample = 1, 
  colsample_bytree = 1,
  colsample_bynode = 1,
  gamma = 10,
  lambda = 1,
  alpha = 1
)

# train final model
xgb.final <- xgboost(
  params = params,
  data = x_train.matrix,
  label = Y,
  nrounds = 100,
  objective = "binary:logistic",
  verbose = 0
)

# training
xgb.train <- predict(xgb.final,x_train.matrix)
xgb.train <- ifelse(xgb.train > 0.5,"W","L")
xgb.train <- as.factor(xgb.train)
confusionMatrix(xgb.train, reference = train_strat$WL)

# predict on the test data
xgb.pred <- predict(xgb.final, x_test.matrix)
xgb.pred <- ifelse(xgb.pred > 0.5,"W","L")
xgb.pred <- as.factor(xgb.pred)
confusionMatrix(xgb.pred, reference = test_strat$WL)

# variable importance 
vip(ranger.final, num_features = 20, geom='point', aesthetics = list(color = "blue"))
vip(base_nba, num_features = 20, geom='point', aesthetics = list(color = "blue"))
vip(en_fit, num_features = 20, geom='point', aesthetics = list(color = "blue"))
vip(xgb.final, num_features = 20, geom='point', aesthetics = list(color = "blue"))


