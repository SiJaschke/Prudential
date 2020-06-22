# title: "Capstone Project Prudential"
# author: "Sarah Jewel"
# date: "22.6.2020"


if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")


library(data.table)
library(tidyverse)
library(caret)
library(glmnet)
library(matrixStats)
library(ggplot2)
library(ranger)


# process in parallel for libraries which support this
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
library(doParallel) 
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

# load local train data local version
# dl <- "readtrain.csv.zip"
# trainOrig <- fread(text=readLines(unzip(dl, "train.csv")))

#  upload data from github
 zipurl <- "https://github.com/SiJaschke/Prudential/blob/master/train.csv.zip?raw=true"
 destination_file <- "readtrain.csv.zip"
 download.file(zipurl, destination_file, mode="wb")
 unzip("readtrain.csv.zip")
 trainOrig = fread(file="train.csv")



dim(trainOrig)

head(trainOrig)

# data are in a data table
class(trainOrig)

train <- trainOrig

# set the seed to get always the same results
set.seed(1, sample.kind="Rounding")

##########################
### Descriptive Analysis #
##########################

# dimension of data set
dim(trainOrig)
#  59381   128

# data format of columns
str(trainOrig)
# 59381 obs. of  128 variables

# Analysis of fields in data

# looking for a value which can replace NAs
# some Medical_History fields have values between 0 and 204
# these will be standardized to values between 0 and 1 as other columns already are
range(unique(train$Medical_History_10)[-1])
# the NAs will be replaced by a small negative value : -1/240
# and the same value is used for all columns in which NAs have to be replaced 
repNA <- -1/240
repNA

### Id
# Id	unique identifier in first column 
n_distinct(train$Id)
# Id excluded, as not needed and used further in the analysis


### Product_info

# Product_Info_1-7	A set of normalized variables relating to the product applied for
cols <- paste0("Product_Info_",1:7)
sapply(cols,function(i) {return(class(train[[i]]))})

# Product_Info_2 is a character
unique(train$Product_Info_2)
# plot of distribution
train %>% 
  group_by(Product_Info_2) %>%
  summarize(n=n())%>%
  ggplot(aes(Product_Info_2,n)) +
  geom_bar(stat="identity") 
# check if NAs
sum(is.na(train$Product_Info_2))
# no NA
# convert column to factors
cols <- "Product_Info_2"
train[, (cols):= lapply(.SD, as.factor), .SDcols=cols]

# Product_Info_4 has continuous values 
train %>% 
  ggplot(aes(sample=Product_Info_4)) +
  stat_qq(distribution=qunif, size=1) +
  labs(title="Product_Info_4", x="percentage of observations", y="value")
# most of the rows have one of these 4 Product_Info_4 values
train %>% 
  group_by(Product_Info_4) %>%
  summarize(n=n())%>%
  filter(n>5000) 
# check if NAs
sum(is.na(train$Product_Info_4))
# no NA

# remaining Product_Info columns are integer values
cols <- paste0("Product_Info_",c(1,3,5:7))
sapply(cols,function(i) unique(train[[i]]))
# convert columns to factors
train[, (cols):= lapply(.SD, as.factor), .SDcols=cols]
# plot distribution of these 5 columns
cols <- c("Id",cols)
temp <- as.data.frame(melt(train[,..cols], id=1))
temp %>% group_by(variable, value) %>%
  summarize(n=n()) %>%
  ggplot(aes(value,n)) +
  geom_bar(stat="identity") +
  facet_grid(.~variable, scales="free")
# most of the rows have the same value in these fields

# check if NAs
sapply(cols,function(i) sum(is.na(train[[i]])))
# no NAs


# Ins_Age	Normalized age of applicant
train %>% ggplot(aes(Ins_Age)) +
  geom_histogram(binwidth=0.01) 

# Ht	Normalized height of applicant
train %>% ggplot(aes(Ht)) +
  geom_histogram(binwidth=0.01) 

# Wt	Normalized weight of applicant
train %>% ggplot(aes(Wt)) +
  geom_histogram(binwidth=0.01) 

# BMI Normalized BMI of applicant
train %>% ggplot(aes(BMI)) +
  geom_histogram(binwidth=0.005) 

# check if NAs
sum(is.na(train$Ins_Age))
sum(is.na(train$Ht))
sum(is.na(train$Wt))
sum(is.na(train$BMI))
# no NAs

# Employment_Info_1-6	A set of normalized variables relating to the employment history of the applicant.
cols <- paste0("Employment_Info_",1:6)
sapply(cols,function(i) {return(class(train[[i]]))})
sapply(cols,function(i) {return(unique(train[[i]]))})
# check if NAs
sapply(cols,function(i) sum(is.na(train[[i]])))
# lots of NAs
train[,(cols) := lapply(.SD, function(v) ifelse(is.na(v),repNA,v)), .SDcols=cols]

# plot distribution of the 3 categorical variables
cols <- c("Id",paste0("Employment_Info_",c(2,3,5)))
temp <- as.data.frame(melt(train[,..cols], id=1))
temp %>% group_by(variable, value) %>%
  summarize(n=n()) %>%
  ggplot(aes(value,n)) +
  geom_bar(stat="identity") +
  facet_grid(.~variable, scales="free")
# convert columns to factors (exclude column Id)
cols <- cols[-1]
train[, (cols):= lapply(.SD, as.factor), .SDcols=cols]

# plot distribution of the 3 continuous variables
par(mfrow=c(3,1))
train %>% 
  ggplot(aes(sample=Employment_Info_1)) +
  stat_qq(distribution=qunif, size=1) +
  labs(title="Employment_Info_1", x="percentage of observations", y="value")

train %>% 
  ggplot(aes(sample=Employment_Info_4)) +
  stat_qq(distribution=qunif, size=1) +
  labs(title="Employment_Info_4", x="percentage of observations", y="value")

train %>% 
  ggplot(aes(sample=Employment_Info_6)) +
  stat_qq(distribution=qunif, size=1) +
  labs(title="Employment_Info_6", x="percentage of observations", y="value")
par(mfrow=c(1,1))

# InsuredInfo_1-7	A set of integer variables providing information about the applicant.
cols <- paste0("InsuredInfo_",1:7)
sapply(cols,function(i) {return(class(train[[i]]))})
sapply(cols,function(i) {return(unique(train[[i]]))})
# all categorical variables
# check if NAs
sapply(cols,function(i) sum(is.na(train[[i]])))
# no NAs
# convert columns to factors
train[, (cols):= lapply(.SD, as.factor), .SDcols=cols]

# plot distribution of the categorical variables
cols <- c("Id", cols)
temp <- as.data.frame(melt(train[,..cols], id=1))
temp %>% group_by(variable, value) %>%
  summarize(n=n()) %>%
  ggplot(aes(value,n)) +
  geom_bar(stat="identity") +
  facet_grid(.~variable, scales="free")

# Insurance_History_1-9	A set of variables relating to the insurance history of the applicant.
cols <- paste0("Insurance_History_",1:9)
sapply(cols,function(i) {return(class(train[[i]]))})
sapply(cols,function(i) {return(unique(train[[i]]))})

# plot of the distribution of the one continuous variable
train %>% 
  ggplot(aes(sample=Insurance_History_5)) +
  stat_qq(distribution=qunif, size=1) +
  scale_y_log10()+ 
  labs(title="Insurance_History_5", x="percentage of observations", y="value")

# check if NAs
sapply(cols,function(i) sum(is.na(train[[i]])))
# only Insurance_History_5 has lots of NAs
# replace NAs by a small negative value
train[,Insurance_History_5 := ifelse(is.na(train$Insurance_History_5),repNA,Insurance_History_5)]
range(train$Insurance_History_5)

# plot distribution of the categorical variables
# Insurance_History_6 does not exist
cols <- c("Id", cols[c(-5,-6)])
temp <- as.data.frame(melt(train[,..cols], id=1))
temp %>% group_by(variable, value) %>%
  summarize(n=n()) %>%
  ggplot(aes(value,n)) +
  geom_bar(stat="identity") +
  facet_grid(.~variable, scales="free") +
  theme(strip.text.x =element_text(size=6))
 
# convert columns to factors
cols <- cols[-1] # exclude id
train[, (cols):= lapply(.SD, as.factor), .SDcols=cols]

#Family_Hist_1-5	A set of normalized variables relating to the family history of the applicant.
cols <- paste0("Family_Hist_",1:5)
sapply(cols,function(i) {return(class(train[[i]]))})
sapply(cols,function(i) {return(unique(train[[i]]))})

# only the first is a categorical variable
# plot of the categorical variable
train %>% 
  group_by(Family_Hist_1) %>%
  summarize(n=n())%>%
  ggplot(aes(Family_Hist_1,n)) +
  geom_bar(stat="identity", size=0.1) 
# convert column to factors
col <- cols[1] 
train[, (col):= lapply(.SD, as.factor), .SDcols=col]
# check if NAs
sapply(cols,function(i) sum(is.na(train[[i]])))
# lots of NAs in Family_History_2-5

# plot of the distribution of the 4 continuous variables
cols <- c("Id", cols[-1])
temp <- as.data.frame(melt(train[,..cols], id=1))
temp %>% group_by(variable, value) %>%
  summarize(n=n()) %>%
  ggplot(aes(value,n)) +
  geom_point(size=1) +
  scale_y_log10() +
  facet_grid(.~variable)
cols <- cols[-1]
# replace NAs by a small negative value
train[,(cols) := lapply(.SD, function(v) ifelse(is.na(v),repNA,v)), .SDcols=cols]

# Medical_History_1-41	A set of integer variables relating to the medical history of the applicant.
cols <- paste0("Medical_History_",1:41)
sapply(cols,function(i) {return(class(train[[i]]))})
sapply(cols,function(i) {return(unique(train[[i]]))})
# all variables are categorical, except Medical_History_1,10,15,24,32
cols <- c("Id", cols)
temp <- as.data.frame(melt(train[,..cols], id=1))
# plots of variables (omit in report as too many)
par(mfcol=c(11,4))
temp %>% group_by(variable, value) %>%
  summarize(n=n()) %>%
  ggplot(aes(value,n)) +
  geom_bar(stat="identity") +
  facet_grid(.~variable, scales="free")
# convert all categorical columns to factors
cols <- cols[c(-1, -2, -11, -16, -25, -33)]
train[, (cols):= lapply(.SD, as.factor), .SDcols=cols]
# the values in some Medical_History fields are between 0 and 240, these will be normalized
range(train$Medical_History_10)
# NAs are replaced by -1/240 and the values are standardized by dividing by 240 in following columns
cols <- paste0("Medical_History_",c(1,10,15,24,32))
# replace NAs by a small negative value
train[,(cols) := lapply(.SD, function(v) ifelse(is.na(v),repNA,v/240)), .SDcols=cols]

# Medical_Keyword_1-48	A set of dummy variables relating to the presence of/absence of a medical keyword being associated with the application.
cols <- paste0("Medical_Keyword_",1:48)
train[, lapply(.SD, range), .SDcols=cols]
# check if NAs
sapply(cols,function(i) sum(is.na(train[[i]])))
# no NAs
# convert columns to factors
train[, (cols):= lapply(.SD, as.factor), .SDcols=cols]

# Response The target variable, an ordinal variable relating to the final decision associated with an application
# integer variable
class(train$Response)
# can have the following values
unique(train$Response)
# Distribution of the values
train %>% 
  group_by(Response) %>%
  summarize(n=n())%>%
  ggplot(aes(Response,n)) +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(1,8,1))


dim(train)
## rename all factor variables (to distinguish variables that end with a digit in the design matrix):
idx <- sapply(train, class) == "factor"
setnames(train, old=names(train)[idx], new=paste0(names(train)[idx],"_"))
names(train)
################################################
#  data transformed
################################################

# metric used in kaggle competition to measure the quality of the model
# Kappa roughly measures how many predictions are "correct" over and above random guessing.
# code is taken from the 'Metrics' package with a minor modification
ScoreQuadraticWeightedKappa = function (rater.a, rater.b, min.rating = min(c(rater.a, rater.b)), 
                                        max.rating = max(c(rater.a, rater.b))){ 
  rater.a <- factor(rater.a, levels = min.rating:max.rating)
  rater.b <- factor(rater.b, levels = min.rating:max.rating)
  confusion.mat <- table(data.frame(rater.a, rater.b))
  confusion.mat <- confusion.mat/sum(confusion.mat)       # = empirical frequency of each pair
  histogram.a <- table(rater.a)/length(table(rater.a))
  histogram.b <- table(rater.b)/length(table(rater.b))
  expected.mat <- histogram.a %*% t(histogram.b)
  ## frequency of each pair if raters A and B guess independently:
  expected.mat <- expected.mat/sum(expected.mat)
  labels <- as.numeric(as.vector(names(histogram.a)))
  weights <- outer(labels, labels, FUN = function(x, y) (x - y)^2)
  1 - sum(weights * confusion.mat)/sum(weights * expected.mat)
}

## function that transforms non-integer predictions to the possible responses 1-8:
SQWK = function(pred, y){
  x = round(pred,0)
  x = pmax(x,1)
  x = pmin(x,8)
  ScoreQuadraticWeightedKappa(as.integer(x), y, 1L, 8L)
}

## All top-ranked Kaggle submissions used an optimization step that searches
## for the best "cut points" when translating a numeric (non-discretized)
## prediction vector to the grid {1, 2, ..., 8}.

## The following function can be used to compute the kappa metric with alternative
## cut points:
SQWK2 = function(pred, y, cuts){
  x = findInterval(pred, cuts) +1L
  ScoreQuadraticWeightedKappa(x, y, 1L, 8L)
}

## The following function performs the search for optimal cut points:
optimalCuts = function(pred, y){
  deltas0 = c(1.5,1,1,1,1,1,1)
  fn = function(deltas) SQWK2(pred, y, cumsum(deltas))
  res = optim(deltas0, fn, method="L-BFGS-B", lower=0.1, upper=2,
              control=list(fnscale= -1, factr=1e5))
  cumsum(res$par)
}



# exclude the first column with the row number
train <- train[,2:128]
dim(train)

# create model matrix (needed for GLMs , Random forest)
x <- model.matrix(Response~., train)[,-1] # exclude column intercept
dim(x)

# separate a test set
set.seed(1, sample.kind="Rounding")
testIdx <- sample.int(nrow(train),6000,replace=FALSE)
test <- train[testIdx,]
train <- train[-testIdx,]
dim(train)
dim(test)
# separate test set for model matrix
xtest <- x[testIdx,]
x     <- x[-testIdx,]


###################################
# Linear model                    #
###################################
# start with simple linear model
# assumption distribution of error is normal

y <- as.factor(train$Response)
fit <- lm(Response ~ ., data=data.table(Response=train$Response,x))
summary(fit)
pred <- predict(fit, as.data.table(x))

# quadratic weighted kappa based on the direct result of the model
resSQWK <- SQWK(pred, train$Response)
resSQWK 
# plot result
plot(pred, train$Response)
# optimize cuts
cuts <- optimalCuts(pred, y)
# quadratic weighted kappa based on the optimal cuts
resSQWK2 <- SQWK2(pred, y, cuts)
resSQWK2
#  much more better result

#summary(fit)$r.sq # R^2 

plot(pred, train$Response)

# prediction for test data
pred <- predict(fit, as.data.table(xtest))
resSQWK2oos <- SQWK2(pred, test$Response, cuts)
resSQWK2oos

# Create a table to store the results of all models 
results <- tibble(model = "Simple linear model", SQWK = resSQWK, SQWK2 = resSQWK2, SQWK2oos=resSQWK2oos)
results %>% knitr::kable()

# select relevant variables and run model again
fit.a <- anova(fit)
fit.a
colNames <- rownames(fit.a)[fit.a[,5] < 0.05]
colNames
l <- length(colNames)
# number of columns selected
l-1
dim(train)
# run linear model again
form <- as.formula(paste("Response ~ ", paste(colNames[-l], collapse=" + ")))
form
xred <- model.matrix(form, data=data.table(Response=train$Response,x))[,-1]
fit <- lm(Response ~ ., data=data.table(Response=train$Response, xred))
summary(fit)
anova(fit)
pred <- predict(fit, as.data.table(xred))

resSQWK <- SQWK(pred, train$Response) 
resSQWK 
# optimize cuts
cuts <- optimalCuts(pred, y)
cuts

resSQWK2 <- SQWK2(pred, y, cuts)
resSQWK2
# a bit better model

# prediction for test data
pred <- predict(fit, as.data.table(xtest))
resSQWK2oos <- SQWK2(pred, test$Response, cuts)

# add result to table of results
results <- bind_rows(results,
                          tibble(model="Linear model with selected variables",
                                 SQWK = resSQWK, SQWK2 = resSQWK2, SQWK2oos=resSQWK2oos  ))

results %>% knitr::kable()

###################################
# Generalized linear model        #
###################################

# GLM
head(train)

# define y as response
y <- train$Response

# alpha = 1 (=lasso penalty) default
fit.glm <- glmnet(x,y) 
summary(fit.glm)
coef(fit.glm) # coefficients for several lambdas (columns)
plot(fit.glm,xvar="lambda")
# select lambda = exp(-5) just from plot
pred.glm <- predict(fit.glm, newx=x, s=exp(-5))
resSQWK <- SQWK(pred.glm, train$Response) 
resSQWK
cuts <- optimalCuts(pred.glm, y)
cuts 
resSQWK2 <- SQWK2(pred.glm, y, cuts) 
resSQWK2

# prediction for test data
pred.glm <- predict(fit.glm, xtest)
resSQWK2oos <- SQWK2(pred.glm, test$Response, cuts)
resSQWK2oos

# add result to table of results
results <- bind_rows(results,
                     tibble(model="GLM - Lasso (alpha=1)",
                            SQWK = resSQWK, SQWK2 = resSQWK2, SQWK2oos=resSQWK2oos))

results %>% knitr::kable()

# alpha = 0 (Ridge Regression, all variables are selected)
fit.glm.R <- glmnet(x,y, alpha=0) 
summary(fit.glm.R)
coef(fit.glm.R) # coefficients for several lambdas (columns)
plot(fit.glm.R,xvar="lambda")
# select lambda = exp(0) just from plot
pred.glm.R <- predict(fit.glm.R, newx=x, s=exp(0))
resSQWK <- SQWK(pred.glm.R, train$Response) 
resSQWK
cuts <- optimalCuts(pred.glm.R, y)
cuts 
resSQWK2 <- SQWK2(pred.glm.R, y, cuts) 
resSQWK2
# prediction for test data
pred.glm.R <- predict(fit.glm.R, xtest)
resSQWK2oos <- SQWK2(pred.glm.R, test$Response, cuts)
resSQWK2oos
# add result to table of results
results <- bind_rows(results,
                     tibble(model="GLM - Ridge Regression (alpha=0)",
                            SQWK = resSQWK, SQWK2 = resSQWK2, SQWK2oos=resSQWK2oos))

results %>% knitr::kable()

# cross validation to optimize alpha and lambda
# Restrict the maximum number of variables to 800.
set.seed(1, sample.kind="Rounding")
#set.seed(1)
k = 10 # 10 fold cv
# define the folds
folds <- sample(1:k, nrow(train), replace=TRUE)
# define a set of alphas from which the optimal has to be found
alphas <- c(0.4, 0.6, 0.8, 0.85, 0.9, 0.95, 1)

fit.cvglms <- lapply(alphas, function(alpha) {
  return(cv.glmnet(x, y, alpha=alpha, nfolds=k, foldid=folds, standardize=TRUE,
                         dfmax=800, trace.it=TRUE))
})
# plot all cv.glmnet objects
par(mfrow=c(3,3))
for ( i in seq(along=alphas) ) plot(fit.cvglms[[i]])
par(mfrow=c(1,1))

# build a data table for results of different models
l <- list(1,2,3,4,5,6,7)
ldt <- lapply(l, function(i) {
  cv <- fit.cvglms[[i]]
  data.table(MSE=cv$cvm, s=cv$cvsd, lambda=cv$lambda, nzero=cv$nzero, alpha=alphas[i])
})
dt <- rbindlist(ldt)

xyplot(sqrt(MSE) ~ log(lambda), groups=alpha, data=dt, type="l",xlab="log(lambda)",ylab="RMSE",
       auto.key=list(lines=TRUE,points=FALSE,corner=c(0.1,0.9)),
       scales=list(x=list(lim=c(-8,-2)),y=list(lim=c(1.94,1.97))))

# build a data table for results of different models
mldt <- lapply(l, function(i){
  cv <- fit.cvglms[[i]]
  data.table(MSE=cv$cvm[which(cv$lambda.min==cv$lambda)], alpha=alphas[i])
})
mdt <- rbindlist(mldt)
# plot RMSE vs alpha
mdt %>% ggplot(aes(alpha, sqrt(MSE))) +
   geom_point() +
   geom_path() +
  scale_x_continuous(breaks=seq(0.4,1,0.1)) +
    ylab("RMSE")

# look for alpha which minimizes the RMSE
RMSE <- rep(NA, length(alphas))
for (i in 1:length(alphas)) {
 RMSE[i] <-  fit.cvglms[[i]]$cvm[which(fit.cvglms[[i]]$lambda == fit.cvglms[[i]]$lambda.min)]
 RMSE[i] <- sqrt(RMSE[i])
}
alphaMin <- alphas[which.min(RMSE)]
alphaMin 

#par(mfrow=c(3,2))
for ( i in 1:length(alphas)) plot(fit.cvglms[[alphas[i]]])
#par(mfrow=c(1,1))
#plot(fit.cvglms[[1]])

# select optimal alpha, the one for which RMSE is minimized
# calculate model with optimal alpha
fit.cvglm <- cv.glmnet(x, y, alpha=alphaMin, nfolds=5, trace.it=1)
plot(fit.cvglm)

# s= value of penalty parameter lambda
pred.cvglm <- predict(fit.cvglm, newx=x, s="lambda.min")
resSQWK <- SQWK(pred.cvglm, train$Response) 
resSQWK
cuts <- optimalCuts(pred.cvglm, y)
cuts 
resSQWK2 <- SQWK2(pred.cvglm, y, cuts) 
resSQWK2
# prediction for test data
pred.cvglm <- predict(fit.cvglm, xtest)
resSQWK2oos <- SQWK2(pred.cvglm, test$Response, cuts)
resSQWK2oos
# add result to table of results
results <- bind_rows(results,
                     tibble(model="GLM - cross validation",
                            SQWK = resSQWK, SQWK2 = resSQWK2 , SQWK2oos=resSQWK2oos ))

results %>% knitr::kable()

# show factors with beta >0
coef(fit.cvglm)[,1][abs(coef(fit.cvglm)[,1])>0.01]
coef(fit.cvglm)[,1]

# lambda 1 sd distance, fewer variables, but smaller SQWK
pred.cvglm <- predict(fit.cvglm, newx=x, s="lambda.1se")
resSQWK <-  SQWK(pred.cvglm, train$Response) 
resSQWK
cuts <- optimalCuts(pred.cvglm, y)
cuts 
resSQWK2 <- SQWK2(pred.cvglm, y, cuts) 
resSQWK2

# prediction for test data
pred.cvglm <- predict(fit.cvglm, xtest)
resSQWK2oos <- SQWK2(pred.cvglm, test$Response, cuts)
resSQWK2oos

# add result to table of results
results <- bind_rows(results,
                     tibble(model="GLM - cross validation (fewer variables)",
                            SQWK = resSQWK, SQWK2 = resSQWK2, SQWK2oos=resSQWK2oos  ))

results %>% knitr::kable()

###################################
# Random forest                   #
###################################

set.seed(1, sample.kind="Rounding")
#set.seed(1)

fit.ranger <- ranger(x = x, y=train$Response,
                     num.threads=32, verbose=TRUE,
                     num.trees=400, mtry=80, max.depth=40,
                     importance="impurity", write.forest=TRUE, oob.error=TRUE,
                     min.node.size=3,
                     replace=FALSE, sample.fraction=0.6)
fit.ranger                                # summary
i <- importance(fit.ranger)                               
barchart( ~sort(i, decreasing=TRUE)[1:30], horizontal=TRUE, origin=0, xlab="Coefficients")
class(i)

names(fit.ranger)
sqrt(fit.ranger$prediction.error)         
pred.ranger <- fit.ranger$predictions
sqrt(mean((pred.ranger-train$Response)^2))     
resSQWK <- SQWK(pred.ranger, train$Response) 
resSQWK
cuts <- optimalCuts(pred.ranger, train$Response)
cuts
resSQWK2 <- SQWK2(pred.ranger, train$Response, cuts)       
resSQWK2
# prediction for test data
pred.ranger <- predict(fit.ranger, xtest)$predictions
resSQWK2oos <- SQWK2(pred.ranger, test$Response, cuts)
resSQWK2oos

# add result to table of results
results <- bind_rows(results,
                     tibble(model="Ranger - random forest",
                            SQWK = resSQWK, SQWK2 = resSQWK2, SQWK2oos=resSQWK2oos  ))

results %>% knitr::kable()



# turn parallel processing off and run sequentially again:
registerDoSEQ()
