setwd('/Users/Zack/Dropbox/Classes/Stat627/project/')
source('R/center_scaler.R')
library(randomForest)
library(glmnet)
library(tree)
set.seed(123)

train <- read.csv('data/train.csv', header=T)
test <- read.csv('data/test.csv', header=T)

formula <- outcome ~ .-Run-Study

roc.auc <- function(predictions, actual){
  return(ROSE::roc.curve(actual, predictions, plotit = F)$auc)
}

fit.logistic <- function(train, test, formula, params, eval.func){
  fit <- do.call(glm, params)
  predictions <- predict(fit, test, type='response')
  eval <- eval.func(predictions, test$outcome)
  model <- list(
    fit=fit,
    eval=eval,
    params=params
  )
  return(model)
}

fit.rf <- function(train, test, params, eval.func){
  params$x <- model.matrix(formula, train)[, -1]
  params$y <- as.factor(train$outcome)
  fit <- do.call(randomForest, params)
  predictions <- predict(fit, test, type='prob')[, 2]
  eval <- eval.func(predictions, test$outcome)
  model <- list(
    fit=fit,
    eval=eval,
    params=params
  )
  return(model)
}

fit.glmnet <- function(train, test, formula, params, eval.func){
  params$x <- model.matrix(formula, train)[, -1]
  fit <- do.call(cv.glmnet, params)
  test.mm <- model.matrix(formula, test)[, -1]
  predictions <- predict(fit, test.mm, s='lambda.1se')
  eval <- eval.func(predictions, test$outcome)
  model <- list(
    fit=fit,
    eval=eval,
    params=params
  )
  return(model)
}

fit.tree <- function(train, test, formula, params, size, eval.func){
  train_ix <- sample(nrow(train), nrow(train)*.8)
  params$data <- train[train_ix, ]
  fit <- do.call(tree::tree, params)
  pruned <- prune(fit, newdata=train[-train_ix, ])
  size <- pruned$size[which.min(pruned$dev)]
  fit <- prune(fit, best=size)
  predictions <- predict(fit, test, type='vector')[, 2]
  eval <- eval.func(predictions, test$outcome)
  params$size <- size
  model <- list(
    fit=fit,
    eval=eval,
    params=params
  )
  
  return(model)
}

params.rf <- list(
  ntree=100,
  mtry=4, # sqrt(18)
  importance=TRUE
)

params.lasso <- list(
  y=train$outcome,
  family='binomial',
  alpha=1,
  standardize=TRUE,
  type.measure='auc'
)

params.ridge <- list(
  y=train$outcome,
  family='binomial',
  alpha=0,
  standardize=TRUE,
  type.measure='auc'
)

params.logistic <- list(
  formula=formula,
  data=train,
  family=binomial
)

params.tree <- list(
  formula=as.factor(outcome)~.-Run-Study
)

fits <- list()

fits$logistic <- fit.logistic(train, test, formula, params=params.logistic, eval.func=roc.auc)
fits$rf <- fit.rf(train, test, params=params.rf, eval.func=roc.auc)
fits$lasso <- fit.glmnet(train, test, formula, params=params.lasso, eval.func=roc.auc)
fits$ridge <- fit.glmnet(train, test, formula, params=params.ridge, eval.func=roc.auc)
fits$tree <- fit.tree(train, test, formula, params=params.tree, eval.func=roc.auc)

sapply(fits, function(x) x$eval)
