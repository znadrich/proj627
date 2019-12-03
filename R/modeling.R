setwd('/Users/Zack/Dropbox/Classes/Stat627/project/')
source('R/center_scaler.R')
library(glmnet)
library(tree)
library(MASS)
set.seed(123)

train <- read.csv('data/train.csv', header=T)
test <- read.csv('data/test.csv', header=T)

train$outcome <- as.factor(train$outcome)
test$outcome <- as.factor(test$outcome)

formula <- outcome ~ .-Run-Study

# Returns the AUC
roc.auc <- function(predictions, actual){
  return(ROSE::roc.curve(actual, predictions, plotit = F)$auc)
}

# Fit logistic regression and return AUC for test dataset
fit.logistic <- function(test, params, eval.func){
  fit <- do.call(glm, params)
  fit$call <- params$formula
  predictions <- predict(fit, test, type='response')
  eval <- eval.func(predictions, test$outcome)
  model <- list(
    fit=fit,
    eval=eval,
    params=params
  )
  return(model)
}

# Fit LDA and return AUC for test dataset
fit.lda <- function(train, test, params, eval.func){
  fit <- do.call(lda, params)
  fit$call <- params$formula
  predictions <- predict(fit, test)$posterior[, 2]
  eval <- eval.func(predictions, test$outcome)
  model <- list(
    fit=fit,
    eval=eval,
    params=params
  )
  return(model)
}


# Fit elastic net 
#   In param list, alpha can be changed to be lasso or ridge
#   Performs glmnet built in CV to pick best lambda parameter
# return AUC for test dataset
fit.glmnet <- function(train, test, formula, params, eval.func){
  params$x <- model.matrix(formula, train)[, -1]
  fit <- do.call(cv.glmnet, params)
  test.mm <- model.matrix(formula, test)[, -1]
  predictions <- predict(fit, test.mm, s='lambda.1se')
  eval <- eval.func(predictions, test$outcome)
  params$coefs <- as.matrix(coef(fit, s='lambda.1se'))
  model <- list(
    fit=fit,
    eval=eval,
    params=params
  )
  return(model)
}

# Fit tree model
#   Does pruning defined by params$prune.func
# return AUC for test dataset
fit.tree <- function(train, test, formula, params, size, eval.func){
  fit <- tree(data=train, formula=formula)
  if(params$prune.func == 'prune.misclass'){
    cv_tree <- cv.tree(fit, FUN=prune.misclass)
    size <- cv_tree$size[which.min(cv_tree$dev)]
    fit <- prune.misclass(fit, best=size) 
  } else {
    cv_tree <- cv.tree(fit)
    size <- cv_tree$size[which.min(cv_tree$dev)]
    fit <- prune.tree(fit, best=size)  
  }
  fit$call <- params$formula
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

params.lasso <- list(
  y=train$outcome,
  family='binomial', # logistic regression
  alpha=1, # lasso penalty
  standardize=FALSE, # data is already standardized
  type.measure='auc' # for best lambda cv
)

params.ridge <- list(
  y=train$outcome,
  family='binomial', # logistic regression
  alpha=0, # ridge penalty
  standardize=FALSE, # data is already standardized
  type.measure='auc' # for best lambda cv
)

params.logistic <- list(
  formula=formula,
  data=train,
  family=binomial # logistic regression
)

params.lda <- list(
  formula=formula,
  data=train,
  CV=F # Not necessary
)

params.tree <- list(
  formula=outcome~.-Run-Study,
  prune.func = 'prune.misclass'
)

# Fit our models and save to list
fits <- list()
fits$logistic_reg <- fit.logistic(test=test, params=params.logistic, eval.func=roc.auc)
fits$lasso <- fit.glmnet(train=train, test=test, formula=formula, params=params.lasso, eval.func=roc.auc)
fits$ridge <- fit.glmnet(train=train, test=test, formula=formula, params=params.ridge, eval.func=roc.auc)
fits$tree <- fit.tree(train=train, test=test, formula=formula, params=params.tree, eval.func=roc.auc)
fits$LDA <- fit.lda(test=test, params=params.lda, eval.func=roc.auc)

save.image("data/renv.RData")