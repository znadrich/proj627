#####################################################
# 1. Read in data                                   #
# 2. Make train/test split                          #
#####################################################

setwd('/Users/Zack/Dropbox/Classes/Stat627/project/')

sim <- data.table::fread('data/pop_failures.csv')
sim <- as.data.frame(sim)

set.seed(123)
train_test_split <- sample(nrow(sim), .8*nrow(sim))
train <- sim[train_test_split, ]
test <- sim[-train_test_split, ]

write.csv(train, 'data/train.csv', row.names = FALSE)
write.csv(test, 'data/test.csv', row.names = FALSE)
