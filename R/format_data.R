#####################################################
# 1. Read in data                                   #
# 2. Make train/test split                          #
#####################################################

setwd('/Users/Zack/Dropbox/Classes/Stat627/project/')

sim <- data.table::fread('data/pop_failures.csv')
sim <- as.data.frame(sim)
sim$outcome <- as.factor(sim$outcome)

set.seed(1232)
runs <- unique(sim$Run)
train_test_split <- sample(runs, .8*length(runs))
train <- sim[sim$Run %in% train_test_split, ]
test <- sim[!(sim$Run %in% train_test_split), ]

write.csv(train, 'data/train.csv', row.names = FALSE)
write.csv(test, 'data/test.csv', row.names = FALSE)
