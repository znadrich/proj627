#####################################################
# 1. Read in data and format nicely for analysis    #
# 2. Make train/test split                          #
# 3. Save in fst format for compression and loading #
#####################################################

setwd('/Users/Zack/Dropbox/Classes/Stat627/project/')

colnames <- c(
  'elevation',
  'aspect',
  'slope',
  'h_dist_hydro',
  'v_dist_hydro',
  'h_dist_road',
  'hillshade_9am',
  'hillshade_noon',
  'hillshade_3pm',
  'h_dist_fire',
  paste('wilderness_area_', 1:4, sep=''),
  paste('soil_type_', 1:40, sep=''),
  'cover_type'
)

cover_types <- c(
  'Spruce/Fir',
  'Lodgepole Pine',
  'Ponderosa Pine',
  'Cottonwood/Willow',
  'Aspen',
  'Douglas-fir',
  'Krummholz'
)

forests <- data.table::fread('data/covtype.csv', header=F, col.names = colnames)
forests <- as.data.frame(forests)
forests$cover_type <- sapply(forests$cover_type, function(x) cover_types[x])

set.seed(123)
train_test_split <- sample(nrow(forests), .8*nrow(forests))
train <- forests[train_test_split, ]
test <- forests[-train_test_split, ]

fst::write.fst(forests, path='data/covtype.fst', compress=100)
fst::write.fst(train, 'data/covtype_train.fst', compress=100)
fst::write.fst(test, 'data/covtype_test.fst', compress=100)
