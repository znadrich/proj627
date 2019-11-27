##################################################
# Read in data and format nicely for analysis    #
# Save in fst format for compression and loading #
##################################################

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
forests$cover_type <- sapply(forests$cover_type, function(x) cover_types[x])

fst::write.fst(forests, path='data/covtype.fst')
