# Create a list containing the means, sd, and colnames for columns that need to be scaled
create_scaler <- function(data){
  scaler <- scale(data, center=T, scale=T)
  attr_scaler <- attributes(scaler)
  scaler <- list(center=attr_scaler$`scaled:center`, scale=attr_scaler$`scaled:scale`, cols=colnames(data))
  return(scaler)
}

# Apply center/scale to a new dataframe based on a created scaler list, only to columns that need
apply_scaler <- function(data, scaler){
  data[, scaler$cols] <- sapply(scaler$cols, function(i) (data[, i]-scaler$center[i])/scaler$scale[i])
  return(data)
}
