# function to divide geocoord-column from 
# rtweet-dataset into separate columns

getGeoCoords <- function(data){
  df <- data 
  lon <- c()
  lat <- c()
  for (i in 1:length(df$geo_coords)){
    lon <- c(lon, df$geo_coords[[i]][2])
    lat <- c(lat, df$geo_coords[[i]][1])
  }
  df$lat <- lat
  df$lon <- lon
  return(df)
}
