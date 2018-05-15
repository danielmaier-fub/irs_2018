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

mapGeoNet <- function(data){
	data <- data
	del <- which(duplicated(data$status_id))
	if(length(del) > 0){
		data <- data[-which(duplicated(data$status_id)),]
	}
	geolocated <- which(lapply(lapply(data$geo_coords, is.na), all) == FALSE)
	data <- data[geolocated, ]

	nw <- data %>% graphTweets::gt_edges(tweets = text, screen_name, status_id) %>% graphTweets::gt_graph()
	gdf <- igraph::as_data_frame(nw, what="both")
	vert <- gdf$vertices
	cols <- c(which(names(data) == "screen_name"), which(names(data) == "text"), which(names(data) == "geo_coords"))
	df <- data[, cols]
	lon <- c()
	lat <- c()
	for (i in 1:length(df$geo_coords)){
		lon <- c(lon, df$geo_coords[[i]][2])
		lat <- c(lat, df$geo_coords[[i]][1])
	}

	df$lat <- lat
	df$lon <- lon
	names(df)[1] <- "name"

	mdf <- base::merge(x = vert, y=df, by = "name", by.x = TRUE)
	vdf <- mdf[!duplicated(mdf[, "name"], fromLast=T),]

	coordinates(vdf) <- ~lon+lat
	edges <- gdf$edges

	edges <- lapply(1:nrow(edges), function(i) {
		as(rbind(vdf[vdf$name == edges[i, "from"], ], 
           vdf[vdf$name == edges[i, "to"], ]), "SpatialLines")
	})

	for (i in seq_along(edges)) {
	edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
	}

	edges <- do.call(rbind, edges)

	leaflet(vert) %>% addTiles() %>% addMarkers(data = vdf, popup=cat(paste0(vdf$name, ": ", vdf$text))) %>% addPolylines(data = edges)
}
