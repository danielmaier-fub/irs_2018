install.packages("devtools") # Development Tool 
install.packages("igraph") # Network Analysis Software
install.packages("rtweet") # Retrieval Tool for Twitter Data
install.packages("graphTweets") # Contsruct Networks from Data
install.packages("networkD3") # Network Visualization Software
install.packages("leaflet") # Software to draw maps
install.packages("sp") # Software for spacial data
install.packages("rgexf") # Network formatting
install.packages("stm") # Topic modeling
install.packages("quanteda") # text as data tool set
install.packages("xlsx") # import and export excel files
#install.packages("dfm")
install.packages("LDAvis") # topic model visulalization

library(devtools)
library(graphTweets)
library(rtweet)
library(igraph)
library(networkD3)
library(leaflet)
library(sp)
library(rgexf)
library(stm)
library(quanteda)
library(xlsx)
library(LDAvis)
#install_github("mkearney/botrnot") # Bot Identifyer Software
#library(botrnot)


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

	leaflet(vert) %>% addTiles() %>% addMarkers(data = vdf, popup= paste0(vdf$name, ": ", vdf$text)) %>% addPolylines(data = edges)
}

prune_dfm = function(dfm, minimum_threshold = 0.005, maximum_threshold = 1) {
    document_frequency <- Matrix::colSums(dfm > 0)
    threshold_min_abs <- nrow(dfm) * minimum_threshold
    threshold_max_abs <- nrow(dfm) * maximum_threshold
    features_to_keep <- names(which(document_frequency > threshold_min_abs & document_frequency < threshold_max_abs))
    dfm <- dfm[, features_to_keep]
    return(dfm)
}
