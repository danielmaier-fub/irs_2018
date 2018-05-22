#############################
## Setting up your system ###
#############################

# Please execute this line of code
source("https://raw.githubusercontent.com/danielmaier-fub/irs_2018/master/utils.R")

#Just some very brief notes on the R programming environment's logic.
10*2 #it's a calculator, hooray.

a <-10*2 #the <- is used to create variables and add them to the environment. a is now 20.

a^2 #we can now use a for further calculations.

b <- 1:5 #gives you a vector containing the numbers 1 through 5.
c <- c("James", "John", "David", "Patricia", "Maria") #gives you a vector containing five names
df <-data.frame(b,c) #gives you a dataframe consisting of the two variables we created just before.
View(df)

print("hello world!") #functions allow you to do stuff.

install.packages("rtweet") #packages allow you to do even more stuff, as they add to R's basic functions.
library(rtweet) #packages need to be loaded when you start a new session. 

#We are using the rtweet package for all queries to Twitter's APIs. 
#For any questions regarding its functions, consult the package's documentation.
library(rtweet) #opens the package if it has not already been loaded.

#Define appname and access keys.
appname <- "XXXXXXXXXXXX"
key <- "XXXXXXXXXXXXXXXXX"
secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

#Create your access token.
twitter_token <- create_token(app = appname, consumer_key = key, consumer_secret = secret)

#There are different ways to find Tweets relating to a location, 
#as the location might be embedded to the tweet in many different forms.

#One very easy, general way is to just search for the location name as a keyword query.
query_Berlin <-search_tweets2(q = "Berlin", n = 200, type = "recent", include_rts = FALSE, retryonratelimit = TRUE)
View(query_Berlin)

#A less messy, but also less expansive way is to use the geocode parameter. 
#Everything else stays the same.
geocode_Berlin <-search_tweets2(q = "", n = 200, type = "recent", include_rts = FALSE, retryonratelimit = TRUE, geocode = "52.52001,13.40495,20mi")
View(geocode_Berlin)

#Hang on, though. How do I know the geocode for Berlin (or any city)? 
#Luckily, the rtweet library has a handy function for this.
Berlin_coords <-lookup_coords("Berlin")
Berlin_coords
#Careful with ambiguous placenames, though. Best to check if the result is plausible.

#Maybe we are also interested in a combination of some topic and a location. 
#For instance, what do Berliners say about traffic. Obviously, the query here sort of 
#pre-determines what language tweets I'll get.
query_traffic_geocode_Berlin <-search_tweets2(q = "traffic", n = 200, type = "recent", include_rts = FALSE, retryonratelimit = TRUE, geocode = "52.52001,13.40495,20mi")
View(query_traffic_geocode_Berlin)

#We might also be interested into getting just tweets with geotags attached to the tweet itself. 
#This can only be done using the streaming API, instead of the search API. Here, we need to define a 
#bounding box instead of a point radius.
geotag_Berlin <-stream_tweets(q = c(13.0882097323,52.3418234221,13.7606105539,52.6697240587), timeout = 60, verbose = TRUE)
View(geotag_Berlin)

#The stream and search functions do not get you all the metadata attached to user profiles 
#you may be interested in. It therefore makes sense to get the info on the users in our dataset. 
#The lookup_users function does this. This function needs a list of users to query. 
query_traffic_geocode_Berlin_users <-lookup_users(query_traffic_geocode_Berlin$user_id)
View(query_traffic_geocode_Berlin_users)

#Finally, it's convenient to add user info and tweets into one dataset. 
query_traffic_geocode_Berlin_merged <-merge(query_traffic_geocode_Berlin, query_traffic_geocode_Berlin_users)
View(query_traffic_geocode_Berlin_merged)

# Please retrieve some prepared data (do not share)
# Enter in your Browser: http://userpage.fu-berlin.de/maier/irs/data_irs.RData
# and save the file on your computer

# This is where I saved the file
pathToFile <- "C:/Users/maier/Desktop/data_irs.RData"

# Dear R, please load the file associated with the path...
tweets <- get(load(pathToFile))

#############################
## Data inspection ##########
#############################
View(tweets)

# what data class is the object 'tweets'
class(tweets)

# what are the names of the variables we have in the data set?
names(tweets)

# how many tweets have we got?
length(tweets[,1])

# get a table counting the number of occurrences of a language
tlang <- table(tweets$lang)

# print out the object saved
tlang

# sort
tlang <- sort(tlang, decreasing = TRUE)

# creat a barplot
barplot(tlang, main = "Languages in Twitter Data Set")

#############################
## Finding Bots on twitter ##
#############################

# names of two bots and two "human" users 
names <- c("VegivonRou", "zufallshorst", "realDonaldTrump", "85maier1")

# check bot probability
botprob <- botornot(names)
botprob

#############################
## Create networks ##########
#############################

frac <- 1:100

# Find communication relations
edges <- gt_edges(tweets[frac,], tweets = text, screen_name, status_id)

# Convert relations to Networks
graph <- gt_graph(edges)

# Convert to suitable format for visualization
nwD <- igraph_to_networkD3(graph)
nw.df <- data.frame(nwD$links$source, nwD$links$target)

# Visual analysis
networkD3::simpleNetwork(nw.df)

#############################
## Map of geolocated Tweets #
#############################

# which Tweets have a geotag?
geolocated <- which(lapply(lapply(tweets$geo_coords, is.na), all) == FALSE)

# take only the subset of the data with geotags
# and those columns we need ...
names(tweets)
columnsWeNeed <- c(2,5,66)
dataGeo <- data[geolocated, columnsWeNeed]

# Use function from pre-loaded data in step 1 (before we start...)
df <- getGeoCoords(dataGeo)

# create a map
map <- leaflet() %>% addTiles() %>% 
                     addMarkers(lng=df$lon, lat=df$lat, 
                                popup=c(paste0(df$screen_name, ": ", df$text)))

# Print the map
map

mapGeoNet(data)
