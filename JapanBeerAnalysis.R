library(ggplot2)
library(dplyr)
library(ggmap)
library(tidyr)
library(gridExtra)
library(httr)
library(httpuv)
library(jsonlite)
library(base64enc)

## setting wd

setwd("/home/abujold/Desktop/Data/Beer")

## Loading Beer Data
df_beers <- read.csv("../Beer/beers.csv", as.is = T)
df_breweries <- read.csv("../Beer/breweries.csv", as.is = T)

df_japan_breweries_BA <- read.csv("../Beer/jp_breweries_beeradvocate.csv", as.is = T)
df_japan_beers <- read.csv("../Beer/JapaneseBeer.csv", as.is = T)

## Removing junk data

df <-  df_japan_breweries_BA
df$url <- NULL

i = 1
j = 1

for(i in 1:nrow(df)){
  df$BusinessType[i] = df$BusinessType[i+1]
}

for(i in 1:nrow(df)){
  df$Street.Address[i] = df$Street.Address[i+1]
}

for(i in 1:nrow(df)){
  df$City[i] = df$City[i+1]
}

for(i in 1:nrow(df)){
  df$Zipcode[i] = df$Zipcode[i+1]
}

Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]

df_clean <- Nth.delete(df, 2)

df_clean <- data.frame(df_clean, geocode(df_clean$Street.Address, output = "latlon"))
df_latlontest <- geocode(df_clean$Business, output = "latlon")

## Lat lon by City to replace NAs
df_temp <- data.frame(df_clean[is.na(df_clean$lon),]$Business, geocode(df_clean[is.na(df_clean$lon),]$City, output = "latlon"))
colnames(df_temp)[1] <- "Business"
df_clean <- left_join(df_clean, df_temp, by = "Business")

## Cleaning up NAs from merge
df_clean <- df_clean %>%
  mutate(lon = ifelse(is.na(lon.x), lon.y, lon.x), lat = ifelse(is.na(lat.x), lat.y, lat.x))
df_clean$lat.x <- NULL
df_clean$lat.y <- NULL
df_clean$lon.x <- NULL
df_clean$lon.y <- NULL

##### Plotting
## Loading Japan Map
japan <- get_map(location = 'japan', zoom = 5)
ggmap(japan)

## Plotting all Breweries
ggmap(japan, extent = "device") + geom_point(data = df_clean, aes(x = lon, y = lat))

## Density Plot + points
ggmap(japan, extent = "device") +
  geom_point(data = df_clean, aes(x = lon, y = lat, color = BeerCount)) +
  stat_density2d(data = df_clean, aes(x = lon, y = lat))
## Merging reviews into Clean data

df_japan_brewery_rating <- df_japan_beers %>%
  select(Beer, Brewery, Type, ABV, Score, Reviews, Ratings) %>%
  mutate(Ratings = as.numeric(gsub(",","", gsub("\\| ","", Ratings)))) %>%
  mutate(Ratings = ifelse(is.na(Ratings), 1509, Ratings)) %>%
  mutate(Business = Brewery, weight_score = Score * Ratings) %>%
  group_by(Business) %>%
  summarise(rate = sum(weight_score), num_beer = n()) %>%
  mutate(avg_rate = rate/num_beer) %>%
  inner_join(., df_clean, by = "Business")

## Plotting rated breweries on a map
ggmap(japan, extent = "device") +
  geom_point(data = df_japan_brewery_rating, aes(x = lon, y = lat, size = avg_rate), color = "red")

# Pulling out cities
cities_breweries_rated <- unique(df_japan_brewery_rating$City)

# Building out maps programatically
tokyo <- get_googlemap(center = "Tokyo")
sapporo <- get_map(location = "Sapporo")
osaka <- get_map(location = "Osaka")
kyoto <- get_map(location = "Kyoto")

p1 <- ggmap(tokyo) +
  geom_point(data = df_clean, aes(x = lon, y = lat)) +
  stat_density2d(data = df_clean, aes(x = lon, y = lat))

p2 <- ggmap(sapporo) +
  geom_point(data = df_clean, aes(x = lon, y = lat)) +
  stat_density2d(data = df_clean, aes(x = lon, y = lat))

p3 <- ggmap(kyoto) +
  geom_point(data = df_clean, aes(x = lon, y = lat)) +
  stat_density2d(data = df_clean, aes(x = lon, y = lat))

p4 <- ggmap(osaka) +
  geom_point(data = df_clean, aes(x = lon, y = lat)) +
  stat_density2d(data = df_clean, aes(x = lon, y = lat))

grid.arrange(p1, p2, p3, p4, nrow = 2)

## Pulling in Yelp Data

# Your credentials, from https://www.yelp.com/developers/manage_api_keys
consumerKey = "knr6TkfnTswodPhfQKYZVA"
consumerSecret = "BqsRSnzIBPdzB9U5qHMN6VLbhHs"
token = "6N63JrTQyKwjwe_NpbEtbBNjwqesBv2d"
token_secret = "4ITzcJ64Ge64-VTqeCUJBfqxFPU"

https://api.yelp.com/v2/search/?location=San Francisco, CA&category_filter=breweries


