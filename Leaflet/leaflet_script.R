library(leaflet)
library(rgdal)
library(dplyr)
library(ggmap)

# Create leaflet object
m <- leaflet()
# m will be empty object as leaflet create a layer approach 
m

m <- leaflet() %>%
  # Sdd tiles to show the map
  addTiles() %>%
  # set a view with coordinates
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)
m

# ProviderTiles - ThunderForest 
# Not showing cos API Key needed
m <- leaflet() %>%
  addProviderTiles(providers$Thunderforest.Pioneer) %>%
  #set a view with coordinates
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)
m

# ProviderTiles - ESRI
m <- leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  #set a view with coordinates
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)
m

# ProviderTiles - Stamen
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  #set a view with coordinates
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)
m

# Can preview different providertiles here -
# http://leaflet-extras.github.io/leaflet-providers/preview/


# Reading SHP files, need to import rgdal package 
ak_data <- readOGR('data/tl_2013_02_cousub/tl_2013_02_cousub.shp')
fbi_data <- read.csv('data/database.csv')

# Add data to map 
# ProviderTiles - Stamen
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  #set a view with coordinates
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
  # Weight = the size of the border
  addPolygons(data = ak_data,
              color = "#660000",
              weight = 1)
m

# Filtering data for Alaska
ak <- filter(fbi_data, State == "Alaska")

# Missing information on address
# Create the address by combining city and state 
ak <- mutate(ak, address = paste(City,State,"United States"))

# Getting unique addresses
unique_addresses <- unique(ak$address)

# Install ggmap packages
geocodes <- geocode(unique_addresses, source = "dsk")

address_and_coords = data.frame(address = unique_addresses,
                                lon = geocodes$lon,
                                lat = geocodes$lat)

# Combine coordinates with fbi_data 
ak <- left_join(ak, address_and_coords, by = 'address')