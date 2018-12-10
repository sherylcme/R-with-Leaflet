library(leaflet)
library(rgdal)
library(dplyr)
library(ggmap)
library(htmltools)

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

# Adding white noises to the dataset to separate the points
# Individual record to represent on the map
ak$lon  <- jitter(ak$lon, factor = 1)
ak$lat  <- jitter(ak$lat, factor = 1)


# Filtering unsolved cases
ak_unsolved <- ak %>%
  filter(Crime.Type == "Murder or Manslaughter") %>%
  filter(Crime.Solved == "No")

# Labels 
ak_unsolved$label <- paste("<p>", ak_unsolved$City , "</p>",
                           "<p>", ak_unsolved$Month , " ", ak_unsolved$Year , "</p>",
                           "<p>", ak_unsolved$Victim.Sex , " ", ak_unsolved$Victim.Age, "</p>",
                           "<p>", ak_unsolved$Victim.Race , "</p>",
                           "<p>", ak_unsolved$Weapon, "</p>")


# Add Circle Markers to denote the places of crime
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  #set a view with coordinates
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
  # Weight = the size of the border
  addPolygons(data = ak_data,
              color = "#660000",
              weight = 1) %>%
  addCircleMarkers(lng = ak_unsolved$lon,
                   lat = ak_unsolved$lat,
                   color = "ffffff",
                   weight = 1,
                   radius = 5,
                   label = lapply(ak_unsolved$label, HTML),
                   # Cluster the markers together
                   clusterOptions = markerClusterOptions())

m

# Filtering solved cases
ak_solved <- ak %>%
  filter(Crime.Solved == "Yes") %>%
  filter(Crime.Type == "Murder or Manslaughter")

ak_solved$label <- paste("<p>", ak_solved$City , "</p>",
                           "<p>", ak_solved$Month , " ", ak_solved$Year , "</p>",
                           "<p>", ak_solved$Victim.Sex , " ", ak_solved$Victim.Age, "</p>",
                           "<p>", ak_solved$Victim.Race , "</p>",
                           "<p>", ak_solved$Weapon, "</p>")


# Layering for solved and unsolved cases
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  #set a view with coordinates
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
  # Weight = the size of the border
  addPolygons(data = ak_data,
              color = "#660000",
              weight = 1) %>%
  addCircleMarkers(lng = ak_unsolved$lon,
                   lat = ak_unsolved$lat,
                   color = "red",
                   weight = 1,
                   radius = 5,
                   # Adding group name for controls
                   group = "Unsolved",
                   label = lapply(ak_unsolved$label, HTML)) %>%
  addCircleMarkers(lng = ak_solved$lon,
                   lat = ak_solved$lat,
                   color = "blue",
                   weight = 1,
                   radius = 5,
                   group = "Solved",
                   label = lapply(ak_solved$label, HTML)) %>%
  # Specify the groups to overlap
  addLayersControl(overlayGroups = c("Unsolved", "Solved"),
                   # Whether to collapse the control options
                   options = layersControlOptions(collapsed = FALSE))
  
m
