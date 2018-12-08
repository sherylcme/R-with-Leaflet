library(leaflet)

#create leaflet object
m <- leaflet()
# m will be empty object as leaflet create a layer approach 
m

m <- leaflet() %>%
  #add tiles to show the map
  addTiles() %>%
  #set a view with coordinates
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)
m

