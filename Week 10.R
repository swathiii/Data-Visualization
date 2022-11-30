#----------VISUALIZATION OF GEOSPATIAL DATA---------------------

install.packages("ggmap")
library(ggmap)


us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
get_stamenmap(us, zoom = 5) %>%
  ggmap()

#using bounding box of the UK : modifying the map type to only
#provide outlines of major divisions and city, and increasing zoom level to 6
uk <- c(left = -10, bottom = 49, right = 2, top = 59)
get_stamenmap(uk, zoom = 6, maptype = 'toner-lite') %>%
  ggmap()

#zooming the map on sheffield's city centre
sheffield <- c(left = -1.49, bottom = 53.37, right = -1.45, top = 53.39)
get_stamenmap(sheffield, zoom = 15, maptype = 'toner-lite') %>%
  ggmap()

sheffieldCameras <- read.csv('Sheffield_CCTV_Locations.csv')
sheffield <- c(left = -1.49, bottom = 53.37, right = -1.45, top = 53.39)
get_stamenmap(sheffield, zoom = 15, maptype = 'toner-lite') %>%
  ggmap() +
  geom_point(data = sheffieldCameras, aes(x=lon, y=lat), colour = 'red') +
  labs(
    title = 'Position of CCTV cameras in Sheffield city centre in 2017',
    caption = 'Data : Sheffield city council, 2017'
  )


#EXERCISE: map surroundings  of information school 
sheffieldCameras <- read.csv('Sheffield_CCTV_Locations.csv')
informationschool <- c(left = -1.4806, bottom = 53.3797, right = -1.4678, top = 53.3834)
get_stamenmap(informationschool, zoom = 18, maptype = 'toner-lite') %>%
  ggmap() +
  geom_point(data = sheffieldCameras, aes(x = lon, y = lat), colour = 'red') +
  labs(
    title = 'Position of CCTV cameras around Information School in 2017',
    caption = 'Data : Sheffield city council, 2017'
  )


#----------------WORKING WITH OSM DATA-----------------------------------

install.packages('osmdata')
library(osmdata)

#downloading data from OSM
#this query looks for a particular place called heffield city centre and 
#filters out the place which have been indicated as a pub or restaurantt
SCC_pubs_restaurants <- getbb('Sheffield city centre') %>%
  opq() %>%
  add_osm_feature(key = 'amenity',
                  value = c('restaurant', 'pub')) %>%
  osmdata_sf()

View(SCC_pubs_restaurants$osm_points)
#geometry col has the precise location 

#-----plotting the pubs and restaurants on the map----
#downloading a raster image of the region
base_map <- get_map(getbb('Sheffield City Centre'), source = 'stamen')
#using base_map as a base layer and plotting the pubs data
ggmap(base_map) +
  geom_sf(data = SCC_pubs_restaurants$osm_points,
          inherit.aes = FALSE,
          colour = 'blue',
          alpha = .5,
          size  = 1,
          shape = 21) +
  labs( x = "", y = "")

install.packages('sf')
library(sf)

#downloading more features : rivers, roads, streets, highways, railway lines
SCC_highways <- getbb('Sheffield city centre') %>%
  opq() %>%
  add_osm_feature(key= 'highway',
                  value = c('motorway', 'primary', 'motorway_link', 'primary_link')) %>%
  osmdata_sf()

SCC_streets <- getbb('Sheffield city centre') %>%
  opq() %>%
  add_osm_feature(key = 'highway',
                  value = c('secondary', 'tertiary', 'secondary_link', 'tertiary_link')) %>%
  osmdata_sf()

SCC_small_streets <- getbb('Sheffield city centre') %>%
  opq() %>%
  add_osm_feature(key = 'highway',
                  value = c('residential', 'living_street', 'unclassified', 'service', 'footway')) %>%
  osmdata_sf()

SCC_river <- getbb('Sheffield city centre') %>%
  opq() %>%
  add_osm_feature(key = 'waterway', value = 'river') %>%
  osmdata_sf()

SCC_railway <- getbb('Sheffield city centre') %>%
  opq() %>%
  add_osm_feature(key = 'railway', value = 'rail') %>%
  osmdata_sf()

#since this type of data does not have specific points, we use osm_lines
View(SCC_railway$osm_lines)

ggplot() +
  geom_sf(data = SCC_river$osm_lines,
          inherit.aes = FALSE,
          color = 'steelblue',
          size = .8,
          alpha = .3) +
  geom_sf(data = SCC_railway$osm_lines,
          inherit.aes = FALSE,
          color = 'black',
          size = .2, 
          linetype = 'dotdash',
          alpha = .5) +
  geom_sf(data = SCC_streets$osm_lines,
          inherit.aes = FALSE,
          color = 'black',
          size = .3, 
          alpha = .5) +
  geom_sf(data = SCC_small_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#666666',
          size = .2,
          alpha = .3) +
  geom_sf(data = SCC_highways$osm_lines,
          inherit.aes = FALSE,
          color = 'black',
          size = .5, 
          alpha = .6) + 
  geom_sf(data = SCC_pubs_restaurants$osm_points,
          inherit.aes = FALSE,
          colour = 'blue',
          fill = 'blue',
          alpha = .5,
          size = 1,
          shape = 21) +
  theme_void() +
  coord_sf(xlim = c(-1.49, -1.45),
           ylim = c(53.36, 53.39),
           expand = FALSE) +
  theme() +
  labs(
    title = 'Restaurants and pubs in Sheffield City Centre'
  )

#augmenting the map with cctv dataset to see how well the pubs and restaurants are 
#covered by it
ggplot() +
  geom_sf(data = SCC_river$osm_lines,
          inherit.aes = FALSE,
          color = 'steelblue',
          size = .8,
          alpha = .3) +
  geom_sf(data = SCC_railway$osm_lines,
          inherit.aes = FALSE,
          color = 'black',
          size = .2, 
          linetype = 'dotdash',
          alpha = .5) +
  geom_sf(data = SCC_streets$osm_lines,
          inherit.aes = FALSE,
          color = 'black',
          size = .3, 
          alpha = .5) +
  geom_sf(data = SCC_small_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#666666',
          size = .2,
          alpha = .3) +
  geom_sf(data = SCC_highways$osm_lines,
          inherit.aes = FALSE,
          color = 'black',
          size = .5, 
          alpha = .6) + 
  geom_sf(data = SCC_pubs_restaurants$osm_points,
          inherit.aes = FALSE,
          colour = 'blue',
          fill = 'blue',
          alpha = .5,
          size = 1,
          shape = 21) +
  theme_void() +
  geom_point(data = sheffieldCameras, colour = 'red', aes(x=lon, y=lat))+
  coord_sf(xlim = c(-1.49, -1.45),
           ylim = c(53.36, 53.39),
           expand = FALSE) +
  theme(legend.position = 'none') +
  labs(
    title = 'CCTV coverage, Restaurants and pubs in Sheffield City Centre'
  )

#EXERCISE 2 : identifying areas that require police presence




#--------------CREATING A CHOROPLETH---------------------
#it is a map which uses differences in shading, colouring, or the
#placing of symbols within predefined areas to indicate the average
#values of a particular quantity in those areas

world_map <- map_data('world')
View(world_map)

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = 'lightgray', colour = 'white') +
  theme(panel.background = element_blank()) +
  labs(title = 'world map',
       caption = 'maps package, R')

#plotting the european union
eu.countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany","Greece", "Hungary", "Ireland",
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
  "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"
)

eu.map <- map_data('world', region = eu.countries)
ggplot(eu.map, aes(x=long, y=lat, group = group)) +
  geom_polygon(fill = 'lightgrey', colour = 'black') +
  labs(title = 'EU map', caption = 'maps package, R') +
  coord_map()

#we can add information using the fill colour of the polygons
library(WDI)
new_wdi_cache <- WDIcache()

#"NY.GDP.PCAP.KD : GDP per capita (constant since 2015, US$)
#"NY.GDP.PCAP.KD.ZG": GDP per capita growth (annual %)
#"SP.POP.TOTL : Total Population
#"SP.DYN.LE00.IN : Life expectancy at birth, total years

countryDataWDI <- WDI(indicator = c("NY.GDP.PCAP.KD",
                                    "NY.GDP.PCAP.KD.ZG",
                                    "SP.POP.TOTL",
                                    "SP.DYN.LE00.IN"),
                      start = 2019, 
                      end = 2019,
                      extra = TRUE, 
                      cache = new_wdi_cache)

View(countryDataWDI)
View(world_map)

library(tidyverse) #since we're using mutate

#resolving the issue of different names for countries between
#countrydatawdi and world_map
countryDataWDI <- countryDataWDI %>%
  mutate(country = recode(str_trim(country), 
                          'United States' = 'USA',
                          'United Kingdom' = 'UK'))

#joining the two dataframes into a third datafra
#we use left join so all tows of the world map are preserved
#in the new DF even if they do not have a corresponding
#value in the countryDataWDI

countryDataWDIMap <- left_join(world_map, countryDataWDI, by = c('region' = 'country'))

ggplot(countryDataWDIMap, aes(long, lat, group = group )) +
  geom_polygon(aes(fill=NY.GDP.PCAP.KD), colour = 'white') +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = 'GDP per \ncapita growth',
       title = 'World map coloured by GDP per capita growth in 2019',
       caption = 'Data source : World Development Indicators')

#EXERCISE 3 : create other maps using other variables

