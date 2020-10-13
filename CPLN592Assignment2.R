#Load Libraries
setwd("C:/Users/Hannah/Documents/Penn/Fall 2020/CPLN-592/Assignments/Assignment 2/CPLN592Assignment2")
library(tidyverse)
library(tidycensus)
library(kableExtra)
library(tidycensus)
library(sf)
library(gridExtra)
library(grid)
library(knitr)
library(rmarkdown)
library(ggcorrplot)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(tidyr)
library(dplyr)
library(osmdata)
library(mapview)
library(RANN)
library(ggplot2)
library(stargazer)

options(scipen=999)
options(tigris_class = "sf")

#Load Styles
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}
# Load Quantile break functions
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette
palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

# Load nn function
nn_function <- function(measureFrom,measureTo,k) { 
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()}

#load nn 2 functon
nn_function2 <- function(measureFrom,measureTo,k) { 
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(house = mean(Miami_Houses$SalePrice)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()}

#function 4
nn_function2 <- function(measureFrom,measureTo,k) { 
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(house = mean(Miami_Houses$SalePrice))}

#function 3
nn_function3 <- function(measureFrom,measureTo,k) { 
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist }
# load multiple ring buffer
multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{
  #create a list of distances that we'll iterate through to create each ring
  distances <- seq(0, maxDistance, interval)
  #we'll start with the second value in that list - the first is '0'
  distancesCounter <- 2
  #total number of rings we're going to create
  numberOfRings <- floor(maxDistance / interval)
  #a counter of number of rings
  numberOfRingsCounter <- 1
  #initialize an otuput data frame (that is not an sf)
  allRings <- data.frame()
  
  #while number of rings  counteris less than the specified nubmer of rings
  while (numberOfRingsCounter <= numberOfRings) 
  {
    #if we're interested in a negative buffer and this is the first buffer
    #(ie. not distance = '0' in the distances list)
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
    {
      #buffer the input by the first distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #different that buffer from the input polygon to get the first ring
      buffer1_ <- st_difference(inputPolygon, buffer1)
      #cast this sf as a polygon geometry type
      thisRing <- st_cast(buffer1_, "POLYGON")
      #take the last column which is 'geometry'
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add a new field, 'distance' so we know how far the distance is for a give ring
      thisRing$distance <- distances[distancesCounter]
    }
    
    
    #otherwise, if this is the second or more ring (and a negative buffer)
    else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
    {
      #buffer by a specific distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create the next smallest buffer
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #This can then be used to difference out a buffer running from 660 to 1320
      #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
      #bc the area after 660ft in buffer2 = NA.
      thisRing <- st_difference(buffer2,buffer1)
      #cast as apolygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #get the last field
      thisRing <- as.data.frame(thisRing$geometry)
      #create the distance field
      thisRing$distance <- distances[distancesCounter]
    }
    
    #Otherwise, if its a positive buffer
    else 
    {
      #Create a positive buffer
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create a positive buffer that is one distance smaller. So if its the first buffer
      #distance, buffer1_ will = 0. 
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #difference the two buffers
      thisRing <- st_difference(buffer1,buffer1_)
      #cast as a polygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #geometry column as a data frame
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add teh distance
      thisRing$distance <- distances[distancesCounter]
    }  
    
    #rbind this ring to the rest of the rings
    allRings <- rbind(allRings, thisRing)
    #iterate the distance counter
    distancesCounter <- distancesCounter + 1
    #iterate the number of rings counter
    numberOfRingsCounter <- numberOfRingsCounter + 1
  }
  
  #convert the allRings data frame to an sf data frame
  allRings <- st_as_sf(allRings)
}

# Load census API key
census_api_key("91a259a2aaac3093a636d189040e0ff263fc823b", overwrite = TRUE)

# Load Assignment 2 student data
Miami_Houses <- 
  rbind(
    st_read("studentsData.geojson") %>%
    st_transform(st_crs('EPSG:6346')))
Miami_Houses <- st_set_crs(Miami_Houses, 6346)
Miami_Houses <- distinct(Miami_Houses,  .keep_all = TRUE)


# Miami Training Data
Miami_Training <- subset(Miami_Houses, toPredict %in% 0)

# Load census demographic data
tracts18 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2018, state=12, county="Miami-Dade County", geometry=T, output="wide") %>%
  st_transform('EPSG:6346')%>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop), 0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2018") %>%
  dplyr::select(-Whites, -TotalPoverty) 

# Load Crime
miamicrime <- 
  rbind(
    st_read("MiamiCrime/09-27-10-03-2020-miamicrime.shp") %>% 
      st_transform(st_crs('EPSG:6346')))

# Load neighborhoods
nhoodsmiami <- 
  rbind(
  st_read("https://opendata.arcgis.com/datasets/2f54a0cbd67046f2bd100fb735176e6c_0.geojson") %>%
  st_transform('EPSG:6346'))
nhoodsmiami <- st_set_crs(nhoodsmiami, 6346)

nhoodsmiamibeach <-
  rbind(st_read("neighborhoods/miamineighborhoods.shp")%>%
  st_transform('EPSG:6346'))
nhoodsmiamibeach <- st_set_crs(nhoodsmiamibeach,6346)

nhoodsmiami <- nhoodsmiami[2]
nhoodsmiamibeach <-nhoodsmiamibeach[1]%>%
 rename(LABEL = Name)
nhoodsmiami <-st_zm(nhoodsmiami, drop = TRUE, what = "ZM")
nhoodsmiamibeach <-st_zm(nhoodsmiamibeach, drop=TRUE, what = "ZM")
nhoods <- rbind(nhoodsmiami,nhoodsmiamibeach)%>%
  rename(neighborhood = LABEL)


# select only miami tracts
miami <- st_union(nhoods)
tracts.miami.intersect <- st_intersects(miami, tracts18)
tracts18miami <- tracts18[tracts.miami.intersect[[1]],]

# create PricePerSq 
Miami_Houses$PricePerSq <- Miami_Houses$SalePrice/Miami_Houses$AdjustedSqFt

# Plot PricePerSq over neighborhoods
ggplot() +
  geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(data = Miami_Houses, aes(colour = q5(PricePerSq)), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(Miami_Houses,"PricePerSq"),
                      name="Quintile\nBreaks") +
  labs(title="Price Per Square Foot, Miami") +
  mapTheme()

# Nearest neighbor crime
st_c <- st_coordinates

Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    crime_nn2 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miamicrime)), 2)) 
library(sp)
library(SearchTrees)

## Example data
set.seed(1)
distance <- gDistance(Miami_Test,Miami_Houses)

## Find indices of the two nearest points in A to each of the points in B
tree <- createTree(coordinates(A))
inds <- knnLookup(tree, newdat=coordinates(B), k=2)


Miami_Test <-
  Miami_Test %>% 
  mutate(
    houses_nn7 = nn_function2(st_c(st_centroid(Miami_Test)), st_c(st_centroid(Miami_Training)), 2))
houses_nn5 <- nn_function3(st_c(st_centroid(Miami_Test)), st_c(st_centroid(Miami_Training)), 2)
# crime buffer for .5 miles
Miami_Housesbuffer <- st_buffer(Miami_Houses, 402)
crime_in_buffer <- st_join(miamicrime, Miami_Housesbuffer, join = st_within)
crime_buffer_count <- count(as_tibble(crime_in_buffer), Folio) 
Miami_Houses <- left_join(Miami_Houses, crime_buffer_count)%>%
  rename(crimesbuffer = n)

# load beach feature
miamibeach <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/d0d6e6c9d47145a0b05d6621ef29d731_0.geojson") %>%
      st_transform('EPSG:6346'))
miamibeach <- st_set_crs(miamibeach, 6346)
miamibeach <- st_union(miamibeach)
Miami_Houses.centroids <-st_centroid(Miami_Houses)
Miami_Houses$beachDist <-st_distance(Miami_Houses.centroids, miamibeach)
Miami_Houses$beachDist <-as.numeric(Miami_Houses$beachDist)

# load water feature
miamiwater <-
  rbind(st_read("Water/miamiwater.shp")%>%
          st_transform('EPSG:6346'))
miamiwater <- st_set_crs(miamiwater,6346)

# add column for distance to water

miamiwater <- st_union(miamiwater)
Miami_Houses$waterDist <-st_distance(Miami_Houses.centroids, miamiwater)
Miami_Houses$waterDist <-as.numeric(Miami_Houses$waterDist)

#Parks (method 1 - Same output as other variables)
miami_municipalparks <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/a585b193a4764760802f510f8c5b1452_0.geojson") %>%
      st_transform('EPSG:6346'))
miami_municipalparks <- st_set_crs(miami_municipalparks, 6346)
miami_municipalparks <- st_union(miami_municipalparks)
Miami_Houses$ParksDist <- st_distance(Miami_Houses.centroids, miami_municipalparks)
Miami_Houses$ParksDist <-as.numeric(Miami_Houses$ParksDist)

#nearest neighborStarbucks
Starbucks <- st_read("Starbucks.csv")
Starbucks.sf <- st_as_sf(Starbucks, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>% 
  st_transform(st_crs(Miami_Houses))

st_c <- st_coordinates

Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    Starbucks_nn1 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(Starbucks.sf)), 1))



#Nearest golf course (returning same output as other variables)
miamigolfcourses <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/229eeac512b043f8bf5317ec8377f151_0.geojson") %>%
      st_transform('EPSG:6346'))
miamigolfcourses <- st_set_crs(miamigolfcourses, 6346)

miamigolfcourses <- st_union(miamigolfcourses)
Miami_Houses$GolfCourseDist <-st_distance(Miami_Houses.centroids,miamigolfcourses)
Miami_Houses$GolfCourseDist <-as.numeric(Miami_Houses$GolfCourseDist)

#nearest neighbor metromover station (values seem to be too high)
miami_metromover <- st_read("Metromover_Station.csv")
miami_metromover.sf <- st_as_sf(miami_metromover, coords = c("LAT", "LON"), crs = 4326, agr = "constant") %>% 
  st_transform(st_crs(Miami_Houses))

st_c <- st_coordinates

Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    Metromover_nn1 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami_metromover.sf)), 1))



#nearest neighbor metro stations
miami_metros <- st_read("MetroRailStations/Metrorail_Station.csv")
miami_metros.sf <- st_as_sf(miami_metros, coords = c("LON","LAT"), crs = 4326, agr = "constant") %>% 
  st_transform(st_crs(Miami_Houses))
miami_metros.sf <- st_set_crs(miami_metros.sf, 6346)
                               
st_c <- st_coordinates

Miami_Houses.centroids <-st_centroid(miami_metros.sf)

Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    Metros_nn1 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami_metros.sf)), 1))

# highways
miamiHighways <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/6d31141fd24148f0b352f341ef38d161_0.geojson") %>%
      st_transform('EPSG:6346'))
miamiHighways <- st_set_crs(miamiHighways,6346)

miamiHighwaysbuffer.125 <- 
  rbind(
    st_union(st_buffer(miamiHighways, 201.168)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))
miamiHighwaysbuffer.125<-miamiHighwaysbuffer.125%>%
  rename(buffer.125 = Legend)
miamiHighwaysbuffer.25 <- 
  rbind(
    st_union(st_buffer(miamiHighways, 402.336)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))
miamiHighwaysbuffer.25<-miamiHighwaysbuffer.25%>%
  rename(buffer.25 = Legend)
miamiHighwaysbuffer.5 <- 
  rbind(
    st_union(st_buffer(miamiHighways, 804.672)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))
miamiHighwaysbuffer.5<-miamiHighwaysbuffer.5%>%
  rename(buffer.5 = Legend)

Miami_Houses <- st_join(Miami_Houses, miamiHighwaysbuffer.125, join = st_within)
Miami_Houses <- st_join(Miami_Houses, miamiHighwaysbuffer.25, join = st_within)
Miami_Houses <- st_join(Miami_Houses, miamiHighwaysbuffer.5, join = st_within)

Miami_Houses$Highwaydist <- ifelse(grepl("Unioned Buffer", Miami_Houses$buffer.125), Miami_Houses$Highwaydist<-".125",
                            ifelse(grepl("Unioned Buffer", Miami_Houses$buffer.25), Miami_Houses$Highwaydist<-".25",
                                   ifelse(grepl("Unioned Buffer", Miami_Houses$buffer.5), Miami_Houses$Highwaydist<-".5",Miami_Houses$Highwaydist<-"over .5")))

#main roads
miamimainroads <- 
  rbind(
    st_read("tl_2019_12_prisecroads/tl_2019_12_prisecroads.shp") %>%
      st_transform('EPSG:6346'))
miamimainroads <- st_set_crs(miamimainroads,6346)
mainroads.miami.intersect <- st_intersects(miami, miamimainroads)
miamimainroads <- miamimainroads[mainroads.miami.intersect[[1]],]

miamimainroadsbuffer.125 <- 
  rbind(
    st_union(st_buffer(miamimainroads, 201.168)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))
miamimainroadsbuffer.125<-miamimainroadsbuffer.125%>%
  rename(buffer.mainroads.125 = Legend)
miamimainroadsbuffer.25 <- 
  rbind(
    st_union(st_buffer(miamimainroads, 402.336)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))
miamimainroadsbuffer.25<-miamimainroadsbuffer.25%>%
  rename(buffer.mainroads.25 = Legend)
miamimainroadsbuffer.5 <- 
  rbind(
    st_union(st_buffer(miamimainroads, 804.672)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))
miamimainroadsbuffer.5<-miamimainroadsbuffer.5%>%
  rename(buffer.mainroads.5 = Legend)

Miami_Houses <- st_join(Miami_Houses, miamimainroadsbuffer.125, join = st_within)
Miami_Houses <- st_join(Miami_Houses, miamimainroadsbuffer.25, join = st_within)
Miami_Houses <- st_join(Miami_Houses, miamimainroadsbuffer.5, join = st_within)

Miami_Houses$mainroadsdist <- ifelse(grepl("Unioned Buffer", Miami_Houses$buffer.mainroads.125), Miami_Houses$mainroadsdist<-".125",
                                   ifelse(grepl("Unioned Buffer", Miami_Houses$buffer.mainroads.25), Miami_Houses$mainroadsdist<-".25",
                                          ifelse(grepl("Unioned Buffer", Miami_Houses$buffer.mainroads.5), Miami_Houses$mainroadsdist<-".5",Miami_Houses$mainroadsdist<-"over .5")))

# Neighborhoods
Miami_Houses <- st_join(Miami_Houses, nhoods, join = st_within)

# Middle schools
miami.middleschools <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/dd2719ff6105463187197165a9c8dd5c_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.middleschools <- st_set_crs(miami.middleschools,6346)
miami.middleschools <- miami.middleschools[,3]%>%rename(midschool = NAME)
Miami_Houses <- st_join(Miami_Houses, miami.middleschools, join = st_within)

# school district
miami.schooldistricts <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/bc16a5ebcdcd4f3e83b55c5d697a0317_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.schooldistricts <- st_set_crs(miami.schooldistricts,6346)
miami.schooldistricts <- miami.schooldistricts[,2]%>%
  rename(schooldist = ID)
Miami_Houses <- st_join(Miami_Houses, miami.schooldistricts, join = st_within)

# CDD
miami.CDD <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/3a6112890351490faad6f75779a3d4f6_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.CDD <- st_set_crs(miami.CDD,6346)
miami.CDD <- miami.CDD[,2]
Miami_Houses <- st_join(Miami_Houses, miami.CDD, join = st_within)

# police boundary
miami.police <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/6d5ada3d95ed4cf2bedcac0b2cd9421a_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.police <- st_set_crs(miami.police,6346)
miami.police <- miami.police[,2:3]

Miami_Houses <- st_join(Miami_Houses, miami.police, join = st_within)



# libraries
miami.libraries <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/ab490a5cefd04c12b6b5e53a6b60f41c_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.libraries <- st_set_crs(miami.libraries,6346)
st_c <- st_coordinates


Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    library_nn1 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami.libraries)), 1),
    library_nn2 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami.libraries)), 2))

Miami_TestPPP <-as.ppp(st_centroid(Miami_Test))
Miami_TrainingPPP<-as.ppp(st_centroid(Miami_Training))
N <- nncross(st_centroid(Miami_Test), st_centroid(Miami_Training), what="which",k=1)   

#important!
Miami_Test$nnHouse1 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=1)
Miami_Test$nnHouse2 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=2)
Miami_Test$nnHouse3 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=3)
Miami_Test$nnHouse4 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=4)
Miami_Test$nnHouse5 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=5)
Miami_Training$ID <- 1:1819
f<- Miami_Training$SalePrice[Miami_Training$ID==Miami_Test$nnHouse1]
Miami_Test$Price1 <-apply(Miami_Test, 1, f)   
Miami_Training$SalePrice[Miami_Training$ID==Miami_Test$nnHouse1]Miami_Training$SalePrice[Miami_Training$ID==Miami_Test$nnHouse1]Miami_Training$SalePrice[Miami_Training$ID==Miami_Test$nnHouse1]Miami_Test$Price1 <-Miami_Training$SalePrice[Miami_Training$ID==Miami_Test$nnHouse1]


price <- function(data) {
  price1<-Miami_Training$SalePrice[Miami_Training$ID==data]
  return(price1)
}

Miami_Test$price1 <- lapply(Miami_Test$nnHouse1,price)
Miami_Test$price2 <- lapply(Miami_Test$nnHouse2,price)
Miami_Test$price3 <- lapply(Miami_Test$nnHouse3,price)
Miami_Test$price4 <- lapply(Miami_Test$nnHouse4,price)
Miami_Test$price5 <- lapply(Miami_Test$nnHouse5,price)

Miami_Test$price1 <- as.numeric(Miami_Test$price1)
Miami_Test$price2 <- as.numeric(Miami_Test$price2)
Miami_Test$price3 <- as.numeric(Miami_Test$price3)
Miami_Test$price4 <- as.numeric(Miami_Test$price4)
Miami_Test$price5 <- as.numeric(Miami_Test$price5)
Miami_Test$SalePriceAvg <- (Miami_Test$price1 +Miami_Test$price2+Miami_Test$price3+Miami_Test$price4+Miami_Test$price5)/5
for(j in 1:length(Miami_Test)){
  Miami_Test$Price1<-Miami_Training$SalePrice[Miami_Training$ID==Miami_Test$nnHouse1[j]]}

rownames(N) <- Miami_TestPPP$Folio
NXY <- nncross(st_centroid(Miami_Test), st_centroid(Miami_Training), k=3)


# daycares
miami.daycares <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/3ea3c3aa067549ff8f8a8ab80a3cbcbb_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.daycares <- st_set_crs(miami.daycares,6346)
st_c <- st_coordinates


Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    daycare_nn2 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami.daycares)), 2))

# Colleges
miami.colleges <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/7db056c406b943dc8f3f377b99d77588_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.colleges <- st_set_crs(miami.colleges,6346)
st_c <- st_coordinates


Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    colleges_nn3 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami.colleges)), 3))

# contaminated sites

miami.contamination <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/43750f842b1e451aa0347a2ca34a61d7_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.contamination <- st_set_crs(miami.contamination,6346)
st_c <- st_coordinates


Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    contamination_nn3 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami.contamination)), 3))


#  private schools
miami.pschool <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/7fecb87ea1b1494eb2beb13906465de9_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.pschool <- st_set_crs(miami.pschool,6346)
st_c <- st_coordinates


Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    pschool_nn3 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami.pschool)), 3))

# hopsitals
miami.hospitals <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/0067a0e8b40644f980afa23ad34c32c4_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.hospitals <- st_set_crs(miami.hospitals,6346)
st_c <- st_coordinates


Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    hospitals_nn3 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami.hospitals)), 3))

# marinas
miami.marinas <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/f65dec3bacb341f094dd5109e93c4247_0.geojson") %>%
      st_transform('EPSG:6346'))
miami.marinas <- st_set_crs(miami.marinas,6346)
st_c <- st_coordinates


Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    marinas_nn2 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miami.marinas)), 2))

#Census Data
Miami_Houses <- st_join(Miami_Houses,tracts18miami, join=st_within)

# house attributes

Miami_Houses$Pool <- ifelse(grepl("Pool", Miami_Houses$XF1), Miami_Houses$Pool<-"yes",
                            ifelse(grepl("Pool", Miami_Houses$XF2), Miami_Houses$Pool<-"yes",
                                          ifelse(grepl("Pool", Miami_Houses$XF3), Miami_Houses$Pool<-"yes",Miami_Houses$Pool<-"no")))

Miami_Houses$Patio <- ifelse(grepl("Patio", Miami_Houses$XF1), Miami_Houses$Patio<-"yes",
                            ifelse(grepl("Patio", Miami_Houses$XF2), Miami_Houses$Patio<-"yes",
                                   ifelse(grepl("Patio", Miami_Houses$XF3), Miami_Houses$Patio<-"yes",Miami_Houses$Patio<-"no")))

Miami_Houses$Carport <- ifelse(grepl("Carport", Miami_Houses$XF1), Miami_Houses$Carport<-"yes",
                            ifelse(grepl("Carport", Miami_Houses$XF2), Miami_Houses$Carport<-"yes",
                                   ifelse(grepl("Carport", Miami_Houses$XF3), Miami_Houses$Carport<-"yes",Miami_Houses$Carport<-"no")))

Miami_Houses$Whirlpool <- ifelse(grepl("Whirlpool", Miami_Houses$XF1), Miami_Houses$Whirlpool<-"yes",
                            ifelse(grepl("Whirlpool", Miami_Houses$XF2), Miami_Houses$Whirlpool<-"yes",
                                   ifelse(grepl("Whirlpool", Miami_Houses$XF3), Miami_Houses$Whirlpool<-"yes",Miami_Houses$Whirlpool<-"no")))

Miami_Houses$Dock <- ifelse(grepl("Dock", Miami_Houses$XF1), Miami_Houses$Dock<-"yes",
                            ifelse(grepl("Dock", Miami_Houses$XF2), Miami_Houses$Dock<-"yes",
                                   ifelse(grepl("Dock", Miami_Houses$XF3), Miami_Houses$Dock<-"yes",Miami_Houses$Dock<-"no")))



# Load Open streets map data
miami.base <- 
  st_read("https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson") %>%
  filter(NAME == "MIAMI BEACH" | NAME == "MIAMI") %>%
  st_union()
xmin = st_bbox(miami.base)[[1]]
ymin = st_bbox(miami.base)[[2]]
xmax = st_bbox(miami.base)[[3]]  
ymax = st_bbox(miami.base)[[4]]

ggplot() +
  geom_sf(data=miami.base, fill="black") +
  geom_sf(data=st_as_sfc(st_bbox(miami.base)), colour="red", fill=NA)
bars <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("bar", "pub", "restaurant")) %>%
  osmdata_sf()

bars <- 
  bars$osm_points %>%
  .[miami.base,]

bars <- st_transform(bars,6346)
bars <- st_set_crs(bars,6346)
st_c <- st_coordinates


Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    bars_nn2 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(bars)), 2))

ggplot() +
  geom_sf(data=miami.base, fill="black") +
  geom_sf(data=bars, colour="red", size=.75) 

#spatial lag house value
Miami_Houses$SalePriceLag<-ifelse(Miami_Houses$SalePrice==0, Miami_Houses$SalePriceLag<-NA, Miami_Houses$SalePriceLag <-Miami_Houses$SalePrice)
Miami_Houses.centroids<-st_centroid(Miami_Houses)
coords.test <- st_centroid(st_geometry(Miami_Houses), of_largest_polygon=TRUE)
coords <-  st_coordinates(Miami_Houses.centroids)
neighborList <- knn2nb(knearneigh(coords.test, 5))
spatialWeights <- nb2listw(neighborList, style="W")
Miami_Houses$lagPrice <- lag.listw(spatialWeights, Miami_Houses$SalePrice)


#spatial lag house size
Miami_Houses.centroids<-st_centroid(Miami_Houses)
coords.test <- st_centroid(st_geometry(Miami_Houses), of_largest_polygon=TRUE)
coords <-  st_coordinates(Miami_Houses.centroids)
neighborList <- knn2nb(knearneigh(coords.test, 5))
spatialWeights <- nb2listw(neighborList, style="W")
Miami_Houses$lagLot <- lag.listw(spatialWeights, Miami_Houses$LotSize)

#spatial lag lot size
Miami_Houses.centroids<-st_centroid(Miami_Houses)
coords.test <- st_centroid(st_geometry(Miami_Houses), of_largest_polygon=TRUE)
coords <-  st_coordinates(Miami_Houses.centroids)
neighborList <- knn2nb(knearneigh(coords.test, 5))
spatialWeights <- nb2listw(neighborList, style="W")
Miami_Houses$lagSQ <- lag.listw(spatialWeights, Miami_Houses$ActualSqFt)

# coordinates

Miami_Houses.centroidss <-st_centroid(Miami_Houses)
Miami_Houses <- Miami_Houses %>%
  mutate(lat = unlist(map(Miami_Houses.centroidss$geometry,1)),
         long = unlist(map(Miami_Houses.centroidss$geometry,2)))


Lat/Long
Miami_Houses.centroidss <-st_centroid(Miami_Houses)
Miami_Houses <- Miami_Houses %>%
  mutate(lat = unlist(map(Miami_Houses.centroidss$geometry,1)),
         long = unlist(map(Miami_Houses.centroidss$geometry,2)))


# training data
Miami_Training <- subset(Miami_Houses, toPredict %in% 0)

#categorical variabls
Miami_Training <-st_drop_geometry(Miami_Training) 
Miami_Training %>% 
  dplyr::select(SalePrice, Pool,Patio,Carport,Whirlpool,Dock) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of House Attributes", y = "Mean_Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()
Miami_Training %>% 
  dplyr::select(SalePrice, neighborhood) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of Neighborhood", y = "Mean_Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()
Miami_Training %>% 
  dplyr::select(SalePrice, Highwaydist) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of Distance to Highway", y = "Mean_Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()

Miami_Training %>% 
  dplyr::select(SalePrice, schooldist,midschool) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of School Boundaries", y = "Mean_Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()
Miami_Training %>% 
  dplyr::select(SalePrice, Zoning) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of School Boundaries", y = "Mean_Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()
Miami_Training %>% 
  dplyr::select(SalePrice, Zoning) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of School Boundaries", y = "Mean_Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()

Miami_Training_numeric<-Miami_Training[,c("Folio","SalePrice","Property.Zip","Year","Zoning", "LotSize","Bed","Bath","Stories", "YearBuilt",                 
                                                            "ActualSqFt", "neighborhood", "TotalPop", "FemaleBachelors","MaleBachelors","MedHHInc",             
                                           "MedRent","pctWhite","pctBachelors","pctPoverty", "crime_nn2","Pool", "Patio",               
                                           "Carport", "Whirlpool","Dock", "beachDist","waterDist" , "ParksDist" ,"Starbucks_nn1", "GolfCourseDist",       
                                           "Metros_nn1","Highwaydist","midschool","schooldist", "geometry","daycare_nn2","colleges_nn3", "contamination_nn3","pschool_nn3",          
                                           "hospitals_nn3","marinas_nn2","bars_nn2", "lagSQ", "lagLot","SalePriceAvg"  )]


numericVars <- 
  select_if(Miami_Training_numeric, is.numeric) %>% na.omit()
numericVars <-st_drop_geometry(numericVars)
cor(numericVars)

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 



reg.training <- lm(SalePrice ~ ., data = st_drop_geometry(Miami_Training) %>% 
                     dplyr::select(SalePrice,lagSQ,lagLot,Dock,Whirlpool,Carport,Patio,
                                   Pool,pctPoverty,pctBachelors,pctWhite,MedRent,MedHHInc,
                                   marinas_nn2,hospitals_nn3,pschool_nn3,contamination_nn3,
                                   colleges_nn3,daycare_nn2,schooldist,midschool,neighborhood,SalePriceAvg,
                                   Highwaydist,Metros_nn1,GolfCourseDist,waterDist,beachDist,
                                   crime_nn2,ActualSqFt,YearBuilt,Stories,Bath,Bed,LotSize,Zoning
                     ))

reg.training2 <- lm(SalePrice ~ ., data = st_drop_geometry(Miami_Training) %>% 
                     dplyr::select(SalePrice,lagSQ,SalePrice,lagSQ,Dock,Whirlpool,Carport,Patio,
                                   Pool,MedHHInc,
                                   hospitals_nn3,contamination_nn3,
                                   daycare_nn2,midschool,neighborhood,
                                   Highwaydist,
                                   crime_nn2,ActualSqFt,YearBuilt,Stories,LotSize,Zoning,SalePriceAvg
                     ))
#SalePrice,lagSQ,Dock,Whirlpool,Carport,Patio,
#Pool,MedHHInc,
#hospitals_nn3,contamination_nn3,
#daycare_nn2,midschool,neighborhood,
#Highwaydist,
#crime_nn2,ActualSqFt,YearBuilt,Stories,LotSize,Zoning,SalePriceAvg

reg.training3 <- lm(SalePrice ~ ., data = st_drop_geometry(Miami_Training) %>% 
                      dplyr::select(SalePrice,lagSQ,Dock,Whirlpool,Carport,Patio,
                                    Pool,pctPoverty,pctBachelors,pctWhite,MedRent,MedHHInc,
                                    marinas_nn2,hospitals_nn3,pschool_nn3,contamination_nn3,
                                    colleges_nn3,daycare_nn2,schooldist,midschool,neighborhood,SalePriceAvg,
                                    Highwaydist,Metros_nn1,GolfCourseDist,waterDist,beachDist,
                                    crime_nn2,ActualSqFt,YearBuilt,Stories,Bath,Bed,LotSize,Zoning
                      ))
reg1 <- lm(SalePrice ~ ., data = Miami_Training %>% 
             dplyr::select(SalePrice,lagSQ,lagLot,Dock,Whirlpool,Carport,Patio,
                           Pool,pctPoverty,pctBachelors,pctWhite,MedRent,MedHHInc,
                           marinas_nn2,hospitals_nn3,pschool_nn3,contamination_nn3,
                           colleges_nn3,daycare_nn2,schooldist,midschool,neighborhood,
                           Highwaydist,Metros_nn1,GolfCourseDist,waterDist,beachDist,
                           crime_nn2,ActualSqFt,YearBuilt,Stories,Bath,Bed,LotSize,Zoning))

#(SalePrice,lagSQ,lagLot,Dock,Whirlpool,Carport,Patio,
#Pool,pctPoverty,pctBachelors,pctWhite,MedRent,MedHHInc,
#marinas_nn2,hospitals_nn3,pschool_nn3,contamination_nn3,
#colleges_nn3,daycare_nn2,schooldist,midschool,neighborhood,
#Highwaydist,Metros_nn1,GolfCourseDist,waterDist,beachDist,
#crime_nn2,ActualSqFt,YearBuilt,Stories,Bath,Bed,LotSize,Zoning)
#nearest neighbor


Miami_Houses.centroids<-st_centroid(Miami_Houses)
coords.test <- st_centroid(st_geometry(Miami_Houses), of_largest_polygon=TRUE)
coords <-  st_coordinates(Miami_Houses.centroids)
neighborList <- knn2nb(knearneigh(coords.test, 5))
spatialWeights <- nb2listw(neighborList, style="W")
Miami_Houses$lagPrice <- lag.listw(spatialWeights, Miami_Houses$SalePrice)

#Create Walkscore
Miami_Houses <- Miami_Houses %>%
 
WalkScore = getWS(2848967,579756.3,"a0a34de0dd2261f99677763bb3861e33")
Miami_Houses <-sapply(Miami_Houses$long,Miami_Houses$lat,"a0a34de0dd2261f99677763bb3861e33",getWS())

mapview(Miami_Houses)

# internal characteristics
Miami_Training_internal<-Miami_Training[,c("Bed","LotSize",
                                          "Bath","YearBuilt","Stories","ActualSqFt",
                                          
                                                     
                                                "Pool",                 
                                          "Patio",                "Carport",               "Whirlpool",            
                                          "Dock")]
# amenities characteristics
Miami_Training_spatial<-Miami_Training[,c("Property.City","Zoning",
                                           
                                      
                                           "crime_nn2",            
                                                    
                                                  "neighborhood" ,                   
                                                 
                                           "contamination_nn3",           
                                                                     
                                           "MedHHInc",            
                                           "MedRent",               "pctWhite",              "pctBachelors",         
                                           "pctPoverty",                                           "lagLot" ,              
                                           "lagSQ")]

# amenities characteristics
Miami_Training_amenities<-Miami_Training[,c(
                                                 
                                           "beachDist",            "waterDist" ,         "GolfCourseDist",        "Metros_nn1",           
                                           "Highwaydist"   ,                "midschool"  ,          
                                           "schooldist" ,          "daycare_nn2"  ,         "colleges_nn3",        
                                           "pschool_nn3",          "hospitals_nn3",        
                                           "marinas_nn2"          )]

stargazer(output, output2, type = "html", add.lines = list(c("Fixed effects?", "No", "No"),c("Results believable?", "Maybe", "Try again later")))
table1 <-stargazer(Miami_Training_internal)
t1 <- stargazer(
  Miami_Training_internal, type = "text",
  summary.stat = c("min", "p25", "median", "p75", "max", "median", "sd")
)
t1

# important training!
inTrain <- createDataPartition(
  y = paste(Miami_Training$neighborhood), 
  p = .60, list = FALSE)
Miami.training <- Miami_Training[inTrain,] 
Miami.test <- Miami_Training[-inTrain,]  

reg.training <- lm(SalePrice ~ ., data = st_drop_geometry(Miami_Training) %>% 
                     dplyr::select(SalePrice,lagSQ,lagLot,Dock,Whirlpool,Carport,Patio,
                                   Pool,pctPoverty,pctBachelors,pctWhite,MedRent,MedHHInc,
                                   marinas_nn2,hospitals_nn3,pschool_nn3,contamination_nn3,
                                   colleges_nn3,daycare_nn2,schooldist,midschool,neighborhood2,SalePriceAvg,
                                   Highwaydist,Metros_nn1,GolfCourseDist,waterDist,beachDist,
                                   crime_nn2,ActualSqFt,YearBuilt,Stories,Bath,Bed,LotSize,Zoning))

Miami.test <-
  Miami.test %>%
  mutate(SalePrice.Predict = predict(reg.training, Miami.test),
         SalePrice.Error = SalePrice.Predict - SalePrice,
         SalePrice.AbsError = abs(SalePrice.Predict - SalePrice),
         SalePrice.APE = (abs(SalePrice.Predict - SalePrice)) / SalePrice.Predict)%>%
  filter(SalePrice < 5000000)

mean(Miami.test$SalePrice.AbsError, na.rm = T)

mean(Miami.test$SalePrice.APE, na.rm = T)

# automate the test

vars <- c("SalePrice","lagSQ","neighborhood","Dock","ActualSqFt","YearBuilt","LotSize","Zoning","SalePriceAvg")
          

N <- list(1,2,3,4,5,6,7,8)
comb <- sapply(N, function(m) combn(x=vars[2:9], m))

comb2 <- list()
k=0
for(i in seq(comb)){
  tmp <- comb[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    comb2[[k]] <- formula(paste("SalePrice", "~", paste(tmp[,j], collapse=" + ")))
  }
}

fitControl <- trainControl(method = "cv", number = 100)
set.seed(825)
two <-2
MAE.matrix <- matrix(NA, nrow = length(comb2), ncol = two)

for(j in 1:length(comb2)){
  reg.cv <- 
    train(comb2[[j]], data=Miami_Training, 
          method = "lm", trControl = fitControl, na.action = na.pass)
  MAE.matrix[j,1]<-mean(reg.cv$resample[,3])
  MAE.matrix[j,2]<- paste(comb2[j])
}

# automate the test

vars <- c("SalePrice", "marinas_nn1","marinas_nn2","marinas_nn3","marinas_nn4","contamination_nn1",
          "contamination_nn2","contamination_nn3","contamination_nn4","hospitals_nn1","hospitals_nn2",
          "hospitals_nn3","hospitals_nn4","pschool_nn1","pschool_nn2","pschool_nn3","pschool_nn4","colleges_nn1",
          "colleges_nn2","colleges_nn3","colleges_nn4","YearBuilt","LotSize","Bed","Units")

N <- list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
comb <- sapply(N, function(m) combn(x=vars[2:25], m))

comb2 <- list()
k=0
for(i in seq(comb)){
  tmp <- comb[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    comb2[[k]] <- formula(paste("SalePrice", "~", paste(tmp[,j], collapse=" + ")))
  }
}

fitControl <- trainControl(method = "cv", number = 100)
set.seed(825)
two <-2
MAE.matrix <- matrix(NA, nrow = length(comb2), ncol = two)

for(j in 1:length(comb2)){
  reg.cv <- 
    train(comb2[[j]], data=st_drop_geometry(Miami_Houses), 
          method = "lm", trControl = fitControl, na.action = na.pass)
  MAE.matrix[j,1]<-mean(reg.cv$resample[,3])
  MAE.matrix[j,2]<- paste(comb2[j])
}

#automate test 3
vars <- c("SalePrice", "AdjustedSqFt", "LotSize","YearBuilt",
          "marina_nn2","beachDist")

N <- list(1,2,3,4,5)
comb <- sapply(N, function(m) combn(x=vars[2:6], m))

comb2 <- list()
k=0
for(i in seq(comb)){
  tmp <- comb[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    comb2[[k]] <- formula(paste("SalePrice", "~", paste(tmp[,j], collapse=" + ")))
  }
}

fitControl <- trainControl(method = "cv", number = 100)
set.seed(825)
two <-2
MAE.matrix <- matrix(NA, nrow = length(comb2), ncol = two)
MAE.output <- data.frame()

MAE.output <- data.frame(matrix(unlist(comb2), nrow=length(comb2), byrow=T),stringsAsFactors=FALSE)%>%
  rename(comb=matrix.unlist.comb2...nrow...length.comb2...byrow...T.)
MAE.output$MAE <-train(as.character(MAE.output$comb),data=st_drop_geometry(Miami_Houses),method = "lm",trControl=fitControl,na.action=na.pass)

for(j in 1:length(comb2)){
  reg.cv <- 
    train(comb2[[j]], data=st_drop_geometry(Miami_Houses), 
          method = "lm", trControl = fitControl, na.action = na.pass)
  MAE.matrix[j,1]<-mean(reg.cv$resample[,3])
  MAE.matrix[j,2]<- paste(comb2[j])
}

fun1 <- function(x, column){
  train(x[[column]],data=st_drop_geometry(Miami_Houses), 
        method = "lm", trControl = fitControl, na.action = na.pass)
}


neighborhoodtable <-table(Miami_Training$Pool)

ggplot(data.frame(Miami_Training$Zoning), aes(x=Miami_Training$Zoning)) +
  geom_bar()+
  labs(title = "Frequency of Zoning", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()


#test

#create training data
Miami_Houses$Bed <-factor(Miami_Houses$Bed)
Miami_Houses$Bath <-factor(Miami_Houses$Bath)
Miami_Houses$Stories<-factor(Miami_Houses$Stories)
Miami_Houses$neighborhood<-factor(Miami_Houses$neighborhood)
Miami_Houses$Hiqhwaydist <-factor(Miami_Houses$Highwaydist)
Miami_Houses$Pool <-factor(Miami_Houses$Pool)
Miami_Houses$Patio <-factor(Miami_Houses$Patio)
Miami_Houses$Dock <-factor(Miami_Houses$Dock)
Miami_Houses$Carport <-factor(Miami_Houses$Carport)
Miami_Houses$Whirlpool <-factor(Miami_Houses$Whirlpool)
Miami_Houses$schooldist <-factor(Miami_Houses$schooldist)
Miami_Houses$midschool <-factor(Miami_Houses$midschool)


Miami_Training <- subset(Miami_Houses, toPredict %in% 0)

#create test data

`%nin%` = Negate(`%in%`)
Miami_Test <- subset(Miami_Houses,toPredict %nin% 0)


reg.training <- lm(SalePrice ~ ., data = st_drop_geometry(Miami_Training) %>% 
                     dplyr::select(SalePrice,lagSQ,lagLot,Dock,Whirlpool,Carport,
                                   Pool,pctPoverty,MedRent,neighborhood,
                                   marinas_nn2,hospitals_nn3,pschool_nn3,
                                   daycare_nn2,midschool,
                                   Highwaydist,Metros_nn1,waterDist,beachDist,
                                   crime_nn2,ActualSqFt,YearBuilt,Stories,Bath,Bed,LotSize,Zoning
                                   ))
Miami_Test <-
  Miami_Test %>%
  mutate(SalePrice.Predict = predict(reg.training, Miami_Test),
         SalePrice.Error = SalePrice.Predict - SalePrice,
         SalePrice.AbsError = abs(SalePrice.Predict - SalePrice),
         SalePrice.APE = (abs(SalePrice.Predict - SalePrice)) / SalePrice.Predict)%>%
  filter(SalePrice < 5000000)

mean(Miami_Test$SalePrice.AbsError, na.rm = T)
mean(Miami_Test$SalePrice.APE, na.rm = T)
