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
nhoods <- rbind(nhoodsmiami,nhoodsmiamibeach)


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
    crime_nn1 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miamicrime)), 1),
    crime_nn2 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miamicrime)), 2), 
    crime_nn3 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miamicrime)), 3), 
    crime_nn4 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miamicrime)), 4), 
    crime_nn5 = nn_function(st_c(st_centroid(Miami_Houses)), st_c(st_centroid(miamicrime)), 5)) 

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
miamibeach <- st_set_crs(nhoodsmiami, 6346)

Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    beachDist = st_distance(Miami_Houses.centroids, miamibeach))

#water
miamiwater <-
  rbind(st_read("Water/miamiwater.shp")%>%
          st_transform('EPSG:6346'))
miamiwater <- st_set_crs(miamiwater,6346)
miamiwater <- st_union(miamiwater)

Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    waterDist = st_distance(Miami_Houses.centroids, miamiwater))

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

#Census Data
Miami_Houses <- st_join(Miami_Houses,tracts18miami, join=st_within)

# house attributes
Miami_Houses$Pool <- ifelse(grepl("Pool", Miami_Houses$XF1), Miami_Houses$Pool<-"yes",
                            ifelse(grepl("Pool", Miami_Houses$XF2), Miami_Houses$Pool<-"yes",
                                          ifelse(grepl("Pool", Miami_Houses$XF3), Miami_Houses$Pool<-"yes",Miami_Houses$Pool<-"no")))

Miami_Houses$Patio <- ifelse(grepl("Patio", Miami_Houses$XF1), Miami_Houses$Pool<-"yes",
                            ifelse(grepl("Patio", Miami_Houses$XF2), Miami_Houses$Pool<-"yes",
                                   ifelse(grepl("Patio", Miami_Houses$XF3), Miami_Houses$Pool<-"yes",Miami_Houses$Pool<-"no")))

Miami_Houses$Carport <- ifelse(grepl("Carport", Miami_Houses$XF1), Miami_Houses$Pool<-"yes",
                            ifelse(grepl("Carport", Miami_Houses$XF2), Miami_Houses$Pool<-"yes",
                                   ifelse(grepl("Carport", Miami_Houses$XF3), Miami_Houses$Pool<-"yes",Miami_Houses$Pool<-"no")))

Miami_Houses$Whirlpool <- ifelse(grepl("Whirlpool", Miami_Houses$XF1), Miami_Houses$Pool<-"yes",
                            ifelse(grepl("Whirlpool", Miami_Houses$XF2), Miami_Houses$Pool<-"yes",
                                   ifelse(grepl("Whirlpool", Miami_Houses$XF3), Miami_Houses$Pool<-"yes",Miami_Houses$Pool<-"no")))

Miami_Houses$Dock <- ifelse(grepl("Dock", Miami_Houses$XF1), Miami_Houses$Pool<-"yes",
                            ifelse(grepl("Dock", Miami_Houses$XF2), Miami_Houses$Pool<-"yes",
                                   ifelse(grepl("Dock", Miami_Houses$XF3), Miami_Houses$Pool<-"yes",Miami_Houses$Pool<-"no")))



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

ggplot() +
  geom_sf(data=miami.base, fill="black") +
  geom_sf(data=bars, colour="red", size=.75) 

# coordinates
Miami_Houses.centroidss <-st_centroid(Miami_Houses)
Miami_Houses <- Miami_Houses %>%
  mutate(lat = unlist(map(Miami_Houses.centroidss$geometry,1)),
         long = unlist(map(Miami_Houses.centroidss$geometry,2)))


# automate the test

vars <- c("SalePrice", "AdjustedSqFt", "LotSize","YearBuilt",
          "crime_nn2","beachDist")

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

for(j in 1:length(comb2)){
  reg.cv <- 
    train(comb2[[j]], data=st_drop_geometry(Miami_Houses), 
          method = "lm", trControl = fitControl, na.action = na.pass)
  MAE.output$MAE <- mean(reg.cv$resample[,3])
  MAE.matrix[j,1]<-mean(reg.cv$resample[,3])
  MAE.matrix[j,2]<- paste(comb2[j])
}



