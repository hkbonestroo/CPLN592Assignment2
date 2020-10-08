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
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E",
                                            "B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2018, state=12, county="Miami-Dade County", geometry=T, output="wide") %>%
  st_transform('EPSG:6346')%>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
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

Miami_Houses.centroids <-st_centroid(Miami_Houses)
Miami_Houses <-
  Miami_Houses %>%
  mutate(neighborhood= st_join(Miami_Houses.centroids, nhoods, join = st_within)%>%st_drop_geometry())

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

Miami_Houses.centroids <-st_centroid(Miami_Houses)

Miami_Houses <-
  Miami_Houses %>% 
  mutate(
    beachDist = st_distance(Miami_Houses.centroids, miamibeach))

# pool
Miami_Houses$Pool <- ifelse(grepl("Pool", Miami_Houses$XF1), Miami_Houses$Pool<-"yes",
                            ifelse(grepl("Pool", Miami_Houses$XF2), Miami_Houses$Pool<-"yes",
                                          ifelse(grepl("Pool", Miami_Houses$XF3), Miami_Houses$Pool<-"yes",Miami_Houses$Pool<-"no")))



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



