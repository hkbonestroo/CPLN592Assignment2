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
    st_read("studentsData.geojson") %>% 
      st_transform('EPSG:6346')

# Miami Training Data
'%ni%' <- Negate('%in%')
Miami_Training <- subset(Miami_Houses, SalePrice %ni% 0)

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
miamicrime <-st_read("MiamiCrime/09-27-10-03-2020-miamicrime.shp")%>%
  st_transform('EPSG:6346')

# Load neighborhoods
nhoods <- 
  st_read("https://opendata.arcgis.com/datasets/2f54a0cbd67046f2bd100fb735176e6c_0.geojson") %>%
  st_transform('EPSG:6346')
#nhoodsmiamibeach <-
  st_read("miamibeach/miamibeachnb2.shp")%>%
  st_transform('EPSG:6346')
#nhoodsmiami <- nhoodsmiami[2]
#nhoodsmiamibeach <-nhoodsmiamibeach[1]%>%
  rename(LABEL = Name)
#nhoods <- rbind(nhoodsmiami,nhoodsmiamibeach)


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

