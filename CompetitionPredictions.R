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
library(table1)
library(summarytools)
library(arsenal)
library(expss)
library(spatstat)
options(scipen=999)
options(tigris_class = "sf")


Miami_Houses <- 
  rbind(
    st_read("studentsData.geojson") %>%
      st_transform(st_crs('EPSG:6346')))
Miami_Houses <- st_set_crs(Miami_Houses, 6346)

# Load neighborhood boundaries

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


# Create column with neighborhoods name

Miami_Houses <- st_join(Miami_Houses, nhoods, join = st_within)

# fix neighborhoods with limited entries

Miami_Houses$neighborhood2 <- ifelse(grepl("East Grove", Miami_Houses$neighborhood),Miami_Houses$neighborhood2<-"North Grove",
                                     ifelse(grepl("Bird Grove West", Miami_Houses$neighborhood), Miami_Houses$neighborhood2<-"Bird Grove East",
                                            ifelse(grepl("Bay Heights", Miami_Houses$neighborhood), Miami_Houses$neighborhood2<-"North Grove",
                                                   ifelse(grepl("Vizcaya", Miami_Houses$neighborhood), Miami_Houses$neighborhood2<-"North Grove",
                                                          ifelse(grepl("Fair Isle", Miami_Houses$neighborhood), Miami_Houses$neighborhood2<-"North Grove",Miami_Houses$neighborhood2<-Miami_Houses$neighborhood)))))

# create columns for house attributes

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

# create training data

Miami_Training <- subset(Miami_Houses, toPredict %in% 0)

#create test data

`%nin%` = Negate(`%in%`)
Miami_Test <- subset(Miami_Houses,toPredict %nin% 0)

# finding average sale price of 5 nearest neighbors Test set

Miami_TestPPP <-as.ppp(st_centroid(Miami_Test))
Miami_TrainingPPP<-as.ppp(st_centroid(Miami_Training))

Miami_Test$nnHouse1 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=1)
Miami_Test$nnHouse2 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=2)
Miami_Test$nnHouse3 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=3)
Miami_Test$nnHouse4 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=4)
Miami_Test$nnHouse5 <- nncross(Miami_TestPPP, Miami_TrainingPPP, what="which",k=5)
Miami_Training$ID <- 1:2627

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

Miami_Test$SalePriceAvg <- (Miami_Test$price1+Miami_Test$price2+Miami_Test$price3+Miami_Test$price4+Miami_Test$price5)/5

# finding average sale price of 5 nearest neighbors Training Set

Miami_Training$nnHouse1 <- nncross(Miami_TrainingPPP, Miami_TrainingPPP, what="which",k=2)
Miami_Training$nnHouse2 <- nncross(Miami_TrainingPPP, Miami_TrainingPPP, what="which",k=3)
Miami_Training$nnHouse3 <- nncross(Miami_TrainingPPP, Miami_TrainingPPP, what="which",k=4)
Miami_Training$nnHouse4 <- nncross(Miami_TrainingPPP, Miami_TrainingPPP, what="which",k=5)
Miami_Training$nnHouse5 <- nncross(Miami_TrainingPPP, Miami_TrainingPPP, what="which",k=6)

price <- function(data) {
  price1<-Miami_Training$SalePrice[Miami_Training$ID==data]
  return(price1)
}

Miami_Training$price1 <- lapply(Miami_Training$nnHouse1,price)
Miami_Training$price2 <- lapply(Miami_Training$nnHouse2,price)
Miami_Training$price3 <- lapply(Miami_Training$nnHouse3,price)
Miami_Training$price4 <- lapply(Miami_Training$nnHouse4,price)
Miami_Training$price5 <- lapply(Miami_Training$nnHouse5,price)

Miami_Training$price1 <- as.numeric(Miami_Training$price1)
Miami_Training$price2 <- as.numeric(Miami_Training$price2)
Miami_Training$price3 <- as.numeric(Miami_Training$price3)
Miami_Training$price4 <- as.numeric(Miami_Training$price4)
Miami_Training$price5 <- as.numeric(Miami_Training$price5)

Miami_Training$SalePriceAvg <- (Miami_Training$price1+Miami_Training$price2+Miami_Training$price3+Miami_Training$price4+Miami_Training$price5)/5

# predicting values
reg.training <- lm(SalePrice ~ ., data = st_drop_geometry(Miami_Training) %>% 
                      dplyr::select(SalePrice,neighborhood2,SalePriceAvg,Dock,Zoning))

secret_preds3 <- predict(reg.training, newdata = Miami_Test)
output_preds3 <- data.frame(prediction = secret_preds, Folio = Miami_Test$Folio, team_name = "Bonestroo_Rawn")
write.csv(output_preds, "Bonestroo-Rawn.csv")
