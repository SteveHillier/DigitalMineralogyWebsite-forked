library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(e1071)
library(plyr)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

load("data/AfSIS_sub_pt.Rdata")

for (i in 1:length(AfSIS_sub_pt)) {
  
  AfSIS_sub_pt[[i]] <- AfSIS_sub_pt[[i]][which(AfSIS_sub_pt[[i]][[1]] > 6),]
  
}

AfSIS_props <- read.csv(file = "data/AfSIS_reference_updated.csv",
                        header = TRUE, stringsAsFactors = FALSE)

#Remove the SAMPLE_ID currently in the data
AfSIS_props$SAMPLE_ID <- NULL

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

names(AfSIS_sub_pt) <- substrRight(names(AfSIS_sub_pt), 12)
names(AfSIS_sub_pt) <- substring(names(AfSIS_sub_pt), 1, nchar(names(AfSIS_sub_pt))-3)

#convert all upper to lower case
names(AfSIS_sub_pt) <- tolower(names(AfSIS_sub_pt))

#remove these XRD patterns which don't have property data
AfSIS_sub_pt <- AfSIS_sub_pt[-which(!names(AfSIS_sub_pt) %in% AfSIS_props$SSN)]

AfSIS_props <- AfSIS_props[which(AfSIS_props$SSN %in% names(AfSIS_sub_pt)), ]

AfSIS_props <- AfSIS_props[order(AfSIS_props$SSN), ]

AfSIS_sub_pt <- AfSIS_sub_pt[order(names(AfSIS_sub_pt))]

table(AfSIS_props$SSN == names(AfSIS_sub_pt))

#Create an sf dataframe
AfSIS_sf <- st_as_sf(AfSIS_props,
                         coords = c("Longitude", "Latitude"),
                         crs = "+proj=longlat +init=epsg:4326")


#Create a dataframe of continuous soil property variables
v <- sapply(AfSIS_props, class)

AfSIS_props_cont <- AfSIS_props[, which(v == "numeric" | v == "integer")]

#Remove the
AfSIS_props_cont <- AfSIS_props_cont[, -which(names(AfSIS_props_cont) %in% c("SAMPLE_ID",
                                                                        "Cluster",
                                                                        "Plot",
                                                                        "Latitude",
                                                                        "Longitude"))]

#Remove all the mineral concentrations because I don't think they're accurate
AfSIS_props_cont <- AfSIS_props_cont[, -c(59:97)]
AfSIS_props_cont <- data.frame("SSN" = AfSIS_props$SSN,
                               AfSIS_props_cont,
                               stringsAsFactors = FALSE)

#remove_these <- which(AfSIS_props_cont$Ca <= 0.1605 |
#                        AfSIS_props_cont$m3.B > 10 |
#                        AfSIS_props_cont$m3.B <= 0.001 |
#                        AfSIS_props_cont$Mn <= 0.0155 |
#                        AfSIS_props_cont$Ni <= 0.0025 |
#                        AfSIS_props_cont$m3.Cu <= 0.001 |
#                        AfSIS_props_cont$Cu <= 0.001 |
#                        AfSIS_props_cont$m3.Zn <= 0.001 |
#                        AfSIS_props_cont$m3.Zn >= 23.8 |
#                        AfSIS_props_cont$Zn <= 0.002)

#AfSIS_props_cont <- AfSIS_props_cont[-remove_these, ]

#AfSIS_sub_pt <- AfSIS_sub_pt[-remove_these]

# AfSIS_sf <- AfSIS_sf[-remove_these, ]

