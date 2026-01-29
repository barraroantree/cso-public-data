# -------------------------------------------------------------------
# R script to pull and analyse reported crime data from the CSO website
# -------------------------------------------------------------------




# Packages required for this script
# -------------------------------------------------------------------

# # Install packages (run this once)
# remotes::install_github("r-arcgis/arcgis", dependencies = TRUE)
# devtools::install_github("ifellows/ROSM/OpenStreetMap")


# Load packages
library(csodata)
library(xtable)
library(readxl)
library(data.table) # recommended by https://stata2r.github.io/data_table/ 
library(RColorBrewer)


# Load packages for mapping
library(tmap)
library(tmaptools)
library(tmaptools)
library(raster)
library(ggmap)
# library(rgeos)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(sf)
library(osmdata)
library(arcgis)

library(openxlsx)



# map plot options
# ------------------------------------------------------------------------------
sf_use_s2(FALSE)
tmap_mode("plot")

# Paths  
# ------------------------------------------------------------------------------

# set output directory  
setwd("/Users/barraroantree/Data/moddata/crime data/")            # macOS



# load Garda district and division boundaries 
# ------------------------------------------------------------------------------

furl <- "https://services1.arcgis.com/eNO7HHeQ3rUcBllm/arcgis/rest/services/GardaDistricts/FeatureServer/0/"
flayer <- arc_open(furl)
flayer
garda.districts <- arc_select(flayer)

garda.districts.dublin <- garda.districts %>% 
  filter(COUNTY=="DUBLIN")


# load Garda station data  
# ------------------------------------------------------------------------------
furl <- "https://services1.arcgis.com/eNO7HHeQ3rUcBllm/arcgis/rest/services/GardaStations/FeatureServer/0"
flayer <- arc_open(furl)
flayer
garda.stations <- arc_select(flayer)

# identify districts in garda.stations 
garda.districts.tomerge <- garda.districts %>%
  select("District_N":"Area_Sq_Mi","Pop_by_Dis","Housholds_") %>%
  rename(District_Area_Sq_km = Area_Sq_km,
         District_Area_Sq_Mi = Area_Sq_Mi,
         District_Pop = Pop_by_Dis,
         District_Nhh = Housholds_)


garda.stations <- st_join(garda.stations, garda.districts.tomerge, join = st_within, left=TRUE) # Perform the spatial join

# create Dublin version 
garda.stations.dublin <- garda.stations %>% 
  filter(COUNTY=="DUBLIN")

# check national population
garda.districts.population <- garda.districts.tomerge %>%
  st_set_geometry(NULL) %>%
  group_by(District_N) %>%
  summarise(mean(District_Pop)) 
  

garda.state.population <-garda.districts.population %>%
  summarise(sum(`mean(District_Pop)`)) 


# ------------------------------------------------------------------------------
# Plot location of garda stations with district boundaries overlaid
# ------------------------------------------------------------------------------

ggplot() +
  geom_sf() +
  geom_sf(data = garda.stations.dublin, aes(color =STATION_TY), size = 1) +
  geom_sf(data = garda.districts.dublin, fill = "transparent", color = "red") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank()) # Position legend at the top in one row without a title

ggsave("dublin_garda_stations.png")


# ------------------------------------------------------------------------------
# Load crime data 
# ------------------------------------------------------------------------------

# from https://data.cso.ie/table/CJA07 
data.crimes <- cso_get_data("CJA07", include_ids = TRUE)

# split Garda.Station into parts
data.crimes <- data.crimes %>%
  separate(Garda.Station, into = c("Station_Name", "Division"), sep = ",", extra = "merge", fill = "right") %>%
  separate(Station_Name, into = c("Station Code", "STATION_NA"), sep = " ", extra = "merge", fill = "right") %>%
    mutate(`STATION_NA` = toupper(`STATION_NA`))
head(data.crimes)



# rename a few stations that differ from garda.stations 
# STEPASIDE doesn't exist in crimes database, probably because not really a station...
data.crimes <- data.crimes %>%
  mutate(STATION_NA = ifelse(STATION_NA == "BRIDEWELL DUBLIN", "BRIDEWELL", STATION_NA)) %>%
  mutate(STATION_NA = ifelse(STATION_NA == "DUBLIN AIRPORT", "AIRPORT", STATION_NA)) %>%
  mutate(Division = ifelse(Division == " Co Dublin, D.M.R. Eastern Division", " D.M.R. Eastern Division", Division))   



# 2024 recorded crimes nationally, by type 
# ------------------------------------------------------------------------------

# filter to 2024 recorded crime data 
data.crimes.2024 <- data.crimes %>%
  select(STATION_NA, `Station Code`, Division, Type.of.Offence, `2024`) %>%
  pivot_wider(names_from = `Type.of.Offence`, values_from = `2024`)

glimpse(data.crimes.2024)



# 2024 recorded crimes per Dublin district, by type 
# ------------------------------------------------------------------------------

# merge in district info for Dublin stations from garda.stations.dublin
garda.stations.dublin.districtinfo <- garda.stations.dublin %>%
  select(STATION_NA,starts_with("District")) %>%
  st_set_geometry(NULL) 




# Calculate total crimes per station in Dublin 
# ------------------------------------------------------------------------------


# Calculate total offences by station for each year from 2003 to 2023
data.crimes.total <- data.crimes %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Offences") %>%
  group_by(`STATION_NA`, Year) %>%
  summarise(Total.Offences = sum(Offences, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Year`, values_from = `Total.Offences`) %>%
  rename_with(~ paste0("total_", .), starts_with("20"))
head(data.crimes.total)


# filter version to just stations in the DMR regions 
data.crimes.dublin <- data.crimes %>%
  mutate(Division = gsub("D\\.M\\.R.", "DMR", Division)) %>%
  filter(grepl("DMR", Division))
  
head(data.crimes.dublin)
glimpse(data.crimes.dublin)

# Calculate total offences by station for each year from 2003 to 2023
data.crimes.dublin.total <- data.crimes.dublin %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Offences") %>%
  group_by(`STATION_NA`, Year) %>%
  summarise(Total.Offences = sum(Offences, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Year`, values_from = `Total.Offences`) 
head(data.crimes.dublin.total)


# Add a station called STEPASIDE with 0 offences in each year thanks to Shane Ross
years <- as.character(2003:2023)
stepaside_data <- data.frame(STATION_NA = "STEPASIDE", matrix(0, nrow = 1, ncol = length(years)))
colnames(stepaside_data)[2:ncol(stepaside_data)] <- years
data.crimes.dublin.total <- bind_rows(data.crimes.dublin.total, stepaside_data)

# rename year variables with prefix total 
data.crimes.dublin.total <- data.crimes.dublin.total %>%
  rename_with(~ paste0("total_", .), starts_with("20"))


# merge total crimes by station name to garda.stations.dublin
data.crimes.totalbystation <- left_join(garda.stations.dublin,data.crimes.dublin.total) 
data.crimes.totalbystation <- st_set_geometry(data.crimes.totalbystation, NULL) 


# ------------------------------------------------------------------------------
# Calculate and plot crimes per 1,000 by district
# ------------------------------------------------------------------------------

# merge in data for 2024 
data.crimes.totalbydistrict <- data.crimes.totalbystation  %>%
  select("STATION_NA":"STN_CODE", "District_N", "District_C", "total_2003":"total_2024") %>%
  group_by(District_C) %>%
  summarise(across(`total_2003`:`total_2024`, sum, na.rm = TRUE))


# merge back into garda.district dataset and plot by district level 
garda.districts.dublin.withcrimes <- left_join(garda.districts.dublin,data.crimes.totalbydistrict)  %>%
  mutate(across(starts_with("total_"), ~ . / Pop_by_Dis * 1000, .names = "{col}_pc"))

# and export minus geo
garda.districts.dublin.withcrimesnogeo <- garda.districts.dublin.withcrimes %>%
  st_drop_geometry()
write.csv(garda.districts.dublin.withcrimesnogeo, "crimes_by_DMRdistrict.csv", row.names = FALSE)


# plot for 2024
# ------------------------------------------------------------------------------

# example at https://cengel.github.io/R-spatial/mapping.html 


ggplot(garda.districts.dublin.withcrimes)+
  geom_sf(aes(fill = `total_2024`)) +
  scale_fill_distiller(type = "div", palette = "OrRd", direction=1) +
  labs(fill = "Total") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank())
ggsave("dublin_crimes_2024_district.png")




ggplot(garda.districts.dublin.withcrimes)+
  geom_sf(aes(fill = `total_2024_pc`)) +
  scale_fill_gradient2(
    low = "green", 
    mid = "orange", 
    high = "red", 
    midpoint = 0
  ) +
  labs(fill = "Total/1,000 pop") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank())

  


ggplot(garda.districts.dublin.withcrimes)+
  geom_sf(aes(fill = `total_2024_pc`)) +
  scale_fill_distiller(type = "div", palette = "OrRd", direction=1, trans="log10") +
  labs(fill = "Total/1,000 pop") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank())




ggplot(garda.districts.dublin.withcrimes)+
  geom_sf(aes(fill = `total_2024_pc`)) +
  scale_fill_gradient(low = "orange", high = "red") +
  labs(fill = "Total/1,000 pop") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank())


ggsave("dublin_crimesp1000_2024_district.png")


# ends 
