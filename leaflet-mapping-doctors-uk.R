# TF 17/05/18
# Creating an interactive map to show how to visualise licensed doctor gender data for UK countries 

####
# Psuedocode ----
# 1. Load libraries
# 2. Load shapefiles and filter for UK countries 
# 3. Load doctor data and clean 
# 4. Join data to map
# 5. Build map components and final map 
####

####
# 1. Load libraries ----
library(leaflet); library(geojsonio); library(rgdal); library(sp);
library(dplyr); library(plyr); library(data.table); library(RColorBrewer);
library(raster); library(ggplot2); library(rgeos); library(readr);
library(mapproj); library(tictoc); library(ggmap); library(maps);
library(ggthemes); library(htmlwidgets); library(tidyr);
####
# 2. Load shapefiles and filter for UK countries ----

# All shapefiles for whole world 
world_world_countries_shapefile <- shapefile("shapefiles/ne_10m_admin_0_map_units.shp")

# Just a test to view the world shapefiles
# plot(world_countries_shapefile)

# One way to filter countries is by starting letter of country
uk_countries = subset(world_world_countries_shapefile, SUBUNIT %like% "England" | 
                        SUBUNIT%like% "Wales" |
                        SUBUNIT %like% "Scotland" |
                        SUBUNIT %like% "Northern Ireland")

# Not sure if this actually helps run the code, but seems to work
uk_countries <- spTransform(uk_countries, CRS("+proj=longlat +ellps=WGS84"))

####
# 3. Load doctor data and clean ----

# Read in data
doctor_data <- read.csv("data/doctors.csv")

# Select only 2017 data, necessary variables and put in wide format
doctor_data %>% 
  dplyr::filter(Year == 2017) %>% 
  dplyr::select(Country, Gender, Year, Number) %>%
  tidyr::spread(Gender, Number) -> doctor_data

# Remove comma's from numbers and specify as numeric
doctor_data$Male <- as.numeric(sub(",", "", doctor_data$Male, fixed = TRUE))
doctor_data$Female <- as.numeric(sub(",", "", doctor_data$Female, fixed = TRUE))

# Analyse which gender has the greatest number of doctors per country and print statement
# and add proportions based on total per country (assuming total is male plus female which isn't the case in real life)
doctor_data %>%
  mutate(majority_gender = ifelse(Male > Female, "Majority of doctors are male", 
                           ifelse(Female > Male, "Majority of doctors are female", "Gender Equality")))  %>%
  mutate(total = Male + Female) %>%
  mutate(Male_prop = (Male / total)*100) %>%
  mutate(Female_prop = (Female / total)*100) -> doctor_data

# Round proportions to 2.d.p
doctor_data$Male_prop <- format(round(doctor_data$Male_prop, 2), nsmall = 2)
doctor_data$Female_prop <- format(round(doctor_data$Female_prop, 2), nsmall = 2)

# General tidying
doctor_data$Country <- as.character(doctor_data$Country)
doctor_data <- droplevels(doctor_data)
doctor_data$majority_gender <- as.factor(doctor_data$majority_gender)

####
# 4. Join data to map ----
data_for_mapping <- sp::merge(uk_countries,
                              doctor_data,
                              by.x = 'SUBUNIT',
                              by.y = 'Country',
                              duplicateGeoms = TRUE)

leaflet(data_for_mapping) %>%
  addPolygons()




####
# 5. Build map components and final map ----

# Add fill colours for shapefiles based on a value, e.g. gender
map_pal = colorFactor(c('purple', '#4169e1'), data_for_mapping$majority_gender)

# Some text that will appear when we hover over each shapefile,
# this is data which will be wrapped in HTML to generate the tooltip effect 
hoverText <- sprintf("<div style='font-size:12px;width:200px;float:left'>
                     <span style='font-size:18px;font-weight:bold'>%s</span><br/> 
                     <div style='width:95%%'>
                     <span style='float:left'>Male</span>
                     <span style='float:right'>Female</span>
                     <br/>
                     <span style='color:black;float:left'>%s%%</span>
                     <span style='color:black;float:right'>%s%%</span><br clear='all'/>
                     <span style='background:#D4DCF7;width:%s%%;float:left'>&nbsp;</span>
                     <span style='background:#E7CCFC;width:%s%%;float:right'>&nbsp;</span>
                     </div>
                     <br/><span style='font-size:10px'>%s</span>
                     </div>",
                     data_for_mapping$SUBUNIT, 
                     data_for_mapping$Male_prop, data_for_mapping$Female_prop,
                     data_for_mapping$Male_prop, data_for_mapping$Female_prop,
                     data_for_mapping$majority_gender) %>%
  lapply(htmltools::HTML)

# Bringing it all together and generating the map 
leaflet(data_for_mapping,
        options=leafletOptions(attributionControl = FALSE, 
                               dragging = FALSE, zoomControl = FALSE, minZoom = 5.2, maxZoom = 5.2)) %>%
  addPolygons(fillColor=~map_pal(data_for_mapping$majority_gender),
              weight = 1,
              label = ~hoverText,
              color = "grey",
              labelOptions = labelOptions(
                offset = c(-100,-140),
                #direction='bottom',
                textOnly = T,
                style=list(
                  'background'='rgba(255,255,255,0.95)',
                  'border-color' = 'rgba(0,0,0,1)',
                  'border-radius' = '4px',
                  'border-style' = 'solid',
                  'border-width' = '4px')),
              highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)) %>%
  setMaxBounds(lat1 = 60, lng1 = 8.05, lat2 = 50, lng2 = -15.) %>%
  htmlwidgets::onRender(
    "function(el, t) {
    var myMap = this;
    // get rid of the ugly grey background
    myMap._container.style['background'] = '#ffffff';
    }") 
    


