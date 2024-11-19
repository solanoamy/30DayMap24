#load packages
library(tidyverse)
library(dplyr)
library(sf)
library(leaflet.providers)
library(mapview)
library(tmaptools)
library(maptiles)
library(basemaps)
library(tmap)
library(systemfonts)
library(grid)


#load in population characteristics data from robert redford conservancy https://socal-sustainability-atlas-claremont.hub.arcgis.com/datasets/claremont::ces-population-characteristics-percentile/about
popchars <- st_read("https://services.arcgis.com/hVnyNvwbpFFPDV5j/arcgis/rest/services/CES_Test/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

# function for filtering
`%notin%` <- Negate(`%in%`)

#keep only certain counties
popchars <- popchars %>% 
  filter(County %in% c("Riverside", "San Bernardino")) %>%
  st_make_valid()
  
socalcities <- st_read("https://maps.scag.ca.gov/scaggis/rest/services/OpenData/City_Boundaries/MapServer/0/query?outFields=*&where=1%3D1&f=geojson")

mapcities <- c("Beaumont","Calimesa","Corona","Eastvale","Jurupa Valley", 
               "Moreno Valley","Norco","Perris","Riverside","San Bernardino", 
               "San Jacinto","Chino","Chino Hills","Colton", "Upland","Rialto",
               "Fontana","Grand Terrace","Highland","Loma Linda","Montclair",
               "Ontario","Rancho Cucamonga","Redlands", "Claremont","Pomona")

IEcities <- socalcities %>%
  dplyr::select(-created_user,-created_date,-last_edited_date,-last_edited_user, -YEAR) %>%
  dplyr::filter(CITY %in% mapcities | OBJECTID ==168)

biggercitieslist <- c("Banning","Beaumont","Calimesa","Corona","Eastvale","Jurupa Valley", "Hesperia",
                      "Moreno Valley","Norco","Perris","Riverside","San Bernardino", "La Verne",
                      "San Jacinto","Chino","Chino Hills","Colton", "Lake Elsinore","Wildomar","Covina",
                      "Fontana","Grand Terrace","Highland","Loma Linda","Montclair","Baldwin Park",
                      "Ontario","Rancho Cucamonga","Redlands", "Claremont","Pomona", "Diamond Bar", "San Dimas")

mapcities <- socalcities %>%
  dplyr::filter(CITY %in% biggercitieslist)


boundary <- sfheaders::sf_remove_holes(st_union(IEcities))
boundary_poly <- st_cast(boundary, "POLYGON", do_split = FALSE)
IEboundary <- st_sf(boundary_poly)

#remove certain communities
filtered_socal <- st_filter(popchars, IEboundary) %>%
  filter(FID %notin% c(2846,4147,3777,2749,2877,3772,3709, 4157) & TotPop19 > 0)


disadvantaged <- st_read("https://maps.scag.ca.gov/scaggis/rest/services/OpenData/DisadvantagedCommunities/MapServer/0/query?outFields=*&where=1%3D1&f=geojson")


objIDs_disadvantage <- c(1580, 239,272,225,284, 1579, 1581, 1593, 1592,388, 144,
                         118, 145, 123, 117, 273, 271, 116, 146, 142, 319, 1583, 1590,
                         338, 141, 143, 1584, 1573, 1588, 1574, 1595, 313, 318, 1589, 
                         1585,1578,294, 1575, 302, 1591, 295, 303, 1576, 297, 298, 326, 1577)
disadvantage <- disadvantaged %>%
  dplyr::filter(County %in% c("Riverside", "San Bernardino") & OBJECTID %notin% objIDs_disadvantage)


allchars <- st_read("https://services.arcgis.com/hVnyNvwbpFFPDV5j/arcgis/rest/services/CES_Test/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
allchars <- allchars %>% st_make_valid() %>%
  filter(County %in% c("Riverside","San Bernardino","Los Angeles", "Santa Barbara",
                       "Orange", "Ventura", "San Diego","Imperial"))



map <- tm_basemap("CartoDB.PositronNoLabels") +  
  tm_shape(mapcities) +
  tm_polygons(fill = NA, col = NA) +
  tm_shape(allchars) +
  tm_polygons(fill = "wheat1", fill_alpha = 0.6, col = NA) +
  tm_shape(filtered_socal) +
  tm_polygons("DieselPM_P",  
              fill.scale = tm_scale_continuous(values = "carto.sunset"),
              fill.legend = tm_legend(title = "",
                                      position = tm_pos_in("right", "bottom"),
                                      orientation = "landscape", text.size = 0.6,
                                      frame = NA, text.color = "hotpink4",
                                      width = 8, height = 2),
              col = NA) +
  tm_shape(disadvantage)+
  tm_polygons("County", fill=NA,
              col="seashell2",lwd=0.9, fill.legend = tm_legend(show=FALSE))+
  tm_layout(frame = FALSE, asp=1.2)+
  tm_credits("Amy Solano  \n30 Day Map Challenge, Day 16  \nSources: City Boundaries, SCAG; \nCalEnviroScreen 4.0 \n", position=c("left", "bottom"), 
             size=0.5, color="grey40")

# grid.text because tmap wont let me bold my title
png("outputs/16-amy.png", width = 6, height = 4, units = "in", res = 300)

print(map)
grid.text("EXPOSURE TO DIESEL\nPARTICULATE MATTER\nEMISSIONS IN THE\nINLAND EMPIRE",
  x = 0.13, y = 0.86, just = "left",
  gp = gpar(fontfamily = "Tahoma", fontface = "bold", fontsize = 14, col = "hotpink4", lineheight=0.8))
grid.text("Many communities in the urban parts of\nRiverside and San Bernardino counties are\nin the upper percentiles of exposure to\nparticulate matter from diesel trucks.\nDisadvantaged areas (outlined in white) have\ngreater levels of exposure, meaning they are\nat a higher risk for the harmful effects\nof air pollution.", 
          x= 0.87, y = 0.75, just="right",
          gp=gpar(fontfamily = "SF Pro", fontface = "bold", fontsize = 6.5, 
                  col = "hotpink4", lineheight=0.8))

dev.off()



