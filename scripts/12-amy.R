#load packages
library(ggplot2)
library(ggspatial)
library(gganimate)
library(tidyverse)
library(dplyr)
library(sf)
library(glue)
library(mapview)
library(tmaptools)
library(maptiles)
library(basemaps)
library(ggtext)
library(tmap)
library(prettymapr)
library(RAQSAPI)
library(gifski)
library(png)
library(systemfonts)

# function for filtering
`%notin%` <- Negate(`%in%`)

#remove irrelevant counties
exclude_counties <- c("Los Angeles", "Orange", "Imperial", "Ventura", "Santa Barbara", "Kern")
objectids <- c(182, 160,  91,  45,  53,  55,  81, 164, 181, 186, 135, 147, 154,
               66, 147, 141, 138, 156, 160, 175, 13, 165, 139, 145, 144, 153,
               163, 171, 162, 184,  22, 9,  96, 59)

#city data
socalcities <- st_read("https://maps.scag.ca.gov/scaggis/rest/services/OpenData/City_Boundaries/MapServer/0/query?outFields=*&where=1%3D1&f=geojson")


mapcities <- c("Banning","Beaumont","Calimesa","Corona","Eastvale","Jurupa Valley", "Hesperia",
               "Moreno Valley","Norco","Perris","Riverside","San Bernardino", "La Verne",
               "San Jacinto","Chino","Chino Hills","Colton", "Lake Elsinore","Wildomar","Covina",
               "Fontana","Grand Terrace","Highland","Loma Linda","Montclair", "Hesperia","Baldwin Park",
               "Ontario","Rancho Cucamonga","Redlands", "Claremont","Pomona", "Diamond Bar", "San Dimas")

socalcities <- socalcities %>%
  dplyr::select(-created_user,-created_date,-last_edited_date,-last_edited_user, -YEAR) %>%
  dplyr::filter(CITY %in% mapcities)

#scag disadvantaged communities layer
disadvantaged <- st_read("https://maps.scag.ca.gov/scaggis/rest/services/OpenData/DisadvantagedCommunities/MapServer/0/query?outFields=*&where=1%3D1&f=geojson")


objIDs_disadvantage <- c(1580, 239,272,225,284, 1579, 1581, 1593, 1592,388, 144,
                         118, 145, 123, 117, 273, 271, 116, 146, 142, 319, 1583, 1590,
                         338, 141, 143, 1584, 1573, 1588, 1574, 1595, 313, 318, 1589, 
                         1585,1578,294, 1575, 302, 1591, 295, 303, 1576, 297, 298, 326, 1577)
disadvantage <- disadvantaged %>%
  dplyr::filter(County %in% c("Riverside", "San Bernardino") & OBJECTID %notin% objIDs_disadvantage)


#read in  warehouse data from Robert Redford Conservancy
warehouses <- st_read("https://services.arcgis.com/hVnyNvwbpFFPDV5j/arcgis/rest/services/Warehouses424/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")


objs_warehouses <- c("Desert Hot Springs","Murrieta","Banning",
                     "unincorporated", "Hesperia", "Victorville", "Apple Valley", "Needles",
                     "Catherdral City", "Coachella", "Adelanto", "Lucerne Valley", "Barstow",
                     "French Valley", "Temecula", "Hemet", "Yucaipa", "Yucca Valley",
                     "Warm Springs", "Oak Hills", "Palm Springs", "Cathedral City", "Twentynine Palms")

warehouses <- warehouses %>%
  filter(county %notin% exclude_counties & year_built %notin% c(2024, 2025)
         & year_built >= 2013 & place_name %notin% objs_warehouses)



options(gganimate.nframes = 100)  



labels <- data.frame(Label = c("**Over 700 new warehouses have been built<br>in the Inland Empire over the last ten years,
                               with<br>a pronounced footprint in<span style='color:slateblue'> disadvantaged areas.**</span>",
                               "**<span style='font-size:3.5pt'>Amy Solano<br>30 Day Map Challenge Day 12 - Time and Space<br>Sources: City Boundaries, Southern California Association of Governments,
                               <br>Warehouses, Robert Redford Conservancy at Pitzer College</span>**"),
                     Lat = c(34.248153069811775, 33.60427774911162),
                     Lon = c(-118.10516410125591, -118.10516410125591))
labels.sf <- st_as_sf(labels, coords = c('Lon', 'Lat'), crs=4326)

warehousemap <- ggplot() +
  annotation_map_tile("https://a.basemaps.cartocdn.com/dark_nolabels/${z}/${x}/${y}.png") +
  geom_sf(data = socalcities, color = "transparent", fill = "transparent") +
  geom_sf(data = warehouses, color = "goldenrod1", lwd = 0.9, aes(group = year_built)) +
  geom_sf(data = disadvantage, fill = alpha("slateblue", 0.25), color = alpha("slategray2", 1), lwd = 0.2) +
  shadow_mark(lwd = 0.5) +
  transition_time(as.integer(year_built)) +
  enter_fade() + enter_grow() +
  ease_aes("linear") +
  geom_richtext(data = labels.sf, aes(label = Label, geometry = geometry),
                stat = "sf_coordinates", size = 2, color = "gray90", hjust = 0,
                family = "Helvetica", lineheight = 1.2, fill = NA, label.color = NA) +
  labs(title = "**<span style='font-size:6pt'>YEAR:</span><br>{frame_time}**", 
       subtitle = "<br><br><br>**NEW WAREHOUSES IN THE<br>INLAND EMPIRE, 2013-2023**") +
  theme(
    plot.title = element_markdown(
      margin = margin(120, 0,-135,0),
      hjust = 0.9, family = "Tahoma", color = "goldenrod1", size = 15
    ),
    plot.subtitle = element_markdown(
      margin = margin(0,0,-38,0), 
      hjust = 0.05, family = "Futura", color = "gray90", size = 11
    ),
    plot.background=element_blank(),panel.background = element_blank(),
    axis.title.y = element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), panel.border = element_blank(),
    axis.title=element_blank(),
    plot.margin=unit(c(-70,0,-40,0), "pt"))
  

animate(warehousemap, height = 4, width=4.8, units= "in", res=160)
anim_save("12-amy.gif")



