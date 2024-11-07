#load packages
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(dplyr)
library(sf)
library(mapview)
library(tmaptools)
library(maptiles)
library(basemaps)
library(ggtext)
library(prettymapr)


# function for filtering
`%notin%` <- Negate(`%in%`)

socalcities <- st_read("https://maps.scag.ca.gov/scaggis/rest/services/OpenData/City_Boundaries/MapServer/0/query?outFields=*&where=1%3D1&f=geojson")
#link to data used https://gisdata-scag.opendata.arcgis.com/datasets/SCAG::city-boundaries-scag-region/explore?location=33.741329%2C-117.572345%2C8.79

countiesrel <- c("Imperial", "Ventura")
objectids <- c(182, 160,  91,  45,  53,  55,  81, 164, 181, 186, 135, 147, 154,
               66, 147, 141, 138, 156, 160, 175, 13, 165, 139, 145, 144, 153,
               163, 171, 162, 184,  22, 9,  96, 59)

socalcities <- socalcities %>%
  dplyr::select(-created_user,-created_date,-last_edited_date,-last_edited_user) %>%
  dplyr::filter(COUNTY %notin% countiesrel & OBJECTID %notin% objectids) %>%
  mutate(commute_stop = case_when(CITY == "Jurupa Valley" ~ "Jurupa Valley",
                                  CITY == "Baldwin Park" ~ "Baldwin Park",
                                  CITY == "Perris" ~ "Perris"),
         commute_stop = replace_na(commute_stop, "none"))


timelabels <- data.frame(
  Label = c("**4am:  \nLeave Perris**","45 minutes  \n26 miles", "**4:45am: Arrive Jurupa**  \nDrop me off",
            "1 hour  \n33 miles","**6am: Arrive Baldwin Park**  \nBegin 10 hour shift"),
  Lat = c(33.80739932218187, 33.99911478105432, 33.913907454040874, 34.12232579774237,34.015804),
  Lon = c(-117.0901450575651, -117.2037073938029, -117.5446605490479,  -117.74500845071265, -118.024433)
)
timelabels.sf <- st_as_sf(timelabels, coords = c('Lon', 'Lat'), crs=4326)

routecoords <- data.frame(
  Lat = c(
    33.80235087495649, 33.905940316001015, 33.91703173091917, 33.91703173091917,
    33.9174014198903, 33.93329652822231, 33.94327566162914, 33.95436221576789,
    33.95103640116657, 33.95620983437987, 33.96655575715472,
    33.997675087470384, 34.02087497450734, 34.024277782270666, 34.031082988492116,
    34.026546244983905, 34.02711335118749, 34.062266532829874, 34.06340026379305,
    34.06793503596789, 34.070769145334054, 34.07303636456621, 34.06453397958866
  ),
  Lon = c(
    -117.23330048856477, -117.27695868528393, -117.29121442298815, -117.32952671806822,
    -117.32908122626499, -117.34690089839525, -117.35224680003435, -117.36249311150924,
    -117.43599925904662, -117.44446360330849, -117.4649562262583,
    -117.49697848044251, -117.51096071639644, -117.5581753409047, -117.66560571840903,
    -117.71761023236014, -117.7484023787786, -117.79493273336645, -117.80861813177464,
    -117.83598892859105, -117.86746534492988, -117.94341930609535, -117.98447550131995
  )
)
routepoints.sf <- st_as_sf(routecoords, coords = c('Lon', 'Lat'), crs=4326)
routepoints.sf$name <- "name"

routemap <- routecoords %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("LINESTRING")


bbox <- st_bbox(socalcities)
part1 <- data.frame(
  x1 = 33.91703173091917,
  y1 = -117.32952671806822,
  x2 = 33.99911478105432,
  y2 = -117.2037073938029
)

part2 <- data.frame(
  x1 = 34.12232579774237,
  y1 = -117.74500845071265,
  x2 = 34.062266532829874,
  y2 = -117.79493273336645
)


ggplot() +
  annotation_map_tile("https://a.basemaps.cartocdn.com/dark_nolabels/${z}/${x}/${y}.png") +
  geom_sf(data = routemap, color = "slateblue4", linewidth = 1.8, alpha = 0.8) +
  geom_sf(data = socalcities, aes(fill = commute_stop), color = "grey20", lwd=0.1)+
  scale_fill_manual(values=c("goldenrod1","papayawhip",
                             "transparent","lightgoldenrod1"))+
  geom_spatial_segment(data = part1, aes(x = y1, y = x1, xend = y2, yend = x2),crs=4326,
               size = 1, alpha = 0.8, lineend = "round", color="slateblue4") +
  geom_spatial_segment(data = part2, aes(x = y1, y = x1, xend = y2, yend = x2),crs=4326,
                       size = 1, alpha = 0.8, lineend = "round", color="slateblue4") +
  geom_richtext(data = timelabels.sf, aes(label = Label, geometry = geometry), 
                stat = "sf_coordinates", size = 2.5, color = "white", family = "sans", lineheight = 1,
                fill = NA, label.color = NA) +
  labs(title="**The Road to the American Dream**",
       subtitle="My parents made this commute from <span style = 'color:#ffec8b;'>**Perris**</span> to <span style = 'color:#daa520;'>**Baldwin Park**</span> every day for 3 years.",
       caption = "Amy Solano  \n30 Day Map Challenge  \nDay 5 - A Journey  \nSource: City Boundaries, Southern California Association of Governments")+
  theme_void() +
  theme(
    plot.title = element_markdown(size = 16, family = "sans", color = "white"),
    plot.subtitle = element_markdown(size = 8, family = "sans", color = "white"),
    plot.caption = element_markdown(size = 6, color = "grey80", family = "sans", hjust=0),
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = NA)
  )

