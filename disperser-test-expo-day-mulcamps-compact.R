library(ncdf4)
library(data.table)
library(tidyverse)
library(parallel)
library(sf)
library(viridis)
library(ggplot2)
library(scales)
library(ggsn)
library(gridExtra)
library(ggmap)
library(ggrepel)
library(fst)
library( USAboundaries)
library(raster)

exp_day_unit_grids.c <- read.csv(file = './exp_day_unit_multi_gridsc.csv')
names(exp_day_unit_grids.c) <- sub("^X", "", names(exp_day_unit_grids.c))

# create raster object
campfire_day.r <- rasterFromXYZ( exp_day_unit_grids.c,
                                 crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")

# NA for disperseR means 0
campfire_day.r[is.na( campfire_day.r)] <- 0




# create sf polygon object
campfire_day.sp <- rasterToPolygons( campfire_day.r)
campfire_day.sf <- st_as_sf( campfire_day.sp)



# melt back to long format
campfire_day.sf.m <-
  as.data.table( campfire_day.sf) %>%
  melt( id.vars = 'geometry',
        variable.name = 'month',
        value.name = 'N')

# download some US data
states <- USAboundaries::us_states()

campfire_day.sf.m$geometry <-
  st_transform( campfire_day.sf.m$geometry,
                crs = st_crs( states))

#################################################
##normalize the N values
#################################################

MEAN <- mean(campfire_day.sf.m$N)

SD <- sd(campfire_day.sf.m$N)

campfire_day.sf.m$normalize <- (campfire_day.sf.m$N - MEAN)/SD

campfire_day.sf.m$month <- substring(campfire_day.sf.m$month, 2)
campfire_day.sf.m$date <- as.Date(campfire_day.sf.m$month, format="%Y%m%d")

#################################################
# create the plot

ny_states <- states[states$statefp == "36", ]

ny_bbox <- st_bbox(ny_states)

campfire_gg <-
  ggplot( ) +
  # add state coundaries
  
  # add the disperser grid
  geom_sf( data = campfire_day.sf.m[date %in% as.Date( c(
                                                  '2018-11-23',
                                                  '2018-11-24',
                                                  '2018-11-25',
                                                  '2018-11-26',
                                                  '2018-11-27',
                                                  '2018-11-28'))],
           aes( fill = normalize   , geometry = geometry),
           alpha = .75, color = NA) +
  geom_sf( data = ny_states,
           aes( geometry = geometry),
           color = 'black',
           inherit.aes = FALSE, fill=NA) +
  # change the fill & color scale
  scale_fill_viridis( 
                      limits = c( 0, 4), 
                      breaks = c( 0, 2, 4),
                      labels = c( '0', '2', '4'),
                      oob = scales::squish) +

  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # create panels for each day
  facet_wrap( . ~ date, ncol = 3) +
  # set boundaries over NY
  # coord_sf( xlim = c( -130, -70), ylim = c( 25, 50)) +
  coord_sf( xlim = c( -79.76212, -71.85621), ylim = c( 40.50244, 45.01468)) +
  # set thematic elements
  theme_minimal() +
  labs(title = "Hyads Campfire exposure in New York (2018)",
       fill = expression("Unitless"),
       x = NULL,
       y = NULL)+
  theme( axis.title = element_text( size = 12),
         axis.text = element_blank(),
         strip.text = element_text( size = 12),
         legend.position = 'bottom')

campfire_gg



