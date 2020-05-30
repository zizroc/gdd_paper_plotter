#This script plots Figure 1 for ERL paper.
#January 2020
#Marcus Thomson
#NCEAS & IIASA (formerly)

library(tidyverse)
library(raster)
library(sf)
library(spData)
library(cowplot)

dem_dir <- dir("/home/thomson/Data/DEM/PRISM_800m_DEM/", 
               full.names = TRUE)
#Elevation map of Utah
#DEM from PRISM climate group
# Refrence: PRISM Climate Group, Oregon State University, http://prism.oregonstate.edu, created 15 Jan 2020
dem_tm1 <- dem_dir[grep("bil.bil", dem_dir)]
dem_tm2 <- dem_tm1[!grepl("aux.xml", dem_tm1)]
dem_ras <- raster(dem_tm2)
crs(dem_ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

dem_ut <- crop(dem_ras, c(xmin = -114.5, 
                          xmax = -107.5, 
                          ymin = 36.5, 
                          ymax = 42.5))

dem_spdf <- as(dem_ut, "SpatialPixelsDataFrame")
dem_df <- as.data.frame(dem_spdf)
colnames(dem_df) <- c("values", "x", "y")

# Reference: https://gis.utah.gov/data/boundaries/citycountystate/
cp_path <- path.expand("/home/thomson/Data/Shape/CP")
gb_path <- path.expand("/home/thomson/Data/Shape/GB")
ut_path <- path.expand("/home/thomson/Data/Shape/Utah")

cp_sh <- read_sf(dsn = cp_path, layer = "Flowline_CO14_NSI")
gb_sh <- read_sf(dsn = gb_path, layer = "Flowline_GB16_NSI")
ut_sh <- read_sf(dsn = ut_path, layer = "Utah")

gb_Sh = st_transform(gb_sh, crs = 4269)
cp_Sh = st_transform(cp_sh, crs = 4269) #2163
ut_Sh = st_transform(ut_sh, crs = 4269) #2163

us_states_proj = st_transform(us_states, crs = 4269)


UT_shape <- data.frame(x = c(-114, -109, -109, -111, -111, -114), 
                       y = c(37, 37, 41, 41, 42, 42))

#These data come from the Canadian Archaeological Radiocarbon Database (CARD).
#Reference: Martindale, Andrew, Richard Morlan, Matthew Betts, Michael Blake, Konrad Gajewski, Michelle Chaput, Andrew Mason, and Pierre Vermeersch (2016) Canadian Archaeological Radiocarbon Database (CARD 2.1), accessed January 10, 2020.
#Range Creek data come from Prehistoric Archaeology in Range Creek Canyon, Utah: A Summary of Activities of the Range Creek Field Station.
#Reference: Boomgarden, S. A., Metcalfe, D., & Springer, C. (2014). Prehistoric archaeology in range Creek canyon, Utah: a summary of the activities of the range Creek field station. Utah Archaeology, 27(1), 9-32.
source("/home/thomson/Scripts/Fremont_radiocarbon_age_calibration.R") #CalAges

site_loc <- CalAges %>% 
  filter(year_CE > 0, 
         longitude > -115) %>% 
  group_by(latitude, longitude, significance) %>% 
  summarize(count = n())

site_names <- data.frame(names = c("GSL", "Parowan Valley", "Range Creek", "Uinta Basin", "Uinta Mountains", "Sevier Valley", "Five Finger Ridge", "Fremont River"), 
                         lon = c(-112.208224, -112.807833, -110.128215, -109.909544, -110.1, -112.079377, -112.342431, -110.948731), 
                         lat = c(41.472336, 37.938423, 39.331768, 40.192731, 40.704514, 38.706294, 38.573012, 38.349220)) %>% 
  arrange(lat) %>% 
  mutate(index = seq(1:8))

m1 <- ggplot(data = site_loc) + 
  geom_tile(data = dem_df, 
            aes(x = x, 
                y = y, 
                fill = values), 
            alpha = 0.7, 
            show.legend = TRUE) + 
  scale_fill_gradient(low = "black", 
                      high = "white", 
                      name = "Elevation \n(m asl)") +
  geom_sf(data = us_states_proj,
          fill = "transparent") + 
  # geom_polygon(data = UT_shape, 
  #              aes(x = x, y = y), 
  #              inherit.aes = FALSE, 
  #              alpha = 0.1, 
  #              colour = "white") + 
  geom_point(data = site_loc, 
             aes(x = longitude, 
                 y = latitude, 
                 shape = significance), 
             size = 3, 
             alpha = 0.6, 
             colour = "darkblue") + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 17), 
                     name = "CARD site \nsignificance") + 
  geom_text(data = site_names, 
            aes(x = lon, y = lat, label = index), 
            colour = "white", 
            size = 5) + 
  labs(
    x = "degrees west longitude", 
    y = "degrees north latitude"
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "left", 
    legend.key = element_rect(fill = "#ffffff", 
                              color = "#ffffff"), 
    legend.box.background = element_rect(fill = "lightgrey")
  ) + 
  xlim(-114.25, -107) + 
  ylim(36.75, 43)


# library(spData)



#Shapefile data from the Utah AGRC
# Reference: https://gis.utah.gov/data/boundaries/citycountystate/
cp_path <- path.expand("/home/thomson/Data/Shape/CP")
gb_path <- path.expand("/home/thomson/Data/Shape/GB")
ut_path <- path.expand("/home/thomson/Data/Shape/Utah")

cp_sh <- read_sf(dsn = cp_path, layer = "Flowline_CO14_NSI")
gb_sh <- read_sf(dsn = gb_path, layer = "Flowline_GB16_NSI")
ut_sh <- read_sf(dsn = ut_path, layer = "Utah")

gb_Sh = st_transform(gb_sh, crs = 4269)
cp_Sh = st_transform(cp_sh, crs = 4269) #2163
ut_Sh = st_transform(ut_sh, crs = 4269) #2163

us_states_proj = st_transform(us_states, crs = 4269)

#This will the be inset map
m2 <- ggplot() + 
  geom_sf(data = us_states_proj,
          fill = "white") + 
  geom_sf(data = gb_Sh %>%
            st_combine,
          colour = "#C8CBE2",
          alpha = 0.7) +
  geom_sf(data = cp_Sh %>%
            st_combine,
          colour = "#E2C0B1",
          alpha = 0.5) +
  geom_sf(data = us_states_proj,
          fill = "transparent",
          alpha = 0.2) +
  xlim(-116, -107) + 
  ylim(36.25, 42.5) + 
  geom_text(data = data.frame(x = c(-114, -109.5), 
                              y = c(40, 38.5), 
                              names = c("Great \n \tBasin", "Colorado \n \tPlateau")), 
            aes(x, y, label = names), 
            size = 4) + 
  theme_minimal() + 
  # geom_point(data = site_loc, 
  #            aes(x = longitude, 
  #                y = latitude, 
  #                shape = significance), 
  #            alpha = 0.5, 
  #            size = 1, 
  #            colour = "darkblue") + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme(
    legend.position = "none", 
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )

gg_inset_map = ggdraw() +
  draw_plot(m1) +
  draw_plot(m2, x = 0.55, y = 0.72, width = 0.30, height = 0.30)


png(file="/home/thomson/ERL_paper/Plots/Figure1_updated.png", w = 1800, h = 1700, res=300)
grid.newpage()
v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2 <- viewport(width = 0.38, height = 0.30, x = 0.77, y = 0.84) #plot area for the inset map
print(m1, vp=v1) 
print(m2, vp=v2)
dev.off()
  

