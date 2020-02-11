#This script finds site elevations for ERL paper.
#January 2020
#Marcus Thomson
#NCEAS & IIASA (formerly)

require(tidyverse)
require(raster)
#This package might require a new library. R will tell you in an error message.
#Use: sudo apt install libudunits2-dev 
require(elevatr)

dem_dir <- dir("/home/thomson/Data/DEM/PRISM_800m_DEM/", 
               full.names = TRUE)
radc_dir<- dir("/home/thomson/Data/CARD", 
               full.names = TRUE)

#These data come from the Canadian Archaeological Radiocarbon Database (CARD).
#Reference: Martindale, Andrew, Richard Morlan, Matthew Betts, Michael Blake, Konrad Gajewski, Michelle Chaput, Andrew Mason, and Pierre Vermeersch (2016) Canadian Archaeological Radiocarbon Database (CARD 2.1), accessed January 10, 2020.
df   <- read.csv(radc_dir[grep("allUTandFremont", radc_dir)], header = TRUE, stringsAsFactors = FALSE)

#These data come from Prehistoric Archaeology in Range Creek Canyon, Utah: A Summary of Activities of the Range Creek Field Station.
#Reference: Boomgarden, S. A., Metcalfe, D., & Springer, C. (2014). Prehistoric archaeology in range Creek canyon, Utah: a summary of the activities of the range Creek field station. Utah Archaeology, 27(1), 9-32.
df_rc<- read.csv(radc_dir[grep("Range Creek", radc_dir)], header = TRUE, stringsAsFactors = FALSE)

DF <- df %>% 
  dplyr::mutate(mean = Normalized.age, 
                sigma = NA.Sigma) %>% 
  dplyr::select(mean, 
                sigma, 
                Significance, 
                Latitude, 
                Longitude)

#Discriminates between "Fremont" and "non-Fremont" archaeological contexts.
#This filter uses the Significance IDs given in CARD. "Fremont" is defined as 
#anything "Fremont" and "Fremont?", and "Other" is any data satisfying the search 
#criteria used in CARD (i.e., mentions of "Fremont" and/or "Utah") that was not 
#included in "Fremont".
DF_fremont <- DF %>% 
  filter(Significance == "Fremont" | Significance == "Fremont?") %>% 
  na.omit() %>% 
  mutate(ID = "Fremont")

DF_other <- DF %>% 
  filter(Significance != "Fremont" & Significance != "Fremont?") %>% 
  na.omit() %>% 
  mutate(ID = "Other")

DF_reclass <- rbind(DF_fremont, 
                    DF_other) %>% 
  arrange(., rev(ID))

DF_tmp <- DF_reclass %>% 
  dplyr::select(mean, sigma, ID, Longitude, Latitude) %>% 
  rename("significance" = "ID", 
         "longitude" = "Longitude", 
         "latitude" = "Latitude")

#Adds data from Range Creek Fremont
#Site location arbitrarily chosen to be near geographic middle of Range Creek reach.
DF_rc <- df_rc %>% 
  rename("mean" = "Radiocarbon.age..mean.BP.", 
         "sigma" = "SE") %>% 
  mutate("significance" = "Fremont", 
         "longitude" = -110.217,
         "latitude" = 39.429) %>% 
  dplyr::select(mean, sigma, significance, longitude, latitude)

DF_ap <- rbind(DF_tmp, 
               DF_rc)
rm(DF_tmp, DF_rc, DF, DF_reclass, DF_fremont, DF_other)

#Two methods for determining site elevations from lat-lon coordinates are given below.
#One method uses the raster::extract() function to pull these data from the DEM raster 
#provided by PRISM. The other method is to use the elevatr::get_elev_point() function 
#to pull these data from the USGS Elevation Point Query Service <http://ned.usgs.gov/epqs/>.

#PRISM raster data
#DEM from PRISM climate group
#Refrence: PRISM Climate Group, Oregon State University, http://prism.oregonstate.edu, created 15 Jan 2020
# dem_tm1 <- dem_dir[grep("bil.bil", dem_dir)]
# dem_tm2 <- dem_tm1[!grepl("aux.xml", dem_tm1)]
# dem_ras <- raster(dem_tm2)
# crs(dem_ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# ap_ras  <- raster::extract(dem_ras, 
#                            DF_ap[,4:5], 
#                            method = "bilinear")
# DF_elev <- data.frame(DF_ap, 
#                       elevation_prism = ap_ras) %>% 
#   dplyr::select(mean, 
#                 sigma, 
#                 longitude, 
#                 latitude, 
#                 significance, 
#                 elevation_prism)

#USGS EPQS data or Amazon Web Services elevation data (latter is faster)
#I suggest that you do this for unique lon-lat coordinates in DF_ap, or it can take a while.
prj_dd  <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
prj_dd  <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
ap_sp   <- SpatialPoints(DF_ap[,4:5], 
                         proj4string = CRS(prj_dd))
# ap_spdf <- SpatialPointsDataFrame(ap_sp, 
#                                   proj4string = CRS(prj_dd), 
#                                   data = DF_ap[,1:3])

#This is time-consuming for this dataset
# df_elev_epqs <- get_elev_point(ap_sp,
#                                prj = prj_dd,
#                                src = "epqs")

df_elev_aws  <- get_elev_point(ap_sp,
                               prj = prj_dd,
                               src = "aws")

DF_elev <- data.frame(data.frame(df_elev_aws), 
                      DF_ap[,1:3]) %>% 
  dplyr::select(mean, 
                sigma, 
                longitude, 
                latitude, 
                significance, 
                elevation)

# save(DF_elev, file = "/home/thomson/Data/Rdata/AP_sites_ages_elev_aws.Rdata")
