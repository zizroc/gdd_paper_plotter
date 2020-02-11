#Range Creek sites calibration

library(tidyverse)
library(Bchron)
library(gridExtra)
library(zoo)
library(gtable)
library(grid)

#These data come from Prehistoric Archaeology in Range Creek Canyon, Utah: A Summary of Activities of the Range Creek Field Station.
#Reference: Boomgarden, S. A., Metcalfe, D., & Springer, C. (2014). Prehistoric archaeology in range Creek canyon, Utah: a summary of the activities of the range Creek field station. Utah Archaeology, 27(1), 9-32.
file <- dir("/mnt/chromeos/MyFiles/Data files", full.names = TRUE)
df   <- read.csv(file[grep("Range Creek", file)], header = TRUE, stringsAsFactors = FALSE)

DF_rc <- df %>% 
  rename("mean" = "Radiocarbon.age..mean.BP.", 
         "sigma" = "SE") %>% 
  mutate("significance" = "Fremont", 
         "longitude" = -110.217,
         "latitude" = 39.429) 

tmp <- raster::extract(dem_ras, 
                       data.frame(x=DF_rc$longitude, 
                                  y=DF_rc$latitude), 
                       method = "simple")
DF_rc_elev <- data.frame(DF_rc, elevation = tmp)
rm(tmp)

ages_rc <- BchronCalibrate(ages = DF_rc_elev$mean, 
                           ageSds = DF_rc_elev$sigma, 
                           calCurves = rep("intcal13", 
                                           length(DF_rc_elev$mean)))


for(i in seq_along(ages_rc)){
  #keep only those sites that satisfy 1-sigma precision
  #fill each location year-by-year until 68.27% is filled
  tmp0 <- data.frame(year = ages_rc[[i]]$ageGrid, 
                     density = ages_rc[[i]]$densities) %>% 
    arrange(., -density) %>% 
    mutate(cumu_dens = cumsum(density)) %>% 
    filter(cumu_dens <= 0.6827) %>% 
    dplyr::select(year, density)
  
  tmp1 <- data.frame(tmp0, 
                     longitude = DF_rc_elev[i,12], 
                     latitude = DF_rc_elev[i,13], 
                     elevation = DF_rc_elev[i,14])
  rm(tmp0)
  if(i==1) Tmp <- tmp1
  else Tmp <- rbind(Tmp, tmp1)
  rm(tmp1)
}
CalAges_rc <- Tmp %>% 
  group_by(year, longitude, latitude, elevation) %>% 
  dplyr::summarise(sum_density = sum(density, 
                                     na.rm = TRUE)) %>% 
  mutate(year_CE = 1950-year, 
         significance = "Fremont", 
         n = n())
rm(Tmp)
