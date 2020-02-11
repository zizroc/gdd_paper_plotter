#Fremont radiocarbon age calibration

library(Bchron)

if(!exists("DF_elev")){
  #See elevations source file for references
  source("/home/thomson/Scripts/Fremont_elevations.R") #DF_elev
} 


DF_frem_elev <- DF_elev %>% 
  filter(significance == "Fremont")

DF_othe_elev <- DF_elev %>% 
  filter(significance == "Other")

#Calibrate according to Intcal13 curve.
ages_fremont <- BchronCalibrate(ages = DF_frem_elev$mean, 
                                ageSds = DF_frem_elev$sigma, 
                                calCurves = rep("intcal13", 
                                                length(DF_frem_elev$mean)))

ages_other <- BchronCalibrate(ages = DF_othe_elev$mean, 
                              ageSds = DF_othe_elev$sigma, 
                              calCurves = rep("intcal13", 
                                              length(DF_othe_elev$mean)))



#The following smooths the SPD with a window of k years.
k = 31
for(i in seq_along(ages_fremont)){
  #keep only those sites that satisfy 1-sigma precision
  #fill each location year-by-year until 68.27% is filled
  tmp0 <- data.frame(year = ages_fremont[[i]]$ageGrid, 
                     density = ages_fremont[[i]]$densities) %>% 
    arrange(., -density) %>% 
    mutate(cumu_dens = cumsum(density)) %>% 
    filter(cumu_dens <= 0.6827) %>% 
    dplyr::select(year, density)
  
  tmp1 <- data.frame(tmp0, 
                     longitude = DF_frem_elev[i,3], 
                     latitude = DF_frem_elev[i,4], 
                     elevation = DF_frem_elev[i,6])
  rm(tmp0)
  if(i==1) Tmp <- tmp1
  else Tmp <- rbind(Tmp, tmp1)
  rm(tmp1)
}
CalAges_fremont <- Tmp %>% 
  group_by(year, longitude, latitude, elevation) %>% 
  dplyr::summarise(sum_density = sum(density, 
                                     na.rm = TRUE)) %>% 
  mutate(year_CE = 1950-year, 
         significance = "Fremont", 
         n = n()) %>% 
  mutate(rav_density = zoo::rollapply(sum_density, 
                                      k, 
                                      mean, 
                                      align = "center", 
                                      fill=NA))
rm(Tmp)


for(i in seq_along(ages_other)){
  #keep only those sites that satisfy 1-sigma precision
  #fill each location year-by-year until 68.27% is filled
  tmp0 <- data.frame(year = ages_other[[i]]$ageGrid, 
                     density = ages_other[[i]]$densities) %>% 
    arrange(., -density) %>% 
    mutate(cumu_dens = cumsum(density)) %>% 
    filter(cumu_dens <= 0.6827) %>% 
    dplyr::select(year, density)
  
  tmp1 <- data.frame(tmp0, 
                     longitude = DF_othe_elev[i,3], 
                     latitude = DF_othe_elev[i,4], 
                     elevation = DF_othe_elev[i,6])
  rm(tmp0)
  if(i==1) Tmp <- tmp1
  else Tmp <- rbind(Tmp, tmp1)
  rm(tmp1)
}
CalAges_other <- Tmp %>% 
  group_by(year, longitude, latitude, elevation) %>% 
  dplyr::summarise(sum_density = sum(density, 
                                     na.rm = TRUE)) %>% 
  mutate(year_CE = 1950-year, 
         significance = "Other", 
         n = n()) %>% 
  mutate(rav_density = zoo::rollapply(sum_density, 
                                      k, 
                                      mean, 
                                      align = "center", 
                                      fill=NA))
rm(Tmp)

save(DF_elev, file = "/home/thomson/Data/DF_elev.Rdata")

CalAges <- rbind(CalAges_fremont, 
                 CalAges_other)

sites <- DF_elev %>% 
  group_by(longitude, latitude, significance) %>% 
  summarize(nsites = n())

save(CalAges, file = "/home/thomson/Data/CalAges.Rdata")