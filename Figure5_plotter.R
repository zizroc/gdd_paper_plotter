#This script plots Figure 5 for ERL paper
#January 2020
#Marcus Thomson
#NCEAS & IIASA (formerly)

library(tidyverse)
library(cowplot)
library(zoo)
library(grid)
library(gridExtra)

data_dir <- dir("/home/thomson/Data/Rdata/GDD", full.names = TRUE)

load(file = data_dir[grep("003", data_dir)])
em003 <- cGDD
rm(cGDD)
load(file = data_dir[grep("004", data_dir)])
em004 <- cGDD
rm(cGDD)
load(file = data_dir[grep("005", data_dir)])
em005 <- cGDD
rm(cGDD)

gdd <- rbind(em003, 
             em004, 
             em005) %>% 
  filter(gdd > 0) #%>% 
  # group_by(lon, lat, year_CE) %>% 
  # summarize(gdd_em = mean(gdd))

# load(file = "/home/thomson/Data/prec.Rdata") #prec - only over the growing season
load(file = "/home/thomson/Data/prec_365days.Rdata") #prec - annual precipitation

prec <- prec %>% 
  rename("lon" = "longitude", 
         "lat" = "latitude")

gdd_prec <- inner_join(gdd, 
                       prec, 
                       by = c("year_CE", "lon", "lat", "em"))

load("/home/thomson/Data/CalAges.Rdata") #CalAges

Sites_ages <- CalAges %>% 
  ungroup() %>% 
  filter(year_CE >= 850, 
         year_CE < 1450) %>% 
  rename("lon" = "longitude", 
         "lat" = "latitude") %>% 
  dplyr::select(-year, -rav_density, -n) %>% 
  ungroup()

load(file = "/home/thomson/Data/Rdata/CKDE_means_coord.Rdata") #CKDE_means_coord
ckde_means_coord <- CKDE_means_coord %>% 
  rename("lon" = "longitude", 
         "lat" = "latitude")

load(file = "/home/thomson/Data/Rdata/GDD_sites.Rdata") #GDD_sites
load(file = "/home/thomson/Data/Rdata/PPT_sites.Rdata") #PPT_sites

gdd_ppt_sites <- inner_join(GDD_sites, 
                            PPT_sites, 
                            by = c("lon", "lat", "year_CE"))

gdd_ppt_spd <- inner_join(gdd_ppt_sites, 
                          Sites_ages, 
                          by = c("lon", "lat", "year_CE")) %>% 
  rename("spd" = "sum_density")

gdd_ppt_spd2 <- full_join(gdd_ppt_sites, 
                           ckde_means_coord,
                          by = c("lon", "lat", "year_CE"), 
                          keep = TRUE) %>% 
  rename("spd" = "sum_density")

#some of the sites used in the climate analysis are not shared in the CARD database, so these need to be removed
gdd_ppt_spd2 <- gdd_ppt_spd2[!is.na(gdd_ppt_spd2$significance),]


# ggplot(data = gdd_ppt_spd, 
#        aes(x = gdd_em_zsc, 
#            y = ppt_em_zsc)) + 
#   geom_point() + 
#   facet_wrap(~significance)

Longevity <- gdd_ppt_spd %>% 
  group_by(
    lon, 
    lat, 
    significance
    ) %>% 
  summarise(
    spd_med = median(spd),
    spd_ave = mean(spd), 
    onset_yr = min(year_CE), 
    aband_yr = max(year_CE), 
    longevity = n(), 
    gdd_zsc_mx = max(gdd_em_zsc), 
    gdd_zsc_mn = min(gdd_em_zsc), 
    gdd_zsc_med = median(gdd_em_zsc), 
    gdd_zsc_ave = mean(gdd_em_zsc), 
    gdd_ave = mean(gdd_em), 
    gdd_sd = sd(gdd_em), 
    ppt_zsc_mx = max(ppt_em_zsc), 
    ppt_zsc_mn = min(ppt_em_zsc), 
    ppt_zsc_med = median(ppt_em_zsc), 
    ppt_zsc_ave = mean(ppt_em_zsc), 
    ppt_zsc_sd = sd(ppt_em_zsc), 
    ppt_ave = mean(ppt_em)) %>% 
  ungroup()


#What are the parts of the SPDs that are between the 850-1449 CE limits?

fig5_si <- ggplot(gdd_ppt_spd2 %>% 
                    filter(year_CE >= 850 & year_CE <= 1449)) + 
  geom_col(aes(year_CE, 
               spd, 
               fill = significance),
           alpha = 0.5, 
           position = "identity") + 
  scale_y_continuous(
    breaks = c(0, 0.0025, 0.005), 
    labels = c(0, 0.0025, 0.005)
  ) + 
  facet_wrap(~group_ind, 
             nrow = 5) + 
  # theme_minimal() + 
  theme(
    legend.position = "bottom", 
    axis.text.x = element_text(angle = 90)
  ) + 
  labs(
    x = "year CE", 
    y = "Summed probability"
  )


Longevity2 <- gdd_ppt_spd2 %>% 
  filter(
    year_CE >= 850, 
    year_CE <= 1449
  ) %>% 
  group_by(
    lon, 
    lat, 
    significance, 
    elevation
  ) %>% 
  summarise(
    onset_yr = min(year_CE), 
    aband_yr = max(year_CE), 
    longevity = n(), 
    gdd_zsc_mx = max(gdd_em_zsc, na.rm = TRUE), 
    gdd_zsc_mn = min(gdd_em_zsc, na.rm = TRUE), 
    gdd_zsc_med = median(gdd_em_zsc, na.rm = TRUE), 
    gdd_zsc_ave = mean(gdd_em_zsc), na.rm = TRUE,    
    gdd_zsc_sd = sd(gdd_em_zsc, na.rm = TRUE), 
    gdd_ave = mean(gdd_em, na.rm = TRUE), 
    gdd_sd = sd(gdd_em, na.rm = TRUE), 
    ppt_zsc_mx = max(ppt_em_zsc, na.rm = TRUE), 
    ppt_zsc_mn = min(ppt_em_zsc, na.rm = TRUE), 
    ppt_zsc_med = median(ppt_em_zsc, na.rm = TRUE), 
    ppt_zsc_ave = mean(ppt_em_zsc, na.rm = TRUE), 
    ppt_zsc_sd = sd(ppt_em_zsc, na.rm = TRUE), 
    ppt_ave = mean(ppt_em, na.rm = TRUE), 
    spd_sum = sum(spd, na.rm = TRUE), 
    spd_qnt = quantile(spd, na.rm = TRUE)[3], 
    spd_ave = mean(spd, na.rm = TRUE)) %>% 
  ungroup()

length(Longevity2$onset_yr[Longevity2$onset_yr==850])
length(Longevity2$aband_yr[Longevity2$aband_yr==1449])

Longevity3 <- ckde_means_coord %>% 
  filter(year_CE >= 300, 
         year_CE < 1449) %>% 
  group_by(
    lon, 
    lat, 
    significance, 
    elevation
  ) %>% 
  summarise(
    sum_sp = sum(sum_density), 
    ave_sp = mean(sum_density), 
    med_sp = median(sum_density), 
    max_sp = max(sum_density), 
    sd_sp = sd(sum_density), 
    fourth_quantile = quantile(sum_density)[4], 
    longevity = n()
  ) %>% 
  ungroup()

tmp <- Longevity3 %>%
  filter(longevity < 1449 & significance == "Other") %>% 
  dplyr::select(longevity, sd_sp)
cor.test(x=as.vector(unlist(tmp[,1])), y=as.vector(unlist(tmp[,2])))

#elevation
#Fremont: 0.2917311, p-value = 0.132
#Other: 0.1485458 ,  p-value = 0.3136

#sd_sp
#Fremont: -0.3366999, p-value = 0.07978
#Other: -0.6199149,  p-value = 2.618e-06

#med_sp
#Fremont: -0.8840427, p-value = 4.484e-10
#Other: -0.7800986, p-value = 6.337e-11



p1a <- ggplot(data = Longevity3 %>% 
                filter(longevity < 1149), 
             aes(x = longevity, 
                 y = elevation)) + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    x = "Longevity (years), between 300-1449 CE",
    y = "Elevation (m asl)", 
    tag = "A"
  ) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 9)
  ) + 
  coord_cartesian(xlim = c(0, 1150))

p2a <- ggplot(data = Longevity3 %>% 
                filter(longevity < 1149), 
              aes(x = longevity, 
                  y = sd_sp)) + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    x = "Longevity (years), between 300-1449 CE",
    y = "Variability of summed probability", 
    tag = "B"
  ) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 9)
  ) + 
  coord_cartesian(xlim = c(0, 1150))

p3a <- ggplot(data = Longevity3 %>% 
                filter(longevity < 1149)) + 
  geom_vline(xintercept = 0, alpha = 0.5) + 
  geom_density(aes(longevity, 
                     fill = significance), 
               colour = 0, 
               alpha = 0.6) + 
  scale_fill_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    x = "Longevity (years), between 300-1449 CE",
    y = "Density of summed probability", 
    tag = "C"
  ) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 9)
  ) + 
  coord_cartesian(xlim = c(0, 1150))

p4a <- ggplot(data = Longevity2 %>% 
                filter(longevity < 600)) + 
  geom_vline(xintercept = 0, alpha = 0.5) + 
  geom_density(aes(longevity, 
                   fill = significance), 
               colour = 0, 
               alpha = 0.6) + 
  scale_fill_manual(values = c("Fremont" = "#786290", 
                               "Other" = "#627F90")) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    x = "Longevity (years), between 850-1449 CE",
    y = "Density of summed probability", 
    tag = "D"
  ) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 9)
  ) + 
  coord_cartesian(xlim = c(0, 600))

p5a <- ggplot(data = Longevity2 %>% 
                filter(longevity < 600)) + 
  geom_point(aes(x = gdd_zsc_med, 
                 y = spd_qnt,
                   colour = significance, 
                 shape = significance), 
               alpha = 0.6) + 
  scale_fill_manual(values = c("Fremont" = "#786290", 
                               "Other" = "#627F90")) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    x = "Median of GDD z-score, 850-1449 CE",
    y = "Top 25% of SP", 
    tag = "D"
  ) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 9)
  )

df_elev <- DF_elev %>% 
  ungroup() %>% 
  rename("lon" = "longitude", 
         "lat" = "latitude") %>% 
  filter(significance == "Fremont") %>% 
  dplyr::select(lon, lat, elevation) %>% 
  ungroup()
  

test <- inner_join(Longevity, df_elev, by = c("lon", "lat"))

#spd_med, #gdd_ave, #ppt_ave, #ppt_zsc_med

#elevation
#Fremont: 0.3615233, p-value = 0.09008
#Other: 0.2316819 , p-value = 0.2995

#spd_med
#Fremont: -0.745503, p-value = 4.457e-05
#Other: -0.7243114, p-value = 0.000138

#gdd_zsc_mx
#Fremont: -0.03128075, p-value = 0.8873
#Other: -0.31305, p-value = 0.156

#gdd_ave
#Fremont: -0.354501, p-value = 0.09697
#Other:  -0.2135237 , p-value = 0.34

#gdd_sd
#Fremont: -0.3206648, p-value = 0.1357
#Other: -0.3354593, p-value = 0.127

#ppt_zsc_med
#Fremont: 0.3551264, p-value = 0.09634
#Other: 0.5782844, p-value = 0.004815

#ppt_zsc_sd
#Fremont: 0.3551264, p-value = 0.09634
#Other:  0.3551264, p-value = 0.09634

#ppt_ave
#Fremont: 0.4280891, p-value = 0.04156
#Other: 0.276151, p-value = 0.2135

tmp <- Longevity2 %>%
  filter(longevity < 600 & significance == "Other") %>%
  dplyr::select(longevity, ppt_zsc_sd)
cor.test(x=as.vector(unlist(tmp[,1])), y=as.vector(unlist(tmp[,2])))

p1 <- ggplot(data = Longevity2 %>% 
               filter(longevity < 600), 
             aes(x = longevity, 
                 y = gdd_zsc_med)) + 
  geom_hline(yintercept = 0, 
             alpha = 0.5) + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    x = "Longevity (years)",
    y = "Median of GDD z-score", 
    tag = "A"
  ) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 9)
  ) + 
  coord_cartesian(xlim = c(0, 600))

p2 <- ggplot(data = Longevity2 %>% 
               filter(longevity < 600), 
             aes(x = longevity, 
                 y = gdd_zsc_sd)) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7) + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    x = "Longevity (years)",
    y = "Variability of GDD z-score", 
    tag = "B"
  ) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 9)
  ) + 
  coord_cartesian(xlim = c(0, 600))

p3 <- ggplot(data = Longevity2 %>% 
               filter(longevity < 600), 
             aes(x = longevity, 
                 y = ppt_zsc_med)) + 
  geom_hline(yintercept = 0, 
             alpha = 0.5) + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    x = "Longevity (years)", 
    y = "Median of precip. z-score", 
    tag = "C"
  ) + 
  theme(
    legend.position = "none", 
    strip.text = element_blank(), 
    axis.title = element_text(size = 9)
    ) + 
  coord_cartesian(xlim = c(0, 600))


#Fremont like PPT z-score >0 but not too much; Other uncertain
p4 <- ggplot(data = Longevity2 %>% 
               filter(longevity < 600), 
             aes(x = longevity, 
                 y = ppt_zsc_sd)) + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    x = "Longevity (years)",
    y = "Variability of precip. z-score", 
    tag = "D"
  ) + 
  theme(
    legend.position = "none",
    strip.text = element_blank(), 
    axis.title = element_text(size = 9) 
  ) + 
  coord_cartesian(xlim = c(0, 600))

#Fremont like PPT z-score >0 but not too much; Other uncertain
p5 <- ggplot(data = Longevity, 
             aes(x = longevity, 
                 y = ppt_zsc_sd)) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7)  + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance) + 
  labs(
    # x = "Number of occupied years between 850-1449 CE", 
    y = "Variability of precipitation \nwithin occupation range", 
    tag = "E"
  ) + 
  theme(
    legend.position = "none",
    strip.text = element_blank(), 
    axis.title = element_text(size = 9)
  ) + 
  coord_cartesian(xlim = c(0, 600))

#Fremont like PPT z-score >0 but not too much; Other uncertain
p6 <- ggplot(data = Longevity, 
             aes(x = longevity, 
                 y = ppt_ave)) + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance, 
             scales = "free_y") + 
  labs(
    # x = "Number of occupied years between 850-1449 CE", 
    y = "Mean precipitation \nwithin occupation range", 
    tag = "F"
  ) + 
  theme(
    legend.position = "none",
    strip.text = element_blank(), 
    axis.title = element_text(size = 9)
  ) + 
  coord_cartesian(xlim = c(0, 600))

gridplots <- cowplot::plot_grid(p1, 
                               p2, 
                               p3,
                               p4, 
                               align = "lr", 
                               nrow = 2)
x.grob <- textGrob("Number of years occupied between 850-1449 CE", 
                   gp=gpar(fontsize=11))
y.grob <- textGrob("Variable evaluated only for occupied years", 
                   gp=gpar(fontsize=11), 
                   rot = 90)
png(file="/home/thomson/ERL_paper/Plots/Figure6_updated.png", w = 2400, h = 1600, res=300)
grid.arrange(arrangeGrob(gridplots), 
             left = y.grob,
             bottom = x.grob)
dev.off()


gridplots <- cowplot::plot_grid(p1a, 
                                p2a, 
                                p3a,
                                p4a, 
                                align = "lr", 
                                nrow = 2)
x.grob <- textGrob("Number of years occupied for the specified range", 
                   gp=gpar(fontsize=11))
y.grob <- textGrob("Variable evaluated only for occupied years", 
                   gp=gpar(fontsize=11), 
                   rot = 90)
png(file="/home/thomson/ERL_paper/Plots/Figure6_SI_updated.png", w = 2400, h = 1600, res=300)
grid.arrange(arrangeGrob(gridplots), 
             left = y.grob,
             bottom = x.grob)
dev.off()


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

ggplot() + 
  geom_tile(data = dem_df, 
            aes(x = x, 
                y = y, 
                fill = values), 
            alpha = 0.6, 
            show.legend = TRUE) + 
  scale_fill_gradient(low = "black", 
                      high = "white", 
                      name = "Elevation \n(m asl)") + 
  scale_size_continuous(name = "Longevity \n(years)") +
  geom_sf(data = us_states_proj,
          fill = "transparent") + 
  geom_point(data = Longevity2, 
             aes(x = lon, 
                 y = lat, 
                 size = longevity, 
                 shape = significance, 
                 colour = significance),  
             alpha = 1) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  facet_wrap(~significance) + 
  labs(
    x = "degrees west longitude", 
    y = "degrees north latitude"
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom", 
    legend.key = element_rect(fill = "#ffffff", 
                              color = "#ffffff"), 
    legend.box.background = element_rect(fill = "lightgrey")
  ) + 
  xlim(-114.25, -107) + 
  ylim(36.75, 43)






p7 <- ggplot(data = test, 
             aes(x = gdd_sd, 
                 y = elevation)) + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance, 
             scales = "free_y") + 
  labs(
    x = "Variabiliy of GDD \nwithin occupation range", 
    y = "Mean elevation (m asl)", 
    tag = "A"
  ) + 
  theme(
    legend.position = "none",
    strip.text = element_blank(), 
    axis.title = element_text(size = 9)
  )

p8 <- ggplot(data = test, 
             aes(x = ppt_ave, 
                 y = elevation)) + 
  geom_smooth(aes(colour = significance), 
              method = "lm", 
              show.legend = FALSE) + 
  geom_point(aes(colour = significance, 
                 shape = significance), 
             alpha = 0.7) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  scale_shape_manual(values = c("Fremont" = 15, 
                                "Other" = 19)) + 
  theme_minimal() + 
  facet_wrap(~significance, 
             scales = "free_y") + 
  labs(
    x = "Mean precipitation (mm) \nwithin occupation range", 
    y = "Mean elevation (m asl)", 
    tag = "B"
  ) + 
  theme(
    legend.position = "none",
    strip.text = element_blank(), 
    axis.title = element_text(size = 9)
  )

png(file="/home/thomson/ERL_paper/Plots/Figure6c.png", w = 1800, h = 1000, res=300)
cowplot::plot_grid(p7, 
                   p8, 
                   align = "v", 
                   ncol = 2)
dev.off()

# #p1
# #spd_med: r = 0.43, p = 0.038
# #spd_med: r = 0.47, p = 0.029, for longevity > 99 years
# #spd_med: r = 0.31, p = 0.20, for longevity > 99 & < 500 years
# #spd_med: r = 0.52, p = 0.034, for longevity > 200 years
# 
# cor.test(Longevity$longevity, 
#          Longevity$spd_med)[3]
# cor.test(Longevity$longevity, 
#          Longevity$spd_med)[4]
# 
# #p2
# #gdd_zsc_ave: r = 0.16, p = 0.45
# #gdd_zsc_ave: r = 0.25, p = 0.28, for longevity > 99 years
# #gdd_zsc_ave: r = 0.34, p = 0.16, for longevity > 99 & < 500 years
# #gdd_zsc_ave: r = -0.50, p = 0.04, for longevity > 200 years
# cor.test(Longevity$longevity, 
#          Longevity$gdd_zsc_ave)[3]
# cor.test(Longevity$longevity, 
#          Longevity$gdd_zsc_ave)[4]
# 
# #p3
# #gdd_sd: r = -0.20, p = 0.35
# #gdd_sd: r = -0.16, p = 0.49, for longevity > 99 years
# #gdd_sd: r = -0.19, p = 0.44, for longevity > 99 & < 500 years
# #gdd_sd: r = 0.32, p = 0.20, for longevity > 200 years
# cor.test(Longevity$longevity, 
#          Longevity$gdd_sd)[3]
# cor.test(Longevity$longevity, 
#          Longevity$gdd_sd)[4]
# 
# #p4
# #ppt_zsc_ave: r = -0.43, p = 0.039
# #ppt_zsc_ave: r = -0.24, p = 0.28, for longevity > 99 years
# #ppt_zsc_ave: r = -0.23, p = 0.34, for longevity > 99 & < 500 years
# #ppt_zsc_ave: r = -0.16, p = 0.52, for longevity > 200 years
# cor.test(Longevity$longevity, 
#          Longevity$ppt_zsc_ave)[3]
# cor.test(Longevity$longevity, 
#          Longevity$ppt_zsc_ave)[4]
# 
# #p5
# #ppt_sd: no correlation
# #ppt_sd: no correlation, for longevity > 99 years
# #ppt_sd: no correlation, for longevity > 99 & < 500 years
# #ppt_sd: r = 0.23, p = 0.37, for longevity > 200 years
# cor.test(Longevity$longevity, 
#          Longevity$ppt_zsc_sd)[3]
# cor.test(Longevity$longevity, 
#          Longevity$ppt_zsc_sd)[4]
# 
# #p6
# #ppt_ave: r = 0.18, p = 0.40
# #ppt_ave: r = 0.20, p = 0.38, for longevity > 99 years
# #ppt_ave: no correlation, for longevity > 99 & < 500 years
# #ppt_ave: r = 0.24, p = 0.35, for longevity > 200 years
# cor.test(Longevity$longevity, 
#          Longevity$ppt_ave)[3]
# cor.test(Longevity$longevity, 
#          Longevity$ppt_ave)[4]
# 
# 
# 
# ggplot(gdd_ppt_spd, 
#        aes(x = gdd_em, 
#            y = ppt_em)) + 
#   geom_point(aes(size = spd, 
#                  colour = significance, 
#                  shape = significance), 
#              alpha = 0.3) + 
#   scale_colour_manual(values = c("Fremont" = "#786290", 
#                                  "Other" = "#627F90")) + 
#   scale_shape_manual(values = c("Fremont" = 15, 
#                                 "Other" = 19)) + 
#   scale_size_continuous(name = "SPD") + 
#   theme_minimal() + 
#   labs(
#     x = "GDD ensemble mean", 
#     y = "Precipitation ensemble mean (mm)"
#   ) + 
#   theme(
#     legend.position = "bottom"
#   ) + 
#   facet_wrap(~significance)
