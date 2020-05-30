#This script plots Figure 3 for ERL paper.
#January 2020
#Marcus Thomson
#NCEAS & IIASA (formerly)

library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)
library(zoo)
library(cowplot)

#See elevations source file for references
# source("/home/thomson/Scripts/Fremont_elevations.R") #DF_elev
load(file = "/home/thomson/Data/Rdata/AP_sites_ages_elev_aws.Rdata") #DF_elev

#These data come from the Canadian Archaeological Radiocarbon Database (CARD).
#Reference: Martindale, Andrew, Richard Morlan, Matthew Betts, Michael Blake, Konrad Gajewski, Michelle Chaput, Andrew Mason, and Pierre Vermeersch (2016) Canadian Archaeological Radiocarbon Database (CARD 2.1), accessed January 10, 2020.
#Range Creek data come from Prehistoric Archaeology in Range Creek Canyon, Utah: A Summary of Activities of the Range Creek Field Station.
#Reference: Boomgarden, S. A., Metcalfe, D., & Springer, C. (2014). Prehistoric archaeology in range Creek canyon, Utah: a summary of the activities of the range Creek field station. Utah Archaeology, 27(1), 9-32.
source("/home/thomson/Scripts/Fremont_radiocarbon_age_calibration.R") #CalAges

#We only consider sites near modern Utah state boundary
CalAges <- CalAges %>% 
  filter(longitude > -115)

CalAges0 <- CalAges %>% 
  filter(year_CE >= 0) %>% 
  arrange(., year_CE) %>% 
  mutate(year_bin = round(year_CE, -2))


CalAges1 <- CalAges %>% 
  filter(year_CE >= -25 & 
           year_CE < 1575) %>% 
  arrange(year_CE) %>% 
  mutate(bin = cut(year_CE, 
                   breaks = seq(25, 1975, by = 100),
                   labels = seq(50, 1900, by = 100)))

CalAges3 <- rbind(CalAges2a <- CalAges1 %>% 
                    filter(longitude >= -111.5 & 
                             significance == "Fremont") %>% 
                    mutate(grouping = "GB"), 
                  CalAges2b <- CalAges1 %>% 
                    filter(longitude < -111.5 & 
                             significance == "Fremont") %>% 
                    mutate(grouping = "CP"))


tmp <- CalAges1 %>% 
  mutate(bin = cut(year_CE, 
                   breaks = seq(0, 1950, by=100))) %>% 
  group_by(bin, significance) %>% 
  summarize(count = n())




#Proxy values
source("/home/thomson/Scripts/SW_paleoclimate_datasets.R")

#smoothed temperature proxy for Southern Colorado Plateau
k = 51
scp_temp <- sc_temp_DF %>% 
  mutate(smoothed_anom = zoo::rollapply(temp_anomaly, 
                                      k, 
                                      mean, 
                                      align = "center", 
                                      fill = NA), 
         location = "Southern Colorado Plateau") %>% 
  dplyr::select(year_CE, 
                smoothed_anom, 
                location)

gb_temp <- christiansen2012_DF %>% 
  mutate(smoothed_anom = zoo::rollapply(anomaly, 
                                   k, 
                                   mean, 
                                   align = "center", 
                                   fill = NA), 
         location = "Great Basin") %>% 
  dplyr::select(year_CE, 
                smoothed_anom, 
                location)



scp_gb_temp <- rbind(scp_temp, 
                     gb_temp)

ensemble_mean <- scp_gb_temp %>% 
  group_by(year_CE) %>% 
  summarise(smoothed_anom = mean(smoothed_anom, 
                      na.rm = TRUE)) %>% 
  mutate(location = "Mean")

scp_gb_Temp <- rbind(scp_gb_temp, 
                     ensemble_mean)

p1 <- ggplot(data = scp_gb_Temp, aes(x = year_CE, 
                                     y = smoothed_anom)) + 
  geom_line(data = scp_gb_Temp %>% 
              filter(location != "Mean"), 
            aes(colour = location), 
            alpha = 0.6) + 
  geom_line(data = scp_gb_Temp %>% 
              filter(location == "Mean"), 
            colour = "red", 
            size = 1.5, 
            alpha = 0.7) + 
  scale_color_manual(values = c("Great Basin" = "purple", 
                                "Southern Colorado Plateau" = "blue")) + 
  theme(
    legend.position = c(0.25, 0.9), 
    legend.direction = "vertical", 
    legend.title = element_blank(),
    legend.background = element_blank(), 
    legend.key = element_blank()
  ) + 
  labs(
    x = "Year CE", 
    y = "Temp. anom. (°C)", 
    tag = "A"
  ) + 
  scale_x_continuous(labels = seq(0, 2000, by = 250), 
                     breaks = seq(0, 2000, by = 250), 
                     limits = c(0, 1500))




p2 <- ggplot() +
  geom_point(data = CalAges %>% 
               filter(year_CE >= 0),
             aes(x = year_CE,
                 y = elevation, 
                 size = sum_density^0.25, 
                 colour = significance), 
             # shape = "|", 
             alpha = 0.02) + 
  geom_smooth(data = CalAges %>% 
                filter(year_CE >= 0 & year_CE < 1450),
              aes(x = year_CE,
                  y = elevation, 
                  weight = sum_density),
              colour = "black", 
              method = "loess", 
              span = 0.9, 
              se = FALSE, 
              size = 1, 
              linetype = "solid",
              alpha = 0.5) + 
  scale_color_manual(values = c("Fremont" = "#786290", 
                               "Other" = "#627F90")) + 
  facet_wrap(~significance, 
             ncol = 2) + 
  theme_minimal() + 
  labs(
    y = "Elevation (m asl)", 
    tag = "A"
  ) + 
  theme(
    legend.position = "none", 
    axis.title.x = element_blank(), 
    # axis.text.x = element_blank(), 
    plot.margin = margin(t=0, r=0, b=-0.5, l=0, unit = "cm")
  ) + 
  # ylim(1120, 2050) +
  scale_x_continuous(labels = seq(0, 2000, by = 250), 
                     breaks = seq(0, 2000, by = 250)) + 
  coord_cartesian(xlim = c(0, 1500))





k = 31
for(i in seq_along(ages_fremont)){
  tmp <- data.frame(ages_fremont[[i]]$ageGrid, 
                    ages_fremont[[i]]$densities)
  if(i==1) Tmp <- tmp
  else Tmp <- rbind(Tmp, tmp)
  rm(tmp)
}
CalAges0_fremont <- Tmp %>% 
  dplyr::rename(year = ages_fremont..i...ageGrid, 
                density = ages_fremont..i...densities) %>% 
  group_by(year) %>% 
  dplyr::summarise(sum_density = sum(density)) %>% 
  mutate(year_CE = 1950-year) %>% 
  dplyr::select(year_CE, sum_density) %>% 
  mutate(significance = "Fremont") %>% 
  mutate(rav_density = rollapply(sum_density, 
                                 k, 
                                 mean, 
                                 align = "center", 
                                 fill=NA))

for(i in seq_along(ages_other)){
  tmp <- data.frame(ages_other[[i]]$ageGrid, 
                    ages_other[[i]]$densities)
  if(i==1) Tmp <- tmp
  else Tmp <- rbind(Tmp, tmp)
  rm(tmp)
}
CalAges0_other <- Tmp %>% 
  dplyr::rename(year = ages_other..i...ageGrid, 
                density = ages_other..i...densities) %>% 
  group_by(year) %>% 
  dplyr::summarise(sum_density = sum(density)) %>% 
  mutate(year_CE = 1950-year) %>% 
  dplyr::select(year_CE, sum_density) %>% 
  mutate(significance = "Other") %>% 
  mutate(rav_density = rollapply(sum_density, 
                                 k, 
                                 mean, 
                                 align = "center", 
                                 fill=NA))

CalAges0 <- rbind(CalAges0_fremont, CalAges0_other)

#This just cleans up the plotting.
CalAges0[CalAges0$year_CE==1950,]$sum_density <- 0
CalAges0[CalAges0$year_CE==0,]$sum_density <- 0


p3 <- ggplot(data = CalAges0 %>% 
               filter(year_CE >= 0),
             mapping = aes(x = year_CE, 
                           y = rav_density, 
                           fill = significance)) + 
  scale_fill_manual(values = c("Fremont" = "#786290", 
                               "Other" = "#627F90")) + 
  geom_area(alpha = 0.6, col = "black", size = 0.2) + 
  facet_wrap(~significance, 
             ncol = 2) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank(), 
    strip.text.x = element_blank(), 
    legend.spacing.x = unit(0.5, 'cm'), 
    legend.text = element_text(margin = margin(r = 20))
  ) + 
  labs(x = "Year CE", 
       y = "SPD", 
       tag = "B") + 
  scale_x_continuous(position = "bottom", 
                     breaks = seq(0, 2000, by = 250), 
                     labels = seq(0, 2000, by = 250), 
                     limits = c(0, 1500))

p3_legend <- cowplot::get_legend(p3)

#grid.arrange(p1, p2, nrow = 2)

load(file = "/home/thomson/Data/Rdata/fremont_ckde.Rdata") #frem_ckde
load(file = "/home/thomson/Data/Rdata/other_ckde.Rdata") #othe_ckde

tmp1 <- data.frame(significance = "Fremont", 
                   frem_ckde$res.matrix %>% 
                     melt(., value.name = "SP", na.rm = TRUE) %>% 
                     rename("year_CE" = Var1, "simulation" = Var2))
tmp2 <- data.frame(significance = "Other", 
                   othe_ckde$res.matrix %>% 
                     melt(., value.name = "SP", na.rm = TRUE) %>% 
                     rename("year_CE" = Var1, "simulation" = Var2))
tmp_ap <- rbind(tmp1, tmp2)

means_ap <- rbind(data.frame(significance = "Fremont", 
                             year_CE = c(1:2000), 
                             SP = rowMeans(frem_ckde$res.matrix)), 
                  data.frame(significance = "Other", 
                             year_CE = c(1:2000), 
                             SP = rowMeans(othe_ckde$res.matrix))) %>% 
  na.omit()

p4 <- ggplot() + 
  geom_line(data = tmp_ap, 
            aes(x = year_CE, y = SP, colour = significance), 
            alpha = 0.25, 
            size = 0.1) + 
  geom_line(data = means_ap, 
            aes(x = year_CE, y = SP, colour = significance), 
            size = 1) + 
  scale_colour_manual(values = c("Fremont" = "#786290", 
                                 "Other" = "#627F90")) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    strip.text.x = element_blank()
  ) + 
  facet_wrap(~significance, 
             ncol = 2) + 
  labs(x = "Year CE", 
       y = "Summed Probability", 
       tag = "B") + 
  scale_x_continuous(position = "bottom", 
                     breaks = seq(0, 2000, by = 250), 
                     labels = seq(0, 2000, by = 250), 
                     limits = c(0, 1500))

#Make a data.frame of calibrated dates summed for each site
DF_frem_Elev <- data.frame(DF_frem_elev, 
                           DateID = DF_frem_elev %>% 
                             rownames() %>% 
                             as.integer(), 
                           group_ind = DF_frem_elev %>% 
                             group_indices(longitude, latitude, elevation))
DF_othe_Elev <- data.frame(DF_othe_elev, 
                           DateID = DF_othe_elev %>% 
                             rownames() %>% 
                             as.integer(), 
                           group_ind = DF_othe_elev %>% 
                             group_indices(longitude, latitude, elevation))

ncount <- as.integer(ages_fremont2[[1]]$DateID)
for(i in seq_along(ncount)){
  tmp <- data.frame(DateID = i, ages_fremont2[[2]][i])
  names(tmp) <- c("DateID", "calBP", "PrDens")
  if(i==1) Tmp <- tmp
  if(i >1) Tmp <- rbind(Tmp, tmp)
  if(i==max(ncount)){
    ages_frem <- Tmp
    rm(Tmp, tmp)
  }
}
rm(ncount)
ncount <- as.integer(ages_other2[[1]]$DateID)
for(i in seq_along(ncount)){
  tmp <- data.frame(DateID = i, ages_other2[[2]][i])
  names(tmp) <- c("DateID", "calBP", "PrDens")
  if(i==1) Tmp <- tmp
  if(i >1) Tmp <- rbind(Tmp, tmp)
  if(i==max(ncount)){
    ages_othe <- Tmp
    rm(Tmp, tmp)
  }
}
rm(ncount)

#Sum probability densities over sites (groups)
ages_coord_frem <- left_join(ages_frem, 
                             DF_frem_Elev %>% 
                               dplyr::select(DateID, group_ind, longitude, latitude, significance, elevation), 
                             by = "DateID") %>% 
  group_by(group_ind, calBP, longitude, latitude, elevation, significance) %>% 
  summarize(SPD = sum(PrDens)) %>% 
  ungroup()
ages_coord_othe <- left_join(ages_othe, 
                             DF_othe_Elev %>% 
                               dplyr::select(DateID, group_ind, longitude, latitude, significance, elevation), 
                             by = "DateID") %>% 
  group_by(group_ind, calBP, longitude, latitude, elevation, significance) %>% 
  summarize(SPD = sum(PrDens)) %>% 
  ungroup()

ages_coord_ap <- rbind(ages_coord_frem, 
                       ages_coord_othe)


#Now, instead of summing probability densities over groups, find composite kernel density functions (CKDE) by site

#Do the following separately for each site

ncount <- unique(DF_frem_Elev$group_ind)[order(unique(DF_frem_Elev$group_ind))]
for(i in seq_along(ncount)){
  
  tmp_bins = binPrep(sites = rownames(DF_frem_Elev %>% 
                                        filter(group_ind == i)), 
                     ages = DF_frem_Elev %>% 
                       filter(group_ind == i) %>% 
                       pull(mean), 
                     h = 10)
  
  tmp_ages <- rcarbon::calibrate(x = DF_frem_Elev %>% 
                                   filter(group_ind == i) %>% 
                                   pull(mean), 
                                 errors = DF_frem_Elev %>% 
                                   filter(group_ind == i) %>% 
                                   pull(sigma), 
                                 calCurves = "intcal13")
  
  ages_tmp_rand = sampleDates(tmp_ages, 
                              bins = tmp_bins, 
                              nsim = 1000, 
                              verbose = FALSE)
  
  tmp_ckde = ckde(ages_tmp_rand, 
                  timeRange = c(1950,-49), 
                  bw = 30)
  
  tmp <- rowMeans(tmp_ckde$res.matrix)
  
  if(i==1) Tmp <- tmp
  if(i >1) Tmp <- cbind(Tmp, tmp)
  if(i==max(ncount)){
    frem_ckde_means <- data.frame(yrBP = seq(max(tmp_ckde$timeRange), min(tmp_ckde$timeRange), by = -1), 
                                  Tmp)
    names(frem_ckde_means) <- c("yrBP", paste0("Site_", ncount))
    rm(tmp, Tmp)
  }
}
Frem_ckde_means <- frem_ckde_means %>% 
  reshape2::melt(.,id.vars = "yrBP", value.name = "sum_density") %>% 
  separate(variable, c("discard", "group_ind"), "_") %>% 
  mutate(group_ind = as.numeric(group_ind)) %>% 
  mutate(year_CE = 1950-yrBP) %>% 
  dplyr::select(-discard) %>% 
  na.omit()

ncount <- unique(DF_othe_Elev$group_ind)[order(unique(DF_othe_Elev$group_ind))]
for(i in seq_along(ncount)){
  
  tmp_bins = binPrep(sites = rownames(DF_othe_Elev %>% 
                                        filter(group_ind == i)), 
                     ages = DF_othe_Elev %>% 
                       filter(group_ind == i) %>% 
                       pull(mean), 
                     h = 10)
  
  tmp_ages <- rcarbon::calibrate(x = DF_othe_Elev %>% 
                                   filter(group_ind == i) %>% 
                                   pull(mean), 
                                 errors = DF_othe_Elev %>% 
                                   filter(group_ind == i) %>% 
                                   pull(sigma), 
                                 calCurves = "intcal13")
  
  ages_tmp_rand = sampleDates(tmp_ages, 
                              bins = tmp_bins, 
                              nsim = 1000, 
                              verbose = FALSE)
  
  tmp_ckde = ckde(ages_tmp_rand, 
                  timeRange = c(1950,-49), 
                  bw = 30)
  
  tmp <- rowMeans(tmp_ckde$res.matrix)
  
  if(i==1) Tmp <- tmp
  if(i >1) Tmp <- cbind(Tmp, tmp)
  if(i==max(ncount)){
    othe_ckde_means <- data.frame(yrBP = seq(max(tmp_ckde$timeRange), min(tmp_ckde$timeRange), by = -1), 
                                  Tmp)
    names(othe_ckde_means) <- c("yrBP", paste0("Site_", ncount))
    rm(tmp, Tmp)
  }
}
Othe_ckde_means <- othe_ckde_means %>% 
  reshape2::melt(.,id.vars = "yrBP", value.name = "sum_density") %>% 
  separate(variable, c("discard", "group_ind"), "_") %>% 
  mutate(group_ind = as.numeric(group_ind)) %>% 
  mutate(year_CE = 1950-yrBP) %>% 
  dplyr::select(-discard) %>% 
  na.omit()

#Now merge these with the site locations

frem_coord <- DF_frem_Elev %>% 
  dplyr::select(longitude, latitude, elevation, significance, group_ind) %>% 
  ungroup() %>% 
  distinct()
othe_coord <- DF_othe_Elev %>% 
  dplyr::select(longitude, latitude, elevation, significance, group_ind) %>% 
  ungroup() %>% 
  distinct()

frem_ckde_means_coord <- left_join(Frem_ckde_means, 
                                   frem_coord, 
                                   by = "group_ind")
othe_ckde_means_coord <- left_join(Othe_ckde_means, 
                                   othe_coord, 
                                   by = "group_ind")

CKDE_means_coord <- rbind(frem_ckde_means_coord, 
                          othe_ckde_means_coord)

# save(CKDE_means_coord, file = "/home/thomson/Data/Rdata/CKDE_means_coord.Rdata")

p5 <- ggplot(CKDE_means_coord) + 
  geom_point(aes(x=year_CE, 
                 y=elevation, 
                 size = sum_density^2, 
                 colour = significance), 
             alpha = 0.02, 
             shape = 20) + 
  geom_smooth(data = CKDE_means_coord %>% 
                filter(significance == "Fremont", 
                       year_CE > 400 & year_CE < 1300), 
              aes(x = year_CE,
                  y = elevation, 
                  weight = sum_density),
              colour = "black", 
              method = "loess", 
              span = 0.8, 
              se = FALSE, 
              size = 1, 
              linetype = "solid",
              alpha = 0.5) + 
  geom_smooth(data = CKDE_means_coord %>% 
                filter(significance == "Other", 
                       year_CE > 0 & year_CE < 1450), 
              aes(x = year_CE,
                  y = elevation, 
                  weight = sum_density),
              colour = "black", 
              method = "loess", 
              span = 0.8, 
              se = FALSE, 
              size = 1, 
              linetype = "solid",
              alpha = 0.5) + 
  facet_wrap(~significance) + 
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title.x = element_blank()
  ) + 
  scale_color_manual(values = c("Fremont" = "#786290", 
                                "Other" = "#627F90")) + 
  labs(
    y = "elevation (m asl)", 
    tag = "A"
  ) + 
  scale_x_continuous(labels = seq(0, 2000, by = 250), 
                     breaks = seq(0, 2000, by = 250), 
                     limits = c(0, 1500)) + 
  coord_cartesian(xlim = c(0, 1500))


png(file="/home/thomson/ERL_paper/Plots/Figure3_updated1.png", w = 1600, h = 1600, res=300)
plot_grid(p5, 
          p4, 
          as_ggplot(p3_legend), 
          rel_heights = c(7, 4, 1), 
          ncol = 1, 
          axis = "lr", 
          align = "v")
dev.off()

#Better aligns plot stack than grid.arrange().
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)

#See: https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange

plot_grid(p2, p3, align = "v", nrow = 2, rel_heights = c(0.4, 0.25))


g <- rbind(g2,g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths) # use the largest widths
# g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)



#Find 1-sigma ranges

tmp <- CalAges %>% 
  mutate(bin = ntile(year, 5))
group_by(elevation, significance) %>% 
  summarize(max(sum_density))


p1 <- ggplot(data = CalAges, aes(x = factor(seq(min(year_CE), max(year_CE), by =50)), y = elevation, fill = significance)) + 
  geom_boxplot()



###

#I have not quite reproduced the plot from the paper.
#To do this, I need to isolate the CO Plateau Fremont from the Great Basin Fremont


# p1 <- ggplot(data = CalAges1,
#              aes(x = factor(CalAges1$year_CE,
#                             levels = seq(0, 1550, by = 50),
#                             labels = seq(0, 1550, by = 50)),
#                  y = elevation)) +
# p1 <- ggplot(data = CalAges1,
#              aes(x = bin,
#                  y = elevation)) + 
#   geom_boxplot(outlier.shape = 3, 
#                outlier.size = 0.8, 
#                notch = TRUE) + 
#   # geom_boxplot(aes(fill = significance), 
#   #              position = "dodge", 
#   #              outlier.shape = 3, 
#   #              outlier.size = 0.8) + 
#   # facet_wrap(~significance, 
#   #            nrow = 2) + 
#   theme_minimal() + 
#   labs(
#     x = "Year CE", 
#     y = "Elevation (m asl)"
#   ) + 
#   theme(
#     axis.text.x = element_text(angle = 00, 
#                                hjust = 0.5), 
#     legend.position = "none"
#   ) + 
#   xlim(0, 1450)
# scale_x_discrete(labels = seq(0, 1500, by = 250), 
#                  breaks = seq(25, 1525, by = 250))
# 
# #Fremont only, with GB and CP discriminated
# p2 <- ggplot(data = CalAges3 %>% 
#                filter(significance == "Fremont"),
#              aes(x = bin,
#                  y = elevation)) +
#   geom_boxplot(aes(fill = grouping), 
#                position = "dodge", 
#                outlier.shape = 3, 
#                outlier.size = 0.8) + 
#   # facet_wrap(~grouping, 
#   #            ncol = 2) + 
#   theme_minimal() + 
#   labs(
#     x = "Year CE", 
#     y = "Elevation (m asl)"
#   ) + 
#   theme(
#     axis.text.x = element_text(angle = 00, 
#                                hjust = 0), 
#     legend.position = "none"
#   ) + 
#   scale_x_discrete(labels = seq(0, 1500, by = 250), 
#                    breaks = seq(25, 1525, by = 250))