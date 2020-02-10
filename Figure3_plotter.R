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
source("/home/thomson/Scripts/Fremont_elevations.R") #DF_elev

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




p2 <- ggplot(data = CalAges %>% 
               filter(year_CE >= 0),
             aes(x = year_CE,
                 y = elevation)) +
  geom_point(aes(alpha = (sum_density)^0.25, 
                 colour = significance), 
             shape = "|", 
             size = 1) + 
  geom_smooth(aes(weight = sum_density, 
                  colour = significance), 
              method = "loess", 
              span = 0.8, 
              se = FALSE, 
              size = 1, 
              alpha = 0.7) + 
  scale_color_manual(values = c("Fremont" = "#786290", 
                               "Other" = "#627F90")) + 
  facet_wrap(~significance, 
             ncol = 2) + 
  theme_minimal() + 
  labs(
    x = "Year CE", 
    y = "Elevation (m asl)", 
    tag = "A"
  ) + 
  theme(
    legend.position = "none", 
    # axis.title.x = element_blank(), 
    # axis.text.x = element_blank(), 
    plot.margin = margin(t=0, r=0, b=-0.5, l=0, unit = "cm")
  ) + 
  # ylim(1120, 2050) +
  scale_x_continuous(labels = seq(0, 2000, by = 250), 
                     breaks = seq(0, 2000, by = 250), 
                     limits = c(0, 1500))





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

#grid.arrange(p1, p2, nrow = 2)

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