#This script plots Figure 4 for ERL paper
#January 2020
#Marcus Thomson
#NCEAS & IIASA (formerly)

library(tidyverse)
library(cowplot)
library(zoo)

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
  filter(gdd > 0)

load(file = "/home/thomson/Data/prec.Rdata") #prec - only over the growing season
# load(file = "/home/thomson/Data/prec_365days.Rdata") #prec - annual precipitation

prec <- prec %>% 
  rename("lon" = "longitude", 
         "lat" = "latitude")

# test_ppt <- prec %>% 
#   group_by(year_CE, lon, lat) %>% 
#   summarize(ppt_em = mean(ppt)) %>% 
#   mutate(ppt_ave = zoo::rollapply(ppt_em, 51, mean, fill = NA), 
#          ppt_sd  = zoo::rollapply(ppt_em, 51, sd, fill = NA)) %>% 
#   ungroup() %>% 
#   rename("longitude" = "lon", 
#          "latitude" = "lat")
# 
# test_ppt_sites <- inner_join(test_ppt, CalAges %>% 
#                                ungroup() %>% 
#                                dplyr::select(-n, -rav_density, -year))
# 
# test_ppt_av <- test_ppt_sites %>% 
#   group_by(longitude, latitude) %>% 
#   mutate(ppt_zsc = (ppt_ave - mean(ppt_ave))/sd(ppt_ave))

load("/home/thomson/Data/DF_elev.Rdata") #DF_elev
load("/home/thomson/Data/CalAges.Rdata") #CalAges

Sites_ages <- CalAges %>% 
  filter(year_CE >= 850, 
         significance == "Fremont") %>% 
  rename("lon" = "longitude", 
         "lat" = "latitude") %>% 
  dplyr::select(-rav_density)

sites <- Sites_ages %>% 
  ungroup() %>% 
  dplyr::select(-year, 
                -sum_density, 
                -elevation, 
                -n) %>% 
  group_by(lon, lat) %>% 
  summarize() %>% 
  ungroup()

#averages across ensemble membrs
GDD_sites <- left_join(sites, 
                       gdd, 
                       by = c("lon", "lat")) %>% 
  na.omit() %>% 
  group_by(lon, lat, year_CE) %>% 
  summarise(gdd_em = mean(gdd)) %>% 
  mutate(gdd_em_zsc = (gdd_em - mean(gdd_em))/sd(gdd_em)) %>% 
  mutate(gdd_mav = zoo::rollapply(gdd_em_zsc, 51, mean, fill = NA), 
         gdd_msd = zoo::rollapply(gdd_em, 51, sd, fill = NA)) %>% 
  ungroup()

# save(GDD_sites, file = "/home/thomson/Data/Rdata/GDD_sites.Rdata")

PPT_sites <- left_join(sites, 
                       prec, 
                       by = c("lon", "lat")) %>% 
  na.omit() %>%  
  group_by(lon, lat, year_CE) %>% 
  summarise(ppt_em = mean(ppt)) %>% 
  mutate(ppt_em_zsc = (ppt_em - mean(ppt_em))/sd(ppt_em)) %>% 
  mutate(ppt_mav = zoo::rollapply(ppt_em_zsc, 51, mean, fill = NA), 
         ppt_msd = zoo::rollapply(ppt_em, 51, sd, fill = NA)) %>% 
  ungroup()

# save(PPT_sites, file = "/home/thomson/Data/Rdata/PPT_sites.Rdata")
  
  # mutate(ppt_diff = ppt - mean(ppt), 
  #        ppt_zscore = ppt_diff/sd(ppt)) %>% 
  # mutate( ppt_diff_av = zoo::rollapply(ppt_diff, 101, mean, fill = NA), 
  #         ppt_zscore_av = zoo::rollapply(ppt_zscore, 101, mean, fill = NA), 
  #         ppt_sd = zoo::rollapply(ppt, 101, sd, fill = NA))

#averages across all sites
GDD_summary <- GDD_sites %>% 
  group_by(year_CE) %>% 
  summarise(gdd_zsc_av = mean(gdd_em_zsc), 
            gdd_av = mean(gdd_em)) %>% 
  mutate(gdd_mav = zoo::rollapply(gdd_zsc_av, 51, mean, fill = NA), 
         gdd_msd = zoo::rollapply(gdd_av, 51, sd, fill = NA))
  
PPT_summary <- PPT_sites %>% 
  group_by(year_CE) %>% 
  summarise(ppt_zsc_av = mean(ppt_em_zsc), 
            ppt_av = mean(ppt_em)) %>% 
  mutate(ppt_mav = zoo::rollapply(ppt_zsc_av, 51, mean, fill = NA), 
         ppt_msd = zoo::rollapply(ppt_av, 51, sd, fill = NA))


gdd_summary <- GDD_summary %>% 
  na.omit() %>% 
  rename("Year" = "year_CE")

#Makes annotation rectangles to highlight MCA and LIA shaded with gradients
#according to the strenght of the Moeberg_etal_2005 signal. Pretty slick!
#See: https://stackoverflow.com/questions/33965018/ggplot-how-to-produce-a-gradient-fill-within-a-geom-polygon
n_segs=500
alpha_mca <- data.frame(n_segs = seq(1, n_segs, by = 1),
                        year = seq(850, 1350, length.out = n_segs), 
                        anom = predict(loess(gdd_zsc_av ~ year_CE, data = GDD_summary), 
                                       data.frame(year_CE = seq(850, 1350, length.out = n_segs))) %>% 
                          as.vector(.)) %>% 
  mutate(alpha = 0.005*anom/max(anom))

alpha_lia <- data.frame(n_segs = seq(1, n_segs, by = 1),
                        year = seq(1350, 1850, length.out = n_segs), 
                        anom = predict(loess(gdd_zsc_av ~ year_CE, data = GDD_summary), 
                                       data.frame(year_CE = seq(850, 1350, length.out = n_segs))) %>% 
                          as.vector(.)) %>% 
  mutate(alpha = 0.005*anom/abs(min(anom)))



p1 <- ggplot(data = GDD_sites, 
             aes(x = year_CE)) + 
  geom_vline(data = alpha_mca, 
             aes(xintercept = year, 
                 alpha = alpha), 
             col = "#FC8D79", 
             size = 1) +
  geom_vline(data = alpha_lia, 
             aes(xintercept = year, 
                 alpha = alpha), 
             col = "#DAD1FB", 
             size = 1) + 
  geom_hline(yintercept = 0, size = 0.25, alpha = 0.6) + 
  geom_vline(xintercept = seq(800, 1500, by = 100), alpha = 0.25, size = 0.25) + 
  geom_line(aes(y = gdd_mav, group = lon), alpha = 0.3, size = 0.25, colour = "firebrick") + 
  geom_line(data = GDD_summary, aes(x = year_CE, y = gdd_mav), colour = "firebrick", size = 1) + 
  theme_minimal() + 
  labs(
    x = "cal BP", 
    y = "GDD z-score", 
    tag = "A"
    ) + 
  theme(
    legend.position = "none", 
  ) + 
  scale_x_continuous(position = "top", 
                     breaks = seq(0, 2000, by = 100), 
                     labels = seq(1950, -50, by = -100), 
                     limits = c(850, 1449)) + 
  annotate("text", 
           x = 1100, 
           y = -0.4, 
           label = "MCA", 
           size = 7, 
           alpha = 0.6) + 
  annotate("text", 
           x = 1400, 
           y = -0.4, 
           label = "LIA", 
           size = 7, 
           alpha = 0.6)

p2 <- ggplot(data = GDD_sites, aes(x = year_CE)) + 
  geom_vline(data = alpha_mca, 
             aes(xintercept = year, 
                 alpha = alpha), 
             col = "#FC8D79", 
             size = 1) +
  geom_vline(data = alpha_lia, 
             aes(xintercept = year, 
                 alpha = alpha), 
             col = "#DAD1FB", 
             size = 1) + 
  geom_vline(xintercept = seq(800, 1500, by = 100), alpha = 0.25, size = 0.25)  + 
  geom_line(aes(y = gdd_msd, group = lat), alpha = 0.3, size = 0.25, colour = "purple") + 
  geom_line(data = GDD_summary, aes(x = year_CE, y = gdd_msd), colour = "purple", size = 1) + 
  theme_minimal() + 
  labs(
    y = "sta. dev. (GDD)", 
    tag = "B"
  ) + 
  theme(
    legend.position = "none", 
    axis.title.x = element_blank(), 
    axis.text.x = element_blank()
  ) + 
  scale_x_continuous(breaks = seq(0, 2000, by = 100), 
                     limits = c(850, 1449))


p3 <- ggplot(data = PPT_sites, 
             aes(x = year_CE)) + 
  geom_vline(data = alpha_mca, 
             aes(xintercept = year, 
                 alpha = alpha), 
             col = "#FC8D79", 
             size = 1) +
  geom_vline(data = alpha_lia, 
             aes(xintercept = year, 
                 alpha = alpha), 
             col = "#DAD1FB", 
             size = 1) + 
  geom_hline(yintercept = 0, size = 0.25, alpha = 0.6) + 
  geom_vline(xintercept = seq(800, 1500, by = 100), alpha = 0.25, size = 0.25)  + 
  geom_line(aes(y = ppt_mav, group = lon), 
            alpha = 0.2, 
            size = 0.25, 
            colour = "darkblue") + 
  geom_line(data = PPT_summary, 
            aes(x = year_CE, 
                y = ppt_mav), 
            colour = "darkblue", 
            size = 1) + 
  theme_minimal() + 
  labs(
    y = "Prec. z-score", 
    tag = "C"
  ) + 
  theme(
    legend.position = "none", 
    axis.title.x = element_blank(), 
    axis.text.x = element_blank()
  ) + 
  scale_x_continuous(breaks = seq(0, 2000, by = 100), 
                     limits = c(850, 1449))

#These data come from the Canadian Archaeological Radiocarbon Database (CARD).
#Reference: Martindale, Andrew, Richard Morlan, Matthew Betts, Michael Blake, Konrad Gajewski, Michelle Chaput, Andrew Mason, and Pierre Vermeersch (2016) Canadian Archaeological Radiocarbon Database (CARD 2.1), accessed January 10, 2020.
#Range Creek data come from Prehistoric Archaeology in Range Creek Canyon, Utah: A Summary of Activities of the Range Creek Field Station.
#Reference: Boomgarden, S. A., Metcalfe, D., & Springer, C. (2014). Prehistoric archaeology in range Creek canyon, Utah: a summary of the activities of the range Creek field station. Utah Archaeology, 27(1), 9-32.
source("/home/thomson/Scripts/Fremont_radiocarbon_age_calibration.R") #CalAges

load(file = "/home/thomson/Data/Rdata/ap_sites_ckde_elev.Rdata") #tmp_ap
load(file = "/home/thomson/Data/Rdata/ap_sites_mean_ckde_elev.Rdata") #means_ap
load(file = "/home/thomson/Data/CalAges.Rdata") #CalAges

p4 <- ggplot() + 
  geom_line(data = tmp_ap %>% 
              filter(significance == "Fremont"), 
            aes(x = year_CE, 
                y = SP), 
            colour = "#786290", 
            alpha = 0.25, 
            size = 0.5) + 
  # scale_colour_manual(values = c("Fremont" = "#786290", 
  #                                "Other" = "#627F90")) + 
  geom_line(data = means_ap %>% 
              filter(significance == "Fremont"), 
            aes(x = year_CE, y = SP), 
            colour = "#786290") + 
   
  # geom_line(data = means_ap %>% 
  #             filter(significance == "Other"), 
  #           aes(x = year_CE, y = SP), 
  #           colour = "#627F90") + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title.x = element_blank(), 
    axis.text.x = element_blank()
  ) + 
  labs(
    y = "Sum. Prob.",
    tag = "D"
  ) + 
  scale_x_continuous(breaks = seq(0, 2000, by = 100)) + 
  coord_cartesian(xlim = c(850, 1450))

load(file = "/home/thomson/Data/Rdata/CKDE_means_coord.Rdata") #CKDE_means_coord

p5 <- ggplot() + 
  geom_smooth(data = CKDE_means_coord %>% 
                filter(significance == "Fremont", 
                       year_CE > 400 & year_CE < 1300), 
              aes(x = year_CE,
                  y = elevation, 
                  weight = sum_density),
              colour = "black", 
              method = "loess", 
              span = 0.6, 
              se = FALSE, 
              size = 1, 
              linetype = "solid",
              alpha = 0.5) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title.x = element_blank(), 
    axis.text.x = element_blank()
  ) + 
  labs(
    y = "elev. (m asl)",
    tag = "E"
  ) + 
  scale_x_continuous(breaks = seq(0, 2000, by = 100)) + 
  coord_cartesian(xlim = c(850, 1450), 
                  ylim = c(1750, 1850))



# 
# 
# 
# p4 <-   ggplot(data = CalAges %>% 
#                  filter(year_CE >= 0, 
#                         year_CE < 2000, 
#                         significance == "Fremont"),
#                aes(x = year_CE,
#                    y = elevation)) + 
#   geom_vline(xintercept = seq(800, 1500, by = 100), alpha = 0.25, size = 0.25) +
#   geom_point(aes(alpha = (sum_density)^0.25, 
#                  colour = significance), 
#              shape = "|", 
#              size = 1) + 
#   geom_smooth(aes(weight = sum_density, 
#                   colour = significance), 
#               method = "loess", 
#               span = 0.7, 
#               se = FALSE, 
#               size = 1, 
#               alpha = 0.7) + 
#   scale_color_manual(values = c("Fremont" = "#786290")) + 
#   theme_minimal() + 
#   labs(
#     x = "Year CE", 
#     y = "Elev. (m asl)", 
#     tag = "D"
#   ) + 
#   theme(
#     legend.position = "none", 
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank()
#   ) + 
#   scale_x_continuous(breaks = seq(0, 2000, by = 100)) + 
#   coord_cartesian(xlim = c(850, 1450), 
#                   ylim = c(1750, 1900))

#fill overlying histograms with onset time and end-time for occupations
#at each location, there should be an onset and end-time

begin_fremont <- CalAges %>% 
  filter(significance == "Fremont") %>% 
  group_by(longitude, latitude) %>% 
  summarize(year = min(year_CE, 
                           na.rm = TRUE)) %>% 
  mutate(group = "Onset")

end_fremont <- CalAges %>% 
  filter(significance == "Fremont") %>% 
  group_by(longitude, latitude) %>% 
  summarize(year = max(year_CE, 
                       na.rm = TRUE)) %>% 
  mutate(group = "Abandonment")
 
median_fremont <- CalAges %>% 
  filter(significance == "Fremont") %>% 
  group_by(longitude, latitude) %>% 
  summarize(year = median(year_CE, 
                       na.rm = TRUE)) %>% 
  mutate(group = "Median")

onset_fremont <- rbind(begin_fremont, 
                       end_fremont, 
                       median_fremont)

begin_ap <- CalAges %>%  
  group_by(longitude, latitude) %>% 
  summarize(year = min(year_CE, 
                       na.rm = TRUE)) %>% 
  mutate(group = "Occupation")

end_ap <- CalAges %>% 
  group_by(longitude, latitude) %>% 
  summarize(year = max(year_CE, 
                       na.rm = TRUE)) %>% 
  mutate(group = "Abandonment")

onset_ap <- rbind(begin_ap, 
                  end_ap) %>% 
  filter(year >= 0)

#this isn't a good way to compute longevity
#should be number of years occupied in the span
occ_ap <- onset_ap %>% 
  filter(group == "Occupation") %>% 
  mutate(occ_year = year) %>% 
  dplyr::select(longitude, latitude, occ_year)
aba_ap <- onset_ap %>% 
  filter(group == "Abandonment") %>% 
  mutate(aba_year = year) %>% 
  dplyr::select(longitude, latitude, aba_year)

occ_aba_ap <- inner_join(occ_ap, 
                         aba_ap, 
                         by = c("longitude", "latitude")) %>% 
  mutate(longevity = aba_year - occ_year)


# 
# p5 <- ggplot(data = onset_fremont %>% 
#            filter(group == "Median"),
#          aes(year, fill = group)) + 
#   geom_vline(xintercept = seq(800, 1500, by = 100), alpha = 0.25, size = 0.25) + 
#   geom_histogram(bins=13, 
#                  alpha = 0.7) + 
#   scale_fill_manual(values = c("Median" = "darkblue")) + 
#   geom_hline(yintercept = 0, size = 0.25, alpha = 0.6) + 
#   theme_minimal() + 
#   labs(
#     x = "Year CE", 
#     y = "Site count", 
#     tag = "D"
#   ) + 
#   theme(
#     legend.position = c(0.9, 0.9),
#     legend.title = element_blank(), 
#     axis.text.x = element_blank(), 
#     axis.title.x = element_blank()
#   ) + 
#   scale_x_continuous(breaks = seq(0, 2000, by = 100), 
#                      labels = seq(0, 2000, by = 100), 
#                      limits = c(850, 1449))

p6 <- ggplot(data = onset_fremont %>% 
               filter(group != "Median"),
             aes(year, fill = group)) + 
  geom_vline(xintercept = seq(800, 1500, by = 100), alpha = 0.25, size = 0.25) + 
  geom_histogram(bins=13, 
                 alpha = 0.7) + 
  scale_fill_manual(values = c("Onset" = "darkblue", 
                               "Abandonment" = "darkred")) + 
  geom_hline(yintercept = 0, size = 0.25, alpha = 0.6) + 
  theme_minimal() + 
  labs(
    x = "Year CE", 
    y = "Site count", 
    tag = "E", 
    fill = "Fremont occupation"
  ) + 
  guides(
    fill = guide_legend(reverse = TRUE)
  ) + 
  theme(
    legend.position = c(0.15, 0.9)
  ) + 
  scale_x_continuous(breaks = seq(0, 2000, by = 100), 
                     labels = seq(0, 2000, by = 100), 
                     limits = c(850, 1449))



load(file = "/home/thomson/Data/Rdata/expnull_frem.Rdata") #expnull_frem

roc_data <- expnull_frem$result.roc %>% 
  mutate(year_CE = 1950 - calBP)

expnull_frem_clean <- expnull_frem$result.roc %>% 
  na.omit() %>% 
  mutate(year_CE = 1950 - calBP)

envel <- rbind(data.frame(x = expnull_frem_clean$year_CE, 
                          y = expnull_frem_clean$lo.roc), 
               data.frame(x = expnull_frem_clean$year_CE, 
                          y = expnull_frem_clean$hi.roc) %>% 
                 arrange(., -x))


#Values from significant positive/negative local deviations for Rate of Change analysis from modelTest()
sig_devs <- rbind(data.frame(id = "positive", 
                             max_yrBP = c(1625, 1343, 364, 308, 153), 
                             min_yrBP = c(1583, 1085, 353, 206, 57)), 
                  data.frame(id = "negative",  
                             max_yrBP = c(1851, 823, 595, 198), 
                             min_yrBP = c(1658, 623, 368, 155))) %>% 
  mutate(min_yearCE = 1950-max_yrBP, 
         max_yearCE = 1950-min_yrBP, 
         min_SP = -1, 
         max_SP = 1)

p7 <- ggplot(data = roc_data, 
             aes(x = year_CE)) + 
  geom_hline(yintercept = 0, 
             alpha = 0.5) + 
  geom_polygon(data = envel, 
               aes(x = x, y = y), 
               fill = "#786290", 
               alpha = 0.3) + 
  geom_line(aes(y = roc), 
            size = 1, 
            colour = "#786290") + 
  geom_vline(xintercept = seq(sig_devs$min_yearCE[1], sig_devs$max_yearCE[1], 1), 
             colour = "blue", 
             alpha = 0.1, 
             size = 0.2) + 
  geom_vline(xintercept = seq(sig_devs$min_yearCE[2], sig_devs$max_yearCE[2], 1), 
             colour = "blue", 
             alpha = 0.1, 
             size = 0.2) + 
  geom_vline(xintercept = seq(sig_devs$min_yearCE[3], sig_devs$max_yearCE[3], 1), 
             colour = "blue", 
             alpha = 0.1, 
             size = 0.2) + 
  geom_vline(xintercept = seq(sig_devs$min_yearCE[4], sig_devs$max_yearCE[4], 1), 
             colour = "blue", 
             alpha = 0.1, 
             size = 0.2) + 
  geom_vline(xintercept = seq(sig_devs$min_yearCE[5], sig_devs$max_yearCE[5], 1), 
             colour = "blue", 
             alpha = 0.1, 
             size = 0.2) + 
  geom_vline(xintercept = seq(sig_devs$min_yearCE[6], sig_devs$max_yearCE[6], 1), 
             colour = "red", 
             alpha = 0.1, 
             size = 0.2) + 
  geom_vline(xintercept = seq(sig_devs$min_yearCE[7], sig_devs$max_yearCE[7], 1), 
             colour = "red", 
             alpha = 0.1, 
             size = 0.2) + 
  geom_vline(xintercept = seq(sig_devs$min_yearCE[8], sig_devs$max_yearCE[8], 1), 
             colour = "red", 
             alpha = 0.1, 
             size = 0.2) + 
  geom_vline(xintercept = seq(sig_devs$min_yearCE[9], sig_devs$max_yearCE[9], 1), 
             colour = "red", 
             alpha = 0.1, 
             size = 0.2) + 
  labs(
    x = "Year CE", 
    y = "Frem. occ. \nrate of change", 
    tag = "E"
  )  + 
  theme_minimal() + 
  scale_x_continuous(breaks = seq(0, 2000, by = 100), 
                     labels = seq(0, 2000, by = 100)) + 
  coord_cartesian(xlim = c(850, 1449), 
                  ylim = c(-0.032, 0.01))



png(file="/home/thomson/ERL_paper/Plots/Figure4_updated.png", w = 1800, h = 2600, res=300)
cowplot::plot_grid(p1, 
                   p2,
                   p3,
                   p4, 
                   # p5, 
                   p7, 
                   align = "v", 
                   nrow = 5, 
                   rel_heights = c(1.1, 1.1, 1.1, 1, 1.2))
dev.off()

