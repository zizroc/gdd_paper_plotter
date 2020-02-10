#This script plots Figure 2 for ERL paper.
#January 2020
#Marcus Thomson
#NCEAS & IIASA (formerly)

library(tidyverse)
library(Bchron)
library(gridExtra)
library(zoo)
library(gtable)
library(grid)

#Set paths
prox_path <- path.expand("/home/thomson/Data/Paleoclimate/")
scrp_path <- path.expand("/home/thomson/Scripts/")
radc_path <- path.expand("/home/thomson/Data/CARD/")
rdat_path <- path.expand("/home/thomson/Data/Rdata/")

source(paste0(scrp_path, "SW_paleoclimate_datasets.R"))
#loads data for moberg, etc.

#These data come from the Canadian Archaeological Radiocarbon Database (CARD).
#Reference: Martindale, Andrew, Richard Morlan, Matthew Betts, Michael Blake, Konrad Gajewski, Michelle Chaput, Andrew Mason, and Pierre Vermeersch (2016) Canadian Archaeological Radiocarbon Database (CARD 2.1), accessed January 10, 2020.
load(paste0(rdat_path, "AP_sites_ages_elev.Rdata")) #DF_elev

# #These data come from the Canadian Archaeological Radiocarbon Database (CARD).
# #Reference: Martindale, Andrew, Richard Morlan, Matthew Betts, Michael Blake, Konrad Gajewski, Michelle Chaput, Andrew Mason, and Pierre Vermeersch (2016) Canadian Archaeological Radiocarbon Database (CARD 2.1), accessed January 10, 2020.
# file <- dir("/mnt/chromeos/MyFiles/Data files", full.names = TRUE)
# df   <- read.csv(file[grep("allUTandFremont", file)], header = TRUE, stringsAsFactors = FALSE)
# 
# DF <- df %>% 
#   dplyr::mutate(mean = Normalized.age, 
#                 sigma = NA.Sigma) %>% 
#   dplyr::select(mean, 
#                 sigma, 
#                 Significance, 
#                 Latitude, 
#                 Longitude)
# 
# #Discriminates between "Fremont" and "non-Fremont" archaeological contexts.
# #This filter uses the Significance IDs given in CARD. "Fremont" is defined as 
# #anything "Fremont" and "Fremont?", and "Other" is any data satisfying the search 
# #criteria used in CARD (i.e., mentions of "Fremont" and/or "Utah") that was not 
# #included in "Fremont".
# DF_fremont <- DF %>% 
#   filter(Significance == "Fremont" | Significance == "Fremont?") %>% 
#   na.omit()
# 
# DF_other <- DF %>% 
#   filter(Significance != "Fremont" & Significance != "Fremont?") %>% 
#   na.omit()

DF_fremont <- DF_elev %>% 
  filter(significance == "Fremont")
DF_other <- DF_elev %>% 
  filter(significance == "Other")

#Calibrate according to Intcal13 curve.
ages_fremont <- BchronCalibrate(ages = DF_fremont$mean, 
                                ageSds = DF_fremont$sigma, 
                                calCurves = rep("intcal13", 
                                                length(DF_fremont$mean)))

ages_other <- BchronCalibrate(ages = DF_other$mean, 
                              ageSds = DF_other$sigma, 
                              calCurves = rep("intcal13", 
                                              length(DF_other$mean)))

#The following smooths the SPD with a window of k years.
k = 31
for(i in seq_along(ages_fremont)){
  tmp <- data.frame(ages_fremont[[i]]$ageGrid, 
                    ages_fremont[[i]]$densities)
  if(i==1) Tmp <- tmp
  else Tmp <- rbind(Tmp, tmp)
  rm(tmp)
}
CalAges_fremont <- Tmp %>% 
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
CalAges_other <- Tmp %>% 
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

CalAges <- rbind(CalAges_fremont, CalAges_other)

#This just cleans up the plotting.
CalAges[CalAges$year_CE==1950,]$sum_density <- 0
CalAges[CalAges$year_CE==0,]$sum_density <- 0


#Makes annotation rectangles to highlight MCA and LIA shaded with gradients
#according to the strenght of the Moeberg_etal_2005 signal. Pretty slick!
#See: https://stackoverflow.com/questions/33965018/ggplot-how-to-produce-a-gradient-fill-within-a-geom-polygon
n_segs=500
alpha_mca <- data.frame(n_segs = seq(1, n_segs, by = 1),
                        year = seq(850, 1350, length.out = n_segs), 
                        anom = predict(loess(Anomaly ~ Year, data = moberg), data.frame(Year = seq(850, 1350, length.out = n_segs))) %>% 
                          as.vector(.)) %>% 
  mutate(alpha = (0.05/abs(anom/max(anom)))^2)
alpha_lia <- data.frame(n_segs = seq(1, n_segs, by = 1),
                        year = seq(1350, 1850, length.out = n_segs), 
                        anom = predict(loess(Anomaly ~ Year, data = moberg), data.frame(Year = seq(850, 1350, length.out = n_segs))) %>% 
                          as.vector(.)) %>% 
  mutate(alpha = (0.05/abs(anom/min(anom)))^2)

p1 <- ggplot(data = moberg, 
             aes(x = Year)) + 
  geom_vline(data = alpha_mca, 
             aes(xintercept = year, 
                 alpha = alpha), 
             col = "#FC8D79", 
             size = 0.1) +
  geom_vline(data = alpha_lia, 
             aes(xintercept = year, 
                 alpha = alpha), 
             col = "#DAD1FB", 
             size = 0.1) + 
  geom_hline(yintercept = 0, 
             size = 0.25, 
             alpha = 0.7) + 
  # geom_line(data = pages2k_DF, 
  #           aes(x = year_CE, 
  #               y = smoothed_anomaly), 
  #           size = 0.8, 
  #           colour = "blue", 
  #           alpha = 0.7,  
  #           inherit.aes = FALSE) + 
  # geom_line(data = christiansen2012_DF, 
  #           aes(x = year_CE, 
  #               y = anom_smoothed), 
  #           size = 0.8, 
#           colour = "purple", 
#           alpha = 0.7, 
#           inherit.aes = FALSE) + 
geom_line(mapping = aes(y = LF), 
          size = 0.8, 
          colour = "red", 
          alpha = 0.7) + 
  geom_line(mapping = aes(y = LF_low),
            size = 0.2,
            colour = "red", 
            alpha = 0.7) + 
  geom_line(mapping = aes(y = LF_high),
            size = 0.2, 
            colour = "red", 
            alpha = 0.7) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.position = "none"
  ) + 
  labs(x = "cal BP", 
       y = "Temp. anom. (°C)", 
       tag = "A") + 
  annotate("text", 
           x = 1100, 
           y = -0.7, 
           label = "MCA", 
           size = 8, 
           alpha = 0.6) + 
  annotate("text", 
           x = 1600, 
           y = -0.1, 
           label = "LIA", 
           size = 8, 
           alpha = 0.6) + 
  # annotate("text", 
  #          x = c(100, 100, 100), 
  #          y = c(0.21, -0.12, -0.44), 
  #          label = c("Christiansen & \nLjungqvist (2012)", 
  #                    "PAGES2k (2019)", 
  #                    "Moberg et al. (2005)"), 
  #          size = 3) + 
  scale_x_continuous(position = "top", 
                     breaks = seq(0, 2000, by = 250), 
                     labels = seq(1950, -50, by = -250), 
                     limits = c(0, 2000))

p2 <- ggplot(data = CalAges, 
             mapping = aes(x = year_CE, 
                           y = rav_density, 
                           fill = significance)) + 
  scale_fill_manual(values = c("Fremont" = "#786290", 
                               "Other" = "#627F90")) + 
  geom_area(alpha = 0.6, position = "stack", col = "black", size = 0.2) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.spacing.x = unit(0.5, 'cm'), 
    legend.text = element_text(margin = margin(r = 20))
  ) + 
  labs(x = "Year CE", 
       y = "SPD", 
       tag = "B") + 
  scale_x_continuous(position = "bottom", 
                     breaks = seq(0, 2000, by = 250), 
                     labels = seq(0, 2000, by = 250), 
                     limits = c(0, 2000))

#grid.arrange(p1, p2, nrow = 2)

#Better aligns plot stack than grid.arrange().
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

g <- rbind(g1,g2, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths) # use the largest widths
# g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)

#Computes Pearson correlation parameters.

# #sum_density is the sum of all SPDs for a given year
# #rav_density is rolling average of density (smoothed by k years)
# CalAges_sum <- CalAges %>% 
#   group_by(year_CE) %>% 
#   summarise(den_sum = sum(sum_density), 
#             rav_sum = sum(rav_density)) %>% 
#   filter(year_CE >= 0) %>% 
#   rename(Year = year_CE)
# 
# CalAges_moberg <- inner_join(CalAges_sum, moberg, by = "Year") %>% 
#   filter(Year >= 500, 
#          Year < 1450)
# 
# cor.test(CalAges_moberg$rav_sum, CalAges_moberg$Anomaly)
# 
# cor.test(diff(CalAges_moberg$rav_sum,1), diff(CalAges_moberg$Anomaly,1))
# 
# 
# chisq.test(CalAges_moberg$rav_sum/max(CalAges_moberg$rav_sum), 
#         CalAges_moberg$Anomaly/max(CalAges_moberg$Anomaly), 
#         simulate.p.value = TRUE)
# 
# chisq.test(CalAges_moberg$rav_sum, CalAges_moberg$Anomaly)


