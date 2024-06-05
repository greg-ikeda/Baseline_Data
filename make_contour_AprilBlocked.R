#### SETUP ####
# How to use this file:
# 1. Edit parameters in lines 18-46
# 2. Run setup (this section) and remove short casts (next section)
# 3. Run any other sections you'd like figures from

rm(list = ls())
library(tidyverse)
library(viridis)
library(lubridate)
library(kcmarine)
library(readxl)
library(metR)
library(cmocean)
library(RColorBrewer)
z_drive <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/"
source(paste0(z_drive, 
              "Figures and Scripts/CTD Contour - R/contour_functions.R"))

# What station do you want figures from?
station <- "KSBP01"

# Where do you want to save figures?
save_folder <- paste0(z_drive, 
                      "Figures and Scripts/CTD Contour - R/Figures/")

# Select years to plot - scales will be the same for all years
min_yoi <- 2020  # first year to plot
max_yoi <- 2023 # last year to plot

# Do you want contour lines on your sigma-theta plots?
sigmat_contour_alpha <- 0.1  # if you want contour lines, use 0.1; else use 0

# What do you want your baseline to be for anomaly plots?
base_start <- 1998
base_end <- 2013

# How wide do you want your depth bins (0.5, 1, 2, 5 probably best)
bin_width <- 0.5

# Do you want a uniform figure bottom (i.e. cut off deep data)?
# Only applies to regular contours, anomaly plots will always be cut off
fig_cutoff <- TRUE

# How narrowly spaced do you want the color bins in each of these plots?
# Original values are in comments following semi-colon
acc_DO_anom <- 0.05  # DO anomaly; 0.05
acc_DO <- 0.2  # DO; 0.2
acc_S_anom <- 0.025  # salinity anomaly; 0.025
acc_S <- 0.1  # salinity; 0.1
acc_T_anom <- 0.05  # temperature anomaly; 0.05
acc_T <- 0.2  # temperature; 0.2
acc_sigmaT <- 0.2  # sigma-theta density; 0.2

# No need to change anything below here!
folder <- paste0(z_drive, 
                 "CTD_data_repository/", 
                 station, "/", sep = "")
fname <- list.files(folder, pattern = "_qcd.csv")

CTDdata <- bin_CTD(paste0(folder, fname), 
                   bin_width)

baseline_data <- CTDdata %>% 
  filter(Year >= base_start, 
         Year <= base_end)

depth_baseline <- baseline_data %>% 
  group_by(Year, YearDay) %>% 
  summarize(MaxDepth = max(BinDepth, na.rm = T)) %>% 
  arrange(MaxDepth) %>% 
  pull(MaxDepth)
while (min(depth_baseline) < (mean(depth_baseline) - 2*sd(depth_baseline))) {
  depth_baseline <- depth_baseline[-1]
}
max_depth_anom <- min(depth_baseline)

baseline <- baseline_data %>% 
  group_by(Month, BinDepth) %>% 
  filter(BinDepth <= max_depth_anom) %>% 
  summarize(BaselineDensity = mean(Density, na.rm = T), 
            BaselineDO = mean(DO, na.rm = T), 
            BaselineSigmaT = mean(SigmaTheta, na.rm = T), 
            BaselineSalinity = mean(Salinity, na.rm = T), 
            BaselineTemperature = mean(Temperature, na.rm = T))

anomaly_data <- full_join(CTDdata, baseline) %>% 
  mutate(DensityAnomaly = ifelse(is.na(BaselineDensity), NA, 
                                 Density - BaselineDensity), 
         DOAnomaly = ifelse(is.na(BaselineDO), NA, 
                            DO - BaselineDO), 
         SigmaTAnomaly = ifelse(is.na(BaselineSigmaT), NA, 
                                SigmaTheta - BaselineSigmaT), 
         SalinityAnomaly = ifelse(is.na(BaselineSalinity), NA, 
                                  Salinity - BaselineSalinity), 
         TemperatureAnomaly = ifelse(is.na(BaselineTemperature), NA, 
                                     Temperature - BaselineTemperature))

#### Remove short casts ####
short_stations <- c("LTBC43", "LSVV01")
if (!(station %in% short_stations)) {
  temp <- anomaly_data %>% 
    group_by(Year, YearDay) %>% 
    summarize(Max_BinDepth = max(BinDepth)) %>% 
    mutate(Short = if_else(Max_BinDepth <= 40, 
                           T, F))
}

anomaly_data <- left_join(anomaly_data, temp)

anomaly_data <- anomaly_data %>% 
  filter(!Short) %>% 
  select(!Short)


#### Temperature anomaly contour plot ####
limit_data <- anomaly_data %>% 
  filter(Year >= min_yoi, 
         Year <= max_yoi, 
         BinDepth <= max_depth_anom)
lim <- max(abs(min(limit_data$TemperatureAnomaly, na.rm = T)), 
           abs(max(limit_data$TemperatureAnomaly, na.rm = T)))
lim <- round_any(lim, accuracy = acc_T_anom, f = ceiling)

mybreaks <- seq(-lim, lim, by = acc_T_anom)  # originally 0.02
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""

for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, 
           !is.na(TemperatureAnomaly), 
           BinDepth <= max_depth_anom) %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(TemperatureAnomaly = mean(TemperatureAnomaly, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !is.na(TemperatureAnomaly), 
           BinDepth <= max_depth_anom) %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>%
    group_by(YearDay, BinDepth) %>% 
    summarize(TemperatureAnomaly = mean(TemperatureAnomaly, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !is.na(TemperatureAnomaly), 
             BinDepth <= max_depth_anom) %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>%
      group_by(YearDay, BinDepth) %>% 
      summarize(TemperatureAnomaly = mean(TemperatureAnomaly, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, y = BinDepth, z = TemperatureAnomaly), 
                            na.fill = T, breaks = mybreaks) + 
    scale_fill_craftfermenter(
      breaks = mybreaks, 
      palette = "RdBu", 
      direction = -1, 
      limits = c(-lim, lim), 
      labels = mylabels, 
      guide = guide_colorbar(show.limits = T, ticks = F)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", fill = expression(degree*C), 
         title = paste(yoi, station, "temperature anomaly"))
  ggsave(paste(save_folder, station, "_", yoi, "_", "T_anomaly_contour.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Salinity anomaly contour plot ####
limit_data <- anomaly_data %>% filter(Year >= min_yoi, 
                                      Year <= max_yoi, 
                                      BinDepth >= 5, 
                                      BinDepth <= max_depth_anom)
lim <- max(abs(min(limit_data$SalinityAnomaly, na.rm = T)), 
           abs(max(limit_data$SalinityAnomaly, na.rm = T)))
lim <- round(lim)

mybreaks <- seq(-lim, lim, by = acc_S_anom)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
mylabels[1] <- paste0("<", mylabels[1])
mylabels[length(mylabels)] <- paste0(">", tail(mylabels, 1))

for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, 
           !is.na(SalinityAnomaly), 
           BinDepth <= max_depth_anom) %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(SalinityAnomaly = mean(SalinityAnomaly, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !is.na(SalinityAnomaly), 
           BinDepth <= max_depth_anom) %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(SalinityAnomaly = mean(SalinityAnomaly, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !is.na(SalinityAnomaly), 
             BinDepth <= max_depth_anom) %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>% 
      group_by(YearDay, BinDepth) %>% 
      summarize(SalinityAnomaly = mean(SalinityAnomaly, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  data_to_plot <- data_to_plot %>% 
    mutate(SalinityAnomaly = case_when(SalinityAnomaly < -lim ~ -lim + 1e-3, 
                                       SalinityAnomaly > lim ~ lim - 1e-3, 
                                       TRUE ~ SalinityAnomaly))
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, y = BinDepth, z = SalinityAnomaly), 
                            na.fill = T, breaks = mybreaks) + 
    scale_fill_craftfermenter(
      breaks = mybreaks, 
      palette = "RdBu", 
      direction = -1, 
      limits = c(-lim, lim), 
      labels = mylabels, 
      guide = guide_colorbar(show.limits = T, ticks = F)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", fill = "PSU", 
         title = paste(yoi, station, "salinity anomaly"))
  ggsave(paste(save_folder, station, "_", yoi, "_", "S_anomaly_contour.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Oxygen anomaly contour plot ####
limit_data <- anomaly_data %>% filter(Year >= min_yoi, 
                                      Year <= max_yoi, 
                                      BinDepth <= max_depth_anom)
lim <- max(abs(min(limit_data$DOAnomaly, na.rm = T)), 
           abs(max(limit_data$DOAnomaly, na.rm = T)))
lim <- round_any(lim, accuracy = acc_DO_anom, f = ceiling)

mybreaks <- mybreaks <- seq(-lim, lim, by = acc_DO_anom)
mylabels <- round(mybreaks,2)
mylabels[!(mylabels == round(mylabels))] <- ""

for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, 
           !is.na(DOAnomaly), 
           BinDepth <= max_depth_anom) %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(DOAnomaly = mean(DOAnomaly, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !is.na(DOAnomaly), 
           BinDepth <= max_depth_anom) %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(DOAnomaly = mean(DOAnomaly, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !is.na(DOAnomaly), 
             BinDepth <= max_depth_anom) %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>% 
      group_by(YearDay, BinDepth) %>% 
      summarize(DOAnomaly = mean(DOAnomaly, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, y = BinDepth, z = DOAnomaly), 
                            na.fill = T, breaks = mybreaks) + 
    scale_fill_craftfermenter(
      breaks = mybreaks, 
      palette = "RdBu", 
      direction = -1, 
      limits = c(-lim, lim), 
      labels = mylabels, 
      guide = guide_colorbar(show.limits = T, ticks = F)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", fill = "mg/L", 
         title = paste(yoi, station, "DO anomaly"))
  ggsave(paste(save_folder, station, "_", yoi, "_DO_anomaly_contour.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Temperature contour plot - full ####
if (fig_cutoff) {
  maxdepth_data <- anomaly_data %>% 
    filter(Year >= min_yoi, Year <= max_yoi) %>% 
    group_by(Year, YearDay) %>% 
    summarize(MaxDepth = max(BinDepth, na.rm = T))
  
  max_depth <- min(maxdepth_data$MaxDepth)
}

limit_data <- anomaly_data %>% 
  filter(Year >= min_yoi, 
         Year <= max_yoi) %>% 
  {if (fig_cutoff) filter(., BinDepth <= max_depth) else .}

min_lim <- round_any(min(limit_data$Temperature, na.rm = T), 
                     accuracy = acc_T, f = floor)
max_lim <- round_any(max(limit_data$Temperature, na.rm = T), 
                     accuracy = acc_T, f = ceiling)

mybreaks <- seq(min_lim, max_lim, by = acc_T)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""

for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, 
           !is.na(Temperature)) %>% 
    {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(Temperature = mean(Temperature, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !(is.na(Temperature))) %>% 
    {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>%
    group_by(YearDay, BinDepth) %>% 
    summarize(Temperature = mean(Temperature, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !(is.na(Temperature))) %>% 
      {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>% 
      group_by(YearDay, BinDepth) %>% 
      summarize(Temperature = mean(Temperature, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, 
                                y = BinDepth, 
                                z = Temperature), 
                            na.fill = fig_cutoff, 
                            breaks = mybreaks) + 
    scale_fill_cmocean(name = "thermal", 
                       breaks = mybreaks, 
                       limits = c(min_lim, max_lim), 
                       labels = mylabels, 
                       guide = guide_colorbar(show.limits = T, 
                                              ticks = F)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", 
         title = paste(yoi, station, "temperature"), 
         fill = expression( degree*C))
  ggsave(paste(save_folder, station, "_", yoi, "_T_contour.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Temperature contour plot - surface ####
for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, !is.na(Temperature), BinDepth <= 50) %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(Temperature = mean(Temperature, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !(is.na(Temperature)), 
           BinDepth <= 50) %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>%
    group_by(YearDay, BinDepth) %>% 
    summarize(Temperature = mean(Temperature, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !(is.na(Temperature)), 
             BinDepth <= 50) %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>% 
      group_by(YearDay, BinDepth) %>% 
      summarize(Temperature = mean(Temperature, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, 
                                y = BinDepth, 
                                z = Temperature), 
                            na.fill = T, 
                            breaks = mybreaks) + 
    scale_fill_cmocean(name = "thermal", 
                       breaks = mybreaks, 
                       limits = c(min_lim, max_lim), 
                       labels = mylabels, 
                       guide = guide_colorbar(show.limits = T, 
                                              ticks = F)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", 
         title = paste(yoi, station, "temperature - surface"), 
         fill = expression( degree*C))
  ggsave(paste(save_folder, station, "_", yoi, "_T_contour_surface.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Salinity contour plot - full ####
min_S <- 28

if (fig_cutoff) {
  maxdepth_data <- anomaly_data %>% 
    filter(Year >= min_yoi, Year <= max_yoi) %>% 
    group_by(Year, YearDay) %>% 
    summarize(MaxDepth = max(BinDepth, na.rm = T))
  
  max_depth <- min(maxdepth_data$MaxDepth)
}

limit_data <- anomaly_data %>% filter(Year >= min_yoi, 
                                      Year <= max_yoi) %>% 
  {if (fig_cutoff) filter(., BinDepth <= max_depth) else .}

min_lim <- min_S
max_lim <- round_any(max(limit_data$Salinity, na.rm = T), 
                     accuracy = acc_S, f = ceiling)

mybreaks <- seq(min_lim, max_lim, by = acc_S)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
mylabels[1] <- paste0("<", mylabels[1])

# Full profile
for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, 
           !is.na(Salinity)) %>% 
    {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(Salinity = mean(Salinity, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !(is.na(Salinity))) %>% 
    {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>%
    group_by(YearDay, BinDepth) %>% 
    summarize(Salinity = mean(Salinity, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !(is.na(Salinity))) %>% 
      {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>% 
      group_by(YearDay, BinDepth) %>% 
      summarize(Salinity = mean(Salinity, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  data_to_plot$Salinity[data_to_plot$Salinity < min_S] <- min_S
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, 
                                y = BinDepth, 
                                z = Salinity), 
                            na.fill = fig_cutoff, 
                            breaks = mybreaks) + 
    scale_fill_cmocean(name = "haline", 
                       breaks = mybreaks, 
                       limits = c(min_lim, max_lim), 
                       labels = mylabels, 
                       guide = guide_colorbar(show.limits = T, 
                                              ticks = F, 
                                              reverse = T)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", fill = "PSU", 
         title = paste(yoi, station, "salinity"))
  ggsave(paste(save_folder, station, "_", yoi, "_S_contour.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Salinity contour plot - surface ####
for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, !is.na(Salinity), BinDepth <= 50) %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(Salinity = mean(Salinity, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !(is.na(Salinity)), 
           BinDepth <= 50) %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>%
    group_by(YearDay, BinDepth) %>% 
    summarize(Salinity = mean(Salinity, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !(is.na(Salinity)), 
             BinDepth <= 50) %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>% 
      group_by(YearDay, BinDepth) %>% 
      summarize(Salinity = mean(Salinity, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  data_to_plot$Salinity[data_to_plot$Salinity < min_S] <- min_S
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, y = BinDepth, z = Salinity), 
                            na.fill = T, breaks = mybreaks) + 
    scale_fill_cmocean(name = "haline", 
                       breaks = mybreaks, 
                       limits = c(min_lim, max_lim), 
                       labels = mylabels, 
                       guide = guide_colorbar(show.limits = T, ticks = F, reverse = T)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", fill = "PSU", 
         title = paste(yoi, station, "salinity - surface"))
  ggsave(paste(save_folder, station, "_", yoi, "_S_contour_surface.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Sigma-t contour plot - full ####
if (fig_cutoff) {
  maxdepth_data <- anomaly_data %>% 
    filter(Year >= min_yoi, Year <= max_yoi) %>% 
    group_by(Year, YearDay) %>% 
    summarize(MaxDepth = max(BinDepth, na.rm = T))
  
  max_depth <- min(maxdepth_data$MaxDepth)
}

min_lim <- 20
max_lim <- 23.6

mybreaks <- seq(min_lim, max_lim, by = acc_sigmaT)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
mylabels[1] <- paste0("<", min_lim)
mylabels[length(mylabels)] <- paste0(">", max_lim)

for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, 
           !is.na(SigmaTheta))  %>% 
    {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(SigmaTheta = mean(SigmaTheta, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !(is.na(SigmaTheta)))%>% 
    {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>%
    group_by(YearDay, BinDepth) %>% 
    summarize(SigmaTheta = mean(SigmaTheta, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !(is.na(SigmaTheta))) %>% 
      {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>% 
      group_by(YearDay, BinDepth) %>% 
      summarize(SigmaTheta = mean(SigmaTheta, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  data_to_plot$SigmaTheta[data_to_plot$SigmaTheta < min_lim] <- min_lim + 1e-3
  data_to_plot$SigmaTheta[data_to_plot$SigmaTheta > max_lim] <- max_lim - 1e-3
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, 
                                y = BinDepth, 
                                z = SigmaTheta), 
                            na.fill = fig_cutoff, 
                            breaks = mybreaks, 
                            color = alpha("white", sigmat_contour_alpha)) + 
    scale_fill_cmocean(name = "dense", 
                       breaks = mybreaks, 
                       limits = c(min_lim, max_lim), 
                       labels = mylabels, 
                       guide = guide_colorbar(show.limits = T, 
                                              ticks = F, 
                                              reverse = T)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", fill = expression(kg/m^3), 
         title = bquote(.(yoi)~.(station)~sigma[Theta]))
  ggsave(paste(save_folder, station, "_", yoi, "_sigmat_contour.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Sigma-t contour plot - surface ####
for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, !is.na(SigmaTheta), BinDepth <= 50) %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(SigmaTheta = mean(SigmaTheta, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !(is.na(SigmaTheta)), 
           BinDepth <= 50) %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>%
    group_by(YearDay, BinDepth) %>% 
    summarize(SigmaTheta = mean(SigmaTheta, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !(is.na(SigmaTheta)), 
             BinDepth <= 50) %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>% 
      group_by(YearDay, BinDepth) %>% 
      summarize(SigmaTheta = mean(SigmaTheta, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  data_to_plot$SigmaTheta[data_to_plot$SigmaTheta < min_lim] <- min_lim + 1e-3
  data_to_plot$SigmaTheta[data_to_plot$SigmaTheta > max_lim] <- max_lim - 1e-3
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, y = BinDepth, z = SigmaTheta), 
                            na.fill = T, breaks = mybreaks, color = alpha("white", sigmat_contour_alpha)) + 
    scale_fill_cmocean(name = "dense", 
                       breaks = mybreaks, 
                       limits = c(min_lim, max_lim), 
                       labels = mylabels, 
                       guide = guide_colorbar(show.limits = T, ticks = F, reverse = T)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", fill = expression(kg/m^3), 
         title = bquote(.(yoi)~.(station)~sigma[Theta]~-surface))
  ggsave(paste(save_folder, station, "_", yoi, "_sigmat_contour_surface.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Oxygen contour plot - full ####
if (fig_cutoff) {
  maxdepth_data <- anomaly_data %>% 
    filter(Year >= min_yoi, Year <= max_yoi) %>% 
    group_by(Year, YearDay) %>% 
    summarize(MaxDepth = max(BinDepth, na.rm = T))
  
  max_depth <- min(maxdepth_data$MaxDepth)
}

limit_data <- anomaly_data %>% 
  filter(Year >= min_yoi, Year <= max_yoi) %>% 
  {if (fig_cutoff) filter(., BinDepth <= max_depth) else .}
min_lim <- round_any(min(limit_data$DO, na.rm = T),
                     accuracy = acc_DO, f = floor)
max_lim <- round_any(max(limit_data$DO, na.rm = T),
                     accuracy = acc_DO, f = ceiling)

mybreaks <- seq(min_lim, max_lim, by = acc_DO)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""

# Full depth
for (yoi in min_yoi:max_yoi) {
  data_to_plot <- anomaly_data %>% 
    filter(Year == yoi, 
           !is.na(DO)) %>% 
    {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
    group_by(YearDay, BinDepth) %>% 
    summarize(DO = mean(DO, na.rm = T))
  
  extra_data_before <- anomaly_data %>% 
    filter(Year == yoi - 1, 
           !(is.na(DO))) %>% 
    {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
    filter(YearDay == max(YearDay)) %>% 
    mutate(YearDay = YearDay - 365) %>%
    group_by(YearDay, BinDepth) %>% 
    summarize(DO = mean(DO, na.rm = T))
  data_to_plot <- full_join(data_to_plot, extra_data_before)
  
  if (max(anomaly_data$Year) > yoi) {
    extra_data_after <- anomaly_data %>% 
      filter(Year == yoi + 1, 
             !(is.na(DO))) %>% 
      {if (fig_cutoff) filter(., BinDepth <= max_depth) else .} %>% 
      filter(YearDay == min(YearDay)) %>% 
      mutate(YearDay = YearDay + 365) %>% 
      group_by(YearDay, BinDepth) %>% 
      summarize(DO = mean(DO, na.rm = T))
    data_to_plot <- full_join(data_to_plot, extra_data_after)
  }
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    metR::geom_contour_fill(aes(x = YearDay, 
                                y = BinDepth, 
                                z = DO), 
                            na.fill = fig_cutoff, 
                            breaks = mybreaks) + 
    scale_fill_craftfermenter(
      breaks = mybreaks, 
      palette = "PuBu", 
      direction = -1, 
      limits = c(min_lim, max_lim), 
      labels = mylabels, 
      guide = guide_colorbar(show.limits = T, ticks = F)) + 
    metR::geom_contour2(aes(x = YearDay, y = BinDepth, z = DO), 
                        na.fill = T, breaks = 7) + 
    # scale_fill_cmocean(name = "oxy", 
    #                    breaks = mybreaks, 
    #                    limits = c(min_lim, max_lim), 
    #                    labels = mylabels, 
    #                    guide = guide_colorbar(show.limits = T, ticks = F)) + 
    scale_y_reverse(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(0, 366)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                  yday(paste(yoi, "-02-01", sep = "")), 
                                  yday(paste(yoi, "-03-01", sep = "")), 
                                  yday(paste(yoi, "-04-01", sep = "")), 
                                  yday(paste(yoi, "-05-01", sep = "")), 
                                  yday(paste(yoi, "-06-01", sep = "")), 
                                  yday(paste(yoi, "-07-01", sep = "")), 
                                  yday(paste(yoi, "-08-01", sep = "")), 
                                  yday(paste(yoi, "-09-01", sep = "")), 
                                  yday(paste(yoi, "-10-01", sep = "")), 
                                  yday(paste(yoi, "-11-01", sep = "")), 
                                  yday(paste(yoi, "-12-01", sep = ""))), 
                       labels = month.abb) + 
    geom_vline(xintercept = unique(data_to_plot$YearDay), alpha = 0.2) + 
    {if(yoi == 2020) geom_rect(xmin = yday("2020-03-24"), 
                               xmax = yday("2020-05-24"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} + 
    {if(yoi == 2023) geom_rect(xmin = yday("2023-04-01"), 
                               xmax = yday("2023-05-01"), 
                               ymin = -Inf, 
                               ymax = Inf, 
                               fill = "gray")} +  
    labs(x = "", y = "Depth (m)", 
         title = paste(yoi, station, "dissolved oxygen"), 
         fill = "mg/L")
  ggsave(paste(save_folder, station, "_", yoi, "_DO_contour.png", sep = ""), 
         dpi = 600, height = 2, width = 8)
}

#### Oxygen contour plot - whole span ####
maxdepth_data <- anomaly_data %>% 
  filter(Year %in% min_yoi:max_yoi) %>% 
  group_by(Year, YearDay) %>% 
  summarize(MaxDepth = max(BinDepth, na.rm = T))

max_depth <- min(maxdepth_data$MaxDepth)

limit_data <- anomaly_data %>% 
  filter(Year %in% min_yoi:max_yoi, 
         BinDepth <= max_depth)
min_lim <- round_any(min(limit_data$DO, na.rm = T),
                     accuracy = acc_DO, f = floor)
max_lim <- round_any(max(limit_data$DO, na.rm = T),
                     accuracy = acc_DO, f = ceiling)

mybreaks <- seq(min_lim, max_lim, by = acc_DO)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""

# Full depth
data_to_plot <- anomaly_data %>% 
  filter(Year %in% min_yoi:max_yoi, 
         !is.na(DO), 
         BinDepth <= max_depth) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
  group_by(Date, BinDepth) %>% 
  summarize(DO = mean(DO, na.rm = T))

extra_data_before <- anomaly_data %>% 
  filter(Year == min_yoi - 1, 
         !(is.na(DO)), 
         BinDepth <= max_depth) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
  filter(Date == max(Date)) %>% 
  group_by(Date, BinDepth) %>% 
  summarize(DO = mean(DO, na.rm = T))
data_to_plot <- full_join(data_to_plot, extra_data_before)

extra_data_after <- anomaly_data %>% 
  filter(Year == max_yoi + 1, 
         !(is.na(DO)), 
         BinDepth <= max_depth) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
  filter(Date == min(Date)) %>% 
  group_by(Date, BinDepth) %>% 
  summarize(DO = mean(DO, na.rm = T))
data_to_plot <- full_join(data_to_plot, extra_data_after)

ggplot(data = data_to_plot) + 
  theme_classic() + 
  metR::geom_contour_fill(aes(x = Date, 
                              y = BinDepth, 
                              z = DO), 
                          na.fill = T, breaks = mybreaks) + 
  scale_fill_craftfermenter(
    breaks = mybreaks,
    palette = "PuBu",
    direction = -1,
    limits = c(min_lim, max_lim),
    labels = mylabels,
    guide = guide_colorbar(show.limits = T, ticks = F)) +
  metR::geom_contour2(aes(x = Date, 
                          y = BinDepth, 
                          z = DO), 
                      na.fill = T, breaks = 7) + 
  metR::geom_contour2(aes(x = Date, 
                          y = BinDepth, 
                          z = DO), 
                      na.fill = T, breaks = 5, color = "red") + 
  # scale_fill_cmocean(name = "oxy",
  #                    breaks = mybreaks,
  #                    limits = c(min_lim, max_lim),
  #                    labels = mylabels,
  #                    guide = guide_colorbar(show.limits = T, ticks = F)) +
  scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(xlim = as.Date(c(paste(min_yoi, "01", "01", sep = "-"), 
                                   paste(max_yoi, "12", "31", sep = "-")))) + 
  scale_x_date(expand = c(0, 0), 
               date_breaks = "1 year", 
               date_labels = "%Y") + 
  geom_rect(xmin = yday("2020-03-24"), 
            xmax = yday("2020-05-24"), 
            ymin = -Inf, 
            ymax = Inf, 
            fill = "gray") + 
  labs(x = "", y = "Depth (m)", 
       title = paste0(min_yoi, "-", max_yoi, " ", station, " dissolved oxygen"), 
       fill = "mg/L")
ggsave(paste0(save_folder, station, "_", min_yoi, "-", max_yoi, "_DO_contour.png"), 
       dpi = 600, height = 2, width = 8)

# DO counts by month, year
DO_count <- data_to_plot %>% 
  mutate(Year = year(Date), 
         Month = month(Date)) %>% 
  filter(Year %in% min_yoi:max_yoi) %>% 
  group_by(Year, Month) %>% 
  summarize(IsLow = any(DO < 7)) %>% 
  summarize(NLow = sum(IsLow))


