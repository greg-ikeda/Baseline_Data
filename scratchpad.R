
params <- c("BaselineTemperature", "BaselineSalinity", "BaselineDO")
for(param in params){
  # Monthly plot 
  monthly_plot <- ggplot(data = baseline,
                         aes_string(x = "BinDepth",
                                    y = param))+
    coord_flip() + 
    scale_x_reverse() + 
    geom_point(aes(color = monthname))
  print(monthly_plot)
}



for(param in params){
  # Monthly plot 
  monthly_plot <- ggplot(data = baseline,
                         aes(x = BinDepth,
                             y = {{param}}))+
    coord_flip() + 
    scale_x_reverse() + 
    geom_point(aes(color = monthname))
  print(monthly_plot)
}

# Temperature Baseline Data ------------------------------------------

# Monthly plot 
temp_monthly_plot <- ggplot(data = baseline_stats,
                            aes(x = BinDepth,
                                y = BaselineTemperature,
                                group = Month)) +
  theme_bw() + 
  coord_flip() + 
  scale_x_reverse() + 
  geom_point(aes(color = monthname)) + 
  labs(title = paste(station_name, "Baseline Temperature (°C)")) +
  ylab(paste0("Monthly Temperature Baseline (", base_start, " - ", base_end, ")"))+
  xlab("Depth (m)")+
  guides(color = guide_legend(title = "Month")) 

temp_monthly_plot
ggsave(here("monthly_baseline_plots", paste0(station, "temperature_monthly.png")),
       dpi = 600, width = 11, height = 8)



# Multipanel plot
temp_multipanel_plot <- ggplot(data = baseline_stats,
                               aes(x = BinDepth,
                                   y = BaselineTemperature,
                                   group = Month)) +
  theme_bw() + 
  coord_flip() + 
  scale_x_reverse() + 
  geom_point() + 
  geom_line(aes(x = BinDepth,
                y = min_Temperature),
            linewidth = 0.1)+
  geom_line(aes(x = BinDepth,
                y = max_Temperature),
            linewidth = 0.1)+
  labs(title = paste(station_name, "Baseline Temperature (°C)")) +
  ylab(paste0("Monthly Temperature Baseline (", base_start, " - ", base_end, ")"))+
  xlab("Depth (m)")+
  facet_wrap(~monthname)

temp_multipanel_plot
ggsave(here("multipanel_baseline_plots", paste0(station, "temperature_multipanel.png")),
       dpi = 300, width = 12, height = 9)




# Salinity Baseline Data ------------------------------------------

# Monthly plot 
salinity_monthly_plot <- ggplot(data = baseline,
                                aes(x = BinDepth,
                                    y = BaselineSalinity,
                                    group = Month)) +
  theme_bw() + 
  coord_cartesian(ylim = c(24,31)) +
  coord_flip() + 
  scale_x_reverse() + 
  geom_point(aes(color = monthname)) + 
  geom_line(aes(color = monthname),
            alpha = 0.1,
            linewidth = 0.6) +
  labs(title = paste(station_name, "Baseline Salinity (PSU)")) +
  ylab(paste0("Monthly Salinity Baseline (", base_start, " - ", base_end, ")"))+
  xlab("Depth (m)")+
  guides(color = guide_legend(title = "Month"))

salinity_monthly_plot
ggsave(here("monthly_baseline_plots", paste0(station, "_salinity_monthly.png")),
       dpi = 600, width = 11, height = 8)


# Multipanel plot
salinity_multipanel_plot <- ggplot(data = baseline,
                                   aes(x = BinDepth,
                                       y = BaselineSalinity,
                                       group = Month)) +
  theme_bw() + 
  coord_cartesian(ylim = c(24,31)) +
  coord_flip() + 
  scale_x_reverse() + 
  geom_point() + 
  geom_line(aes(x = BinDepth,
                y = min_Salinity),
            linewidth = 0.1)+
  geom_line(aes(x = BinDepth,
                y = max_Salinity),
            linewidth = 0.1,
            alpha = 0.5)+
  labs(title = paste(station_name, "Baseline Salinity (PSU)")) +
  ylab(paste0("Monthly Salinity Baseline (", base_start, " - ", base_end, ")"))+
  xlab("Depth (m)")+
  facet_wrap(~monthname)

salinity_multipanel_plot
ggsave(here("multipanel_baseline_plots", paste0(station, "_salinity_multipanel.png")),
       dpi = 300, width = 12, height = 9)




# DO Baseline Data ------------------------------------------

# Monthly plot 
DO_monthly_plot <- ggplot(data = baseline,
                          aes(x = BinDepth,
                              y = BaselineDO,
                              group = Month)) +
  theme_bw() + 
  coord_flip() + 
  scale_x_reverse() + 
  geom_point(aes(color = monthname)) + 
  labs(title = paste(station_name, "Baseline Dissolved Oxygen (mg/L)")) +
  ylab(paste0("Monthly DO Baseline (", base_start, " - ", base_end, ")"))+
  xlab("Depth (m)")+
  guides(color = guide_legend(title = "Month")) 

DO_monthly_plot
ggsave(here("monthly_baseline_plots", paste0(station, "DO_monthly.png")),
       dpi = 600, width = 11, height = 8)


# Multipanel plot
DO_multipanel_plot <- ggplot(data = baseline,
                             aes(x = BinDepth,
                                 y = BaselineDO,
                                 group = Month)) +
  theme_bw() + 
  coord_flip() + 
  scale_x_reverse() + 
  geom_point() + 
  geom_line(aes(x = BinDepth,
                y = min_DO),
            linewidth = 0.1)+
  geom_line(aes(x = BinDepth,
                y = max_DO),
            linewidth = 0.1)+
  labs(title = paste(station_name, "Baseline Dissolved Oxygen (mg/L)")) +
  ylab(paste0("Monthly DO Baseline (", base_start, " - ", base_end, ")"))+
  xlab("Depth (m)")+
  facet_wrap(~monthname)

DO_multipanel_plot
ggsave(here("multipanel_baseline_plots", paste0(station, "DO_multipanel.png")),
       dpi = 300, width = 12, height = 9)













# JUST IN CASE ------------------------------------------------------------


#### SETUP ####
# How to use this file:
# 1. Edit parameters in lines 18-46
# 2. Run setup (this section), load data and remove short casts (next 2 sections)
# 3. Run any other sections you'd like figures from

rm(list = ls())
library(tidyverse)
library(viridis)
library(lubridate)
library(kcmarine)
library(here)
library(plotly) #Enables interactive plots
library(htmlwidgets)

z_drive <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/"
source(paste0(z_drive, 
              "Figures and Scripts/CTD Contour - R/contour_functions.R"))

# What station(s) do you want figures from?
stations <- c("KSBP01")

# What do you want your baseline to be for anomaly plots?
base_start <- 1998
base_end <- 2013

# How wide do you want your depth bins (0.5, 1, 2, 5 probably best)
bin_width <- 0.5

#### Load data ####
# No need to change anything below here!
for (station in stations) {
  folder <- paste0(z_drive, 
                   "CTD_data_repository/", 
                   station, "/", sep = "")
  fname <- list.files(folder, pattern = "_qcd.csv")
  
  temp <- bin_CTD(paste0(folder, fname), bin_width)
  
  if (station == stations[1]) {
    CTDdata <- temp
  } else {
    CTDdata <- CTDdata %>% add_row(temp)
  }
}

# Generate geographical name from station code
if(station == "KSBP01"){
  station_name <- "Point Jefferson"
} else if(station == "NSEX01"){
  station_name <- "East Passage"
}

# Calculate Baseline ------------------------------------------------------

baseline_data <- CTDdata %>% 
  filter(Year >= base_start, 
         Year <= base_end)

# Idenfity true baseline start/end if different from specified baseline period

if(min(baseline_data$Year) != base_start){
  true_base_start <- min(baseline_data$Year)
} else{
  true_base_start <- base_start
}

if(max(baseline_data$Year) != base_end){
  true_base_start <- max(baseline_data$Year)
} else{
  true_base_end <- base_end
}

depth_baseline <- baseline_data %>% 
  group_by(Locator, Year, YearDay) %>% 
  summarize(MaxDepth = max(BinDepth, na.rm = T))
for (station in stations) {
  temp <- depth_baseline %>% 
    filter(Locator == station) %>% 
    arrange(MaxDepth) %>% 
    pull(MaxDepth)
  while (min(temp) < (mean(temp) - 2*sd(temp))) {
    temp <- temp[-1]
  }
  if (station == stations[1]) {
    max_depth_anom <- tibble(Locator = station, MaxDepthAnom = min(temp))
  } else {
    max_depth_anom <- max_depth_anom %>% 
      add_row(tibble(Locator = station, MaxDepthAnom = min(temp)))
  }
}

baseline_data <- left_join(baseline_data, max_depth_anom)

#### Remove short casts ####
short_stations <- c("LTBC43", "LSVV01")
if (!(station %in% short_stations)) {
  temp <- baseline_data %>% 
    group_by(Locator, Month) %>% 
    summarize(Max_BinDepth = max(BinDepth)) %>% 
    mutate(Short = if_else(Max_BinDepth <= 40, 
                           T, F))
}

baseline <- left_join(baseline_data, temp) %>%
  mutate(Month = as.factor(Month),
         monthname = month.abb[Month]) %>%
  filter(!Short) %>% 
  select(!Short)

baseline_stats <- baseline %>% 
  group_by(Locator, Month, BinDepth, MaxDepthAnom) %>% 
  filter(BinDepth <= MaxDepthAnom) %>% 
  summarize(BaselineDensity = mean(Density, na.rm = T), 
            BaselineDO = mean(DO, na.rm = T), 
            BaselineSigmaT = mean(SigmaTheta, na.rm = T), 
            BaselineSalinity = mean(Salinity, na.rm = T), 
            BaselineTemperature = mean(Temperature, na.rm = T),
            # Standard deviations
            sd_Density = sd(Density, na.rm = T), 
            sd_DO = sd(DO, na.rm = T), 
            sd_SigmaT = sd(SigmaTheta, na.rm = T), 
            sd_Salinity = sd(Salinity, na.rm = T), 
            sd_Temperature = sd(Temperature, na.rm = T),
            #minimums
            min_Density = min(Density, na.rm = T), 
            min_DO = min(DO, na.rm = T), 
            min_SigmaT = min(SigmaTheta, na.rm = T), 
            min_Salinity = min(Salinity, na.rm = T), 
            min_Temperature = min(Temperature, na.rm = T),
            # maximums
            max_Density = max(Density, na.rm = T), 
            max_DO = max(DO, na.rm = T), 
            max_SigmaT = max(SigmaTheta, na.rm = T), 
            max_Salinity = max(Salinity, na.rm = T), 
            max_Temperature = max(Temperature, na.rm = T)) %>%
  mutate(monthname = month.abb[Month])

baseline_stats$monthname <- factor(baseline_stats$monthname, 
                                   levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Loop Parameters ---------------------------------------------------------

params <- c("BaselineTemperature", "BaselineSalinity", "BaselineDO")

for(param in params){
  
  if(param == "BaselineTemperature"){
    minlim <- NA
    maxlim <- NA
    param_name <- "Temperature"
    param_title <- "Temperature (°C)"
    param_min <- "min_Temperature"
    param_max <- "max_Temperature"
    
  } else if(param == "BaselineSalinity"){
    minlim <- 24
    maxlim <- 32
    param_name <- "Salinity"
    param_title <- "Salinity (PSU)"
    param_min <- "min_Salinity"
    param_max <- "max_Salinity"
    
  } else if(param == "BaselineDO"){
    minlim <- NA
    maxlim <- NA
    param_name <- "DO"
    param_title <- "Dissolved Oxygen (mg/L)"
    param_min <- "min_DO"
    param_max <- "max_DO"
  }
  
  # Monthly plot 
  monthly_plot <- ggplot(data = baseline_stats,
                         aes_string(x = "BinDepth",
                                    y = param,
                                    group = "Month")) +
    theme_bw() + 
    coord_cartesian(ylim = c(minlim, maxlim)) +
    coord_flip() + 
    scale_x_reverse() + 
    geom_point(aes(color = monthname)) + 
    geom_line(aes(color = monthname),
              alpha = 0.1,
              linewidth = 0.6) +
    labs(title = paste(station_name, "-", param_title)) +
    ylab(paste0("Monthly Baseline ", param_title,  " (", true_base_start, " - ", true_base_end, ")"))+
    xlab("Depth (m)")+
    guides(color = guide_legend(title = "Month"))
  
  monthly_plot
  ggsave(here("monthly_baseline_plots", paste0(station, "_", param_name, "_monthly.png")),
         dpi = 600, width = 11, height = 8)
  
  htmlwidgets::saveWidget(ggplotly(monthly_plot), 
                          title = paste0(param_title), 
                          file = here("monthly_baseline_plots", paste0(station, "_", param_name, "_monthly.html")))
  
  
  # Multipanel plot
  multipanel_plot <- ggplot(data = baseline_stats,
                            aes_string(x = "BinDepth",
                                       y = param,
                                       group = "Month")) +
    theme_bw() + 
    coord_cartesian(ylim = c(minlim, maxlim)) +
    coord_flip() + 
    scale_x_reverse() + 
    geom_point() + 
    geom_line(aes_string(x = "BinDepth",
                         y = param_min),
              linewidth = 0.1)+
    geom_line(aes_string(x = "BinDepth",
                         y = param_max),
              linewidth = 0.1,
              alpha = 0.5)+
    labs(title = paste(station_name, param_title)) +
    ylab(paste0("Monthly Baseline",  param_title, " (", true_base_start, " - ", true_base_end, ")"))+
    xlab("Depth (m)")+
    facet_wrap(~monthname)
  
  multipanel_plot
  ggsave(here("multipanel_baseline_plots", paste0(station, "_", param_name, "_multipanel.png")),
         dpi = 300, width = 12, height = 9)
}

# Erase folders created by htmltools
created_dirs_monthly <- list.dirs(here("monthly_baseline_plots"))
unlink(created_dirs_monthly[2:length(created_dirs_monthly)], recursive = TRUE)

created_dirs_multipanel <- list.dirs(here("multipanel_baseline_plots"))
unlink(created_dirs_multipanel[2:length(created_dirs_multipanel)], recursive = TRUE)





# -------------------------------------------------------------------------

#### SETUP ####
# How to use this file:
# 1. Edit parameters in lines 18-46
# 2. Run setup (this section), load data and remove short casts (next 2 sections)
# 3. Run any other sections you'd like figures from

rm(list = ls())
library(tidyverse)
library(viridis)
library(lubridate)
library(kcmarine)
library(here)
library(plotly) #Enables interactive plots
library(htmlwidgets)

z_drive <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/"
source(paste0(z_drive, 
              "Figures and Scripts/CTD Contour - R/contour_functions.R"))

# What station(s) do you want figures from?
stations <- c("KSBP01")

# What do you want your baseline to be for anomaly plots?
base_start <- 1998
base_end <- 2013

# How wide do you want your depth bins (0.5, 1, 2, 5 probably best)
bin_width <- 0.5

#### Load data ####
# No need to change anything below here!
for (station in stations) {
  folder <- paste0(z_drive, 
                   "CTD_data_repository/", 
                   station, "/", sep = "")
  fname <- list.files(folder, pattern = "_qcd.csv")
  
  temp <- bin_CTD(paste0(folder, fname), bin_width)
  
  if (station == stations[1]) {
    CTDdata <- temp
  } else {
    CTDdata <- CTDdata %>% add_row(temp)
  }
}

# Generate geographical name from station code
if(station == "KSBP01"){
  station_name <- "Point Jefferson"
} else if(station == "NSEX01"){
  station_name <- "East Passage"
}

# Calculate Baseline ------------------------------------------------------

baseline_data <- CTDdata %>% 
  filter(Year >= base_start, 
         Year <= base_end)

# Idenfity true baseline start/end if different from specified baseline period

if(min(baseline_data$Year) != base_start){
  true_base_start <- min(baseline_data$Year)
} else{
  true_base_start <- base_start
}

if(max(baseline_data$Year) != base_end){
  true_base_start <- max(baseline_data$Year)
} else{
  true_base_end <- base_end
}

depth_baseline <- baseline_data %>% 
  group_by(Locator, Year, YearDay) %>% 
  summarize(MaxDepth = max(BinDepth, na.rm = T))
for (station in stations) {
  temp <- depth_baseline %>% 
    filter(Locator == station) %>% 
    arrange(MaxDepth) %>% 
    pull(MaxDepth)
  while (min(temp) < (mean(temp) - 2*sd(temp))) {
    temp <- temp[-1]
  }
  if (station == stations[1]) {
    max_depth_anom <- tibble(Locator = station, MaxDepthAnom = min(temp))
  } else {
    max_depth_anom <- max_depth_anom %>% 
      add_row(tibble(Locator = station, MaxDepthAnom = min(temp)))
  }
}

baseline_data <- left_join(baseline_data, max_depth_anom)

#### Remove short casts ####
short_stations <- c("LTBC43", "LSVV01")
if (!(station %in% short_stations)) {
  temp <- baseline_data %>% 
    group_by(Locator, Month) %>% 
    summarize(Max_BinDepth = max(BinDepth)) %>% 
    mutate(Short = if_else(Max_BinDepth <= 40, 
                           T, F))
}

baseline <- left_join(baseline_data, temp) %>%
  mutate(Month = as.factor(Month),
         monthname = month.abb[Month]) %>%
  filter(!Short) %>% 
  select(!Short)

baseline_stats <- baseline %>% 
  group_by(Locator, Month, BinDepth, MaxDepthAnom) %>% 
  filter(BinDepth <= MaxDepthAnom) %>% 
  summarize(BaselineDensity = mean(Density, na.rm = T), 
            BaselineDO = mean(DO, na.rm = T), 
            BaselineSigmaT = mean(SigmaTheta, na.rm = T), 
            BaselineSalinity = mean(Salinity, na.rm = T), 
            BaselineTemperature = mean(Temperature, na.rm = T),
            # Standard deviations
            sd_Density = sd(Density, na.rm = T), 
            sd_DO = sd(DO, na.rm = T), 
            sd_SigmaT = sd(SigmaTheta, na.rm = T), 
            sd_Salinity = sd(Salinity, na.rm = T), 
            sd_Temperature = sd(Temperature, na.rm = T),
            #minimums
            min_Density = min(Density, na.rm = T), 
            min_DO = min(DO, na.rm = T), 
            min_SigmaT = min(SigmaTheta, na.rm = T), 
            min_Salinity = min(Salinity, na.rm = T), 
            min_Temperature = min(Temperature, na.rm = T),
            # maximums
            max_Density = max(Density, na.rm = T), 
            max_DO = max(DO, na.rm = T), 
            max_SigmaT = max(SigmaTheta, na.rm = T), 
            max_Salinity = max(Salinity, na.rm = T), 
            max_Temperature = max(Temperature, na.rm = T)) %>%
  mutate(monthname = month.abb[Month])

baseline_stats$monthname <- factor(baseline_stats$monthname, 
                                   levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Loop Parameters ---------------------------------------------------------

params <- c("BaselineTemperature", "BaselineSalinity", "BaselineDO")

for(param in params){
  
  if(param == "BaselineTemperature"){
    minlim <- NA
    maxlim <- NA
    param_name <- "Temperature"
    param_title <- "Temperature (°C)"
    param_min <- "min_Temperature"
    param_max <- "max_Temperature"
    
  } else if(param == "BaselineSalinity"){
    minlim <- 24
    maxlim <- 32
    param_name <- "Salinity"
    param_title <- "Salinity (PSU)"
    param_min <- "min_Salinity"
    param_max <- "max_Salinity"
    
  } else if(param == "BaselineDO"){
    minlim <- NA
    maxlim <- NA
    param_name <- "DO"
    param_title <- "Dissolved Oxygen (mg/L)"
    param_min <- "min_DO"
    param_max <- "max_DO"
  }
  
  # Monthly plot 
  monthly_plot <- ggplot(data = baseline_stats,
                         aes_string(x = "BinDepth",
                                    y = param,
                                    group = "Month")) +
    theme_bw() + 
    coord_cartesian(ylim = c(minlim, maxlim)) +
    coord_flip() + 
    scale_x_reverse() + 
    geom_point(aes(color = monthname)) + 
    geom_line(aes(color = monthname),
              alpha = 0.1,
              linewidth = 0.6) +
    labs(title = paste(station_name, "-", param_title)) +
    ylab(paste0("Monthly Baseline ", param_title,  " (", true_base_start, " - ", true_base_end, ")"))+
    xlab("Depth (m)")+
    guides(color = guide_legend(title = "Month"))
  
  monthly_plot
  ggsave(here("monthly_baseline_plots", paste0(station, "_", param_name, "_monthly.png")),
         dpi = 600, width = 11, height = 8)
  
  htmlwidgets::saveWidget(ggplotly(monthly_plot), 
                          title = paste0(param_title), 
                          file = here("monthly_baseline_plots", paste0(station, "_", param_name, "_monthly.html")))
  
  
  # Multipanel plot
  multipanel_plot <- ggplot(data = baseline_stats,
                            aes_string(x = "BinDepth",
                                       y = param,
                                       group = "Month")) +
    theme_bw() + 
    coord_cartesian(ylim = c(minlim, maxlim)) +
    coord_flip() + 
    scale_x_reverse() + 
    geom_point() + 
    geom_line(aes_string(x = "BinDepth",
                         y = param_min),
              linewidth = 0.1)+
    geom_line(aes_string(x = "BinDepth",
                         y = param_max),
              linewidth = 0.1,
              alpha = 0.5)+
    labs(title = paste(station_name, param_title)) +
    ylab(paste0("Monthly Baseline",  param_title, " (", true_base_start, " - ", true_base_end, ")"))+
    xlab("Depth (m)")+
    facet_wrap(~monthname)
  
  multipanel_plot
  ggsave(here("multipanel_baseline_plots", paste0(station, "_", param_name, "_multipanel.png")),
         dpi = 300, width = 12, height = 9)
}

# Erase folders created by htmltools
created_dirs_monthly <- list.dirs(here("monthly_baseline_plots"))
unlink(created_dirs_monthly[2:length(created_dirs_monthly)], recursive = TRUE)

created_dirs_multipanel <- list.dirs(here("multipanel_baseline_plots"))
unlink(created_dirs_multipanel[2:length(created_dirs_multipanel)], recursive = TRUE)





# -------------------------------------------------------------------------




# No need to change anything below here!
for (station in stations) {
  folder <- paste0(z_drive, 
                   "CTD_data_repository/", 
                   station, "/", sep = "")
  fname <- list.files(folder, pattern = "_qcd.csv")
  print(fname)
  
  # temp <- bin_CTD(paste0(folder, fname), bin_width)
  
  # if (station == stations[1]) {
  #   CTDdata <- temp
  # } else {
  #   CTDdata <- CTDdata %>% add_row(temp)
  # }
  
  
  # Generate geographical name from station code
  if(station == "KSBP01"){
    station_name <- "Point Jefferson"
  } else if(station == "NSEX01"){
    station_name <- "East Passage"
  }
  print(station_name)
}




test <- function() {
  do.call("<<-",list("vartest","xxx"))
}
test()
vartest

test("blah")




station_namer_test <- function(station){
  # Generate geographical name from station code
  if(station == "KSBP01"){
    station_name <<- "Point Jefferson"
  } else if(station == "NSEX01"){
    station_name <<- "East Passage"
  } else if(station == "JSUR01"){
    station_name <<- "Brightwater Outfall"
  } else if(station == "KSSK02"){
    station_name <<- "West Point"
  } else if(station == "LSEP01"){
    station_name <<- "South Plant Outfall"
  } else if(station == "LSKQ06"){
    station_name <<- "Alki Outfall"
  } else if(station == "LSNT01"){
    station_name <<- "Dolphin Point"
  } else if(station == "LSVV01"){
    station_name <<- "Fauntleroy Cove"
  } else if(station == "LTBC43"){
    station_name <<- "Denny Way Outfall"
  } else if(station == "LTED04"){
    station_name <<- "Elliot Bay"
  } else if(station == "LTKE03"){
    station_name <<- "Spokane St. Bridge"
  } else if(station == "LTUM03"){
    station_name <<- "Duwamish River - 16th ave S Bridge"
  } else if(station == "MSJN02"){
    station_name <<- "Vashon Outfall"
  } 
}

test_stations <- c("JSUR01", "KSBP01", "LSNT01", "NSEX01")

for(location in test_stations){
  station_namer_test(location)
}








