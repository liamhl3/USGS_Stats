library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(plotly)
library(corrgram)
library(corrplot)
library(sp)
library(sf)
library(leaflet)
library(tidyverse)
library(stringr)
library(here)
library(widgetframe)
library(htmltools)
library(htmlwidgets)
library(tmap)
library(rgdal)
library(caret)
library(ggpubr)
library(car)
library(PerformanceAnalytics)
library(ggcorrplot)
library(naniar)
library(grid)
library(mice)
library(egg)
library(shiny)
library(lubridate)
library(devtools)
library(raster)
library(caTools)
library(tidycensus)
library(dataRetrieval)
library(EGRET)
library(hydroTSM)
library(fasstr)
library(FAdist)


#Early Period

######################################################################
#### Site Parameters
siteNumber <- "02087500"

earlyperiod_startdate <- "1981-01-01"
earlyperiod_enddate <- "2000-12-31"

parameter_code<-"00060"

water_year_start_month<- 10

######################################################################
#### Load Data

#daily discharge data
clayton_daily_data_early <- readNWISDaily(siteNumber,parameter_code,
                            earlyperiod_startdate, earlyperiod_enddate, convert=FALSE)%>% 
                            mutate(Year = year(Date))

# basin area
usgs_basin_area <- readNWISsite(siteNumbers = siteNumber) %>% 
  pull(drain_area_va)

# broad clayton river info
info <- readNWISInfo(siteNumber,parameter_code, interactive = FALSE)

#combine info and clayton daily data df's
eList <- as.egret(info, clayton_daily_data_early, NA, NA)

#renaming Q in clayton daily data to Value
#this is to prep for Log Pearson III in fastr package functions
clayton_usgs_data_early <- clayton_daily_data_early %>% 
                           rename(Value = Q)

#write.csv(clayton_usgs_data_early, 'Early_Period_Full_Data.csv')

######################################################################
#### High Flow Analysis

#set quantiles
highflow_quantiles <- c(0.02, 0.01)

#highflow frequncy calculation
highflow_frequency_early <- compute_annual_frequencies(clayton_usgs_data_early,
                                                 use_max=TRUE, # taking max flow data from a year 
                                                 water_year_start = water_year_start_month, # sets month when water year starts 
                                                 prob_plot_position = "weibull", # plot positions using (i)/(n+1)
                                                 fit_distr = "PIII", # log-Pearson Type III distribution
                                                 fit_distr_method = "MOM",
                                                 fit_quantiles = highflow_quantiles) # indicate quantiles you want calculated
#plot edit
highflow_frequency_early$Freq_Plot$labels$y <- "Discharge, cubic feet per second"

# 1-day daily high flows (24hr period) for 50 + 100 year return periods, by amount of days
# Similar output to Martins spreadsheets
highflow_output_early <- highflow_frequency_early$Freq_Fitted_Quantiles

#write.csv
#write.csv(highflow_output_early, "EARLYOUTPUT_high-flow-return-periods.csv")

######################################################################
#### Plot High Flow Analysis

highflow_plot_early <- highflow_frequency_early$Freq_Plot +
                 coord_flip() +  # Similar to ggplot objects, we can add on more things, like coord flip
                 labs(title = "Neuse River - Clayton Reach High Flows (1981-2000)")+
                 theme(plot.title = element_text(hjust = 0.5))+
                 theme(axis.text.x = element_text(angle = 90))
print(highflow_plot_early)

######################################################################
#### Examine High Flow Analysis Residuals

# Plot the fitting distributions of the 24hr high flows
plot(highflow_frequency_early$Freq_Fitting$`1-Day`)

# View the fitting parameters (shape, scale, location, loglikehood, AIC and BIC etc.)
# of the 24hr high flows
summary(highflow_frequency_early$Freq_Fitting$`1-Day`)


######################################################################
#### Low Flow Analysis

lowflow_quantiles <- c(0.10, 0.20)


# Compute annual low flows frequencies with default arguments
#add min_flow = TRUE?
lowflow_frequency_early <- compute_annual_frequencies(clayton_usgs_data_early,
                                                water_year_start = water_year_start_month,
                                                prob_plot_position = "weibull", # plot positions using (i)/(n+1)
                                                fit_distr = "PIII", # log-Pearson Type III
                                                fit_distr_method = "MOM", 
                                                fit_quantiles = lowflow_quantiles) # method of moments


# Change the y-axis of the plot to US units
lowflow_frequency_early$Freq_Plot$labels$y <- "Discharge, cubic feet per second"

# Summary table
lowflow_output_early <- lowflow_frequency_early$Freq_Fitted_Quantiles # you want the 1-day number (24hr period)

# write.csv
#write.csv(lowflow_output_early, "EARLYOUTPUT_low-flow-return-periods.csv")


######################################################################
#### Plot Low Flow Analysis

lowflow_plot_early <- lowflow_frequency_early$Freq_Plot +
                   coord_flip() + # Similar to ggplot objects, we can add on more things, like coord flip
                   labs(title = "Neuse River - Clayton Reach Low Flows (1981-2000)")+
                    theme(plot.title = element_text(hjust = 0.5))+
                   theme(axis.text.x = element_text(angle = 90))
print(lowflow_plot_early)

######################################################################
#### Examine Low Flow Analysis Residuals

# View the fitting parameters (shape, scale, location, loglikehood, AIC and BIC etc.)
# of the 7-day low flows
summary(lowflow_frequency_early$Freq_Fitting$`7-Day`)

# Plot the fitting distributions of the 7-day low flows
plot(lowflow_frequency_early$Freq_Fitting$`7-Day`)


######################################################################
#### Extra Visulaizations








































#Later Period

######################################################################
#### Site Parameters
siteNumber <- "02087500"

lateperiod_startdate <- "2001-01-01"
lateperiod_enddate <- "2021-12-31"

parameter_code<-"00060"

water_year_start_month<- 10

######################################################################
#### Load Data

#daily discharge data
clayton_daily_data_late <- readNWISDaily(siteNumber,parameter_code,
                                    lateperiod_startdate, lateperiod_enddate, convert=FALSE)%>% 
  mutate(Year = year(Date))

# basin area
usgs_basin_area <- readNWISsite(siteNumbers = siteNumber) %>% 
  pull(drain_area_va)

# broad clayton river info
info <- readNWISInfo(siteNumber,parameter_code, interactive = FALSE)

#combine info and clayton daily data df's
eList <- as.egret(info, clayton_daily_data_late, NA, NA)

#renaming Q in clayton daily data to Value
#this is to prep for Log Pearson III in fastr package functions
clayton_usgs_data_late <- clayton_daily_data_late %>% 
  rename(Value = Q)


#write.csv(clayton_usgs_data_late, 'Late_Period_Full_Data.csv')

######################################################################
#### High Flow Analysis

#set quantiles
highflow_quantiles<-c(0.02, 0.01)

#highflow frequncy calculation
highflow_frequency_late <- compute_annual_frequencies(clayton_usgs_data_late,
                                                 use_max=TRUE, # taking max flow data from a year 
                                                 water_year_start = water_year_start_month, # sets month when water year starts 
                                                 prob_plot_position = "weibull", # plot positions using (i)/(n+1)
                                                 fit_distr = "PIII", # log-Pearson Type III distribution
                                                 fit_distr_method = "MOM",
                                                 fit_quantiles = highflow_quantiles) # indicate quantiles you want calculated
#plot edit
highflow_frequency_late$Freq_Plot$labels$y <- "Discharge, cubic feet per second"

# 1-day daily high flows (24hr period) for 50 + 100 year return periods, by amount of days
# Similar output to Martins spreadsheets
highflow_output_late <- highflow_frequency_late$Freq_Fitted_Quantiles

#write.csv
#write.csv(highflow_output_late, "LATEOUTPUT_high-flow-return-periods.csv")

######################################################################
#### Plot High Flow Analysis

highflow_plot_late <- highflow_frequency_late$Freq_Plot +
  coord_flip() +  # Similar to ggplot objects, we can add on more things, like coord flip
  labs(title = "Neuse River - Clayton Reach High Flows (2001-2021)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))

print(highflow_plot_late)

######################################################################
#### Examine High Flow Analysis Residuals

# Plot the fitting distributions of the 24hr high flows
plot(highflow_frequency_late$Freq_Fitting$`1-Day`)

# View the fitting parameters (shape, scale, location, loglikehood, AIC and BIC etc.)
# of the 24hr high flows
summary(highflow_frequency_late$Freq_Fitting$`1-Day`)


######################################################################
#### Low Flow Analysis

lowflow_quantiles<-c(0.10, 0.20)


# Compute annual low flows frequencies with default arguments
#add min_flow = TRUE?
lowflow_frequency_late <- compute_annual_frequencies(clayton_usgs_data_late,
                                                water_year_start = water_year_start_month,
                                                prob_plot_position = "weibull", # plot positions using (i)/(n+1)
                                                fit_distr = "PIII", # log-Pearson Type III
                                                fit_distr_method = "MOM", 
                                                fit_quantiles = lowflow_quantiles) # method of moments


# Change the y-axis of the plot to US units
lowflow_frequency_late$Freq_Plot$labels$y <- "Discharge, cubic feet per second"

# Summary table
lowflow_output_late <- lowflow_frequency_late$Freq_Fitted_Quantiles # you want the 1-day number (24hr period)

# write.csv
#write.csv(lowflow_output_late, "LATEOUTPUT_low-flow-return-periods.csv")


######################################################################
#### Plot Low Flow Analysis

lowfloe_late_freq <- lowflow_frequency_late$Freq_Plot

lowflow_plot_late <- lowflow_frequency_late$Freq_Plot +
  coord_flip() + # Similar to ggplot objects, we can add on more things, like coord flip
  labs(title = "Neuse River - Clayton Reach Low Flows (2001-2021)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))

print(lowflow_plot_late)

######################################################################
#### Examine Low Flow Analysis Residuals

# View the fitting parameters (shape, scale, location, loglikehood, AIC and BIC etc.)
# of the 7-day low flows
summary(lowflow_frequency_late$Freq_Fitting$`7-Day`)

# Plot the fitting distributions of the 7-day low flows
plot(lowflow_frequency_late$Freq_Fitting$`7-Day`)



