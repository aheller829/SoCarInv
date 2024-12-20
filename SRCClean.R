# Clean soil respiration txt files and prepare for analysis

# Load packages
library(tidyverse)
library(lubridate)


# Set working directory (select folder where SRC files are stored)
setwd("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\SRC Raw")
# Check to make working directory was set correctly
getwd()


# Data should already be copied from txt file into an excel csv, and text separated by comma 
# to create proper columns and headers. Data should be manually matched to site, and a unique
# excel csv created for each site (this may require separating txt data into multiple csvs).
# The csv should be saved with the site FID number as well as the numeric code assigned to the txt file
# by the EGM-5. 

# Read in csv file
src <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\SRC\\FID44_SRC_24091113.csv")


# Change header names to M5 SRC type
colnames(src) <- c("Mtype", "Date",	"Time", "Plot No.", "Rec No.", "CO2	Pressure", "Flow", "H2O",	
                   "Tsensor",	"O2", "Error", "Aux", "V", "PAR",	"Tsoil", "Tair", "Msoil",	
                   "Process", "DC",	"DT",	"SRL_Rate", "SRQ_Rate")


# Add site name and FID
src$FID <- 44
src$Site <- "Selenge DRG-12 11.4"



# First delete ambient CO2 readings (m3 data type) and start/end rows, keeping only SRC measurement data
src <- dplyr::filter(src, Mtype == "M5")


# Quality control check: subtract minimum timestamp from maximum timestamp to be sure that dataframe
# contains only measurements from a single plot. The difference should be less than one hour.
# First, create a new column with HMS data type so times can be manipulated numerically.
src$TimeHMS <- as.difftime(as.character(src$Time))
src$TimeHMS <- as.numeric(src$TimeHMS)
# SUbtract minimum time from maximum time
max(src$TimeHMS) - min(src$TimeHMS)
# Difference should be < 1 (1 = 1 hour). If difference is > 1, check original data to make sure 
# that only measurements from one site are included.




# Save clean data table, make sure to change FID number to match current site
write.csv(src, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\SRC\\Clean_FID44_SRC.csv", row.names = FALSE)


