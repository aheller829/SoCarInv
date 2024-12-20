# Clean soil respiration txt files and prepare for analysis

# Load packages
library(tidyverse)
library(lubridate)


# Set working directory (select folder where SRC files are stored)
setwd("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\SRC Raw")
# Check to make working directory was set correctly
getwd()

# Read in txt files
# First, get all file names from the folder where raw SRC txt files are stored
files <- list.files(path = ".",
                    recursive = FALSE,
                    full.names = TRUE)

# Make an empty list to store dataframes from each file
file_list <- list()


# Read in files from the computer folder where they are stored
for (file in files) {
  data <- read.delim2(file, header = TRUE, sep = ",", dec = ".", fill = TRUE,
                      col.names = paste0("V",seq_len(22)))
  file_list[[file]] <- data
}


# Bind into a single dataframe
src <- dplyr::bind_rows(file_list)


# Change header names to match M5 SRC data type
colnames(src) <- c("Mtype", "Date",	"Time", "Plot.No.", "Rec.No.", "CO2.Pressure", "Flow", "H2O",	
                   "Tsensor",	"O2", "Error", "Aux", "V", "PAR",	"Tsoil", "Tair", "Msoil",	
                   "Process", "DC",	"DT",	"SRL_Rate", "SRQ_Rate")


# First delete ambient CO2 readings (M3 data type) and start/end rows, keeping only SRC measurement data
src <- dplyr::filter(src, Mtype == "M5")

# Remove readings with empty date/time
src <- dplyr::filter(src, Date != "00/00/00")

unique(src$Date)

# Create a unique identifier for each plot/site combination based on date and time
# First change time format to hours, minutes, seconds
names(src)
src$TimeHMS <- as.difftime(as.character(src$Time), format = "%H:%M:%S")
src$TimeHMS <- as.numeric(src$TimeHMS)

# SUbtract minimum time from maximum time by date
str(src)
srctime <- src %>%
  dplyr::group_by(Date) %>%
  dplyr::summarize(Time = diff(range(TimeHMS)))
# If Time > 1, than multiple plots were collected on that day


# Write to csv and manually add site name
write.csv(src, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\SRC.csv", row.names = FALSE)




# Check the number of sites measured per day
srcbyday <- src %>%
  dplyr::group_by(Date, Plot.No.) %>%
  dplyr::summarise(PlotCount = n())



# Keep only the last (final) measurement for each plot
src <- src %>%
  dplyr::group_by(Date, Plot.No.) %>%
  dplyr::slice(which.max(Rec.No.)) %>%
  dplyr::ungroup()
















