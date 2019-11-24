# Initialise and obtain data from google drive

library(googledrive)
library(tidyverse)

# Connect to my google drive where the files have been shared
drive_auth(use_oob = TRUE)

# Identify the filenames
drive_find()

# Download the zip files
drive_download("trip_data_4.csv.zip")
drive_download("trip_fare_4.csv.zip")

# Unzip the files
unzip("trip_data_4.csv.zip")
unzip("trip_fare_4.csv.zip")

# Download the taxi_zone data
taxi_zones_names <- drive_find("taxi_zones.")
map(taxi_zones_names$name,drive_download)