# Initial work

# ---- 1) Load Libraries ----
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(feather)
  library(sf)
  library(scales)
  library(tidyr)
  library(purrr)
  library(broom)
  library(stringr)
  library(randomForest)
  
# ---- 2) Load data ----

  trip_data_raw <- read_csv("trip_data_4.csv")
  trip_fare_raw <- read_csv("trip_fare_4.csv")
  
  names(trip_data_raw)
  names(trip_fare_raw)

# ---- 3) Prep work ----

# What time period?
  min(trip_data_raw$pickup_datetime)
  max(trip_data_raw$pickup_datetime)

# ---- 3a) Combine the data sets ----
  
  data <- left_join(trip_data_raw,trip_fare_raw, by = c("medallion", "hack_license", "vendor_id", "pickup_datetime"))

# save the combined data 
  saveRDS(data,"combined_data.rds")
  data <- readRDS("combined_data.rds")

  # ---- 3b) Taking a quick look ----
  summary(data)

  # ---- 3c) Do some obvious cleaning ---- 
  
  system.time({
  cleaning_1 <- data %>%
    filter(passenger_count != 0) %>%
    filter(trip_time_in_secs > 30) %>%
    filter(trip_distance > 0.1 ) %>%
    filter(pickup_longitude > -75 & pickup_longitude < -73) %>%
    filter(dropoff_longitude > -75 & dropoff_longitude < -73) %>%
    filter(pickup_latitude > 40 & pickup_latitude < 42) %>%
    filter(dropoff_latitude > 40 & dropoff_latitude < 42) 
  }
  )
  
  cleaning_2 <- cleaning_1 %>%
    mutate(avg_speed_mh = trip_distance/trip_time_in_secs * 60 * 60) %>%
    filter(avg_speed_mh < 70)
  
  cleaning_3 <- cleaning_2 %>%
    mutate(pickup_hour = hour(pickup_datetime)) %>%
    mutate(day_of_week = wday(pickup_datetime, label = TRUE))
  
  
  
  # ---- 4) Split the data ---- 
  
  set.seed(11)
  index <- sample(1:nrow(cleaning_3), nrow(cleaning_3)/2)
  
  train <- cleaning_3[index,]
  remainder <- cleaning_3[-index,]
  
  set.seed(41)
  remainder_index <- sample(1:nrow(remainder), nrow(remainder)/2)
  val <- remainder[remainder_index,]
  test <- remainder[-remainder_index,]
  
  library(feather)
  
  # save the split data 
  write_feather(train, "split_data/train.feather")
  write_feather(val, "split_data/val.feather")
  write_feather(test, "split_data/test.feather")
  
# ---- BASIC QUESTIONS ----  
  
# ---- 5) a. What is the distribution of number of passengers per trip?  ---- 

  summary(train$passenger_count)
  
  train %>% summarise(mean = mean(passenger_count)) %>% pull() %>% round(2)
  
  ggplot(train, aes(passenger_count)) +
    geom_bar() + 
    scale_x_continuous(breaks = c(1:6)) + 
    ggtitle("Distribution of Passengers") +
    labs(x = "Number of Passengers") +
    theme_minimal()
    
# ---- 6) b. What is the distribution of payment_type?   ---- 
  
  ggplot(train, aes(payment_type)) +
    geom_bar() + 
    ggtitle("Distribution of Payment Type") +
    labs(x = "Payment Method", y = "Frequency") +
    theme_minimal()
  
  # how does it relate to tip amount
  
  ggplot(train,aes(tip_amount, fill = payment_type)) +
    geom_histogram(bins = 500) + 
    coord_cartesian(xlim = c(0,20),ylim=c(0, 800000))+
    scale_x_continuous(breaks = c(1:20)) +
    scale_y_continuous(labels = comma) 


# ---- 7) c. What is the distribution of fare amount?   ---- 
  summary(train$fare_amount)
  
  train %>% summarise(median(fare_amount)) %>% pull() %>% round(2)
  train %>% summarise(mean(fare_amount)) %>% pull() %>% round(2)
  
  ggplot(train, aes(fare_amount)) +
    geom_histogram(bins = 500) + 
    #coord_cartesian(xlim = c(0,100),ylim=c(0, 800000))+
    #scale_x_continuous(breaks = c(1:20)) +
    scale_y_continuous(labels = comma) +
    ggtitle("Distribution of Fare Amount") +
    labs(x = "Fare Amount", y = "Frequency") +
    theme_minimal()
  
  train_2 %>% select(fare_amount,pickup_zone) %>% filter(fare_amount == 52) %>% 
    count(pickup_zone) %>% arrange(desc(n)) %>% head(1) %>% select(n) %>% pull()
  train_2 %>% select(fare_amount,dropoff_zone) %>% filter(fare_amount == 52) %>% 
    count(dropoff_zone) %>% arrange(desc(n)) %>% head(1) %>% select(n) %>% pull()
  
  pickup_JFK <- train_2 %>%
    select(fare_amount,pickup_zone,dropoff_borough) %>%
    filter(fare_amount == 52) %>%
    filter(pickup_zone == "JFK Airport") %>%
    count(dropoff_borough) %>%
    arrange(desc(n))
    
  dropoff_JFK <- train_2 %>%
    select(fare_amount,dropoff_zone,pickup_borough) %>%
    filter(fare_amount == 52) %>%
    filter(dropoff_zone == "JFK Airport") %>%
    count(pickup_borough) %>%
    arrange(desc(n))

  
  ggplot(train, aes(fare_amount)) +
    geom_density() + 
    ggtitle("Distribution of Fare Amount") +
    theme_minimal()
  
  summary(train$fare_amount)
  
# ---- 8) d. What is the distribution of tip amount?   ---- 
  summary(train$tip_amount)
  
  ggplot(train,aes(tip_amount)) +
    geom_histogram(bins = 500) + 
    ggtitle("Distribution of tip amount") +
    labs(x = "Tip Amount", y = "Frequency") +
    theme_minimal()
    
  ggplot(train,aes(tip_amount, fill = payment_type)) +
    geom_histogram(bins = 500) + 
    coord_cartesian(xlim = c(0,20),ylim=c(0, 800000))+
    scale_x_continuous(breaks = c(1:20)) +
    scale_y_continuous(labels = comma) +
    ggtitle("Only Card Tips are Recorded") +
    labs(x = "Tip Amount", y = "Frequency") +
    theme_minimal()
  
  card_tip <- train %>%
    select(tip_amount,payment_type) %>%
    filter(payment_type == "CRD")
  
  train %>% select(tip_amount,payment_type) %>%
    filter(payment_type == "CRD")%>% summarise(mean = mean(tip_amount)) %>% pull() %>% round(2)
  
  ggplot(card_tip, aes(tip_amount)) +
    geom_histogram(bins = 500) + 
    coord_cartesian(xlim = c(0,20)) +
    ggtitle("Distribution of Tip Amount When Paid With Card") +
    labs(x = "Tip Amount") +
    theme_minimal()
  
# ---- 9) e. What is the distribution of total amount?   ---- 
  
  summary(train$total_amount)
  
  ggplot(train, aes(total_amount)) +
    geom_histogram(bins = 500) + 
    scale_y_continuous(labels = comma) +
    ggtitle("Distribution of Total Amount") +
    labs(x = "Total Amount", y = "Frequency") +
    theme_minimal()

  
# ---- 10) f. What are top 5 busiest hours of the day? ---- 

  busy_hours <- train %>%
    group_by(pickup_hour) %>%
    summarise(count= n()) %>%
    mutate(rank = rank(desc(count))) %>%
    mutate(top_5 = case_when(rank <= 5 ~ "Yes",
                             TRUE ~ "No"))

  ggplot(busy_hours,aes(x = pickup_hour,y= count, fill = top_5)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("grey","red")) +
    scale_y_continuous(labels = comma) +
    ggtitle("Top 5 Busiest Hours - 6-11pm") +
    labs(fill = "Top 5", x = "Pickup Hour", y = "Frequency") + 
    theme_minimal()
  
# ---- 11) g. What are the top 10 busiest locations of the city?  ---- 

  # What it looks like to begin with 
    
    distinct_pickup <- train %>% distinct(pickup_longitude, pickup_latitude) %>% base::sample(5)
    sample_index <- sample(1:nrow(distinct_pickup), nrow(distinct_pickup)/100)
    distinct_sample <- distinct_pickup[sample_index,]

    ggplot(distinct_sample, aes(x = pickup_longitude, y = pickup_latitude)) + 
      geom_point(alpha = 0.3, size = 0.01) + 
      xlim(-74.05,-73.85) + 
      ylim(40.65,40.85) + 
      ggtitle("Coordinates of Pickups") +
      labs(x = "Longitude", y = "Latitude") + 
      theme_minimal()
    
# ---- 11a) Add in Taxi Zones  ---- 
    
  taxi_zones <- read_sf("taxi_zones") %>%
    st_transform(4326)
  
  train_1 <- train %>%
    mutate(pickup_lon = pickup_longitude,
           pickup_lat = pickup_latitude,
           dropoff_lon = dropoff_longitude,
           dropoff_lat = dropoff_latitude)
  
  pickup <- st_as_sf(train_1, coords = c("pickup_longitude", "pickup_latitude"), crs = 4326)
  
  system.time({
    pickup_join <- pickup %>%
      st_join(taxi_zones)
  })
  
  post_pickup <- pickup_join %>%
    mutate(pickup_zone = zone, 
           pickup_location = LocationID,
           pickup_borough = borough,
           pickup_geometry = geometry) %>%
    select(-c(OBJECTID,Shape_Leng,Shape_Area,zone,LocationID,borough,pickup_geometry)) %>%
    st_drop_geometry() 
  
  dropoff <- st_as_sf(post_pickup, coords = c("dropoff_longitude", "dropoff_latitude"), crs = 4326)
  
  system.time({
    dropoff_join <- dropoff %>%
      st_join(taxi_zones)
  })
  
  post_dropoff <- dropoff_join %>%
    mutate(dropoff_zone = zone, 
           dropoff_location = LocationID,
           dropoff_borough = borough,
           dropoff_geometry = geometry) %>%
    select(-c(OBJECTID,Shape_Leng,Shape_Area,zone,LocationID,borough,geometry,dropoff_geometry)) %>%
    st_drop_geometry()
  
  train_2 <- post_dropoff
  
  write_feather(train_2,"split_data/train_2.feather")
  
  names(train_2)

  # Show the busiest locations
  
  busy_pickup_locations <- train_2 %>%
    group_by(pickup_zone) %>%
    summarise(pickup= n()) 
  
  busy_dropoff_locations <- train_2 %>%
    group_by(dropoff_zone) %>%
    summarise(dropoff= n())
  
  taxi_zones_to_borough <- taxi_zone_busy %>%
    select(zone,borough) %>%
    st_drop_geometry()
  
  busy_overall <- busy_pickup_locations %>%
    left_join(busy_dropoff_locations, by = c("pickup_zone" = "dropoff_zone")) %>%
    mutate(overall = pickup + dropoff) %>%
    left_join(taxi_zones_to_borough, by = c("pickup_zone" = "zone")) %>%
    select(zone = pickup_zone, borough, overall) %>%
    mutate(rank = rank(desc(overall))) %>%
    mutate(top_10 = case_when(rank <= 10 ~ "Yes",
                             TRUE ~ "No")) 
  
  busy_overall %>% filter(top_10 == "Yes") %>% arrange (desc(overall)) %>% select(zone, borough,trips = overall)
  
  taxi_zone_busy <- taxi_zones %>%
    left_join(busy_overall, by = "zone")
  
  saveRDS(taxi_zone_busy,"taxi_zone_busy.rds")
  
  taxi_zone_borough <- taxi_zones %>%
    group_by(borough) %>%
    summarise(n = n())
  
  ggplot(taxi_zone_busy) + 
    geom_sf(mapping = aes(fill = overall, colour = borough)) +
    scale_fill_gradient(low = "white", high = "red", label = comma) + 
    ggtitle("Taxi Zone Pickups") +
    labs(x = "Longitude", y = "Latitude", fill = "Number of Pickups", colour = "Borough") + 
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                          size = 0.1))

# ---- 12) h. Which trip has the highest standard deviation of travel time?   ---- 
  
  # Classify a trip as point A to point B in hour. 
  
  high_sd_time <- train_2 %>%
    group_by(pickup_zone,dropoff_zone) %>%
    summarise(st_dev_time = sd(trip_time_in_secs),
              average_distance = mean(trip_distance),
              n = n()) %>%
    filter(!is.na(pickup_zone)) %>%
    filter(!is.na(dropoff_zone)) %>%
    filter(n > 20) %>% 
    arrange(desc(st_dev_time)) %>%
    ungroup() %>%
    head(10)
  
  high_sd_time %>% head(1) %>% mutate(x = round(st_dev_time,0)/60) %>% select(x) %>% pull()
  train_2 %>% filter(pickup_zone == "Maspeth" & dropoff_zone == "JFK Airport") %>% summarise(mean = round(mean(trip_distance),2)) %>% pull()
  
  high_sd_map <- taxi_zone_busy %>%
    select(zone) %>%
    mutate(high_sd = case_when(zone %in% c("Maspeth", "JFK Airport") ~ "Maspeth  to JFK",
                               zone %in% c("South Jamaica", "Midtown South") ~ "South Jamaica  to Midtown South",
                               TRUE ~ ""))
  
  high_sd_map <- taxi_zone_busy %>%
    select(zone) %>%
    mutate(high_sd = case_when(zone %in% c("Maspeth", "JFK Airport") ~ "Maspeth  to JFK",
                               zone %in% c("South Jamaica", "Midtown South") ~ "South Jamaica  to Midtown South",
                               TRUE ~ ""))
  
  ggplot(high_sd_map) + 
    geom_sf(mapping = aes(fill = high_sd)) + 
    scale_fill_manual(values = c("white","red", "blue")) +
    ggtitle("Trips with High Standard Deviation of Time") +
    labs(x = "Longitude", y = "Latitude", fill = "Trip") + 
    coord_sf(xlim = c(-73.7,-74.05),ylim=c(40.55,40.85))+
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                                          size = 0.1))
  
# ---- 13) i. Which trip has most consistent fares?    ---- 
  
  low_sd_fare <- train_2 %>%
    group_by(pickup_zone,dropoff_zone) %>%
    summarise(st_dev_fare = sd(total_amount), 
              average_distance = mean(trip_distance),
              n = n()) %>%
    filter(!is.na(pickup_zone)) %>%
    filter(!is.na(dropoff_zone)) %>%
    filter(n > 20) %>% 
    arrange(st_dev_fare) %>%
    ungroup() %>%
    head(10)
  
  low_sd_map <- taxi_zone_busy %>%
    select(zone) %>%
    mutate(low_sd = case_when(zone %in% c("Cobble Hill", "Columbia Street") ~ "Cobble Hill to Colombia Street",
                               zone %in% c("Seaport", "Battery Park") ~ "Seaport to Battery Park",
                               TRUE ~ ""))
  
  ggplot(low_sd_map) + 
    geom_sf(mapping = aes(fill = low_sd)) + 
    scale_fill_manual(values = c("white","red", "blue")) +
    ggtitle("Trips with Low Standard Deviation in Fare") +
    labs(x = "Longitude", y = "Latitude", fill = "Trip") + 
    coord_sf(xlim = c(-73.95,-74.05),ylim=c(40.66,40.76))+
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.1))
  
  
# ---- OPEN QUESTIONS ----  
  
# ---- 14) a. In what trips can you confidently use respective means as measures of central tendency to estimate fare, time taken, etc.    ---- 

  # Shipiro-Wilk test for normality 
  
  trips <- train_2 %>%
    filter(!is.na(pickup_zone) & !is.na(dropoff_zone)) %>%
    mutate(trip = paste0(pickup_zone," to ", dropoff_zone))
    
  shap_fare <- trips %>%
    select(trip,fare_amount) %>%
    group_by(trip) %>%
    mutate(row = row_number(),
           n = n(),
           min = min(fare_amount),
           max = max(fare_amount),
           dif = min - max) %>%
    filter(row < 5000 & n>30 & dif != 0) %>%
    ungroup() %>%
    select(-c(row,n,min,max,dif)) %>%
    nest(-trip) %>% 
    mutate(shapiro = map(data, ~ shapiro.test(.x$fare_amount)),
           tidied = map(shapiro,tidy)) %>%
    unnest(tidied) %>%
    select(-c(data,shapiro))
  
  shap_fare_1 <- shap_fare %>%
    top_n(10,statistic)
  
  example_1 <- trips %>%
    filter(trip == "Midtown North to Seaport")
  
  ggplot(example_1,aes(fare_amount)) +
    geom_histogram(bins = 50) +
    ggtitle("Distribution of Fare Amount from Midtown North to Seaport") +
    labs(x = "Fare Amount", y = "Frequency") +
    theme_minimal()
  
  shap_fare_2 <- shap_fare %>%
    filter(!str_detect(trip,"JFK")) %>%
    top_n(-10,statistic)
  
  example_2 <- trips %>%
    filter(trip == "Brooklyn Heights to Cobble Hill")
  
  ggplot(example_2,aes(fare_amount)) +
    geom_histogram() +
    ggtitle("Distribution of Fare Amount from Brooklyn Heights to Cobble Hill") +
    labs(x = "Fare Amount", y = "Frequency") +
    theme_minimal()
  
  shap_time <- trips %>%
    select(trip,trip_time_in_secs) %>%
    group_by(trip) %>%
    mutate(row = row_number(),
           n = n(),
           min = min(trip_time_in_secs),
           max = max(trip_time_in_secs),
           dif = min - max) %>%
    filter(row < 5000 & n>30 & dif != 0) %>%
    ungroup() %>%
    select(-c(row,n,min,max,dif)) %>%
    nest(-trip) %>% 
    mutate(shapiro = map(data, ~ shapiro.test(.x$trip_time_in_secs)),
           tidied = map(shapiro,tidy)) %>%
    unnest(tidied) %>%
    select(-c(data,shapiro))
  
  shap_time_1 <- shap_time %>%
    top_n(10,statistic)
  
  example_3 <- trips %>%
    filter(trip == "Financial District North to Central Park")
    
  ggplot(example_3,aes(trip_time_in_secs)) +
    geom_histogram(bins = 50) +
    ggtitle("Distribution of Trip Time from Financial District North to Central Park") +
    labs(x = "Fare Amount", y = "Frequency") +
    theme_minimal()
  
# ---- 15)  b. Can we build a model to predict fare and tip amount given pick up and drop off coordinates, time of day and week?     ----  
  
# ---- 15a) Linear Model of fare_amount ---- 
  
  train_1 <- train %>%
    mutate(pickup_time = as.factor(case_when(pickup_hour >= 2 & pickup_hour < 7 ~ "Early Morning",
                                   pickup_hour >= 7 & pickup_hour < 12 ~ "Morning",
                                   pickup_hour >= 12 & pickup_hour < 18 ~ "Afternoon",
                                   pickup_hour >= 18 & pickup_hour < 22 ~ "Evening",
                                   pickup_hour >= 22 | pickup_hour < 2 ~ "Late Night")))
  
  lm_1 <- lm(data = train_1, formula = fare_amount ~ pickup_longitude + pickup_latitude + 
                dropoff_longitude + dropoff_latitude +
                pickup_time + day_of_week)
  
  val_1 <- val %>%
    mutate(pickup_time = as.factor(case_when(pickup_hour >= 2 & pickup_hour < 7 ~ "Early Morning",
                                   pickup_hour >= 7 & pickup_hour < 12 ~ "Morning",
                                   pickup_hour >= 12 & pickup_hour < 18 ~ "Afternoon",
                                   pickup_hour >= 18 & pickup_hour < 22 ~ "Evening",
                                   pickup_hour >= 22 | pickup_hour < 2 ~ "Late Night")))
  
  val_pred <- val_1 %>%
    mutate(pred = predict.lm(object = lm_1,newdata = val_1))
  
# ---- 15b) Random Forest of fare_amount ----
  
  # Absolutely not going to work with all the data. Maybe try a sample of 5-10k 
  
  set.seed(81)
  rf_sample_index <- sample(1:nrow(train_1), 5000)
  rf_sample <- train_1[rf_sample_index,]
  
  rf_x <- rf_sample[,c(11:14,24:25)]
  rf_y <- rf_sample$fare_amount
  
  rf_1 <- randomForest(y = rf_y, x = rf_x)
  
  val_check <- val_1 %>%
    head(100000) %>%
    select(11:14,24,25,16)
  
  val_pred_rf <- val_check %>%
    mutate(pred = predict(object = rf_1,newdata = val_check))
  
  rf_rmse <- val_pred_rf %>%
    summarise(rmse = sqrt(mean((pred - fare_amount)^2)))

  ggplot(val_pred_rf,aes(x = fare_amount,pred)) + 
    geom_point(alpha = 0.1, size = 0.3) +
    coord_cartesian(xlim = c(0,60),ylim=c(0, 60)) +
    ggtitle("Actual vs Predicted - Fare Amount") +
    labs(x = "Fare Amount", y = "Predicted Fare Amount") +
    theme_minimal()
  
  varImpPlot(rf_1)
  
# ---- 15c) Random Forest of tip_amount ----
  
  rf_y_2 <- rf_sample$tip_amount
  
  rf_2 <- randomForest(y = rf_y_2, x = rf_x)
  
  val_check_2 <- val_1 %>%
    head(100000) %>%
    select(11:14,24,25,19)

  val_pred_rf_2 <- val_check_2 %>%
    mutate(pred = predict(object = rf_2,newdata = val_check))
  
  rf_rmse_2 <- val_pred_rf_2 %>%
    summarise(rmse = sqrt(mean((pred - tip_amount)^2)))
  
  ggplot(val_pred_rf_2,aes(x = tip_amount,pred)) + 
    geom_point(alpha = 0.1, size = 0.3) +
    coord_cartesian(xlim = c(0,20),ylim=c(0, 20)) +
    ggtitle("Actual vs Predicted - Tip Amount") +
    labs(x = "Tip Amount", y = "Predicted Fare Amount") +
    theme_minimal()
  
# ---- 16)  c. If you were a taxi owner, how would you maximize your earnings in a day?   ----
  
# ---- 17)  d. If you were a taxi owner, how would you minimize your work time while retaining the average wages earned by a typical taxi in the dataset?    ----
  
# ---- 18)  e. If you run a taxi company with 10 taxis, how would you maximize your earnings?    ----