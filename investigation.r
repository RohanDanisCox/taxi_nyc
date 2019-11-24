# Initial work

# ---- 1) Load Libraries ----
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(feather)
  library(sf)
  library(scales)
  
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


  # ---- 6) c. What is the distribution of fare amount?   ---- 
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
  
  # ---- 7) d. What is the distribution of tip amount?   ---- 
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
  
  # ---- 7) e. What is the distribution of total amount?   ---- 
  
  summary(train$total_amount)
  
  ggplot(train, aes(total_amount)) +
    geom_histogram(bins = 500) + 
    scale_y_continuous(labels = comma) +
    ggtitle("Distribution of Total Amount") +
    labs(x = "Total Amount", y = "Frequency") +
    theme_minimal()

  
  # ---- 8) f. What are top 5 busiest hours of the day? ---- 

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
  
# ---- 8) g. What are the top 10 busiest locations of the city?  ---- 

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
    
# ---- 8a) Add in Taxi Zones  ---- 
    
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
  
  busy_overall <- busy_pickup_locations %>%
    left_join(busy_dropoff_locations, by = c("pickup_zone" = "dropoff_zone")) %>%
    mutate(overall = pickup + dropoff) %>%
    select(zone = pickup_zone, overall) %>%
    mutate(rank = rank(desc(overall))) %>%
    mutate(top_10 = case_when(rank <= 10 ~ "Yes",
                             TRUE ~ "No"))
  
  busy_overall %>% filter(top_10 == "Yes") %>% arrange (desc(overall)) %>% select(zone, trips = overall)
  
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
    theme_minimal() 
 
