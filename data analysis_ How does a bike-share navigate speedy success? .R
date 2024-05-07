library(psych)
library("magrittr")
library("caret")
library('tidyverse')
library('janitor')
library('lubridate')
library("readr")
library("dplyr")
library("tidyr")
library("quanteda")
library("ggplot2")
library("sp")
library("psych")
library("validate")
library("data.table")
library("leaflet")


bike_ride <- read_csv("cleaning_file_1.csv")
View(head(bike_ride))
names(bike_ride)
View((bike_ride))

distinct_months <- bike_ride %>%
  mutate(month_year = paste0(month, "_", year)) %>%
  distinct(month_year)
View(distinct_months)

bike_ride$month <- ordered(bike_ride$month,levels=c("Jan_23", "Feb_23","Mar_23","Apr_23","May_23","Jun_23","Jul_23","Aug_23","Sep_23","Oct_23","Nov_23","Dec_23","Jan_24","Feb_24"))

bike_ride$week_day <- ordered(bike_ride$week_day, levels = c("Sunday", "Monday", "Tuesday", 
                                                                           "Wednesday", "Thursday", 
                                                                           "Friday", "Saturday"))
#How many customers do we have, their type, casual or member? we have casual 1369442 and member 2285577
View(describe(bike_ride$ride_length, fast=TRUE))
View(table(bike_ride$type_of_customer))

View(setNames(aggregate(ride_length ~ type_of_customer, bike_ride, sum), c("type_of_customer", "total_ride_len(mins)")))



#Differences between members and casual riders, min, median, mode in minutes. 
View(bike_ride %>% 
       group_by(type_of_customer) %>% 
       summarise(min_length_mins = min(ride_length), max_length_min = max(ride_length),
                 median_length_mins = median(ride_length), mean_length_min = mean(ride_length)))

#Now we will calculate the mean, media mode, then see the average length by each day according to customer type. We will then analyze ride length data based on casual, vs annual members and weekday                                 
View(bike_ride %>% 
       group_by(week_day) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()) %>%
       arrange(Avg_length))

View(aggregate(ride_length ~ type_of_customer + week_day, data = bike_ride, FUN = mean) %>%
       arrange(week_day, type_of_customer))

View(bike_ride %>% 
       group_by(type_of_customer, week_day) %>% 
       summarise(number_of_ride = n(),
                 avgerage_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

#Next we will calculate the mean, media mode, then see the average length by month according to customer type. We will then analyze ride length data based on casual, vs annual members and months(januar2023-april2024)                                

View(bike_ride %>% 
       group_by(month) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()) %>%
       arrange(desc(Avg_length)))

View(aggregate(ride_length ~ type_of_customer + month, data = bike_ride, FUN = mean) %>%
       arrange(month, type_of_customer))   


View(bike_ride %>% 
       group_by(type_of_customer, month) %>% 
       summarise(nummber_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length),
                 .groups = "drop"))

View(bike_ride)


Visuals: 
#frequency of Customer types, Casual, member:
ggplot(data = bike_ride, aes(x = type_of_customer)) +
  geom_bar(fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black") +
  labs(title = "Frequency of Customer Types", x = "Customer Type", y = "Frequency")

# Heatmap for spatial density of bike rides
ggplot(data = bike_ride, aes(x = start_lng, y = start_lat)) +
  geom_bin2d() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Spatial Density of Bike Rides", x = "Longitude", y = "Latitude")

#Pie chart for proportion of customer types
ggplot(data = bike_ride, aes(x = "", fill = type_of_customer)) +
  geom_bar(width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Customer Types", fill = "Customer Type") +
  theme_void()

ggplot(data = bike_ride, aes(x = start_lng, y = start_lat, color = ride_length)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Spatial Distribution of Ride Lengths",
       x = "Start Longitude", y = "Start Latitude",
       color = "Ride Length")









##############IGNORE###############
#######Still working on this, no ram                              
#Geographic Analysis
make_a_map <- leaflet(bike_ride) %>%
  addTiles() %>%
  addMarkers(~start_lat, ~start_lng, popup = ~start_station_name)

View(make_a_map)


set.seed(123) # Set seed for reproducibility
train_index <- createDataPartition(bike_ride$ride_length, p = 0.8, list = FALSE)
train_data <- bike_ride[train_index, ]
test_data <- bike_ride[-train_index, ]

# Train a predictive model (e.g., linear regression) to predict ride length
model <- lm(ride_length ~ start_lat + start_lng + end_lat + end_lng + type_of_customer, data = train_data)

# Evaluate the model performance on the test data
predictions <- predict(model, newdata = test_data)





                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    