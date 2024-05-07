#downloading required packages
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("validate")

#loading packages
library('tidyverse')
library('janitor')
library('lubridate')
library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("validate")

#importing my data "X2023_10 <- read_csv("Downloads/2023-10.csv")" used for jan 2023-april 2024
january_2023 <- X2023_01
february_2023 <- X2023_02
march_2023 <- X2023_03
april_2023 <- X2023_04
may_2023 <- X2023_05
june_2023 <- X2023_06
july_2023 <- X2023_07
august_2023 <- X2023_08
september_2023 <- X2023_09
october_2023 <- X2023_10
november_2023 <- X2023_11
december_2023 <- X2023_12
january_2024 <- X2024_01
february_2024 <- X2024_02
march_2024 <- X2024_03
april_2024 <- X2024_04


#data validation
colnames(january_2023)
colnames(february_2023)
colnames(march_2023)
colnames(april_2023)
colnames(may_2023)
colnames(june_2023)
colnames(july_2023)
colnames(august_2023)
colnames(september_2023)
colnames(october_2023)
colnames(november_2023)
colnames(december_2023)
colnames(january_2024)
colnames(february_2024)
colnames(march_2024)
colnames(april_2024)

fourteen_months <- rbind(january_2023, february_2023,march_2023,april_2023,may_2023,june_2023,july_2023,august_2023,september_2023, october_2023,november_2023,december_2023,january_2024,february_2024,march_2024,april_2024)

write.csv(fourteen_months, file="cycle_success.csv",row.names = FALSE)

str(fourteen_months)
View(head(fourteen_months))
View(tail(fourteen_months))
View(str(fourteen_months))
View(summary(fourteen_months))


names(fourteen_months)

#data cleaning
colSums(is.na(fourteen_months)) #counting if our data has any null values 

cleaning_file <- fourteen_months[complete.cases(fourteen_months), ]
cleaning_file <- distinct(cleaning_file)
cleaning_file <- drop_na(cleaning_file)
cleaning_file <- remove_empty(cleaning_file)
cleaning_file <- remove_missing(cleaning_file)

#Remove stolen bikes
cleaning_file <- cleaning_file[!cleaning_file$ride_length>1440,] 
cleaning_file <- cleaning_file[!cleaning_file$ride_length<5,] 

colSums(is.na(cleaning_file)) #checking for clean data, shows no data with na
#confirming my data is clean again... 
str(cleaning_file)
View(head(cleaning_file))
View(tail(cleaning_file))
View(str(cleaning_file))
View(summary(cleaning_file))

cleaning_file <- rename(cleaning_file, type_of_customer = member_casual)
cleaning_file <- rename(cleaning_file, type_of_bike = rideable_type)

#seperating date, day, month, year, then colums for time 
cleaning_file <- cleaning_file %>%
  mutate(date = as.Date(started_at),
         week_day = format(date, "%A"),
         month = format(date, "%b_%y"),
         year = format(date, "%Y"))

cleaning_file <- cleaning_file %>%
  mutate(time = format(as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"), format = "%H:%M"))

cleaning_file$ride_length <- difftime(cleaning_file$ended_at, cleaning_file$started_at, units = "mins") #ride length

#what data will we be using? the bike type, customer, month, year, time, beginning of the trip, ending trip, stations, day of the week... 

cleaning_file <- cleaning_file %>% 
  select(bike_type, costumer_type, month, year, time, started_at, week_day, ride_length)

write.csv(cleaning_file, file = "cleaning_file.csv", row.names = FALSE)


