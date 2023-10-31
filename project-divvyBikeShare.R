#Focusing on how the two rider types(casual, annual members) use the bike share differently.

#Libraries used in our project
results="hide"
library(tidyverse)
library(dplyr)
library(tidyr)
library(skimr)
library(tibble)
library(janitor)
library(lubridate)



#--------------------------------------Overview before Starting------------------------------------------------

#Importing data into our environment.
#The csv files will need to be converted to a data-frame before inspecting the data structure.
BikeTrips_2023_03 <- read.csv("C://Users//LENOVO//Desktop//Aycan_Lizor//Humber//Semester_4//5_Thursday_5424_BigData2//Midterm_Project//202303-divvy-tripdata.csv")
BikeTrips_2023_04 <- read.csv("C://Users//LENOVO//Desktop//Aycan_Lizor//Humber//Semester_4//5_Thursday_5424_BigData2//Midterm_Project//202304-divvy-tripdata.csv")
BikeTrips_2023_05 <- read.csv("C://Users//LENOVO//Desktop//Aycan_Lizor//Humber//Semester_4//5_Thursday_5424_BigData2//Midterm_Project//202305-divvy-tripdata.csv")
BikeTrips_2023_06 <- read.csv("C://Users//LENOVO//Desktop//Aycan_Lizor//Humber//Semester_4//5_Thursday_5424_BigData2//Midterm_Project//202306-divvy-tripdata.csv")


# Calculate the number of rows in each dataset
n_rows_2023_03 <- nrow(BikeTrips_2023_03)
n_rows_2023_04 <- nrow(BikeTrips_2023_04)
n_rows_2023_05 <- nrow(BikeTrips_2023_05)
n_rows_2023_06 <- nrow(BikeTrips_2023_06)

# Calculate the total number of rows
total_rows <- n_rows_2023_03 + n_rows_2023_04 + n_rows_2023_05 + n_rows_2023_06

# Print the total number of rows
print(total_rows)

#------------------------------------------------------------------------------
# Combine the start and end station names from all four datasets
all_stations <- c(BikeTrips_2023_03$start_station_name, BikeTrips_2023_03$end_station_name,
                  BikeTrips_2023_04$start_station_name, BikeTrips_2023_04$end_station_name,
                  BikeTrips_2023_05$start_station_name, BikeTrips_2023_05$end_station_name,
                  BikeTrips_2023_06$start_station_name, BikeTrips_2023_06$end_station_name)

# Convert to a vector and get unique station names
unique_stations <- unique(all_stations)

# Get the total number of unique stations
num_unique_stations <- length(unique_stations)

# Print the result
print(num_unique_stations)
#--------------------------------------------------------------------------------------------
# Combine the ride_id values from all four datasets
all_ride_ids <- c(
  BikeTrips_2023_03$ride_id,
  BikeTrips_2023_04$ride_id,
  BikeTrips_2023_05$ride_id,
  BikeTrips_2023_06$ride_id
)

# Get the total number of unique ride_id values
total_ride_ids <- length(unique(all_ride_ids))

# Print the total number of unique ride_id values
print(total_ride_ids)


#--------------------------------------Step 1 : Processing Data------------------------------------------------


#Integrating the 4 data-frames into one,
#The structure of the DF's need to be verified. If they don't share the same number of columns and data types, 
#they cannot be integrated.
str(BikeTrips_2023_04)
str(BikeTrips_2023_05)
str(BikeTrips_2023_06)
str(BikeTrips_2023_03)

#comparing data type of all 4 dataframes.
compare_df_cols(BikeTrips_2023_04,BikeTrips_2023_05,BikeTrips_2023_06,BikeTrips_2023_03)

#All of the dataframes share the same structure, so integrate them.
BikeTrips <- rbind(BikeTrips_2023_04,BikeTrips_2023_05,BikeTrips_2023_06,BikeTrips_2023_03)

#Verifying that integrated properly.
glimpse(BikeTrips)

#convert  blank value to NA
BikeTrips[BikeTrips==""] <- NA

BikeTrips %>%
  summarise(across(everything(), ~ sum(is.na(.))))

#convert start and end date data type from character to datetime format
BikeTrips$started_at = as_datetime(BikeTrips$started_at)
BikeTrips$ended_at = as_datetime(BikeTrips$ended_at)

#convert the "rideable_type" and "member_casual" columns from character to a categorical data type.
BikeTrips$rideable_type <- as.factor(BikeTrips$rideable_type)
BikeTrips$member_casual <- as.factor(BikeTrips$member_casual)

#create new columns Hour_of_Day, Weekday, Day_of_Month and Month.  
BikeTrips <- mutate(BikeTrips, Weekday = weekdays(BikeTrips$started_at))
BikeTrips <- mutate(BikeTrips, Day_of_Month = day(BikeTrips$started_at))
BikeTrips <- mutate(BikeTrips, Month = month(BikeTrips$started_at, label = TRUE))

tail(BikeTrips$Month == 'Jul')
JulyData <- BikeTrips[BikeTrips$Month == "Jul", ]

BikeTrips <- mutate(BikeTrips, Hour_of_Day = format(as.POSIXct(BikeTrips$started_at, format = "%Y-%m-%d %H:%M:%S"), format = "%H"))

#Format Weekday and Month to a categorical data type and Hour_of_Day to character.
BikeTrips$Hour_of_Day <- as.numeric(as.character(BikeTrips$Hour_of_Day))
BikeTrips$Weekday <- as.factor(BikeTrips$Weekday)
BikeTrips$Month <- as.factor(BikeTrips$Month)

#All columns are in correct format.
str(BikeTrips)

#Added one more column as duration of trip and given numeric datatype.
BikeTrips <- mutate(BikeTrips, Trip_Duration = difftime(BikeTrips$ended_at, BikeTrips$started_at))
BikeTrips$Trip_Duration <- as.numeric(as.character(BikeTrips$Trip_Duration))/60
BikeTrips$Trip_Duration <- round(BikeTrips$Trip_Duration)




#-------------------------------------Step 2 : Cleaning data---------------------------------------------------

#Removing the rows containing NA values.
BikeTrips <- drop_na(BikeTrips)

#Some changes to trip_duration because the maximum value is 12136.00 minutes which is 202.267 hours which is not possible
summary(BikeTrips$Trip_Duration)

#Drawing the line at 960 minutes (16hrs) with a new DF that reflect realistic trip duration.
BikeTrips_Outliers <- subset(BikeTrips, BikeTrips$Trip_Duration > 960)
BikeTrips_1 <- subset(BikeTrips, BikeTrips$Trip_Duration <= 960)

#Now the maximum value is 959 minutes and minimum is 0 minutes.
summary(BikeTrips_1$Trip_Duration)

#Remove columns start_station_id and end_station_id  
BikeTrips_1 <- BikeTrips_1 %>%
  select(-start_station_id, -end_station_id)

#Rename column names for better understanding.
BikeTrips_1 <- BikeTrips_1 %>%
  rename(Bike_ID=ride_id, 
         Bike_Type=rideable_type, 
         Start_Time=started_at, 
         End_Time=ended_at, 
         Start_Station=start_station_name, 
         End_Station=end_station_name, 
         Start_Lat=start_lat, 
         Start_Lng=start_lng, 
         End_Lat=end_lat, 
         End_Lng=end_lng, 
         Rider_Type=member_casual)

#Displays first few rows
head(BikeTrips_1)

#Sort the data in ascending order by start time column.
BikeTrips_1 <- arrange(BikeTrips_1, Start_Time)

#ordering the weekdays column.
BikeTrips_1$Weekday <- ordered(BikeTrips_1$Weekday, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#Displays first few rows
head(BikeTrips_1)





#-----------------------------------------Step 3 : Analysis/Visualization------------------------------------------


#1. counting of each rider type.
BikeTrips_1 %>%
  count(Rider_Type, sort = TRUE)

#Answer: member 968349, casual 553703


#2. counting of the bike types used by the two groups of riders.
BikeTrips_1 %>%
  count(Rider_Type, Bike_Type, sort = TRUE)

#Answer:member(classic_bike 582301, electric_bike 386048) / 
#       casual(classic_bike 276317, electric_bike 238575, docked_bike 38811)


#3. Percentage distribution of two groups of riders.
BikeTrips_1 %>%
  count(Rider_Type, sort = TRUE) %>%
  mutate(n = n/sum(n) * 100)

#Answer: member 63.62128 % and casual 36.37872 %


#4. Percentage distribution of bike type of two groups of riders.
BikeTrips_1 %>%
  count(Rider_Type, Bike_Type) %>%
  group_by(Rider_Type) %>%
  mutate(n = n/sum(n) * 100) %>%
  pivot_wider(names_from = Rider_Type, values_from = n,
              values_fill = list(n = 0))

#Answer: Casual (classic_bike   49.9%, docked_bike 7.01%, electric_bike  43.1%)
#        member (classic_bike   60.1%, docked_bike 0%, electric_bike  39.9%)


#Analysis based on above 3 calculation: 
# 63.62% of the riders are annual members, while 36.37% are casual riders.
# Among the different bike types, annual members preferred classic bikes over electric bikes and didn't ride docked bikes at all.
# Casual riders were the only rider type to use the docked bikes.


#Now Visualization
# Visualization 1 (Type of Bike )
library(dplyr)
library(ggplot2)

count_bike_type <- BikeTrips_1 %>%
  count(Bike_Type, sort = TRUE)

count_bike_type <- count_bike_type %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(count_bike_type, aes(x = "", y = n, fill = Bike_Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Distribution of Rider Types") +
  geom_text(aes(label = paste(Bike_Type, "\n", n, " (", round(percentage, 1), "%)")), position = position_stack(vjust = 0.5))

# Visualization 2 (Count of both riders)

# Calculate counts and percentages for each Rider_Type
count_percent <- BikeTrips_1 %>%
  count(Rider_Type) %>%
  mutate(percentage = n / sum(n) * 100)

# Define light_colors
light_colors <- c("#FF9999", "#66B3FF")

# Create the bar plot with labels and percentages
ggplot(count_percent, aes(x = Rider_Type, y = n, fill = Rider_Type)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = light_colors) +
  labs(x = "Rider Type", y = "Rider Count",
       title = "Annual vs Casual Members") +
  geom_text(aes(label = paste(n, " (", round(percentage, 1), "%)")),
            position = position_stack(vjust = 0.5))


# Visualization 3 (Total rides of Different bike type of both the rider groups)
library(ggplot2)

# Your existing ggplot code

# Calculate counts and percentages for each Bike_Type and Rider_Type combination
count_percent <- BikeTrips_1 %>%
  group_by(Bike_Type, Rider_Type) %>%
  summarise(Count = n()) %>%
  group_by(Rider_Type) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Define custom colors
custom_colors <- c("green", "blue")

# Create the bar plot with labels and percentages
ggplot(count_percent, aes(x = Bike_Type, y = Count, fill = Rider_Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Rider_Type, scales = "free_x") +
  labs(x = "Bike Type", y = "Total Rides",
       title = "Bike Type Preferences of both riders") +
  guides(x = guide_axis(angle = 0)) +
  scale_fill_manual(values = custom_colors) +
  geom_text(aes(label = paste(Count, " (", round(Percentage, 1), "%)")),
            position = position_stack(vjust = 0.5))



#Visualization 4 (stating time of both riders on hourly basis on a day)
ggplot(BikeTrips_1, mapping = aes(x = Hour_of_Day, y = Rider_Type, fill = Rider_Type)) +
  theme_bw() +
  geom_boxplot() +
  labs(x = "Hour of Day", y = "Rider Type",
       title = "Start Time Tendenciesof both riders")


#Visualization 5 (Number of starts per hour of both the riders group)
ggplot(BikeTrips_1, aes(x = Hour_of_Day, fill = Rider_Type)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "dodge") + 
  facet_wrap(~Rider_Type, scales = "free_x") +
  labs(x = "Hour of Day", y = "Total Rides", 
       title = "SPH (Starts Per Hour)")


#Visualization 6 (Number of starts per hour every day)
ggplot(BikeTrips_1, mapping = aes(x = Hour_of_Day, fill = Rider_Type))+
  theme_bw() +
  geom_bar() +
  facet_wrap(~Weekday, scales = "free_x") +
  labs(x = "Hour of Day", y = "Total Rides",
       title = "SPH By Weekday")


#Visualization 7 (stating time of both riders group every day)
ggplot(BikeTrips_1, mapping = aes(x = Hour_of_Day, y = Rider_Type, fill = Rider_Type))+
  theme_bw() +
  geom_boxplot() +
  facet_wrap(~Weekday, scales = "free_x") +
  labs(x = "Hour of Day", y = "Rider Type",
       title = "Start Time Tendencies By Weekday")


#Visualization 8 (Total rides per day, relative to each rider type)
ggplot(BikeTrips_1, mapping = aes(x = Weekday, fill = Rider_Type)) +
  theme_bw() +
  geom_bar() +
  facet_wrap(~Rider_Type, scales = "free_x") +
  labs(x = "Weekday", y = "Total Rides",
       title = "Rides Per Day By Rider Type") +
  guides(x = guide_axis(angle = 45))


#Visualization 9 (Total rides per month, relative to each rider type)
ggplot(BikeTrips_1, mapping = aes(x = Month, fill = Rider_Type)) +
  theme_bw() +
  geom_bar() +
  facet_wrap(~Rider_Type, scales = "free_x") +
  labs(x = "Month", y = "Total Rides",
       title = "Rides Per Month By Rider Type") +
  guides(x = guide_axis(angle = 50))


#creating a new DF for calculating average daily trip duration.
Temp_DF <- aggregate(Trip_Duration ~ Rider_Type + Weekday, BikeTrips_1, mean, sort = TRUE)

Temp_DF_1 <- aggregate(Trip_Duration ~ Rider_Type, BikeTrips_1, mean)

AVG_duration_DF <- BikeTrips_1 %>%
  group_by(Weekday, Rider_Type) %>%
  summarise(Avg_Trip_Duration=mean(Trip_Duration), .groups = "keep")

#Visualization 10 (Average trip duration everyday)
ggplot(AVG_duration_DF, mapping = aes(x = Weekday, y = Avg_Trip_Duration, fill = Rider_Type)) +
  theme_bw() +
  geom_col(position = "dodge") +
  facet_wrap(~Rider_Type, scales = "free_x") +
  labs(x = "Weekday", y = "Average Trip Duration",
       title = "Average Daily Trip Duration") +
  guides(x = guide_axis(angle = 45))


#Analysis based on visualization :
#1. Majority of annual members use the Cyclist Bike-Share services to commute to and from work 
    #as indicated by how consistent the start times are and the predictability of their respective 
    #average trip duration. While the casual riders use these services for other casual purposes 
    #because of their longer average trip duration.
#2. Also due to short trip duration of annual riders, they may be living in city areas compared to casual riders(suburban areas).

