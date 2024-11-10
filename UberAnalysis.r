library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse) 
library(DT)
library(scales)
library(readxl)
# creating a vector for colours using their unicodes

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
colors
apr <- read_xlsx("C:\\Users\\manic\\Downloads\\uber-raw-data-apr14.xlsx")
may <- read_xlsx("C:\\Users\\manic\\Downloads\\uber-raw-data-may14.xlsx")
june <- read_xlsx("C:\\Users\\manic\\Downloads\\uber-raw-data-jun14.xlsx")
july <- read_xlsx("C:\\Users\\manic\\Downloads\\uber-raw-data-jul14.xlsx")
aug <- read_xlsx("C:\\Users\\manic\\Downloads\\uber-raw-data-aug14.xlsx")
sept <- read_xlsx("C:\\Users\\manic\\Downloads\\uber-raw-data-sep14.xlsx")

# to combine data together wrt to rows

data <- rbind(apr, may, june, july, aug, sept)
cat("The dimensions of the data are:", dim(data))

#print first 6 rows 

head(data)

data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date.Time <- ymd_hms(data$Date.Time)

#Create individual columns for month day and year

data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label=TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label=TRUE))

# Add Time variables as well 

data$second = factor(second(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$hour = factor(hour(hms(data$Time)))

# Look at the data
head(data)

hourly_data <- data %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

# Shos data in a searchable js table

datatable(hourly_data)

# Plot the data by hour

ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="steelblue", 
           color="red") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

# Aggregate data by day of the month

day_data <- data %>% group_by(day) %>% dplyr::summarize(Trips = n())

# Aggregate the data by month and hour

month_hour_data <- data %>% group_by(month, hour) %>%  dplyr::summarize(Total = n())
ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)


# Plot the data for the day

ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)
day_data

# Collect data by day of the week and month

day_month_data <- data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
day_month_data

# Plot the above data

ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# no of trips placed during months in an year

month_data <- data %>% group_by(month) %>% dplyr::summarize(Total = n())

month_data
ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips in a month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)
