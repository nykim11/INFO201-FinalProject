library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

# Reads the csv for both atlantic and pacific regions
atlantic.df <- read.csv('data/atlantic.csv', stringsAsFactors = FALSE)
pacific.df <- read.csv('data/pacific.csv', stringsAsFactors = FALSE)
both.df <- rbind(atlantic.df, pacific.df)

# Vincent
# Returns a storm dataframe that includes only core values plus MAXIMUM WIND, 
# with latitude and longitude values converted from NESW to +/-
maxWindDF <- function(df) {
  df <- df %>%
    select(ID, Name, Date, Time, Latitude, Longitude, Maximum.Wind) %>% 
    filter(Maximum.Wind >= 0)
  
  n <- nchar(df$Latitude)
  pos.neg.lat <- substr(df$Latitude, n, n)
  pos.neg.lat[pos.neg.lat == 'N'] <- 1
  pos.neg.lat[pos.neg.lat == 'S'] <- -1
  df$Latitude <- as.double(substr(df$Latitude, 1, n - 1)) * as.integer(pos.neg.lat)
  
  n <- nchar(df$Longitude)
  pos.neg.long <- substr(df$Longitude, n, n)
  pos.neg.long[pos.neg.long == 'E'] <- 1
  pos.neg.long[pos.neg.long == 'W'] <- -1
  pos.neg.long <- as.integer(pos.neg.long)
  df$Longitude <- as.double(substr(df$Longitude, 1, n - 1)) * as.integer(pos.neg.long)
  
  df$Longitude[df$Longitude < 0] <- df$Longitude[df$Longitude < 0] + 360
  
  return(df)
}

# Vincent
# Returns a storm dataframe describing starting locations and maximum wind speeds
startLocMaxWindDF <- function(df) {
  df <- maxWindDF(df) %>% 
    group_by(ID) %>% 
    arrange(Date, Time) %>% 
    mutate(Maximum.Wind = max(Maximum.Wind)) %>% 
    group_by(ID) %>% 
    filter(Date == min(Date)) %>% 
    group_by(ID, Date) %>% 
    filter(Time == min(Time))
  
  return(df)
}

atlantic.slocmaxwind <- startLocMaxWindDF(atlantic.df)
pacific.slocmaxwind <- startLocMaxWindDF(pacific.df)
all.maxwind <- maxWindDF(both.df)

# Loads the world map data
world <- map_data('world2')

# Anushree - Formatted the data needed to make graphs
atlantic.data <- atlantic.df %>% 
  #filter(Low.Wind.NE != -999) %>% 
  mutate(Proper.Date = as.Date(as.character(Date), "%Y%m%d")) 
pacific.data <- pacific.df %>% 
  #filter(Low.Wind.NE != -999) %>% 
  mutate(Proper.Date = as.Date(as.character(Date), "%Y%m%d")) 
both.data <- both.df %>% 
  #filter(Low.Wind.NE != -999) %>% 
  mutate(Proper.Date = as.Date(as.character(Date), "%Y%m%d"))

# Anushree - Data frame for Status key
status.key <- c('TD', 'TS', 'HU', 'SD', 'DB', 'SS', 'EX', 'LO', 'WV')
status.def <- c('Tropical Depression', 'Tropical Storm', 'Hurricane', 'Subtropical Depression', 'DB',
                'Subtropical Storm', 'Extratropical Systems', 'Low', 'Tropical Wave')
storm.intensity.size <- c(6, 7, 9, 4, 3, 8, 5, 1, 2)
storm.status.table <- data.frame(status.key, status.def, storm.intensity.size)
storm.status.table <- rename(storm.status.table, 'Key' = status.key, 'Storm Category' = status.def, 
                             'Intensity Level' = storm.intensity.size)

# Anushree - Initialize final.data that is being used for all graphs
final.data <- data.frame('empty')

# Cliff
# Function to get the first and last recorded data for each of the different
# storms in one data frame
FirstLastWind <- function(df) {
  temp.min <- df %>%
    group_by(ID) %>%
    filter(Date == min(Date)) %>%
    filter(Time == min(Time))
  
  temp.max <- df %>%
    group_by(ID) %>%
    filter(Date == max(Date)) %>%
    filter(Time == max(Time))
  
  df <- rbind(temp.min, temp.max) %>% arrange(ID)
  
  return(df)
}

# Cliff
# Function to calculate the difference between the first and last longitude and
# latitude for the purpose of calculating the angle between the two points
DiffDF <- function(df) {
  df <- df %>%
    group_by(ID) %>% summarize(dLat=diff(Latitude), dLong=diff(Longitude))
  return(df)
}

# Cliff
# Calculates the angle between the two points
ToAngle <- function(df) {
  return(
    ifelse(df$dLong == 0,
           ifelse(df$dLat > 0, 90, 270),
           ifelse(df$dLong < 0, (atan(df$dLat/df$dLong) / pi * 180) + 180,
                  ifelse(df$dLat < 0, (atan(df$dLat/df$dLong) / pi * 180) + 360,
                         (atan(df$dLat/df$dLong) / pi * 180)))))
}

# Cliff
# Calculates the direction based on the angle between the two points
ToDirection <- function(df) {
  return(
    ifelse(df$Angle > 337.5 | df$Angle < 22.5, "East",
           ifelse(df$Angle >= 22.5 & df$Angle <= 67.5, "Northeast",
                  ifelse(df$Angle > 67.5 & df$Angle < 112.5, "North",
                         ifelse(df$Angle >= 112.5 & df$Angle <= 157.5, "Northwest",
                                ifelse(df$Angle > 157.5 & df$Angle < 202.5, "West",
                                       ifelse(df$Angle >= 202.5 & df$Angle <= 247.5, "Southwest",
                                              ifelse(df$Angle > 247.5 & df$Angle < 292.5, "South", "Southeast"))))))))
}

# ATLANTIC
# Setting up the atlantic data
atlantic.q5 <- FirstLastWind(maxWindDF(atlantic.df))
atlantic.diff.q5 <- DiffDF(atlantic.q5)

# Joins the maximum speed of each storm (not just first and last recorded info),
# the differences between the longitude and latitude between the first and last
# recorded location, and the angle and direction between the two points
atlantic.full.q5 <- left_join(atlantic.slocmaxwind, atlantic.diff.q5) %>%
  select(ID, Name, Date, Maximum.Wind, dLat, dLong)
atlantic.full.q5$Angle <- ToAngle(atlantic.full.q5)
atlantic.full.q5$Direction <- ToDirection(atlantic.full.q5)

# PACIFIC
# Setting up the atlantic data
pacific.q5 <- FirstLastWind(maxWindDF(pacific.df))
pacific.diff.q5 <- DiffDF(pacific.q5)

# Joins the maximum speed of each storm (not just first and last recorded info),
# the differences between the longitude and latitude between the first and last
# recorded location, and the angle and direction between the two points
pacific.full.q5 <- left_join(pacific.slocmaxwind, pacific.diff.q5) %>%
  select(ID, Name, Date, Maximum.Wind, dLat, dLong)
pacific.full.q5$Angle <- ToAngle(pacific.full.q5)
pacific.full.q5$Direction <- ToDirection(pacific.full.q5)
