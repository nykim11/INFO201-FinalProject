# Atlantic data initialize
world.map <- map_data("world")
northern.world <- world.map %>% filter(lat >= 0)
colnames(northern.world)[1:2] = c("Longitude", "Latitude")

atlantic <- read.csv("data/atlantic.csv", stringsAsFactors = FALSE) %>% 
  filter(Minimum.Pressure != -999) %>% 
  filter(Low.Wind.NW != -999)

atlantic$number <- as.numeric(rownames(atlantic))
check <- atlantic %>% filter(grepl("W",Longitude))

atlantic$Latitude <- substr(atlantic$Latitude, 1, nchar(atlantic$Latitude) - 1)
atlantic$Longitude <- substr(atlantic$Longitude, 1, nchar(atlantic$Longitude) - 1)

atlantic$Longitude[check$number] <- - as.numeric(atlantic$Longitude[check$number])

atlantic$Latitude <- as.numeric(atlantic$Latitude)
atlantic$Longitude <- as.numeric(atlantic$Longitude)


# Pacific data initialize
world2.map <- map_data("world2")
northern.world2 <- world2.map %>% filter(lat >= 0)
colnames(northern.world2)[1:2] = c("Longitude", "Latitude")

pacific <- read.csv("data/pacific.csv", stringsAsFactors = FALSE) %>% 
  filter(Minimum.Pressure != -999) %>% 
  filter(Low.Wind.NW != -999)

pacific$number <- as.numeric(rownames(pacific))
check2 <- pacific %>% filter(grepl("W",Longitude))

pacific$Latitude <- substr(pacific$Latitude, 1, nchar(pacific$Latitude) - 1)
pacific$Longitude <- substr(pacific$Longitude, 1, nchar(pacific$Longitude) - 1)

pacific$Longitude[check2$number] <- 360 - as.numeric(pacific$Longitude[check2$number])

pacific$Latitude <- as.numeric(pacific$Latitude)
pacific$Longitude <- as.numeric(pacific$Longitude)


# Make a data frame for both pacific and atlantic
pacific$Longitude <- as.numeric(pacific$Longitude)
atlantic$Longitude <- 360 + as.numeric(atlantic$Longitude)
north.america <- northern.world %>% filter(Longitude < 20)

test <- world.map
test$long <- test$long + 100
test$long[test$long >= 180] <- test$long[test$long >= 180] - 360
test <- test %>% filter(long > -150 & long < 125) %>% filter(lat > 0)
test$long <- test$long +260
ggplot() +
  geom_polygon(data = test, aes(x = long, y = lat, group = group)) +
  coord_quickmap()
colnames(test)[1:2] = c("Longitude", "Latitude")

pacific$number <- NULL
pacific$area <- "pacific"
atlantic$number <- NULL
atlantic$area <- "atlantic"
hurricane <- full_join(pacific, atlantic)

# Hurricane characteristic
hurricane.characteristic <- hurricane %>% group_by(ID) %>% 
  summarize(Total.Max.Wind = max(Maximum.Wind), 
            Total.Min.Pressure = min(Minimum.Pressure),
            Start.Date = min(as.numeric(Date)), 
            End.Date = max(as.numeric(Date)))
hurricane <- hurricane %>% left_join(hurricane.characteristic, by = "ID")