#data exploration page

library(tidyverse)
cityweather <- read_csv("cityweather.csv", col_names = TRUE)

#create new variables

cityweather <- cityweather %>% 
  select(-visibility) %>% mutate(
  tempdiff = temp_max - temp_min, 
  lon_cat = ifelse(Lon >= -75, "[-75, -65)", ifelse(Lon >= -85, "[-85,-75)", ifelse(Lon >= -95, "[-95,-85)", ifelse(Lon >= -105, "[-105,-95)", ifelse(Lon >= -115, "[-115,-105)", "[-125,-115)"))))),
  lat_cat = ifelse(Lat >= 45, "[45,50)", ifelse(Lat >= 40, "[40,45)", ifelse(Lat >= 35, "[35,40)", ifelse(Lat >= 30, "[30,35)", "[25,30)")))),
  humidity_cat = ifelse(humidity >= 80, "[80,100)", ifelse(humidity >= 60, "[60,80)", ifelse(humidity >= 40, "[40,60)", "[0,40)"))), 
  temp_cat = ifelse(temp >= 16, "Very High", ifelse(temp >= 8, "High", ifelse(temp >= 0, "Low", "Very Low"))),
  pressure_cat = ifelse(pressure >= 1030, "Very High", ifelse(pressure >= 1020, "High", ifelse(pressure >= 1010, "Low", "Very Low"))),
  wind_speed_cat = ifelse(wind_speed >= 9, "Very High", ifelse(wind_speed >= 6, "High", ifelse(wind_speed >= 3, "Low", "Very Low"))),
  wind_deg_cat = ifelse(wind_deg >= 315, "North", ifelse(wind_deg >= 225, "West", ifelse(wind_deg >= 135, "South", ifelse(wind_deg >= 45, "East", "North")))))

cityweather$lon_cat <- ordered(cityweather$lon_cat, levels = c("[-125,-115)","[-115,-105)","[-105,-95)","[-95,-85)","[-85,-75)","[-75, -65)"))
cityweather$lat_cat <- ordered(cityweather$lat_cat, levels = c("[25,30)","[30,35)","[35,40)","[40,45)","[45,50)"))
cityweather$humidity_cat <- ordered(cityweather$humidity_cat, levels = c("[0,40)","[40,60)","[60,80)","[80,100)"))
cityweather$temp_cat <- ordered(cityweather$temp_cat, levels = c("Very Low","Low","High","Very High"))
cityweather$pressure_cat <- ordered(cityweather$pressure_cat, levels = c("Very Low","Low","High","Very High"))
cityweather$wind_speed_cat <- ordered(cityweather$wind_speed_cat, levels = c("Very Low","Low","High","Very High"))
cityweather$wind_deg_cat <- ordered(cityweather$wind_deg_cat, levels = c("North", "East", "South", "West"))

write_csv(cityweather, file = "USCities/cityweather2.csv")


#Categorical Summaries

#lat x long
as.data.frame.data.frame(table(cityweather$lon_cat, cityweather$lat_cat, deparse.level = 2))

#wind speed x wind direction
table(cityweather$wind_speed_cat, cityweather$wind_deg_cat, deparse.level = 2)

#wind speed x wind direction x temp
tab <- table(cityweather$wind_speed_cat, cityweather$wind_deg_cat, cityweather$temp_cat, deparse.level = 2)

as.data.frame(tab)
#lon x humidity
datatable(table(cityweather$lon_cat, cityweather$humidity_cat, deparse.level = 2))

#lat by humidity
table(cityweather$lat_cat, cityweather$humidity_cat, deparse.level = 2)

#temp by humidity
table(cityweather$temp_cat, cityweather$humidity_cat, deparse.level = 2)

#Numerical Summaries

cityweather %>% group_by(lat_cat) %>% summarise(avg_temp = mean(temp), sd_temp = sd(temp), avg_tempdiff = mean(tempdiff), sd_tempdiff = sd(tempdiff))

cityweather %>% group_by(lon_cat) %>% summarise(avg_temp = mean(temp), sd_temp = sd(temp), avg_tempdiff = mean(tempdiff), sd_tempdiff = sd(tempdiff))

cityweather %>% group_by(lat_cat) %>% summarise(avg_humidity = mean(humidity), sd_humidity = sd(humidity), avg_pressure = mean(pressure), sd_pressure = sd(pressure))

cityweather %>% group_by(lon_cat) %>% summarise(avg_humidity = mean(humidity), sd_humidity = sd(humidity), avg_pressure = mean(pressure), sd_pressure = sd(pressure))

cityweather %>% group_by(lat_cat) %>% summarise(avg_wind_speed = mean(wind_speed), sd(wind_speed), avg_wind_direction = mean(wind_deg), sd_wind_direction = sd(wind_deg))

cityweather %>% group_by(lon_cat) %>% summarise(avg_wind_speed = mean(wind_speed), sd(wind_speed), avg_wind_direction = mean(wind_deg), sd_wind_direction = sd(wind_deg))

#Visual Summaries

#histogram of Latitude
ggplot(cityweather, aes(Lat)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") +
  labs(title = "Histogram of Latitude", x = "Latitude")

#histogram of Longitude
ggplot(cityweather, aes(Lon)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
  labs(title = "Histogram of Longitude", x = "Longitude")

#histogram of Temp
ggplot(cityweather, aes(temp)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
  labs(title = "Histogram of Temperature", x = "Temperature (C)")

#histogram of pressure
ggplot(cityweather, aes(pressure)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
  labs(title = "Histogram of Pressure", x = "Pressure (kg/cm2)")

#histogram of humidity
ggplot(cityweather, aes(humidity)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
  labs(title = "Histogram of Humidity", x = "Humidity (%)")

#histogram of wind_speed
ggplot(cityweather, aes(wind_speed)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
  labs(title = "Histogram of Wind Speed", x = "Wind Speed (m/s)")

#histogram of wind_deg
ggplot(cityweather, aes(wind_deg)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
  labs(title = "Histogram of Wind Direction", x = "Wind Direction (degrees)")

#histogram of num_clouds
ggplot(cityweather, aes(numclouds)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
  labs(title = "Histogram of Number of Clouds", x = "Number of Clouds")


#scatter of Lat x Lon
ggplot(cityweather, aes(Lon,Lat)) + geom_point() + 
  labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude")

#scatter of Lat x Lon colored by temp_cat
ggplot(cityweather, aes(Lon,Lat)) + geom_point(aes(color = temp_cat)) + 
  labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
  scale_color_discrete(name = "Temperature")

#scatter plot of Lat x Lon colored by humidity_cat
ggplot(cityweather, aes(Lon,Lat)) + geom_point(aes(color = humidity_cat)) + 
  labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
  scale_color_discrete(name = "Humidity")

#scatter plot of Lat x Lon colored by pressure_cat
ggplot(cityweather, aes(Lon,Lat)) + geom_point(aes(color = pressure_cat)) + 
  labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
  scale_color_discrete(name = "Pressure")

#scatter plot of Lat x Lon colored by wind_speed_cat
ggplot(cityweather, aes(Lon,Lat)) + geom_point(aes(color = wind_speed_cat)) + 
  labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
  scale_color_discrete(name = "Wind Speed")

#scatter plot of Lat x Lon colored by wind_deg_cat
ggplot(cityweather, aes(Lon,Lat)) + geom_point(aes(color = wind_deg_cat)) + 
  labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
  scale_color_discrete(name = "Wind Direction")

#Temp vs Humidity by Latitude
ggplot(cityweather, aes(temp,humidity)) + geom_point(aes(color = lat_cat)) + scale_color_discrete(name = "Latitude") + labs(title = "Temperature vs. Humidity", x = "Temperature (C)", y = "Humidity (%)")

#Temp vs Humidity by Longitude
ggplot(cityweather, aes(temp,humidity)) + geom_point(aes(color = lon_cat)) + scale_color_discrete(name = "Longitude") + labs(title = "Temperature vs. Humidity", x = "Temperature (C)", y = "Humidity (%)")

#wind speed vs. wind direction
ggplot(cityweather, aes(wind_deg,wind_speed)) + geom_point(aes(color = wind_deg_cat)) + 
  labs(title = "Wind Speed vs. Wind Direction", x = "Wind Direction (degrees)", y = "Wind Speed (m/s)") + 
  scale_color_discrete("Wind Direction")
