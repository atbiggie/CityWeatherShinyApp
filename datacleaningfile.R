
library(jsonlite)
library(httr)
library(tidyverse)

weather.api <- function(lon_left, lat_bottom, lon_right, lat_top, zoom, api.id, units = "metric") {
  
  #assign the individual pieces of the required url to their respective objects  
  base <- "https://api.openweathermap.org/data/2.5/box/city?bbox="
  latlonzoom_piece <- paste(lon_left,lat_bottom,lon_right,lat_top,zoom, sep = ",")
  
  id_piece <- paste("&appid=", api.id, sep = "")
  
  unit_piece <- paste("&units=", units)
  
  #paste pieces together  
  my.url <- paste(base,latlonzoom_piece,id_piece,unit_piece, sep = "")
  
  #access the Weather API using the URL assembled above
  weather.info <- GET(my.url)
  
  #convert the content of the accessed data from raw to character, then present in readable data frame format
  final <- weather.info$content %>% rawToChar() %>% fromJSON()
  
  return(final$list)
}
  


#Query West -> East between lat 35,40 across entire US

zero_call <- weather.api(-125,35,-120,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
zero_call <- zero_call %>% data.frame(zero_call$id, zero_call$dt, zero_call$name, zero_call$visibility, zero_call$rain, zero_call$snow, zero_call$coord, zero_call$main, zero_call$wind, zero_call$clouds) %>% 
select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

first_call <- weather.api(-120,35,-115,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
first_call <- first_call %>% data.frame(first_call$id, first_call$dt, first_call$name, first_call$visibility, first_call$rain, first_call$snow, first_call$coord, first_call$main, first_call$wind, first_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

second_call <- weather.api(-115,35,-110,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
second_call <- second_call %>% data.frame(second_call$id, second_call$dt, second_call$name, second_call$visibility, second_call$rain, second_call$snow, second_call$coord, second_call$main, second_call$wind, second_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

third_call <- weather.api(-110,35,-105,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
third_call <- third_call %>% data.frame(third_call$id, third_call$dt, third_call$name, third_call$visibility, third_call$rain, third_call$snow, third_call$coord, third_call$main, third_call$wind, third_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

fourth_call <- weather.api(-105,35,-100,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
fourth_call <- fourth_call %>% data.frame(fourth_call$id, fourth_call$dt, fourth_call$name, fourth_call$visibility, fourth_call$rain, fourth_call$snow, fourth_call$coord, fourth_call$main, fourth_call$wind, fourth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

fifth_call <- weather.api(-100,35,-95,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
fifth_call <- fifth_call %>% data.frame(fifth_call$id, fifth_call$dt, fifth_call$name, fifth_call$visibility, fifth_call$rain, fifth_call$snow, fifth_call$coord, fifth_call$main, fifth_call$wind, fifth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

sixth_call <- weather.api(-95,35,-90,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
sixth_call <- sixth_call %>% data.frame(sixth_call$id, sixth_call$dt, sixth_call$name, sixth_call$visibility, sixth_call$rain, sixth_call$snow, sixth_call$coord, sixth_call$main, sixth_call$wind, sixth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

seventh_call <- weather.api(-90,35,-85,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
seventh_call <- seventh_call %>% data.frame(seventh_call$id, seventh_call$dt, seventh_call$name, seventh_call$visibility, seventh_call$rain, seventh_call$snow, seventh_call$coord, seventh_call$main, seventh_call$wind, seventh_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

eighth_call <- weather.api(-85,35,-80,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
eighth_call <- eighth_call %>% data.frame(eighth_call$id, eighth_call$dt, eighth_call$name, eighth_call$visibility, eighth_call$rain, eighth_call$snow, eighth_call$coord, eighth_call$main, eighth_call$wind, eighth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

ninth_call <- weather.api(-80,35,-75,40,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
ninth_call <- ninth_call %>% data.frame(ninth_call$id, ninth_call$dt, ninth_call$name, ninth_call$visibility, ninth_call$rain, ninth_call$snow, ninth_call$coord, ninth_call$main, ninth_call$wind, ninth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)



#Query West -> East between lat 40,45 across entire US

tenth_call <- weather.api(-125,40,-120,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
tenth_call <- tenth_call %>% data.frame(tenth_call$id, tenth_call$dt, tenth_call$name, tenth_call$visibility, tenth_call$rain, tenth_call$snow, tenth_call$coord, tenth_call$main, tenth_call$wind, tenth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

eleventh_call <- weather.api(-120,40,-115,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
eleventh_call <- eleventh_call %>% data.frame(eleventh_call$id, eleventh_call$dt, eleventh_call$name, eleventh_call$visibility, eleventh_call$rain, eleventh_call$snow, eleventh_call$coord, eleventh_call$main, eleventh_call$wind, eleventh_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

twelfth_call <- weather.api(-115,40,-110,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
twelfth_call <- twelfth_call %>% data.frame(twelfth_call$id, twelfth_call$dt, twelfth_call$name, twelfth_call$visibility, twelfth_call$rain, twelfth_call$snow, twelfth_call$coord, twelfth_call$main, twelfth_call$wind, twelfth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

thirteenth_call <- weather.api(-110,40,-105,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
thirteenth_call <- thirteenth_call %>% data.frame(thirteenth_call$id, thirteenth_call$dt, thirteenth_call$name, thirteenth_call$visibility, thirteenth_call$rain, thirteenth_call$snow, thirteenth_call$coord, thirteenth_call$main, thirteenth_call$wind, thirteenth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

fourteenth_call <- weather.api(-105,40,-100,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
fourteenth_call <- fourteenth_call %>% data.frame(fourteenth_call$id, fourteenth_call$dt, fourteenth_call$name, fourteenth_call$visibility, fourteenth_call$rain, fourteenth_call$snow, fourteenth_call$coord, fourteenth_call$main, fourteenth_call$wind, fourteenth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

fifteenth_call <- weather.api(-100,40,-95,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
fifteenth_call <- fifteenth_call %>% data.frame(fifteenth_call$id, fifteenth_call$dt, fifteenth_call$name, fifteenth_call$visibility, fifteenth_call$rain, fifteenth_call$snow, fifteenth_call$coord, fifteenth_call$main, fifteenth_call$wind, fifteenth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

sixteenth_call <- weather.api(-95,40,-90,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
sixteenth_call <- sixteenth_call %>% data.frame(sixteenth_call$id, sixteenth_call$dt, sixteenth_call$name, sixteenth_call$visibility, sixteenth_call$rain, sixteenth_call$snow, sixteenth_call$coord, sixteenth_call$main, sixteenth_call$wind, sixteenth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

seventeenth_call <- weather.api(-90,40,-85,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
seventeenth_call <- seventeenth_call %>% data.frame(seventeenth_call$id, seventeenth_call$dt, seventeenth_call$name, seventeenth_call$visibility, seventeenth_call$rain, seventeenth_call$snow, seventeenth_call$coord, seventeenth_call$main, seventeenth_call$wind, seventeenth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

eighteenth_call <- weather.api(-85,40,-80,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
eighteenth_call <- eighteenth_call %>% data.frame(eighteenth_call$id, eighteenth_call$dt, eighteenth_call$name, eighteenth_call$visibility, eighteenth_call$rain, eighteenth_call$snow, eighteenth_call$coord, eighteenth_call$main, eighteenth_call$wind, eighteenth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)

nineteenth_call <- weather.api(-80,40,-75,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
nineteenth_call <- nineteenth_call %>% data.frame(nineteenth_call$id, nineteenth_call$dt, nineteenth_call$name, nineteenth_call$visibility, nineteenth_call$rain, nineteenth_call$coord, nineteenth_call$main, nineteenth_call$wind, nineteenth_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today) %>% mutate(snow = NA)

twentieth_call <- weather.api(-75,40,-70,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
twentieth_call <- twentieth_call %>% data.frame(twentieth_call$id, twentieth_call$dt, twentieth_call$name, twentieth_call$visibility, twentieth_call$rain, twentieth_call$coord, twentieth_call$main, twentieth_call$wind, twentieth_call$clouds) %>% select(id, dt, name, visibility, rain, Lon:humidity, speed:today) %>% mutate(snow = NA)

two1_call <- weather.api(-70,40,-65,45,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
two1_call <- two1_call %>% data.frame(two1_call$id, two1_call$dt, two1_call$name, two1_call$visibility, two1_call$rain, two1_call$snow, two1_call$coord, two1_call$main, two1_call$wind, two1_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)


#Query West -> East between lat 30,35 across entire US

two2_call <- weather.api(-120,30,-115,35,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
two2_call <- two2_call %>% data.frame(two2_call$id, two2_call$dt, two2_call$name, two2_call$visibility, two2_call$rain, two2_call$snow, two2_call$coord, two2_call$main, two2_call$wind, two2_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

two3_call <- weather.api(-115,30,-110,35,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
two3_call <- two3_call %>% data.frame(two3_call$id, two3_call$dt, two3_call$name, two3_call$visibility, two3_call$rain, two3_call$snow, two3_call$coord, two3_call$main, two3_call$wind, two3_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

two4_call <- weather.api(-110,30,-105,35,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
two4_call <- two4_call %>% data.frame(two4_call$id, two4_call$dt, two4_call$name, two4_call$visibility, two4_call$rain, two4_call$snow, two4_call$coord, two4_call$main, two4_call$wind, two4_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

two5_call <- weather.api(-105,30,-100,35,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
two5_call <- two5_call %>% data.frame(two5_call$id, two5_call$dt, two5_call$name, two5_call$visibility, two5_call$rain, two5_call$snow, two5_call$coord, two5_call$main, two5_call$wind, two5_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)

two6_call <- weather.api(-100,30,-95,35,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
two6_call <- two6_call %>% data.frame(two6_call$id, two6_call$dt, two6_call$name, two6_call$visibility, two6_call$rain, two6_call$snow, two6_call$coord, two6_call$main, two6_call$wind, two6_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

two7_call <- weather.api(-95,30,-90,35,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
two7_call <- two7_call %>% data.frame(two7_call$id, two7_call$dt, two7_call$name, two7_call$visibility, two7_call$rain, two7_call$snow, two7_call$coord, two7_call$main, two7_call$wind, two7_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

two8_call <- weather.api(-90,30,-85,35,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
two8_call <- two8_call %>% data.frame(two8_call$id, two8_call$dt, two8_call$name, two8_call$visibility, two8_call$rain, two8_call$snow, two8_call$coord, two8_call$main, two8_call$wind, two8_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

two9_call <- weather.api(-85,30,-80,35,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
two9_call <- two9_call %>% data.frame(two9_call$id, two9_call$dt, two9_call$name, two9_call$visibility, two9_call$rain, two9_call$snow, two9_call$coord, two9_call$main, two9_call$wind, two9_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)



#Query West -> East between lat 25,30 across entire US

three0_call <- weather.api(-105,25,-100,30,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three0_call <- three0_call %>% data.frame(three0_call$id, three0_call$dt, three0_call$name, three0_call$visibility, three0_call$rain, three0_call$snow, three0_call$coord, three0_call$main, three0_call$wind, three0_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)

three1_call <- weather.api(-100,25,-95,30,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three1_call <- three1_call %>% data.frame(three1_call$id, three1_call$dt, three1_call$name, three1_call$visibility, three1_call$rain, three1_call$snow, three1_call$coord, three1_call$main, three1_call$wind, three1_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

three2_call <- weather.api(-95,25,-90,30,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three2_call <- three2_call %>% data.frame(three2_call$id, three2_call$dt, three2_call$name, three2_call$visibility, three2_call$rain, three2_call$snow, three2_call$coord, three2_call$main, three2_call$wind, three2_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)

three3_call <- weather.api(-83,25,-78,30,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three3_call <- three3_call %>% data.frame(three3_call$id, three3_call$dt, three3_call$name, three3_call$visibility, three3_call$rain, three3_call$snow, three3_call$coord, three3_call$main, three3_call$wind, three3_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)


#Query West -> East between lat 45,50 across entire US

three4_call <- weather.api(-125,45,-120,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three4_call <- three4_call %>% data.frame(three4_call$id, three4_call$dt, three4_call$name, three4_call$visibility, three4_call$rain, three4_call$snow, three4_call$coord, three4_call$main, three4_call$wind, three4_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

three5_call <- weather.api(-120,45,-115,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three5_call <- three5_call %>% data.frame(three5_call$id, three5_call$dt, three5_call$name, three5_call$visibility, three5_call$rain, three5_call$snow, three5_call$coord, three5_call$main, three5_call$wind, three5_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

three6_call <- weather.api(-115,45,-110,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three6_call <- three6_call %>% data.frame(three6_call$id, three6_call$dt, three6_call$name, three6_call$visibility, three6_call$rain, three6_call$snow, three6_call$coord, three6_call$main, three6_call$wind, three6_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)

three7_call <- weather.api(-110,45,-105,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three7_call <- three7_call %>% data.frame(three7_call$id, three7_call$dt, three7_call$name, three7_call$visibility, three7_call$rain, three7_call$snow, three7_call$coord, three7_call$main, three7_call$wind, three7_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)

three8_call <- weather.api(-105,45,-100,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three8_call <- three8_call %>% data.frame(three8_call$id, three8_call$dt, three8_call$name, three8_call$visibility, three8_call$rain, three8_call$snow, three8_call$coord, three8_call$main, three8_call$wind, three8_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)

three9_call <- weather.api(-100,45,-95,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
three9_call <- three9_call %>% data.frame(three9_call$id, three9_call$dt, three9_call$name, three9_call$visibility, three9_call$rain, three9_call$snow, three9_call$coord, three9_call$main, three9_call$wind, three9_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

four0_call <- weather.api(-95,45,-90,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
four0_call <- four0_call %>% data.frame(four0_call$id, four0_call$dt, four0_call$name, four0_call$visibility, four0_call$rain, four0_call$snow, four0_call$coord, four0_call$main, four0_call$wind, four0_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

four1_call <- weather.api(-90,45,-85,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
four1_call <- four1_call %>% data.frame(four1_call$id, four1_call$dt, four1_call$name, four1_call$visibility, four1_call$rain, four1_call$snow, four1_call$coord, four1_call$main, four1_call$wind, four1_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:humidity, speed:today)

four2_call <- weather.api(-85,45,-80,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
four2_call <- four2_call %>% data.frame(four2_call$id, four2_call$dt, four2_call$name, four2_call$visibility, four2_call$rain, four2_call$snow, four2_call$coord, four2_call$main, four2_call$wind, four2_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)

four3_call <- weather.api(-72,45,-67,50,16,api.id = "27667529a1629f208f81fded8f7552af") %>% select(-weather)
four3_call <- four3_call %>% data.frame(four3_call$id, four3_call$dt, four3_call$name, four3_call$visibility, four3_call$rain, four3_call$snow, four3_call$coord, four3_call$main, four3_call$wind, four3_call$clouds) %>% select(id, dt, name, visibility, rain, snow, Lon:pressure, humidity:today)


#combine into large dataset

cityweather <- rbind(zero_call, first_call, second_call, third_call, fourth_call, fifth_call, sixth_call, seventh_call, eighth_call, ninth_call, tenth_call, eleventh_call, twelfth_call, thirteenth_call, fourteenth_call, fifteenth_call, sixteenth_call, seventeenth_call, eighteenth_call, nineteenth_call, twentieth_call, two1_call, two2_call, two3_call, two4_call, two5_call, two6_call, two7_call, two8_call, two9_call, three0_call, three1_call, three2_call, three3_call, three4_call, three5_call, three6_call, three7_call, three8_call, three9_call, four0_call, four1_call, four2_call, four3_call) %>% rename(wind_speed = speed, wind_deg = deg, numclouds = today) %>% select(-rain,-snow)

write_csv(cityweather, file = "cityweather.csv")


