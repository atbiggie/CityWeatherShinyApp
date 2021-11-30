#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
    
    getData <- reactive({
        newData <- read_csv("cityweather2.csv", col_names = TRUE)
    })
    
    output$CityPlot <- renderPlot({
        
        newData <- getData()
        
        ifelse((input$plot_type == "histogram") & (input$hist_var == "latitude"), 
               print(ggplot(newData, aes(Lat)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") +
                         labs(title = "Histogram of Latitude", x = "Latitude")),
               
        ifelse((input$plot_type == "histogram") & (input$hist_var == "longitude"), 
               print(ggplot(newData, aes(Lon)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
                         labs(title = "Histogram of Longitude", x = "Longitude")), 
               
        ifelse((input$plot_type == "histogram") & (input$hist_var == "temperature"), 
               print(ggplot(newData, aes(temp)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
                         labs(title = "Histogram of Temperature", x = "Temperature (C)")),
              
        ifelse((input$plot_type == "histogram") & (input$hist_var == "pressure"),
               print(ggplot(newData, aes(pressure)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
                         labs(title = "Histogram of Pressure", x = "Pressure (kg/cm2)")),
        
        ifelse((input$plot_type == "histogram") & (input$hist_var == "humidity"),
               print(ggplot(newData, aes(humidity)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
                         labs(title = "Histogram of Humidity", x = "Humidity (%)")),
        
        ifelse((input$plot_type == "histogram") & (input$hist_var == "wind speed"), 
               print(ggplot(newData, aes(wind_speed)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
                         labs(title = "Histogram of Wind Speed", x = "Wind Speed (m/s)")),
              
        ifelse((input$plot_type == "histogram") & (input$hist_var == "wind direction"), 
               print(ggplot(newData, aes(wind_deg)) + geom_histogram(aes(y = ..density..), fill = "#225ea8") + geom_density(alpha = 0.3, fill = "#fc8d59", color = "#fc8d59") + 
                         labs(title = "Histogram of Wind Direction", x = "Wind Direction (degrees)")),
               
        ifelse((input$plot_type == "scatter plot") & (input$scat_var == "none"),
               print(ggplot(newData, aes(Lon,Lat)) + geom_point() + 
                         labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude")),
        
        ifelse((input$plot_type == "scatter plot") & (input$scat_var == "temperature"),
               print(ggplot(newData, aes(Lon,Lat)) + geom_point(aes(color = temp_cat)) + 
                         labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
                         scale_color_discrete(name = "Temperature")),
               
        ifelse((input$plot_type == "scatter plot") & (input$scat_var == "pressure"), 
               print(ggplot(newData, aes(Lon,Lat)) + geom_point(aes(color = pressure_cat)) + 
                         labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
                         scale_color_discrete(name = "Pressure")),
               
        ifelse((input$plot_type == "scatter plot") & (input$scat_var == "humidity"),
               print(ggplot(newData, aes(Lon,Lat)) + geom_point(aes(color = humidity_cat)) + 
                         labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
                         scale_color_discrete(name = "Humidity")),
        
        ifelse((input$plot_type == "scatter plot") & (input$scat_var == "wind speed"),
               print(ggplot(newData, aes(Lon,Lat)) + geom_point(aes(color = wind_speed_cat)) + 
                         labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
                         scale_color_discrete(name = "Wind Speed")),
               
        print(ggplot(newData, aes(Lon,Lat)) + geom_point(aes(color = wind_deg_cat)) + 
                  labs(title = "Scatter Plot of Longitude vs. Latitude", x = "Longitude", y = "Latitude") +
                  scale_color_discrete(name = "Wind Direction")))))))
               
    )))))))
        

    })
    
    
    output$Tables <- renderTable({
        
        newData <- getData()
        
        if((input$tab_type == "contingency") & (input$cont_vars == "latitude x longitude")){
            table(newData$lon_cat, newData$lat_cat, deparse.level = 2)
        } else if ((input$tab_type == "contingency") & (input$cont_vars == "wind speed x wind direction")){
            table(newData$wind_speed_cat, newData$wind_deg_cat, deparse.level = 2)
        } else if((input$tab_type == "contingency") & (input$cont_vars == "wind speed x wind direction x temperature")){
            table(newData$wind_speed_cat, newData$wind_deg_cat, newData$temp_cat, deparse.level = 2)
        } else if((input$tab_type == "contingency") & (input$cont_vars == "longitude x humidity")){
            table(newData$lon_cat, newData$humidity_cat, deparse.level = 2)
        } else if((input$tab_type == "contingency") & (input$cont_vars == "latitude x humidity")){
            table(newData$lat_cat, newData$humidity_cat, deparse.level = 2)
        } else {
            table(newData$temp_cat, newData$humidity_cat, deparse.level = 2)
        }
        
    })
    
    
    output$Data <- DT::renderDT({
        
        dat <- getData()
        
        dat$lon_cat <- as.factor(dat$lon_cat)
        dat$lat_cat <- as.factor(dat$lat_cat)
        dat$humidity_cat <- as.factor(dat$humidity_cat)
        dat$temp_cat <- as.factor(dat$temp_cat)
        dat$pressure_cat <- as.factor(dat$pressure_cat)
        dat$wind_speed_cat <- as.factor(dat$wind_speed_cat)
        dat$wind_deg_cat <- as.factor(dat$wind_deg_cat)
        
        if (input$datavars == "all") {
            dat
        }else{
            dat %>% select(input$datavars)
        }
        

    }, filter = "top")


    
    output$download_filtered <- 
    
        downloadHandler(
            filename = "Filtered Data.csv",
            content = function(file){
                write.csv(getData()[input[["Data_rows_all"]], input$datavars], file)
            }
        )

})
