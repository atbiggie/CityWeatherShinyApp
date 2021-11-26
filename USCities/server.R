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
    
    

})
