#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Weather Data From U.S. Cities"),
    
    tabsetPanel(
        tabPanel("About", fluid = TRUE,
                 mainPanel("This is an about page")
                 ),
        
        tabPanel("Data Exploration", fluid = TRUE,
         # Sidebar with a slider input for number of bins
          sidebarLayout(
            sidebarPanel(
              checkboxInput("filtopt", "Filter observations by conditions chosen in 'Data' tab"),
              
              h3("Plot Options"),
              selectizeInput("plot_type", "Plot Type", selected = "histogram", choices = c("histogram", "scatter plot")),
                         
            conditionalPanel(condition = "input.plot_type == 'histogram'", selectizeInput("hist_var", "Select Variable to Plot:", selected = "latitude", choices = c("latitude", "longitude", "temperature", "pressure", "humidity", "wind speed", "wind direction"))),
                         
            conditionalPanel(condition = "input.plot_type == 'scatter plot'", selectizeInput("scat_var", "Select Overlay Variable:", selected = "none", choices = c("none", "temperature", "pressure", "humidity", "wind speed", "wind direction"))),
            
            h3("Table Options"),
            
            selectizeInput("tab_type", "Table Type", selected = "numerical summary", choices = c("contingency", "numerical summary")),
            
            conditionalPanel(condition = "input.tab_type == 'contingency'", selectizeInput("cont_vars", "Select the desired table:", choices = c("latitude x longitude", "wind speed x wind direction", "wind speed x wind direction x temperature", "longitude x humidity", "latitude x humidity", "temperature x humidity"), selected = "latitude x longitude")),
            
            conditionalPanel(condition = "input.tab_type == 'numerical summary'", selectizeInput("num_group", "Group by:", choices = c("latitude", "longitude"), selected = "latitude")),
            
            conditionalPanel(condition = "input.tab_type == 'numerical summary'", selectizeInput("num_vars", "Summarize:", choices = c("temperature info", "humidity and pressure info", "wind info"), selected = "temperature info"))
            
                     ),
                     
                     
    # Show a plot of the generated distribution
            mainPanel(
              plotOutput("CityPlot"),
              tableOutput("Tables")
                     )
                 )),
    
    navbarMenu("Modeling",
             tabPanel("Modeling Info", fluid = TRUE,
                      mainPanel(
                          br(),
                          h4("is this working?")
                      )),
             tabPanel("Model Fitting", fluid = TRUE,
                      mainPanel(
                          h5("This is fun!!!")
                      )),
             tabPanel("Prediction", fluid = TRUE,
                      mainPanel(
                          br(),
                          "Hola hola ebola~"
                      ))
             ),
    
    tabPanel("Data", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                  h3("Data Table Options"),
                  
                  checkboxGroupInput("datavars", "Select variables to subset:", choices = c("all", "id", "dt", "name", "Lon", "Lat", "temp", "feels_like", "temp_min", "temp_max", "pressure", "humidity", "wind_speed", "wind_deg", "numclouds", "tempdiff", "lon_cat", "lat_cat", "humidity_cat", "temp_cat", "pressure_cat", "wind_speed_cat", "wind_deg_cat"), selected = "all"),
                  
                  downloadButton("download_filtered", "Download Table")
               
                  ),
               
               mainPanel(
                   DTOutput("Data")
               )
             )
             )
    )


))
