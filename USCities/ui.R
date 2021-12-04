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
                 mainPanel(
                     h3("Let's examine some weather data..."),
                     p("The purpose of this app is to employ various data exploration and modeling techniques to analyze a weather dataset and ultimately predict Latitude given a set of variable values."),
                     p("The data was queried from the ", a("OpenWeather API", href = "http://openweathermap.org/api"), " using an endpoint that allows users to call current weather data from several cities within a rectangle zone of geographic coordinates. (Note that this endpoint will be deprecated beginning January 1st, 2022). The dataset used within this app contains weather data queried on November 23rd, 2021 from all cities within the continental United States, as well as a handful from Mexico and Canada."),
                     br(),
                     img(src = "apilogo.png", height = 150, width = 350),
                     br(),
                     h4("Variable Descriptions"),
                     p("All measurements are metric, and the variable descriptions are as follows:", tags$li("id: record identification number"), tags$li("dt: date/time stamp"), tags$li("name: city name"), tags$li("Lon: longitude"), tags$li("Lat: latitude"), tags$li("temp: temperature (C)"), tags$li("feels_like: perceived temperature (C)"), tags$li("temp_min: minimum temperature (C)"), tags$li("temp_max: maximum temperature"), tags$li("pressure: pressure (Pa)"), tags$li("humidity: humidity (%)"), tags$li("wind_speed: wind speed (m/s)"), tags$li("wind_deg: wind direction (degrees)"), tags$li("numclouds: number of clouds"), tags$li("tempdiff: temp_max - temp_min (C)")), 
                     p("All variables that end with ", strong("_cat"), " are categorical versions of the variables above."),
                     br(),
                     h4("Tab Descriptions"),
                     p("Use the ", strong("Data Exploration"), " tab to create visual representations of the data, as well as contingency tables and numerical summaries. Choose the type of plot, variables to use in tables, and even filter the data if desired."),
                     p("Use the ", strong("Modeling"), " tab to build and test models to predict Latitude.", tags$ol("The ", strong("Modeling Info"), " page describes each type of model provided, including the benefits and drawbacks of each."), tags$ol("The ", strong("Model Fitting"), " page allows selection of percentage of data used in training and test sets, variables used in the models, and number of folds used in Cross-Validation. It outputs model summaries, relevant graphs, as well as fit statistics to report model performance on the training and test sets."), tags$ol("The ", strong("Prediction"), " page allows selection of a model and variable values in order to predict Latitude.")), 
                     p("The ", strong("Data"), " tab allows users to explore the raw dataset, with the functionality to select variables to view as well as filter rows. The (possibly subsetted/filtered) dataset may be downloaded from this tab as well.")
                 )
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
                      sidebarLayout(
                        sidebarPanel(
                          h3("Model Fitting"),
                          sliderInput("train_p", "Proportion of data in training set:", min = .5, max = .9, value = .7),
                          selectInput("foldnum", "Number of Folds for Cross-Validation", choices = c(3,4,5,6,7,8,9,10), selected = 5),
                          
                          h4("Linear Regression Model"),
                          selectInput("linregvars", "Select variables to include in model:", choices = c("Lon", "temp", "feels_like", "temp_min", "temp_max", "pressure", "humidity", "wind_speed", "wind_deg", "numclouds"), multiple = TRUE),
                          
                          h4("Regression Tree"),
                          selectInput("regtreevars", "Select variables to include in model:", choices = c("Lon", "temp", "feels_like", "temp_min", "temp_max", "pressure", "humidity", "wind_speed", "wind_deg", "numclouds"), multiple = TRUE),
                          
                          h4("Random Forest"),
                          selectInput("randforvars", "Select variables to include in model:", choices = c("Lon", "temp", "feels_like", "temp_min", "temp_max", "pressure", "humidity", "wind_speed", "wind_deg", "numclouds"), multiple = TRUE),
                          
                          actionButton("fit", "Fit Models")
                        ),
                      mainPanel(
                          h3("Linear Regression Model"),
                          verbatimTextOutput("lrMod"),
                          h4("Fit Statistics on Training Set"),
                          verbatimTextOutput("lrfit"),
                          h3("Regression Tree Model"),
                          plotOutput("rtPlot"),
                          h4("Best Value for Tuning Parameter"),
                          verbatimTextOutput("rtMod"),
                          h4("Fit Statistics on Training Set"),
                          verbatimTextOutput("rtfit"),
                          h3("Random Forest Model"),
                          plotOutput("rfPlot"),
                          h4("Best Value for Tuning Parameter"),
                          verbatimTextOutput("rfMod"),
                          h4("Fit Statistics on Training Set"),
                          verbatimTextOutput("rffit"),
                          br(),
                          h3("Fit Statistics on Test Set"),
                          DTOutput("testrun")
                      ))),
             tabPanel("Prediction", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                            h3("Response Prediction"),
                            selectInput("selectmod", "Select Model:", choices = c("Linear Regression", "Regression Tree", "Random Forest"), selected = "Linear Regression"),
                            h4("Set Variable Values"),
                            h5("Note: only variables included in chosen model will be used to make prediction"),
                            sliderInput("Lon", "Lon", min = -124.9936, max = -68.1489, value = -124.9936),
                            sliderInput("temp", "temp", min = -7.24, max = 23.07, value = -7.24),
                            sliderInput("feels_like", "feels_like", min = -12.15, max = 22.19, value = -12.15),
                            sliderInput("temp_min", "temp_min", min = -8.27, max = 22.25, value = -8.27),
                            sliderInput("temp_max", "temp_max", min = -5.82, max = 24.14, value = -5.82),
                            sliderInput("pressure", "pressure", min = 916, max = 1040, value = 916),
                            sliderInput("humidity", "humidity", min = 11, max = 100, value = 11),
                            sliderInput("wind_speed","wind_speed", min = 0, max = 12.86, value = 0),
                            sliderInput("wind_deg", "wind_deg", min = 0, max = 360, value = 0),
                            sliderInput("numclouds", "num_clouds", min = 0, max = 40, value = 0)
                        ),
                        mainPanel(
                          verbatimTextOutput("Pred")
                      )))
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
