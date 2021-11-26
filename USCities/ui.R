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
              h3("Plot Options"),
              selectizeInput("plot_type", "Plot Type", selected = "histogram", choices = c("histogram", "scatter plot")),
            br(),
                         
            conditionalPanel(condition = "input.plot_type == 'histogram'", selectizeInput("hist_var", "Select Variable to Plot:", selected = "latitude", choices = c("latitude", "longitude", "temperature", "pressure", "humidity", "wind speed", "wind direction"))),
                         
            conditionalPanel(condition = "input.plot_type == 'scatter plot'", selectizeInput("scat_var", "Select Overlay Variable:", selected = "none", choices = c("none", "temperature", "pressure", "humidity", "wind speed", "wind direction"))),
            
            h3("Table Options"),
            
            
                     ),
                     
                     
    # Show a plot of the generated distribution
            mainPanel(
              plotOutput("CityPlot")
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
             mainPanel(
                 br(),
                 p("This is the data page!")
             ))
    )


))
