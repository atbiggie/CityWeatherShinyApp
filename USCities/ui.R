### Weather API Shiny App
### Autumn Biggie
### Last updated on 12/05/2021

library(shiny)
library(tidyverse)
library(DT)

# Define UI for application
shinyUI(fluidPage(

    # Application title
    titlePanel("Weather Data From U.S. Cities"),
    
    tabsetPanel(
        #define about page
        tabPanel("About", fluid = TRUE,
                 mainPanel(
                     h3("Let's examine some weather data..."),
                     p("The purpose of this app is to employ various data exploration and modeling techniques to analyze a weather dataset and ultimately predict Latitude given a set of variable values."),
                     p("The data was queried from the ", a("OpenWeather API", href = "http://openweathermap.org/api"), " using an endpoint that allows users to call current weather data from several cities within a rectangle zone of geographic coordinates. Note that this endpoint will be deprecated beginning January 1st, 2022. The dataset used within this app contains weather data queried on November 23rd, 2021 from all cities within the continental United States, as well as a handful from Mexico and Canada."),
                     br(),
                     #add image
                     img(src = "apilogo.png", height = 150, width = 350),
                     br(),
                     h4("Variable Descriptions"),
                     p("All measurements are metric, and the variable descriptions are as follows:", tags$li("id: record identification number"), tags$li("dt: date/time stamp"), tags$li("name: city name"), tags$li("Lon: longitude ((degrees))"), tags$li("Lat: latitude ((degrees))"), tags$li("temp: temperature ((C))"), tags$li("feels_like: perceived temperature ((C))"), tags$li("temp_min: minimum temperature ((C))"), tags$li("temp_max: maximum temperature ((C))"), tags$li("pressure: pressure ((Pa))"), tags$li("humidity: humidity ((percent))"), tags$li("wind_speed: wind speed ((m/s))"), tags$li("wind_deg: wind direction ((degrees))"), tags$li("numclouds: number of clouds"), tags$li("tempdiff: temp_max - temp_min ((C))")), 
                     p("All variables that end with ", strong("_cat"), " are categorical versions of the variables above."),
                     br(),
                     h4("Tab Descriptions"),
                     p("Use the ", strong("Data Exploration"), " tab to create visual representations of the data, as well as contingency tables and numerical summaries. Choose the type of plot, variables to use in tables, and even filter the data if desired."),
                     p("Use the ", strong("Modeling"), " tab to build and test models to predict Latitude.", tags$ol("The ", strong("Modeling Info"), " page describes each type of model provided, including the benefits and drawbacks of each."), tags$ol("The ", strong("Model Fitting"), " page allows selection of percentage of data used in training and test sets, variables used in the models, and number of folds used in Cross-Validation. It outputs model summaries, relevant graphs, as well as fit statistics to report model performance on the training and test sets."), tags$ol("The ", strong("Prediction"), " page allows selection of a model and variable values in order to predict Latitude.")), 
                     p("The ", strong("Data"), " tab allows users to explore the raw dataset, with the functionality to select variables to view as well as filter rows. The - possibly subsetted/filtered - dataset may be downloaded from this tab as well.")
                 )
                 ),
        #define data exploration page
        tabPanel("Data Exploration", fluid = TRUE,
         
          sidebarLayout(
            sidebarPanel(
                
              checkboxInput("filtopt", "Filter observations by conditions chosen in 'Data' tab"),
              
              #plot options
              h3("Plot Options"),
              selectizeInput("plot_type", "Plot Type", selected = "histogram", choices = c("histogram", "scatter plot")),
                         
            conditionalPanel(condition = "input.plot_type == 'histogram'", selectizeInput("hist_var", "Select Variable to Plot:", selected = "latitude", choices = c("latitude", "longitude", "temperature", "pressure", "humidity", "wind speed", "wind direction"))),
                         
            conditionalPanel(condition = "input.plot_type == 'scatter plot'", selectizeInput("scat_var", "Select Overlay Variable:", selected = "none", choices = c("none", "temperature", "pressure", "humidity", "wind speed", "wind direction"))),
            
            #table options
            h3("Table Options"),
            
            selectizeInput("tab_type", "Table Type", selected = "numerical summary", choices = c("contingency", "numerical summary")),
            
            conditionalPanel(condition = "input.tab_type == 'contingency'", selectizeInput("cont_vars", "Select the desired table:", choices = c("latitude x longitude", "wind speed x wind direction", "wind speed x wind direction x temperature", "longitude x humidity", "latitude x humidity", "temperature x humidity"), selected = "latitude x longitude")),
            
            conditionalPanel(condition = "input.tab_type == 'numerical summary'", selectizeInput("num_group", "Group by:", choices = c("latitude", "longitude"), selected = "latitude")),
            
            conditionalPanel(condition = "input.tab_type == 'numerical summary'", selectizeInput("num_vars", "Summarize:", choices = c("temperature info", "humidity and pressure info", "wind info"), selected = "temperature info"))
            
                     ),
                     
                     
    # output plot and tables
            mainPanel(
              plotOutput("CityPlot"),
              tableOutput("Tables")
                     )
                 )),
    
    #define modeling tab
    navbarMenu("Modeling",
               #modeling info page
             tabPanel("Modeling Info", fluid = TRUE,
                      mainPanel(
        
                          h3("Modeling Information"),
                          br(),
                          h4(strong("1. Linear Regression")),
                          withMathJax(),
                          tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script>
                ")),
                          p("The Linear Regression technique attempts to model a response $y$ by the predictors $x_i$. A simple linear regression model is of the general form $$Y_i = \\beta_0 + \\beta_1x_i + E_i$$, but multiple linear regression models can include terms that square the predictors or capture interactions between different predictors. A linear regression model is fit by minimizing the sum of squared residuals, that is, choosing $\\beta_0$, $\\beta_1$,..., $\\beta_i$ that minimize the square of the observed - predicted values. It is called a 'linear' regression model not because the relationship between the response and predictors is linear, but because each $\\beta_i$ is of the first power."),
                          p(strong("Advantages:"), " Linear Regression models are easy to implement and simple to interpret."),
                          p(strong("Disadvantages:"), " It's more difficult to include interaction terms in the model - they aren't chosen in an automated fashion. Also, highly correlated predictors in the same LR model increase variability in the estimated model parameters."),
                          h4(strong("2. Regression Trees")),
                          p("The Regression Tree method splits up the predictor space into regions, producing different predictions for each region. Usually, the mean of the observations within a given region is the prediction. The splits are chosen by using Recursive Binary Splitting. That is, for every possible value of each predictor, the Residual Sum of Squares is minimized, meaning we are trying to find j and s such that $$\\sum_{i:x_i \\epsilon R_1 (j,s)}(y_i-\\bar{y}_{R_1})^2 + \\sum_{i:x_i \\epsilon R_2 (j,s)}(y_i-\\bar{y}_{R_2})^2$$ where $R_1(j,s) = \\{x|x_j < s\\}$ and $R_2(j,s) = \\{x|x_j \\ge s\\}$. After the first split is chosen, the same process is repeated to create many splits, which are then 'pruned' back. The number of nodes created by splits is generally chosen using Cross-Validation. Pruning prevents overfitting and increases bias while decreasing variance, ultimately leading to a better prediction."),
                          p(strong("Advantages:"), " Trees are simple to understand and interpret, predictors don't need to be scaled, no statistical assumptions are required, and variables are selected automatically."),
                          p(strong("Disadvantages:"), " Small changes in data can vastly change the tree, there is no optimal algorithm, and pruning is usually needed."),
                          h4(strong("3. Random Forests")),
                          p("The Random Forest method for fitting a model is similar to the Bagged Tree method. Like the Bagged Tree method, multiple bootstrap samples are taken from the original sample, and a tree is created from each sample. However, the Random Forest method creates the tree using $m$ randomly chosen predictors for each sample instead of all predictors every time. This increases independence between trees, leading to a reduction in variance when the results are averaged. This is especially important when there exists an especially strong predictor in the dataset."),
                          p(strong("Advantages:"),  " Multiple trees are created, increasing precision of estimates. Trees are less correlated, reducing variance. Provides ability to examine variable importance measures."),
                          p(strong("Disadvantages:"), " Random Forest models have very low interpretability. They are more valuable for prediction purposes. These models can also require a lot of computing power to create.")
                          )
                      ),
             #define model fitting page
             tabPanel("Model Fitting", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          h3("Model Fitting"),
                          sliderInput("train_p", "Proportion of data in training set:", min = .5, max = .9, value = .7), 
                          #select cv folds
                          selectInput("foldnum", "Number of Folds for Cross-Validation", choices = c(3,4,5,6,7,8,9,10), selected = 5),
                          #linear reg model
                          h4("Linear Regression Model"),
                          selectInput("linregvars", "Select variables to include in model:", choices = c("Lon", "temp", "feels_like", "temp_min", "temp_max", "pressure", "humidity", "wind_speed", "wind_deg", "numclouds"), multiple = TRUE),
                          #interactions
                          selectInput("linregint", "Select interactions to include in model:", choices = c("Lon:temp", "Lon:feels_like", "Lon:temp_min", "Lon:temp_max", "Lon:pressure", "Lon:humidity", "Lon:wind_speed", "Lon:wind_deg", "Lon:numclouds", "temp:feels_like", "temp:temp_min", "temp:temp_max", "temp:pressure", "temp:humidity", "temp:wind_speed", "temp:wind_deg", "temp:numclouds", "feels_like:temp_min", "feels_like:temp_max", "feels_like:pressure", "feels_like:humidity", "feels_like:wind_speed", "feels_like:wind_deg", "feels_like:numclouds", "temp_min:temp_max", "temp_min:pressure", "temp_min:humidity", "temp_min:wind_speed", "temp_min:wind_deg", "temp_min:numclouds", "temp_max:pressure", "temp_max:humidity", "temp_max:wind_speed", "temp_max:wind_deg", "temp_max:numclouds", "pressure:humidity", "pressure:wind_speed", "pressure:wind_deg", "pressure:numclouds", "humidity:wind_speed", "humidity:wind_deg", "humidity:numclouds", "wind_speed:wind_deg", "wind_speed:numclouds", "wind_deg:numclouds"), multiple = TRUE),
                          #regression tree model
                          h4("Regression Tree"),
                          selectInput("regtreevars", "Select variables to include in model:", choices = c("Lon", "temp", "feels_like", "temp_min", "temp_max", "pressure", "humidity", "wind_speed", "wind_deg", "numclouds"), multiple = TRUE),
                          
                          #random forest vars
                          h4("Random Forest"),
                          selectInput("randforvars", "Select variables to include in model:", choices = c("Lon", "temp", "feels_like", "temp_min", "temp_max", "pressure", "humidity", "wind_speed", "wind_deg", "numclouds"), multiple = TRUE),
                          
                          #fit all models at once
                          actionButton("fit", "Fit Models")
                        ),
                        #output
                      mainPanel(
                          h3("Linear Regression Model"),
                          verbatimTextOutput("lrMod"),
                          h4("Fit Statistics on Training Set"),
                          verbatimTextOutput("lrfit"),
                          h3("Regression Tree Model"),
                          plotOutput("rtPlot2"),
                          h4("Complexity Parameter Selection"),
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
             
             #define prediction page
             tabPanel("Prediction", fluid = TRUE,
                        wellPanel(
                            h3("Response Prediction"),
                            selectInput("selectmod", "Select Model:", choices = c("Linear Regression", "Regression Tree", "Random Forest"), selected = "Linear Regression")),
                            h4("Set Variable Values"),
                            h5("Note: only variables included in chosen model will be used to make prediction"),
                            column(width = 6,
                            sliderInput("Lon", "Lon", min = -124.9936, max = -68.1489, value = -124.9936),
                            sliderInput("temp", "temp", min = -7.24, max = 23.07, value = -7.24),
                            sliderInput("feels_like", "feels_like", min = -12.15, max = 22.19, value = -12.15),
                            sliderInput("temp_min", "temp_min", min = -8.27, max = 22.25, value = -8.27),
                            sliderInput("temp_max", "temp_max", min = -5.82, max = 24.14, value = -5.82)
                            ),
                            column(width = 6,
                            sliderInput("pressure", "pressure", min = 916, max = 1040, value = 916),
                            sliderInput("humidity", "humidity", min = 11, max = 100, value = 11),
                            sliderInput("wind_speed","wind_speed", min = 0, max = 12.86, value = 0),
                            sliderInput("wind_deg", "wind_deg", min = 0, max = 360, value = 0),
                            sliderInput("numclouds", "num_clouds", min = 0, max = 40, value = 0)
                            ),
                        mainPanel(
                            wellPanel(
                          verbatimTextOutput("Pred")
                            )
                      ))
             ),
    
    #define data page
    tabPanel("Data", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                  h3("Data Table Options"),
                  
                  #subset columns
                  checkboxGroupInput("datavars", "Select variables to subset:", choices = c("all", "id", "dt", "name", "Lon", "Lat", "temp", "feels_like", "temp_min", "temp_max", "pressure", "humidity", "wind_speed", "wind_deg", "numclouds", "tempdiff", "lon_cat", "lat_cat", "humidity_cat", "temp_cat", "pressure_cat", "wind_speed_cat", "wind_deg_cat"), selected = "all"),
                  
                  #download subsetted table
                  downloadButton("download_filtered", "Download Table")
               
                  ),
               
               mainPanel(
                   DTOutput("Data")
               )
             )
             )
    )


))
