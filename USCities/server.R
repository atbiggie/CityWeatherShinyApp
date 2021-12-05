### Weather API Shiny App
### Autumn Biggie
### last updated 12/05/2021

library(shiny)
library(tidyverse)
library(DT)
library(caret)
library(randomForest)

# Define server logic
shinyServer(function(session, input, output) {
    
    #import data
    getData <- reactive({
        newData <- read_csv("cityweather2.csv", col_names = TRUE)
    })
    
    #change filter option on data exploration page based on subsetting choices on data page
    observeEvent(input$datavars != "all", 
                 updateCheckboxInput(session, "filtopt", value = 0))
    
#define plots
    output$CityPlot <- renderPlot({
      
      if(input$filtopt){
        newData <- getData()[input[["Data_rows_all"]], ]
      }else{
        newData <- getData()
      }
    
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
    
    #define contingency tables and numerical summaries
    output$Tables <- renderTable({
        
        if(input$filtopt){
            newData <- getData()[input[["Data_rows_all"]], ]
        }else{
            newData <- getData()
        }
        
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
        } else if((input$tab_type == "contingency") & (input$cont_vars == "temperature x humidity")){
            table(newData$temp_cat, newData$humidity_cat, deparse.level = 2)
        } else if((input$tab_type == "numerical summary") & (input$num_group == "latitude") & (input$num_vars == "temperature info")){
            newData %>% group_by(lat_cat) %>% summarise(avg_temp = mean(temp), sd_temp = sd(temp), avg_tempdiff = mean(tempdiff), sd_tempdiff = sd(tempdiff))
        } else if((input$tab_type == "numerical summary") & (input$num_group == "latitude") & (input$num_vars == "humidity and pressure info")){
            newData %>% group_by(lat_cat) %>% summarise(avg_humidity = mean(humidity), sd_humidity = sd(humidity), avg_pressure = mean(pressure), sd_pressure = sd(pressure))
        } else if((input$tab_type == "numerical summary") & (input$num_group == "latitude") & (input$num_vars == "wind info")){
            newData %>% group_by(lat_cat) %>% summarise(avg_wind_speed = mean(wind_speed), sd_wind_speed = sd(wind_speed), avg_wind_direction = mean(wind_deg), sd_wind_direction = sd(wind_deg))
        } else if((input$tab_type == "numerical summary") & (input$num_group == "longitude") & (input$num_vars == "wind info")){
            newData %>% group_by(lon_cat) %>% summarise(avg_wind_speed = mean(wind_speed), sd_wind_speed = sd(wind_speed), avg_wind_direction = mean(wind_deg), sd_wind_direction = sd(wind_deg))
        } else if((input$tab_type == "numerical summary") & (input$num_group == "longitude") & (input$num_vars == "humidity and pressure info")){
            newData %>% group_by(lon_cat) %>% summarise(avg_humidity = mean(humidity), sd_humidity = sd(humidity), avg_pressure = mean(pressure), sd_pressure = sd(pressure))
        } else {
            newData %>% group_by(lon_cat) %>% summarise(avg_temp = mean(temp), sd_temp = sd(temp), avg_tempdiff = mean(tempdiff), sd_tempdiff = sd(tempdiff))
        }
        
    })
    
    #define raw data table
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
        

    }, filter = "top") #add obs filter option


    #define download button
    output$download_filtered <- 
    
        downloadHandler(
            filename = "Filtered Data.csv",
            content = function(file){
                write.csv(getData()[input[["Data_rows_all"]], input$datavars], file)
            }
        )
    
    #define linear regression model
    linreg <- eventReactive(input$fit, {
        newData1 <- getData() #%>% select(Lat, input$linregvars)
        set.seed(123)
        trainIndex1 <- createDataPartition(newData1$Lat, p = input$train_p, list = FALSE)
        train1 <- newData1[trainIndex1, ] #%>% 
            #mutate_if(is.numeric, scale)
        test1 <- newData1[-trainIndex1,]
        
        
        withProgress(message = "Training Linear Regression Model", {
        fit_lr <- lm(as.formula(paste("Lat ~ ", paste(input$linregvars, collapse = "+"), paste("+", input$linregint, collapse = "+"))), data = train1)
        })
    })
    
    #output linreg training info
    output$lrMod <- renderPrint({
        summary(linreg())
        })
    
    output$lrfit <- renderPrint({
        mse <- mean(residuals(linreg())^2)
        rmse <- sqrt(mse)
        adj_r2 <- summary(linreg())$adj.r.squared
        data.frame(RMSE = rmse, adj.R2 = adj_r2)
    })
    
    #define regression tree
    regtree <- eventReactive(input$fit, {
        newData2 <- getData() %>% select(Lat, input$regtreevars)
        
        set.seed(123)
        trainIndex2 <- createDataPartition(newData2$Lat, p = input$train_p, list = FALSE)
        train2 <- newData2[trainIndex2, ]
        test2 <- newData2[-trainIndex2,]
        
        withProgress(message = "Training Regression Tree Model", {
        regtree_fit <- train(Lat ~ ., 
                             data = train2,
                             method = "rpart",
                             trControl = trainControl(method = "cv", number = as.numeric(input$foldnum)),
                             tuneGrid = expand.grid(cp = seq(from = 0, to = 0.1, by = 0.001)))
        })
    })
    
    #output regtree info
    output$rtPlot <- renderPlot({
        plot(regtree())
    })
    
    output$rtMod <- renderPrint({
        regtree()$bestTune
    })
    
    output$rtfit <- renderPrint({
        regtree()$results[1, ]
    })
    
    output$rtPlot2 <- renderPlot({
        plot(regtree()$finalModel)
        text(regtree()$finalModel)
    })
    
    #output random forest model
    rf <- eventReactive(input$fit, {
        newData3 <- getData() %>% select(Lat, input$randforvars)
        
        set.seed(123)
        trainIndex3 <- createDataPartition(newData3$Lat, p = input$train_p, list = FALSE)
        train3 <- newData3[trainIndex3, ]
        test3 <- newData3[-trainIndex3,]
        
        withProgress(message = "Training Random Forest Model", {
        rf_fit <- train(Lat ~ ., 
                        data = train3,
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = as.numeric(input$foldnum)),
                        tuneGrid = expand.grid(mtry = c(1:10))
        )
                  })
    })
    
    #define best random forest model that will be used for plotting variable importance
    rfplot <- eventReactive(input$fit, {
        newData <- getData() %>% select(Lat, input$randforvars)
        
        set.seed(123)
        trainIndex <- createDataPartition(newData$Lat, p = input$train_p, list = FALSE)
        train <- newData[trainIndex, ]
        test <- newData[-trainIndex,]
        
        
        rf_fit <- randomForest(Lat ~., data = train, mtry = 8, importance = TRUE)
  
    })
    
    #output rf training info
    output$rfPlot <- renderPlot({
        varImpPlot(rfplot())
    })
    
    output$rfMod <- renderPrint({
        rf()$bestTune
    })
    
    output$rffit <- renderPrint({
        rf()$results
    })
    
    #predict on test set
    test_tab <- eventReactive(input$fit, {
        newData <- getData()
        
        set.seed(123)
        trainIndex <- createDataPartition(newData$Lat, p = input$train_p, list = FALSE)
        train <- newData[trainIndex, ]
        test <- newData[-trainIndex,]
        
        lr_pred <- predict(linreg(), newdata = test)
        Linear_Regression <- postResample(lr_pred, test$Lat)
        
        tree_pred <- predict(regtree(), newdata = test)
        Regression_Tree <- postResample(tree_pred, test$Lat)
        
        rf_pred <- predict(rf(), newdata = test)
        Random_Forest <- postResample(rf_pred, test$Lat)
        
        rbind(Linear_Regression,Regression_Tree,Random_Forest)
        
    })
    
    #report model fits on test set
    output$testrun <- renderDT({
        test_tab()
    })
    
    #define prediction tab server info
    output$Pred <- renderPrint({
        
        #user input for variable values
        newDat <- data.frame(Lon = input$Lon, temp = input$temp, feels_like = input$feels_like, temp_min = input$temp_min, temp_max = input$temp_max, pressure = input$pressure, humidity = input$humidity, wind_speed = input$wind_speed, wind_deg = input$wind_deg, numclouds = input$numclouds)
        
        #predictions for different models
        
        if (input$selectmod == "Linear Regression") {
            pred <- predict(linreg(), newdata = newDat)
            pred <- round(pred, digits = 3)
            paste("For these values, the predicted response for Latitude is", pred, sep = " ")
        } else if (input$selectmod == "Regression Tree"){
            pred <- predict(regtree()$finalModel, newdata = newDat)
            pred <- round(pred, digits = 3)
            paste("For these values, the predicted response for Latitude is", pred, sep = " ")
        } else {
            pred <- predict(rf()$finalModel, newdata = newDat)
            pred <- round(pred, digits = 3)
            paste("For these values, the predicted response for Latitude is", pred, sep = " ")
        }
        
        
        
    })
    
    

})
