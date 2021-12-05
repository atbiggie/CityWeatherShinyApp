# CityWeatherShinyApp  

The purpose of this app is to employ various data exploration and modeling techniques to analyze a weather dataset and ultimately predict Latitude given a set of variable values. The data was queried from the [OpenWeather API](http://openweathermap.org/api) using an endpoint that allows users to call current weather data from several cities within a rectangle zone of geographic coordinates. Note that this endpoint will be deprecated beginning January 1st, 2022. The dataset used within this app contains weather data queried on November 23rd, 2021 from all cities within the continental United States, as well as a handful from Mexico and Canada.

## Required Packages  

* **shiny** : creates the shiny app
* **tidyverse** : a collection of packages for many tasks including data manipulation and plotting
* **DT** : creates interactive data tables
* **caret** : model building and training
* **randomForest** : random forest model building  

## Install Packages Above  

`install.packages(c("shiny", "tidyverse", "DT", "caret", "randomForest"))`  

## Code to Run App  

`shiny::runGitHub(repo = "atbiggie/CityWeatherShinyApp", subdir = "USCities", ref = "main")`  
