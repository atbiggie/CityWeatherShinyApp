### Model Fitting File
library(caret)
library(randomForest)

cityweather <- read_csv("USCities/cityweather2.csv", col_names = TRUE)

#split data
set.seed(123)
trainIndex <- createDataPartition(cityweather$Lat, p = 0.7, list = FALSE)
train <- cityweather[trainIndex, ]
test <- cityweather[-trainIndex,]

#fit linear regression model
fit_lr <- lm(Lat ~ Lon + temp_min + temp_max + pressure + humidity + wind_speed + wind_deg + Lon:temp_min + Lon:temp_max + Lon:pressure + Lon:humidity + temp_min:temp_max + temp_min:pressure + temp_max:humidity + pressure:humidity + pressure:wind_speed + pressure:wind_deg + humidity:wind_speed + wind_speed:wind_deg, data = train)
summary(fit_lr)

#report fit stats for LR model on training set
mse1_train <- mean(residuals(fit_lr)^2)
lr_rmse_train <- sqrt(mse1)

#run linear regression model on test set
lr_test <- lm(Lat ~ Lon + temp_min + temp_max + pressure + humidity + wind_speed + wind_deg + Lon:temp_min + Lon:temp_max + Lon:pressure + Lon:humidity + temp_min:temp_max + temp_min:pressure + temp_max:humidity + pressure:humidity + pressure:wind_speed + pressure:wind_deg + humidity:wind_speed + wind_speed:wind_deg, data = test)
summary(lr_test)

#report fit stats for LR model on test set
lr_pred <- predict(lr_test, newdata = test)
Linear_Regression <- postResample(lr_pred, test$Lat)

#fit regression tree

regtree_fit <- train(Lat ~ Lon + temp + feels_like + temp_min + temp_max + humidity + pressure + wind_speed + wind_deg + numclouds, 
                       data = train,
                       method = "rpart", 
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv", number = 5),
                       tuneGrid = expand.grid(cp = seq(from = 0, to = 0.1, by = 0.001))
)

regtree_fit

plot(regtree_fit)

#get fit stats for regression tree on test set
tree_pred <- predict(regtree_fit, newdata = test)
Regression_Tree <- postResample(tree_pred, test$Lat)

#fit random forest model
rf_fit <- train(Lat ~ Lon + temp + feels_like + temp_min + temp_max + humidity + pressure + wind_speed + wind_deg + numclouds, data = train,
                method = "rf", 
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "cv", number = 10),
                tuneGrid = expand.grid(mtry = c(1:10))
)

rf_fit$results
rf_fit$bestTune

rf_fit2 <- randomForest(Lat ~ Lon + temp + feels_like + temp_min + temp_max + humidity + pressure + wind_speed + wind_deg + numclouds, data = train, mtry = 7, importance = TRUE)

summary(rf_fit2)
varImpPlot(rf_fit2)

#report fit stats for rf model on test set
rf_pred <- predict(rf_fit, newdata = test)
Random_Forest <- postResample(rf_pred, test$Lat)

#combine and report final fit statistics
report_stats <- rbind(Linear_Regression,Regression_Tree,Random_Forest)

#predict!

#linear reg
predict(fit_lr, newdata = data.frame(Lon = -110, temp_min= 0, temp_max = 11, pressure = 990, humidity = 61, wind_speed = 3, wind_deg = 210))

#reg tree
predict(regtree_fit, newdata = data.frame(Lon = -110, temp = 6, feels_like = 6, temp_min= 0, temp_max = 11, pressure = 990, humidity = 78, wind_speed = 3, wind_deg = 210, numclouds = 7))

#random forest
predict(rf_fit, newdata = data.frame(Lon = -110, temp = 22, feels_like = 21, temp_min= 15, temp_max = 26, pressure = 990, humidity = 61, wind_speed = 3, wind_deg = 210, numclouds = 7))

