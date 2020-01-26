#weight gained using calories consumed
Calories_consumed <- read.csv(file.choose())
View(Calories_consumed)
summary(Calories_consumed)
var(Calories_consumed$Calories.Consumed)
sd(Calories_consumed$Calories.Consumed)
var(Calories_consumed$Weight.gained..grams.)
sd(Calories_consumed$Weight.gained..grams.)
WeightGainModel <- lm(Weight.gained..grams. ~ Calories.Consumed, data = Calories_consumed)
summary(WeightGainModel)
plot(Calories_consumed)
#Hence the P-value<< 0.05 and R-Square value is 0.8968.so the  model will predict the output 89.68% time correct

#delivery time using sorting time
delivery_time <- read.csv(file.choose())
summary(delivery_time)
var(delivery_time$Delivery.Time)
sd(delivery_time$Delivery.Time)
var(delivery_time$Sorting.Time)
sd(delivery_time$Sorting.Time)
deliverTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = delivery_time)
summary(deliverTimeModel)
plot(deliverTimeModel)
###Hence the P<< 0.05 and R-Square value is 0.6823.so the model will predict the output 68.23% time correct
library(mvinfluence)
influenceIndexPlot(deliverTimeModel)
deliverTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = delivery_time[c(-5,-9,-21),])
summary(deliverTimeModel)
plot(deliverTimeModel)
###buy removing 3 points Multiple R-Square value is increased to 0.8332,so the model will predict the output 83.32% time correct


#emp data Churn out rate
Emp_data <- read.csv(file.choose())
summary(Emp_data)
var(Emp_data$Salary_hike)
sd(Emp_data$Salary_hike)
var(Emp_data$Churn_out_rate)
sd(Emp_data$Churn_out_rate)
Churn_out_rate_Model <- lm(Churn_out_rate ~ Salary_hike, data = Emp_data)
summary(Churn_out_rate_Model)
plot(Churn_out_rate_Model)
# P<<0.05 and R-Square value is 0.8312 That's mean this model will predict the output 83.12% time correct


# Salary hike
Salary_hike <- read.csv(file.choose())
summary(Salary_hike)
var(Salary_hike$YearsExperience)
sd(Salary_hike$YearsExperience)
var(Salary_hike$Salary)
sd(Salary_hike$Salary)
Salary_hike_Model <- lm(Salary ~ YearsExperience, data = Salary_hike)
summary(Salary_hike_Model)
plot(Salary_hike_Model)
#Hence the P<< 0.05 and R-Square value is 0.957 so the model will predict the output 95.7% time correct
