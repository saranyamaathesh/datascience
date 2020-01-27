###multilinear for startups data
startup_50 <- read.csv(file.choose())
summary(startup_50)
View(startup_50)
var(startup_50$`R.D.Spend`)                                                        
sd(startup_50$`R.D.Spend`)
var(startup_50$Administration)
sd(startup_50$Administration)
var(startup_50$`Marketing.Spend`)
sd(startup_50$`Marketing.Spend`)
var(startup_50$Profit)
sd(startup_50$Profit)
unique(startup_50$State)
startup_50 <- cbind(startup_50,ifelse(startup_50$State=="New York",1,0), ifelse(startup_50$State=="California",1,0),  ifelse(startup_50$State=="Florida",1,0))
View(startup_50)
names(startup_50)[6]<-paste("New.york")
names(startup_50)[7]<-paste("California")
names(startup_50)[8]<-paste("Florida")
View(startup_50)
plot(startup_50[-4]) 
View(startup_50)
plot(startup_50[-4,-6,-7,-8])
library(corpcor)
cor2pcor(cor(startup_50[-4]))
colnames(startup_50)
Profit_Model <- lm(Profit~`R.D.Spend`+Administration+`Marketing.Spend`, data = startup_50)
summary(Profit_Model)
library(car)
influenceIndexPlot(Profit_Model)
influencePlot(Profit_Model,id.n=3)
Profit_Model_Inf <- lm(Profit~`R.D.Spend`+Administration+`Marketing.Spend`, data = startup_50[-c(50,49),])
summary(Profit_Model_Inf)
Profit_Model <- lm(Profit~`R.D.Spend`+Administration+`Marketing.Spend`, data = startup_50)
class(startup_50$`Marketing.Spend`)
vif(Profit_Model)
summary(Profit_Model)
avPlots(Profit_Model)
Profit_Model_Revised <- lm(Profit~`R.D.Spend`+Administration+`Marketing.Spend`+`New.york`+California+Florida, data = startup_50)
library(MASS)
stepAIC(Profit_Model_Revised)
Profit_Model_Final <- lm(Profit~`R.D.Spend`+`Marketing.Spend`, data = startup_50)
summary(Profit_Model_Final)
plot(Profit_Model_Final)
qqPlot(Profit_Model_Final, id.n=5)
#R square value is 0.9483 and all p is also small.






#multilinear regression for sales of the computer
library(data.table)
Computer_Data<-read.csv(file.choose())
View(Computer_Data)
colnames(Computer_Data)
str(Computer_Data)
Computer_Data$cd_dummy1 <- ifelse(Computer_Data$cd=="yes",1,0)
Computer_Data$multi_dummy1 <- ifelse(Computer_Data$multi=='yes',1,0)
Computer_Data$premium_dummy1 <- ifelse(Computer_Data$premium=='yes',1,0)
Computer_Data$cd_dummy2 <- ifelse(Computer_Data$cd=='no',1,0)
Computer_Data$multi_dummy2 <- ifelse(Computer_Data$multi=='no',1,0)
Computer_Data$premium_dummy2 <- ifelse(Computer_Data$premium=='no',1,0)
Computer_Data$X = NULL
Computer_Data$cd = NULL
Computer_Data$multi = NULL
Computer_Data$premium = NULL
View(Computer_Data)
str(Computer_Data)
summary(Computer_Data)
colnames(Computer_Data)
attach(Computer_Data)
Computer_model<-lm(price~speed+hd+ram+screen+ads+trend+cd_dummy1+multi_dummy1+premium_dummy1,data = Computer_Data)
summary(Computer_model)
library(car)
influenceIndexPlot(Computer_model, id.n=3)
avPlots(Computer_model)
library(MASS)
stepAIC(Computer_model)
Computer_model_final <- lm(price ~ speed+hd+ram+screen+ads+trend+cd_dummy1+multi_dummy1+premium_dummy1, data = Computer_Data)
summary(Computer_model_final)
plot(Computer_model_final)
qqPlot(Computer_model_final, id.n=5)
#R square value is 0.7756 and all p is also small.








#multilinear regression for Price of tayota data
ToyotaCorolla <-read.csv(file.choose())
Corolla <- ToyotaCorolla[, c('Price','Age_08_04','KM','HP','cc','Doors','Gears','Quarterly_Tax','Weight')]
Corolla_Model <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Corolla)
summary(Corolla_Model)
vif(Corolla_Model)
avPlots(Corolla_Model)
stepAIC(Corolla_Model)
Corolla_Model_final <- lm(Price ~ Age_08_04+KM+HP+log(cc)+Gears+Quarterly_Tax+Weight,data = Corolla)
summary(Corolla_Model_final)
#R square value is 0.8664 and all p is also small.