#############labratory data ##########
lab<-read.csv(file.choose())   
View(lab)
Stacked_Data <- stack(lab)
View(Stacked_Data)
attach(Stacked_Data)

#############Normality test###############
library(nortest)
ad.test(Stacked_Data$values) 
#### p-value = 0.05072 >0.05 so p high null fly => It follows normal distribution
############# Variance test ###############
library(car)
leveneTest(Stacked_Data$values~Stacked_Data$ind, data = Stacked_Data)   #Test for equal Variance
# p-value = 0.05161 >0.05 so p high null fly => It follows normal distribution
################ One-way Anova ########
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
###P value >0.05  p high and null fly
###There is no difference in TAT of reports of laboratories 






























l