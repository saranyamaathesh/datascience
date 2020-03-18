library(nortest)
lab<-read.csv(file.choose())
View(lab)
colnames(lab)
attach(lab)
#############Normality test###############
shapiro.test(Laboratory.1)
# p-value = 0.5508 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Laboratory.2)
# p-value = 0.8637 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Laboratory.3)
# p-value = 0.4205 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Laboratory.4)
# p-value = 0.6619 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############

library(car)
str(lab)
lab$Laboratory.2<- as.factor(lab$Laboratory.2)
lab$Laboratory.3<- as.factor(lab$Laboratory.3)
lab$Laboratory.4<- as.factor(lab$Laboratory.4)
leveneTest(Laboratory.1~Laboratory.2*Laboratory.3*Laboratory.4,center=median ,data = lab)
leveneTest(lab$Laboratory.1~., data = lab)
##P=1.00>0.05 then p high and ull fly i.e h0= all standard deviatiions  are equal.. 
###Ha= atleast one variables is different from other variable..


################ One-way Anova ########
Anova_results <- aov(Laboratory.1~.,data = lab)
summary(Anova_results)
# p-value = 0.104 > 0.05 accept null hypothesis 
# All Proportions all equal 

