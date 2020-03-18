#############ratioratory data ##########
ratio<-read.csv(file.choose())   
View(ratio)
stacked_ratio<-stack(ratio)
attach(stacked_ratio)
View(stacked_ratio)
table(stacked_ratio$ind,stacked_ratio$values)
chisq.test(table(stacked_ratio$ind,stacked_ratio$values))
###P value 0.2931>0.05  p high and null fly
###all the proportions are equal