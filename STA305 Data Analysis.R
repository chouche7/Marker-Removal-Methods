setwd("D:\\School Work\\STA305\\Data Analysis") #Set my directory

install.packages("pwr2") 
library(pwr2) 

chem <- read.csv("Chemical Data.csv") #read the data
chemicaldata <- chem[0:24,0:3] #Clean the dataset
chemicaldata <- droplevels(chemicaldata)

attach(chemicaldata)

#Interaction Model
model <- lm(Difference ~ Chemical*Material) 
summary(model)
anova(model)

#Residual graph for IM
plot(model, main= "Chemical/Material Interaction vs. Difference of Removal", which = 2)
hist(model$residuals, main= "Chemical/Material Interaction vs. Difference of Removal", xlab = "Interaction Model Residuals")

#Calculating cook's distance and influential points
inf = sort(cooks.distance(model))
inf

#General Additive Model
model2 <- lm(Difference ~ Chemical + Material)
summary(model2)
anova(model2)

#Residual graph for GAM
plot(model2, main= "Chemical and Material vs. Difference of Removal", which = 2)
hist(model2$residuals,  main= "Chemical and Material vs. Difference of Removal", xlab = "General Additive Model Residual")

#Calculating cook's distance and influential points
inf = sort(cooks.distance(model2))
inf

#Boxplots
boxplot(Difference~Chemical, xlab="Chemical Type", ylab="Difference of Removal", ylim=c(0, 200), main="Chemical vs. Difference of Removal")
boxplot(Difference~Material, xlab="Material", ylab="Difference of Removal", ylim=c(0, 200), main="Material vs. Difference of Removal")

#Two Sample t-test + Barplot for Material
means = c(mean(Difference[Material == "White Board"]), mean(Difference[Material == "Tile"]))
Differences.Material = data.frame(means, c("White Board", "Tile"))
t.test(Difference[Material == "White Board"], Difference[Material == "Tile"], var.equal = FALSE)

#Sample Size Prediction
ss.2way(a=4, b=2, alpha=0.05, beta=0.2, f.A=0.5, f.B=0.5, B=100)
