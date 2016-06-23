setwd("C:/Users/DELL/Desktop/ISSMTECHASSIGNMENTS/Kaggle/Wine/")
wine=read.csv(file = "wine.csv",header = TRUE)
str(wine)
summary(wine)

model1=lm(Price~AGST,data=wine)
summary(model1)
SSE1=sum(model1$residuals^2)

model2=lm(Price~AGST+HarvestRain,data=wine)
summary(model2)
SSE2=sum(model2$residuals^2)

model3=lm(Price~AGST+HarvestRain+WinterRain+Age+FrancePop,data=wine)
summary(model3)
SSE3=sum(model3$residuals^2)

model4=lm(Price~AGST+HarvestRain+WinterRain+Age,data=wine)
summary(model4)
SSE4=sum(model4$residuals^2)

wineTest=read.csv("wine_test.csv")
predictTest=predict(model4,wineTest)
SSETest=sum((predictTest-wineTest$Price)^2)
SSTTest=sum((wineTest$Price-mean(wine$Price))^2)
R2=1-(SSETest/SSTTest)

