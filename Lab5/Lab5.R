setwd("f:/university/IV semester/ADIS/Lab5/")

data <- read.csv("winequality-red.csv", sep=",", header = TRUE, dec = '.')

#Дослідити дані, підготувати їх для побудови регресійної моделі
str(data)
summary(data)
data[!complete.cases(data),] #пропущених значень немає

#Розділити дані на навчальну та тестову вибірки
div <- nrow(data)/3*2
ndata <- data[data$quality[1:div],] #навчальна вибірка - дві третини даних
tdata <- data[data$quality[(div+1):nrow(data)],] #тестова вибірка

#Побудувати декілька регресійних моделей для прогнозу якості вина
model1 <- lm(formula = quality ~ alcohol, data = ndata)
summary(model1)

model2 <- lm(formula = quality ~ density+citric.acid, data = ndata)
summary(model2)

model3 <- lm(formula = quality ~ pH+density+alcohol, data = ndata)
summary(model3)

model4 <- nls(quality ~ a*pH^k, data=ndata, start=list(a=1,k=0.05))
summary(model4)

model5 <- lm(formula = quality ~ density+I(citric.acid^3), data=ndata)
summary(model5)

#Використовуючи тестову вибірку, з'ясувати яка з моделей краща
tdata$model1 <- predict(model1, tdata)
tdata$model2 <- predict(model2, tdata)
tdata$model3 <- predict(model3, tdata)
tdata$model4 <- predict(model4, tdata)
tdata$model5 <- predict(model5, tdata)
sort(apply(tdata[13:17],2,function(x) sum(x-tdata$quality)^2), decreasing = FALSE)

# додаткове завдання

#Дослідити дані
data <- read.csv("Data4.csv",sep=";",dec = ",", fileEncoding = "latin1")
str(data)
summary(data)
data[!complete.cases(data),]

#перевірка на мультиколінеарність
cor(data[,4:7])

#діаграми розсіювання
library(car) 
scatterplotMatrix(~Cql+Ie+Iec+Is, data=data,diagonal=NA)

#Побудувати декілька регресійних моделей (використати лінійну регресію та поліноміальну регресію обраного вами виду)
model1 <- lm(formula = Cql ~ Is, data = data)
summary(model1)

model2 <- lm(formula = Cql ~ Ie+Iec, data = data)
summary(model2)

model3 <- nls(Cql ~ a*Ie^k, data = data, start=list(a=1,k=0.05))
summary(model3)

#Використовуючи тестову вибірку з файлу Data4t.csv, з'ясувати яка з моделей краща
tdata <- read.csv("Data4t.csv",sep=";",dec = ",", fileEncoding = "latin1")
tdata$model1 <- predict(model1, tdata)
tdata$model2 <- predict(model2, tdata)
tdata$model3 <- predict(model3, tdata)
sort(apply(tdata[8:10],2,function(x) sum(x-tdata$Cql)^2), decreasing = FALSE)
