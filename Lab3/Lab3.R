library(dplyr)
setwd("f:/university/IV semester/ADIS/Lab3/")

#Записати дані у data frame
data <- read.csv("Data2.csv", sep=";", header = TRUE, dec = ',')

#Дослідити структуру даних
str(data)

#Виправити помилки в даних
names(data)[names(data) == "Populatiion"] <- "Population"

print(head(data$GDP.per.capita[order(data$GDP.per.capita)])) #від'ємне значення
print(head(data$Population[order(data$Population)]))
print(head(data$CO2.emission[order(data$CO2.emission)]))
print(head(data$Area[order(data$Area)])) #від'ємне значення

data$GDP.per.capita <- abs(data$GDP.per.capita)
data$Area <- abs(data$Area)

#Побудувати діаграми розмаху та гістограми
boxplot(data$GDP.per.capita)
hist(data$GDP.per.capita)

#Додати стовпчик із щільністю населення
datanew <- cbind(data, Population.per.area = data$Population / data$Area)

###########################################

#Чи є пропущені значення? Замінити середніми

length(data$Country.Name[is.na(data$Country.Name)])
length(data$Region[is.na(data$Region)])
length(data$GDP.per.capita[is.na(data$GDP.per.capita)]) #пропущені значення
length(data$Population[is.na(data$Population)]) #пропущені значення
length(data$CO2.emission[is.na(data$CO2.emission)]) #пропущені значення
length(data$Area[is.na(data$Area)])

data$GDP.per.capita[is.na(data$GDP.per.capita)] <- mean(data$GDP.per.capita, na.rm = TRUE)
data$Population[is.na(data$Population)] <- mean(data$Population, na.rm = TRUE)
data$CO2.emission[is.na(data$CO2.emission)] <- mean(data$CO2.emission, na.rm = TRUE)

#Яка країна має найбільший ВВП на людину (GDP per capita)? Яка має найменшу площу?

head(data[order(data$GDP.per.capita, decreasing = TRUE),], 1)
head(head(data[order(data$Area),]), 1)

#В якому регіоні середня площа країни найбільша?
group <- data%>%group_by(Region)%>%summarise(avArea = mean(Area))
head(as.data.frame(group), 1)

#Знайдіть країну з найбільшою щільністю населення у світі? У Європі та центральній Азії?
head(datanew[order(datanew$Population.per.area, decreasing = TRUE),], 1)
head(filter(datanew[order(datanew$Population.per.area, decreasing = TRUE),], datanew$Region == "Europe & Central Asia"), 1)

#Чи співпадає в якомусь регіоні середнє та медіана ВВП?
group <- data%>%group_by(Region)%>%summarise(mean_ = mean(GDP.per.capita), median_ = median(GDP.per.capita))
as.data.frame(filter(group, group$mean_==group$median_))

#Вивести топ 5 країн та 5 останніх країн по ВВП та кількості СО2 на душу населення.
datanew <- cbind(data, Population.per.area = data$Population / data$Area, CO2.per.capita = data$CO2.emission / data$Population)
head(datanew[order(datanew$GDP.per.capita,datanew$CO2.per.capita, decreasing = c(TRUE, TRUE)),], 5)
tail(datanew[order(datanew$GDP.per.capita,datanew$CO2.per.capita, decreasing = c(TRUE, TRUE)),], 5)