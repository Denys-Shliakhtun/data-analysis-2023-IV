library(dplyr)
library(jpeg)
library(maptools)
library(sf)
library(terra)
setwd("f:/university/IV semester/ADIS/Lab4/")

# Основне завдання


# 1.Подивитись, проаналізувати структуру
data <- read.csv("Data2.csv", sep=";", header = TRUE, dec = ',')
str(data)

# виправлення помилок в даних

# перейменувати колонку
names(data)[names(data) == "Populatiion"] <- "Population"

# від'ємні значення взяти по модулю
data$GDP.per.capita <- abs(data$GDP.per.capita)
data$Area <- abs(data$Area)

# замінити пропущені значення на середні
data$GDP.per.capita[is.na(data$GDP.per.capita)] <- mean(data$GDP.per.capita, na.rm = TRUE)
data$Population[is.na(data$Population)] <- mean(data$Population, na.rm = TRUE)
data$CO2.emission[is.na(data$CO2.emission)] <- mean(data$CO2.emission, na.rm = TRUE)

str(data)
summary(data)

# 2.Вказати, чи є параметри, що розподілені за нормальним законом
shapiro.test(data$GDP.per.capita) 
shapiro.test(data$Population) 
shapiro.test(data$CO2.emission) 
shapiro.test(data$Area)

# 3.Перевірити гіпотезу про рівність середнього і медіани для одного з параметрів
wilcox.test(data$GDP.per.capita, mu=median(data$GDP.per.capita), conf.int=T) 
wilcox.test(data$Population, mu=median(data$Population), conf.int=T) 
wilcox.test(data$CO2.emission, mu=median(data$CO2.emission), conf.int=T) 
wilcox.test(data$Area, mu=median(data$Area), conf.int=T)

# 4.Вказати, в якому регіоні розподіл викидів СО2 найбільш близький до нормального
data%>%group_by(Region)%>%summarise(p=shapiro.test(data$CO2.emission)$p.value)
data2 <- data%>%filter(Region != "North America")%>%group_by(Region)%>%summarise(p=shapiro.test(data$CO2.emission)$p.value)
data2[order(-data2$p),]

# 5.Побудувати кругову діаграму населення по регіонам
diagram <- data%>%group_by(Region)%>%summarise(population=sum(Population))
pie(diagram$population, labels = diagram$Region)


# Додаткове завдання


# Завдання 1

# 1.	Завантажити карту України  Ukraine.jpg
image <- readJPEG("Ukraine.jpg")

par(mar = c(0, 0, 0, 0))
plot(1, xlim = c(0, 831), ylim = c(0, 553), xlab = "", ylab = "")
lim <- par()
rasterImage(image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

# 2.	Розмістити бульбашки, що відповідають їх населенню, на довільних 5 містах (статистику взяти в інтернеті)

# Визначити міста і координати
Reg <- c("Суми", "Луцьк", "Вінниця", "Херсон", "Одеса")
xy <- locator(5)

# Розмістити бульбашки, при цьому нормалізувати їхні значення
Estimates <- c(264753, 217486, 370026,  279131, 1010537)
mycex <- 10 * (Estimates - min(Estimates)) / max(Estimates) + 2 
colpts <- rgb(0, 0, 1, 0.7)
points(xy$x, xy$y, cex = mycex,  pch = 21, bg = colpts)

# 3.	Знайти найбільшу відстань між містами в пікселях та кілометрах

dist_pixel <- max(dist(data.frame(xy)))
cat("Найбільша відстань у пікселях:", dist_pixel, "\n")

# Відстань між Сумами і Луцьком 660 км
dist_km <- 660 / dist(data.frame(xy))[1] * dist_pixel
cat("Найбільша відстань в кілометрах:", dist_km, "\n")


# Завдання 3

# 1. Завантажити shape-файл с областями України.

Regions <- read_sf(dsn = "UKR_ADM1.shp")
plot(Regions["Name"]) 

# 2. Побудувати картограми для прибутку населення на 1 особу і ВВП по регіонам за 2016 рік.

Sys.setlocale("LC_ALL", "C")
GDP<-read.csv("ukr_GDP.csv",sep=';',dec=',', header=T, skip = 1)  # Валовий регіональний продукт
DPP<-read.csv("ukr_DPP.csv",sep=";",dec=",", header=T, skip = 1)  # Прибуток населення на 1 особу

# картограма прибутку населення на 1 особу по регіонам за 2016 рік.
Regions$DPP = NA
for(i in 1:nrow(DPP))
  Regions$DPP[i] <- DPP$X2016[DPP$Name == Regions$Name[i]]
plot(Regions["DPP"])

# картограма Валового регіонального продукту (ВРП) по регіонам за 2016 рік.
Regions$GDP = NA
for(i in 1:nrow(GDP))
  Regions$GDP[i] <- GDP$X2016[GDP$Name == Regions$Name[i]]
plot(Regions["GDP"])

# 3. По даним за 2006-2015 роки для кожного регіону розрахувати коефіцієнт кореляції між прибутком населення на 1 особу та ВВП. Відобразити на картограмі.
Regions$Cor = NA
for (i in 1:nrow(GDP))
  Regions$Cor[i] <- cor(
              as.numeric(DPP[GDP$Name == Regions$Name[i], 3:12]),
              as.numeric(GDP[GDP$Name == Regions$Name[i], 3:12]), 
              use = "complete.obs")
plot(Regions["Cor"])
