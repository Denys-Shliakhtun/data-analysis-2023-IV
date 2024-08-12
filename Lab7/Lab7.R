library(dplyr)
library(forecast)
library(plm) 

setwd("f:/university/IV semester/ADIS/Lab7/")

# завдання 1 - часовий ряд для статистики захворювань на Covid в двох сусідніх країнах

# завантаження даних
dataCovid <- read.csv("owid-covid-data.csv", sep=",", header = TRUE, dec = '.')

# вибір колонок лише про країну, дату і зареєстровано випадків за день
dataCovid <- select(dataCovid, location, date, new_cases)

# дослідження даних
colSums(is.na(dataCovid))/nrow(dataCovid)*100
str(dataCovid)
summary(dataCovid)

# виправлення даних, перетворення колонки у формат дати
dataCovid$new_cases[is.na(dataCovid$new_cases)] <- 0
dataCovid$date <- as.Date(dataCovid$date)


# виокремлення даних для Франції і Німеччини

dataCovidFrance <- dataCovid[dataCovid$location == "France",]
dataCovidFrance$location <- NULL

dataCovidGermany <- dataCovid[dataCovid$location == "Germany",]
dataCovidGermany$location <- NULL

plot(dataCovidFrance$date,dataCovidFrance$new_cases, type='l', xlab = "Date", ylab = "New Cases")
lines(dataCovidGermany$date,dataCovidGermany$new_cases, type='l', col="blue")


# створення часових рядів

TSFrance <- ts(dataCovidFrance$new_cases, frequency = 365, start = c(2020,3)) 
str(TSFrance)

TSGermany <- ts(dataCovidGermany$new_cases, frequency = 365, start = c(2020,3)) 
str(TSGermany)

plot(TSFrance, xlab = "Date", ylab = "New Cases")
lines(TSGermany, col = "blue")


# декомпозиція на тренд (T), сезону (S) та випадкову (R) складові

decFrance <- decompose(TSFrance)
plot(decFrance)

decGermany <- decompose(TSGermany)
plot(decGermany)


# визначення автокореляції, при значенні > 0.8 вона є
acf(TSFrance, lag.max = 20)
acf(TSFrance-decFrance$seasonal, lag.max = 20)
acf(TSGermany, lag.max = 200)
acf(TSGermany-decGermany$seasonal, lag.max = 20)

# p < 0.05, нульова гіпотеза про відсутність автокореляції відхиляється
Box.test(TSFrance, lag=20, type="Ljung-Box")
Box.test(TSGermany, lag=20, type="Ljung-Box")

# Зображення приростів 
plot(diff(TSFrance))
hist(diff(TSFrance), breaks = 30)  
plot(diff(TSGermany))
hist(diff(TSGermany), breaks = 30)  

# Прогноз, модель ARIMA
# order= c(p,q,d) - p - скільки попередніх значень враховувати для прогнозу (по автокореляції, або підбором від 1)
#                   q - скільки попередніх значень прогнозу враховувати для прогнозу (підбором від 1)
#                   d - скільки похідних враховувати для прогнозу (до 5)
#                       підбір спочатку d, потім p, потім q 
# AIC(model) інформаційний критерйй Акаіке для моделі (чим менше, тим краще)

#France

# перед тим, як моделювати, треба прибрати сезонну компоненту і не забути її потім додати до прогнозу
TSFranceSeasonal <- TSFrance-decFrance$seasonal

for (i in 0:5) # вибрали 4
{
  print(i)
  print(AIC(arima(TSFranceSeasonal, order = c(0,0,i))))
}

for (i in 0:5) # вибрали 3
{
  print(i)
  print(AIC(arima(TSFranceSeasonal, order = c(i,0,4))))
}

for (i in 1:4) # вибрали 1
{
  print(i)
  print(AIC(arima(TSFranceSeasonal, order = c(3,i,4))))
}

forecFrance<-forecast(arima(TSFranceSeasonal, order = c(3,1,4)), h=365)
plot(forecFrance)

# останнє значення сезонної компоненти в даних, щоб додати сезонну компоненту до прогнозу
last<-decFrance$seasonal[length(decFrance$seasonal)]

# знайти позицію останнього значення
which(decFrance$figure==last)

# починаючи з наступного значення, додати повторюваний фрагмент до прогнозу
season<-c(decFrance$figure[137:365],decFrance$figure[1:136])     
forecFrance$mean<-forecFrance$mean + season
forecFrance$x<-forecFrance$x + decFrance$seasonal
forecFrance$lower<-forecFrance$lower+season
forecFrance$upper<-forecFrance$upper+season

# намалювати прогноз
plot(forecFrance)
plot(forecFrance, xlim=c(2023, 2025))

# Germany

# перед тим, як моделювати, треба прибрати сезонну компоненту і не забути її потім додати до прогнозу
TSGermanySeasonal <- TSGermany-decGermany$seasonal

for (i in 0:5) # вибрали 3
{
  print(i)
  print(AIC(arima(TSGermanySeasonal, order = c(0,0,i))))
}

for (i in 0:5) # вибрали 5
{
  print(i)
  print(AIC(arima(TSGermanySeasonal, order = c(i,0,3))))
}

for (i in 1:4) # вибрали 2
{
  print(i)
  print(AIC(arima(TSGermanySeasonal, order = c(5,i,3))))
}

forecGermany<-forecast(arima(TSGermanySeasonal, order = c(3,2,4)), h=365)
plot(forecGermany)

# останнє значення сезонної компоненти в даних, щоб додати сезонну компоненту до прогнозу
last<-decGermany$seasonal[length(decGermany$seasonal)]

# знайти позицію останнього значення
which(decGermany$figure==last)

# починаючи з наступного значення, додати повторюваний фрагмент до прогнозу
season<-c(decGermany$figure[143:365],decGermany$figure[1:142])     
forecGermany$mean<-forecGermany$mean + season
forecGermany$x<-forecGermany$x + decGermany$seasonal
forecGermany$lower<-forecGermany$lower+season
forecGermany$upper<-forecGermany$upper+season

# намалювати прогноз
plot(forecGermany)
plot(forecGermany, xlim=c(2023, 2025))



# завдання 2 - часовий ряд для курсу гривня/долар або гривня/євро за останні 3 роки

# завантаження даних
dataCurrency <- read.csv("uah_to_euro.csv", sep=",", header = TRUE, dec = '.')

# вибір колонок лише про дату і курс
dataCurrency <- select(dataCurrency, Дата, Офіційний.курс.гривні..грн)
names(dataCurrency)[names(dataCurrency) == "Дата"] <- "Date"
names(dataCurrency)[names(dataCurrency) == "Офіційний.курс.гривні..грн"] <- "Exchange_rate"

# дослідження даних
colSums(is.na(dataCurrency))/nrow(dataCurrency)*100
str(dataCurrency)
summary(dataCurrency)

# перетворення колонки у формат дати
dataCurrency$Date <- as.Date(dataCurrency$Date, format = "%d.%m.%Y")

# виведення даних
plot(dataCurrency$Date,dataCurrency$Exchange_rate, type='l')

# створення часового ряду
TSCurrency <- ts(dataCurrency$Exchange_rate, frequency = 365, start = c(2020,121)) 
str(TSCurrency)
plot(TSCurrency)

# декомпозиція на тренд (T), сезону (S) та випадкову (R) складові
decCurrency <- decompose(TSCurrency)
plot(decCurrency)

# визначення автокореляції, при значенні > 0.8 вона є
acf(TSCurrency, lag.max = 20)
acf(TSCurrency-decCurrency$seasonal, lag.max = 20)

# p < 0.05, нульова гіпотеза про відсутність автокореляції відхиляється
Box.test(TSCurrency, lag=20, type="Ljung-Box")

# Зображення приростів 
plot(diff(TSCurrency))
hist(diff(TSCurrency), breaks = 30)  # залишки моделі мають бути розподілені нормально - як перевірити та подивитись

# перед тим, як моделювати, треба прибрати сезонну компоненту і не забути її потім додати до прогнозу
TSCurrencySeasonal <- TSCurrency-decCurrency$seasonal

for (i in 0:5) # вибрали 5
{
  print(i)
  print(AIC(arima(TSCurrencySeasonal, order = c(0,0,i))))
}

for (i in 0:2) # вибрали 1
{
  print(i)
  print(AIC(arima(TSCurrencySeasonal, order = c(i,0,5))))
}

for (i in 0:4) # вибрали 1
{
  print(i)
  print(AIC(arima(TSCurrencySeasonal, order = c(1,i,5))))
}

forecCurrency<-forecast(arima(TSCurrencySeasonal, order = c(1,1,5)), h=365)

# щоб додати сезонну компоненту до прогнозу треба подивитись, яке було останнє значення сезонної компоненти в наявних даних
last<-decCurrency$seasonal[length(decCurrency$seasonal)]

# знайти це значення в повторюваному фрагменті сезонної компоненти
which(decCurrency$figure==last)

# починаючи з наступного значення, додати повторюваний фрагмент до прогнозу
season<-c(decCurrency$figure[2:365],decCurrency$figure[1])     
forecCurrency$mean<-forecCurrency$mean + season
forecCurrency$x<-forecCurrency$x + decCurrency$seasonal
forecCurrency$lower<-forecCurrency$lower+season
forecCurrency$upper<-forecCurrency$upper+season

# намалювати прогноз
plot(forecCurrency)
plot(forecCurrency, xlim=c(2023, 2025))



# додаткове завдання

# завантаження даних
dataWeather <- read.csv("seattleWeather_1948-2017.csv", sep=",", header = TRUE, dec = '.')
str(dataWeather)
summary(dataWeather)

# заповнення NA
dataWeather[is.na(dataWeather$RAIN),]
dataWeather$RAIN[is.na(dataWeather$RAIN)] <- FALSE
dataWeather$PRCP[is.na(dataWeather$PRCP)] <- 0

# переведення дати
dataWeather$DATE <- as.Date(dataWeather$DATE, format = "%Y-%m-%d")

# 1. Градуси перевести в Цельсії
dataWeather$TMAX <- (dataWeather$TMAX - 32) * 5 / 9
dataWeather$TMIN <- (dataWeather$TMIN - 32) * 5 / 9

# 2. Чи є кореляція між температурою та опадами
cor(dataWeather[2:4], use = "complete") # зв'язок слабкий обернений

# 3. Скласти прогноз опадів на 2018 рік. Оцінити точність прогнозу 

# Чи є сезонна компонента в кількості опадів в Сіетлі? 
TSWeather<-ts(dataWeather$PRCP, frequency = 365, start = c(1948,1))
plot(TSWeather)
decWeather <- decompose(TSWeather) # є сезонна компонента
plot(decWeather)

# Скласти прогноз опадів на 2018 рік
acf(TSWeather-decWeather$seasonal, lag.max = 21)  # оцінка автокореляції, p=4

# перед тим, як моделювати, треба прибрати сезонну компоненту і не забути її потім додати до прогнозу
TSWeatherSeasonal <- TSWeather-decWeather$seasonal

for (i in 0:5) # вибрали 1
{
  print(i)
  print(AIC(arima(TSWeatherSeasonal, order = c(4,0,i))))
}

for (i in 1:3) # вибрали 1
{
  print(i)
  print(AIC(arima(TSWeatherSeasonal, order = c(4,i,1))))
}

forecWeather<-forecast(arima(TSWeatherSeasonal, order = c(4,1,1)), h=365) # order= c(p=4,q=1,d=1) 
forecWeather$model$aic

# останнє значення сезонної компоненти в даних, щоб додати сезонну компоненту до прогнозу
last<-decWeather$seasonal[length(decWeather$seasonal)]

# знайти позицію останнього значення
which(decWeather$figure==last)

# починаючи з наступного значення, додати повторюваний фрагмент до прогнозу
season<-c(decWeather$figure[2:365],decWeather$figure[1])     
forecWeather$mean<-forecWeather$mean + season
forecWeather$x<-forecWeather$x + decWeather$seasonal
forecWeather$lower<-forecWeather$lower+season
forecWeather$upper<-forecWeather$upper+season

# намалювати прогноз
plot(forecWeather)
plot(forecWeather, xlim=c(2016,2019)) 

# оцінка точності прогнозу неможлива за допомогою даного датасету - відсутні фактичні значення для порівняння