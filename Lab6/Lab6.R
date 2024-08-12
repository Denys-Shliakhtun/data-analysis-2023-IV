library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)
library(ggplot2)

setwd("f:/university/IV semester/ADIS/Lab6/")

# основне завдання
data<-read.csv("titanic.csv",sep=",",dec = ".")
str(data)
summary(data)
naPerc
# відсоток відсутніх значень
naPerc<-NULL
naPerc$names<-colnames(data)
naPerc$rate<-colSums(is.na(data))/nrow(data)*100
naPerc$rate[4]<-nrow(data[data[4]=="",])/nrow(data)*100
naPerc$rate[5]<-nrow(data[data[5]=="",])/nrow(data)*100
naPerc$rate[11]<-nrow(data[data[11]=="",])/nrow(data)*100
naPerc$rate[12]<-nrow(data[data[12]=="",])/nrow(data)*100
naPerc
# відсутні значення у колонках Age (20%), Cabin (77%) і Embarked (0.22%)

# для Embarked відкинемо ці рядки (лише 2 шт.)
workData <- data[data[12] != "",]

# для Age заповнимо середнім значенням по такому ж класу каюти
Mean<-data%>%select(Pclass,Age)%>%group_by(Pclass)%>%summarise(mean(Age,na.rm = TRUE))
workData$Age[is.na(workData$Age)&workData$Pclass==1]<-Mean[[2]][1]
workData$Age[is.na(workData$Age)&workData$Pclass==2]<-Mean[[2]][2]
workData$Age[is.na(workData$Age)&workData$Pclass==3]<-Mean[[2]][3]

# для Cabin відкинемо колонку
workData$Cabin<-NULL

# також відкинемо ім'я, квиток, ID
workData$Name <- NULL
workData$Ticket <- NULL
workData$PassengerId <- NULL

# визначення навчальної і тестової вибірки
trainSize = nrow(workData)/3*2 # дві третини навчальна вибірка
set.seed(1)                    
index = sample( seq_len(nrow(workData)), size = trainSize ) # відібрати випадкові індекси рядків 
# поділ вибірки на навчальну та тестову
train = workData[index , ]
test = workData[-index , ]

# побудова 3 різних моделей

# одне дерево 
tree<-rpart(Survived ~ .,data = train, method = "class", control=rpart.control(minbucket = 2))                     
tree
# намалювати дерево
fancyRpartPlot(tree)
# перевірка моделі
testPred<-predict(tree, newdata = test, type="vector")
sqrt(sum((testPred - test$Survived)^2))/length(testPred)/mean(data$Survived)  # похибка  

# ансамбль дерев   
numtrees <- 300
res <- numeric(numtrees)   # масив відповідає за похибку по вказаній кількості дерев
prd <- numeric(nrow(test)) # середні результати, по яким і дивимось похибки
for(i in 1:numtrees){
  # випадково виберемо 80% рядків та стовпчиків з множини даних
  x <- runif(nrow(train))>0.2;
  y <- runif(ncol(train))>0.2;
  # обовязково включимо Survived , бо для нього будуємо модель
  y[1] <- TRUE
  traindata <- train[x,y]
  # генеруємо повне дерево
  atree <- rpart(Survived ~ ., traindata, control=rpart.control(cp=.0))
  # усереднюємо передбачення з усіма попередніми деревами
  prd <- prd + predict(atree, test)
  predictions <- prd / i
  # оцінюємо похибку
  res[i] <- sqrt(sum((predictions - test$Survived)^2))/length(predictions)/mean(data$Survived)
}
plot(res,type="l")
res[numtrees]

# випадковий ліс
library(randomForest)
randForest <- randomForest(Survived ~ ., train)
predictions <- predict(randForest, test)
print(sqrt(sum((as.integer(predictions) - as.integer(test$Survived))^2))/length(predictions))

# одне дерево      0.151
# ансамбль дерев   0.053
# випадковий ліс   0.036



# додаткове завдання

# імпорт даних з файлу Data2.csv і виправлення даних (з КП3/КП4)
data2 <- read.csv("Data2.csv", sep=";", header = TRUE, dec = ',')
str(data2)
# перейменувати колонку
names(data2)[names(data2) == "Populatiion"] <- "Population"
# від'ємні значення взяти по модулю
data2$GDP.per.capita <- abs(data2$GDP.per.capita)
data2$Area <- abs(data2$Area)
# замінити пропущені значення на середні
data2$GDP.per.capita[is.na(data2$GDP.per.capita)] <- mean(data2$GDP.per.capita, na.rm = TRUE)
data2$Population[is.na(data2$Population)] <- mean(data2$Population, na.rm = TRUE)
data2$CO2.emission[is.na(data2$CO2.emission)] <- mean(data2$CO2.emission, na.rm = TRUE)
str(data2)
summary(data2)

# 1. визначити, який регіон домінує в кластерах по ВВП на душу населення та щільності населення

data2$Density<-data2$Population/data2$Area  # cтворимо нову колонку зі значенням щільності населення
workData<-select(data2,2,3,7)    # вибрано лише регіон, ВВП на душу населення і щільність населення

# Метод k-середніх

# оцінюємо моделі з різною кількістю кластерів по tot.withinss
kbest<-c(1:10)
for (i in 1:10) {
  kres <- kmeans(workData[,2:3],i,nstart=20)   # кількість кластерів перебираємо від 1 до 10
  kbest[i]<-kres$tot.withinss
}
plot(kbest)

# найкраща модель з 5 класами
kres <- kmeans(workData[,2:3],5,nstart=20)
workData$cluster<-kres$cluster

ggplot(data=workData,aes(x=GDP.per.capita, y=Density))+
  geom_point(col=workData$cluster)

workData$Population<-data2$Population
workData$Area<-data2$Area

# Групування даних за регіоном та кластером
regionClusters <- workData %>%
  group_by(Region, cluster) %>%
  summarize(GDP.per.capita = sum(GDP.per.capita*Population)/sum(Population), density = sum(Population)/sum(Area)) %>%
  ungroup()

# Знаходження регіону з найбільшою середньою вартістю ВВП на душу населення та щільністю населення в кластерах
topRegion <- regionClusters %>%
  group_by(cluster) %>%
  slice(which.max(GDP.per.capita * density)) %>%
  select(-cluster)

# Виведення результатів
cat("Регіон, що домінує в кластерах:\n")
print(topRegion)


# 2. вивести частотні гістограми всіх показників файла Data2.csv, використовуючи цикл
for (i in 3:6){
  hist(data2[,i],main=colnames(data2)[i])
}

# 3. створити функцію, яка на вхід отримує два набори даних, перевіряє чи є лінійна залежність та виводить True чи False 
# будемо розуміти під «є лінійна залежність», якщо коефіцієнт кореляції по модулю більше 0,8
isLinearDependent<-function(x,y){
  return(abs(cor(x,y))>0.8)
}
cor(data2$GDP.per.capita,data2$CO2.emission)
isLinearDependent(data2$GDP.per.capita,data2$CO2.emission)


