# install package
install.packages("dplyr")

# import data
dataCSV = read.csv("honda_car_selling.csv", TRUE, sep = ",", fileEncoding = "UTF-8-BOM")
print(dataCSV)

# removing all NA
dataCSV <- dataCSV[complete.cases(dataCSV),]

# calling library
library(dplyr)
library(arules)

# count all different model from Honda and arranging from the most sold Honda
modelCount <- dataCSV %>%
  group_by(Car.Model) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# view the top 10 sold Honda
print(modelCount)

# barplot
barplot(modelCount$count, names.arg = modelCount$Car.Model, main = "Honda Type")

# count transmission 
transmissionCount <- dataCSV %>%
  group_by(Suspension) %>%
  summarise(count = n())

# percentage of transmission type and pie chart
percentTrans <- round(transmissionCount$count*100/sum(transmissionCount$count), 1)
pie(transmissionCount$count, labels = transmissionCount$Suspension)

# arules
rules <- apriori(dataCSV, parameter = list(support = 0.2, confidence = 0.8))
inspect(rules)
