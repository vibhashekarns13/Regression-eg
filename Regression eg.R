setwd(choose.dir())
del <- read.csv("Flight Delays.csv") 
head(del)
dim(del)
str(del)

for(i in c("carrier", "dest", "date", "origin", "weather", "dayweek", "daymonth", "delay"))
  del[,i] <- as.factor(del[,i])
del$tailnu <- NULL
str(del)

library(car)
del$delay <- recode(del$delay,"'delayed' = 1; else = 0")

library(caTools)
set.seed(7) 
split <- sample.split(Y = del$schedtime, SplitRatio = 0.6)
train <- del[split,]
test <- del[!split,]

model <- glm(delay ~ ., binomial(link = "logit"), train)
result <- predict(model, test, type = "response")
result <- ifelse(result > 0.5, 1, 0)

mean(result == test$delay)

















