library(fastDummies)
library(dplyr)
library(stargazer)
## Load Files
filenames <- list.files("/Users/sammachin/Documents/Data Science Practice Problems/Titanic",
                        pattern="*.csv", full.names=TRUE)
data_list <- lapply(filenames, read.csv)
test <- data_list[[2]]
train <- data_list[[3]]

#Cleaning Data
train$male <- ifelse(train$Sex == "male", 1,0)
train_dum <- dummy_cols(train, select_columns = "Embarked")
train_num <- select(train_dum,-1,-4,-5,-9,-11,-12,-13,-14,-17)

#Logit Model
lpm <- glm(Survived ~ ., data = train_num)
logit <- glm(Survived ~ ., data = train_num, family = "binomial")
stargazer(lpm, logit, type = "text")
summary(logit)

#Putting test data in same format as mnodel data
test$male <- ifelse(test$Sex == "male", 1,0)
test_dum <- dummy_cols(test, select_columns = "Embarked")

##Testing
pred_lpm <- round(predict(lpm, test, type = "response"), 0)
pred_logit <- round(predict(logit, test, type = "response"),0)
