library(tidyverse)
library(caTools) 
library(caret)
library(randomForest) 

predicting_wOBA_df <- read_csv("data/dataset for xwOBA cape 22 23 early 24.csv")

predicting_wOBA_df <- predicting_wOBA_df %>%
      mutate(
            wOBA_Event = case_when(
                  wOBA_Event == "1B" ~ "Single",
                  wOBA_Event == "2B" ~ "Double",
                  wOBA_Event == "3B" ~ "Triple",
                  T ~ wOBA_Event
            ),
            wOBA_Event = factor(wOBA_Event, levels = c("FieldOut", "Single", "Double", "Triple", "HR"))
      )

# Splitting data in train and test data 
set.seed(123)
samp <- sample(nrow(predicting_wOBA_df), 0.85 * nrow(predicting_wOBA_df))
train <- predicting_wOBA_df[samp, ]
test <- predicting_wOBA_df[-samp, ]

# Fitting Random Forest to the train dataset 
#set.seed(123)  # Setting seed 
classifier_RF = randomForest(wOBA_Event ~ ExitSpeed + Angle, 
                             data = train, 
                             mtry = 2,
                             ntree = 3001,
                             importance=T,
                             nodesize = 42) 

classifier_RF

# Predict on test data
predictions <- predict(classifier_RF, newdata = test)

test_wPredictions <- cbind(test, predictions)

test_wPredictions %>%
      filter(wOBA_Event == predictions) %>%
      nrow()

# find test accuracy
mean(predictions == test$wOBA_Event)


train_predictions <- predict(classifier_RF, newdata = train)
mean(train_predictions == train$wOBA_Event)

# get probs of a ball in play being in each class
predicted_probabilities <- predict(classifier_RF, newdata = predicting_wOBA_df, type = "prob")

# Add original data for context
predicted_prob_df <- cbind(predicting_wOBA_df, predicted_probabilities)
saveRDS(classifier_RF, file = "Cape wOBA, PF, wRC+/cape xwOBA RF model.rds")
