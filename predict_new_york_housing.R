##################################################
#prep
setwd('/Users/administratoro/Desktop/5200/kaggle')
library(caret)
library(lubridate)
library(ISLR)
library(ISLR)
library(caret)
library(caTools)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(randomForest)
library(readr)
library(forcats)
df = read.csv("analysisData.csv")
df_mini = df[1:2000,]
split = createDataPartition(y = df$price, p = 0.75, list = F,groups = 50)


df$host_listings_count[is.na(df$host_listings_count)] <-  sample(0.9:1, sum(is.na(df$host_listings_count)),replace=TRUE)
df$host_total_listings_count[is.na(df$host_total_listings_count)] <-  sample(0.9:1, sum(is.na(df$host_total_listings_count)),replace=TRUE)
df$zipcode[is.na(df$zipcode)] <-  sample(0.9:1, sum(is.na(df$zipcode)),replace=TRUE)
df$accommodates[is.na(df$accommodates)] <-  sample(0.9:1, sum(is.na(df$accommodates)),replace=TRUE)
df$bathrooms[is.na(df$bathrooms)] <-  sample(0.9:1, sum(is.na(df$bathrooms)),replace=TRUE)
df$bedrooms[is.na(df$bedrooms)] <-  sample(0.9:1, sum(is.na(df$bedrooms)),replace=TRUE)
df$beds[is.na(df$beds)] <-  sample(0.9:1, sum(is.na(df$beds)),replace=TRUE)
df$security_deposit[is.na(df$security_deposit)] <-  sample(0.9:1, sum(is.na(df$security_deposit)),replace=TRUE)
df$cleaning_fee[is.na(df$cleaning_fee)] <-  sample(0.9:1, sum(is.na(df$cleaning_fee)),replace=TRUE)
df$guests_included[is.na(df$guests_included)] <-  sample(0.9:1, sum(is.na(df$guests_included)),replace=TRUE)
df$extra_people[is.na(df$extra_people)] <-  sample(0.9:1, sum(is.na(df$extra_people)),replace=TRUE)
df$minimum_nights[is.na(df$minimum_nights)] <-  sample(0.9:1, sum(is.na(df$minimum_nights)),replace=TRUE)
df$maximum_nights[is.na(df$maximum_nights)] <-  sample(0.9:1, sum(is.na(df$maximum_nights)),replace=TRUE)
df$minimum_minimum_nights[is.na(df$minimum_minimum_nights)] <-  sample(0.9:1, sum(is.na(df$minimum_minimum_nights)),replace=TRUE)
df$maximum_minimum_nights[is.na(df$maximum_minimum_nights)] <-  sample(0.9:1, sum(is.na(df$maximum_minimum_nights)),replace=TRUE)
df$maximum_maximum_nights[is.na(df$maximum_maximum_nights)] <-  sample(0.9:1, sum(is.na(df$maximum_maximum_nights)),replace=TRUE)
df$minimum_nights_avg_ntm[is.na(df$minimum_nights_avg_ntm)] <-  sample(0.9:1, sum(is.na(df$minimum_nights_avg_ntm)),replace=TRUE)
df$availability_30[is.na(df$availability_30)] <-  sample(0.9:1, sum(is.na(df$availability_30)),replace=TRUE)
df$availability_60[is.na(df$availability_60)] <-  sample(0.9:1, sum(is.na(df$availability_60)),replace=TRUE)
df$availability_90[is.na(df$availability_90)] <-  sample(0.9:1, sum(is.na(df$availability_90)),replace=TRUE)
df$availability_365[is.na(df$availability_365)] <-  sample(0.9:1, sum(is.na(df$availability_365)),replace=TRUE)
df$number_of_reviews[is.na(df$number_of_reviews)] <-  sample(0.9:1, sum(is.na(df$number_of_reviews)),replace=TRUE)
df$review_scores_rating[is.na(df$review_scores_rating)] <-  sample(0.9:1, sum(is.na(df$review_scores_rating)),replace=TRUE)
df$review_scores_accuracy[is.na(df$review_scores_accuracy)] <-  sample(0.9:1, sum(is.na(df$review_scores_accuracy)),replace=TRUE)
df$review_scores_cleanliness[is.na(df$review_scores_cleanliness)] <-  sample(0.9:1, sum(is.na(df$review_scores_cleanliness)),replace=TRUE)
df$review_scores_checkin[is.na(df$review_scores_checkin)] <-  sample(0.9:1, sum(is.na(df$review_scores_checkin)),replace=TRUE)
df$review_scores_communication[is.na(df$review_scores_communication)] <-  sample(0.9:1, sum(is.na(df$review_scores_communication)),replace=TRUE)
df$review_scores_location[is.na(df$review_scores_location)] <-  sample(0.9:1, sum(is.na(df$review_scores_location)),replace=TRUE)
df$review_scores_value[is.na(df$review_scores_value)] <-  sample(0.9:1, sum(is.na(df$review_scores_communication)),replace=TRUE)
df$calculated_host_listings_count[is.na(df$calculated_host_listings_count)] <-  sample(0.9:1, sum(is.na(df$calculated_host_listings_count)),replace=TRUE)
df$calculated_host_listings_count_entire_homes[is.na(df$calculated_host_listings_count_entire_homes)] <-  sample(0.9:1, sum(is.na(df$calculated_host_listings_count_entire_homes)),replace=TRUE)
df$calculated_host_listings_count_private_rooms[is.na(df$calculated_host_listings_count_private_rooms)] <-  sample(0.9:1, sum(is.na(df$calculated_host_listings_count_private_rooms)),replace=TRUE)
df$calculated_host_listings_count_shared_rooms[is.na(df$calculated_host_listings_count_shared_rooms)] <-  sample(0.9:1, sum(is.na(df$calculated_host_listings_count_shared_rooms)),replace=TRUE)
df$reviews_per_month[is.na(df$reviews_per_month)] <-  sample(0.9:1, sum(is.na(df$reviews_per_month)),replace=TRUE)
df <- df %>%
  mutate(stdmaximum_maximum_nights = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdcalculated_host_listings_count = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdbeds = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdaccommodates = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdbedrooms = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdbathrooms = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdreview_scores_location = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdavailability_365 = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdcleaning_fee = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdhost_total_listings_count = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdnumber_of_reviews = scale(host_listings_count, center = T, scale = T)) %>%
  mutate(stdguests_included = scale(host_listings_count, center = T, scale = T))

train = df[split,]
test = df[-split,]

forest1 = randomForest(price~stdmaximum_maximum_nights+stdcalculated_host_listings_count+zipcode+room_type+stdaccommodates+stdbeds+stdbedrooms+stdbathrooms+
                         stdreview_scores_location+stdavailability_365+stdcleaning_fee+stdhost_total_listings_count+stdnumber_of_reviews+stdguests_included,
                       data=train, 
                       ntree = 100)
pred1 = predict(forest1,newdata=test)
rmse1 = sqrt(mean((pred1-test$price)^2)); rmse1

dscoringData = read.csv('scoringData.csv')
scoringData$beds[is.na(scoringData$beds)] <-  sample(0.9:1, sum(is.na(scoringData$beds)),replace=TRUE)
scoringData$availability_365[is.na(scoringData$availability_365)] <-  sample(0.9:1, sum(is.na(scoringData$availability_365)),replace=TRUE)
scoringData$accommodates[is.na(scoringData$accommodates)] <-  sample(0.9:1, sum(is.na(scoringData$accommodates)),replace=TRUE)
scoringData$bedrooms[is.na(scoringData$bedrooms)] <-  sample(0.9:1, sum(is.na(scoringData$bedrooms)),replace=TRUE)
scoringData$bathrooms[is.na(scoringData$bathrooms)] <-  sample(0.9:1, sum(is.na(scoringData$bathrooms)),replace=TRUE)
scoringData$maximum_maximum_nights[is.na(scoringData$maximum_maximum_nights)] <-  sample(0.9:1, sum(is.na(scoringData$maximum_maximum_nights)),replace=TRUE)
scoringData$calculated_host_listings_count[is.na(scoringData$calculated_host_listings_count)] <-  sample(0.9:1, sum(is.na(scoringData$calculated_host_listings_count)),replace=TRUE)
scoringData$zipcode[is.na(scoringData$zipcode)] <-  sample(0.9:1, sum(is.na(scoringData$zipcode)),replace=TRUE)
scoringData$room_type[is.na(scoringData$room_type)] <-  sample(0.9:1, sum(is.na(scoringData$room_type)),replace=TRUE)
scoringData$review_scores_location[is.na(scoringData$review_scores_location)] <-  sample(0.9:1, sum(is.na(scoringData$review_scores_location)),replace=TRUE)
scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <-  sample(0.9:1, sum(is.na(scoringData$cleaning_fee)),replace=TRUE)
scoringData$guests_included[is.na(scoringData$guests_included)] <-  sample(0.9:1, sum(is.na(scoringData$guests_included)),replace=TRUE)
scoringData$number_of_reviews[is.na(scoringData$number_of_reviews)] <-  sample(0.9:1, sum(is.na(scoringData$number_of_reviews)),replace=TRUE)
scoringData$host_total_listings_count[is.na(scoringData$host_total_listings_count)] <-  sample(0.9:1, sum(is.na(scoringData$host_total_listings_count)),replace=TRUE)

pred_sc = predict(forest1,newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred_sc)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)

###########################################################
tree1 = rpart(price~guest zipcode+room_type+accommodates+bedrooms+bathrooms+review_scores_location+availability_365+cleaning_fee,
        data = train, method = 'anova',
    #    )
        control = rpart.control(cp = 0.0001))
pred1 = predict(tree1,newdata=test)
rmse1 = sqrt(mean((pred1-test$price)^2)); rmse1

tree1 = rpart(price~zipcode+room_type+accommodates+bedrooms+bathrooms+review_scores_location+availability_365+cleaning_fee,
              data = df, method = 'anova',
              control = rpart.control(cp = 0.0001))
scoringData = read.csv('scoringData.csv')
scoringData <- scoringData %>%
  mutate(zipcode = replace(zipcode,,parse_number(zipcode))) %>%
  mutate(zipcode = replace(zipcode,zipcode == '11003',11004)) %>%
  mutate(zipcode = replace(zipcode,zipcode == '11003',11004)) %>%
  mutate(zipcode = replace(zipcode,zipcode == '10309',11559))
pred_score = predict(tree1,newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred_score)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)

#83.15922

#tree2
trControl = trainControl(method='cv',number = 5)
tuneGrid = expand.grid(.cp = seq(from = 0.0001,to = 0.1,by = 0.001))
set.seed(617)
cvModel = train(price~zipcode+room_type+accommodates+bedrooms+bathrooms+review_scores_location+availability_365+cleaning_fee,
                data = train,
                method="rpart",
                trControl = trControl,
                tuneGrid = tuneGrid,na.action = na.pass)
pred2 = predict(cvModel,newdata=test)
rmse2 = sqrt(mean((pred2-test$price)^2)); rmse2
#141.4053

#forest
library(randomForest)
set.seed(617)
forest = randomForest(price~zipcode+room_type+accommodates+bedrooms+bathrooms+review_scores_location+availability_365+cleaning_fee,
                      data = train,ntree = 10, na.action = "na.omit")
pred3 = predict(forest,newdata=test)
rmse_forest = sqrt(mean((pred3-test$price)^2)); rmse_forest
#forest with scoring
scoringData = read.csv('scoringData.csv')
pred_score = predict(tree1,newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred_score)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)

#forest2
set.seed(617)
forest2 = randomForest(price~beds+bathrooms+accommodates+bedrooms+security_deposit+cleaning_fee+review_scores_location+guests_included,
                      data = train,ntree = 100, na.action = "na.exclude")
forest_pred2 = predict(forest2,newdata=test)
rmse_forest2 = sqrt(mean((forest_pred2-test$price)^2)); rmse_forest2

#ranger
library(ranger)
forest_ranger = ranger(price~host_total_listings_count+room_type+accommodates+bathrooms+bedrooms+beds+bed_type+security_deposit+cleaning_fee+guests_included+extra_people+minimum_nights+maximum_nights+availability_30+availability_365,
                       data = train,num.trees = 100)
#pred = predict(forest_ranger, data =test,num.trees = 100)
scoringData = read.csv('scoringData.csv')
pred = predict(forest_ranger,data=scoringData)
rmse_forest_ranger = sqrt(mean((pred$predictions-test$price)^2)); rmse_forest_ranger

#xgboost
library(wrapr)
library(vtreat)
trt = designTreatmentsZ(dframe = df_mini,
                        varlist = names(train)[2:6])

newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']

train_input = prepare(treatmentplan = trt, 
                      dframe = df_mini,
                      varRestriction = newvars)
test_input = prepare(treatmentplan = trt, 
                     dframe = test,
                     varRestriction = newvars)
head(train_input)
