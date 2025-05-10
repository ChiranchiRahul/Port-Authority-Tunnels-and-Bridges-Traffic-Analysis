library(DBI)
library(odbc)
library(dplyr)

con <- dbConnect(odbc::odbc(),
                 .connection_string = paste0(
                   "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
                   "DBQ=D:/Capstone_Project/TBT_Traffic_Database_PA.accdb;"
                 ))

# Optional: List tables to verify
dbListTables(con)

df_Traffic <- dbReadTable(con, "Traffic_Data")

View(df_Traffic)

colSums(is.na(df_Traffic))

con_1 <- dbConnect(odbc::odbc(),
                 .connection_string = paste0(
                   "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
                   "DBQ=D:/Capstone_Project/Weather Database.accdb;"
                 ))


dbListTables(con_1)


df_weather <- dbReadTable(con_1, "Tbl_Weather")

View(df_weather)

merged_traffic_weather<-left_join(df_Traffic,df_weather,by="DATE")
merged_traffic_weather
View(merged_traffic_weather)

colSums(is.na(merged_traffic_weather))

colnames(df_Traffic)

colnames(df_weather)

colnames(merged_traffic_weather)

summary(merged_traffic_weather)

#  Create Time Buckets (Hour)

merged_traffic_weather$Hour <- as.integer(substr(sprintf("%04d", merged_traffic_weather$TIME), 1, 2))

# check the date range of the column "DATE"

range(merged_traffic_weather$DATE)

# check the number of observation 

nrow(merged_traffic_weather)

# now we will take 1% of rows as sample from total observations 5135398

set.seed(42)  # Ensures reproducibility
sample_data_1pct <- merged_traffic_weather[sample(nrow(merged_traffic_weather), 0.01 * nrow(merged_traffic_weather)), ]

View(sample_data_1pct)

# check the missing values of sample dataset


colSums(is.na(sample_data_1pct))


# missing values in the class column means in specific date no car was recorded.
# so, we will replace it with 0

class_cols <- paste0("CLASS.", c(1:9, 11))

sample_data_1pct[class_cols] <- lapply(sample_data_1pct[class_cols], function(x) ifelse(is.na(x), 0, x))


# lets see if there is still any missing values in other columns

colSums(is.na(sample_data_1pct))

# now we can see that weather related column still have large amount of missing values.

# we want to analyze the project for those weather column that contains values

# so, make a character vector for all weather column

weather_col <- c("AWND", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")

# now bring the weather_col vector that only contain the values

cleaned_data <- sample_data_1pct[complete.cases(sample_data_1pct[, weather_col]), ]

View(cleaned_data)

install.packages("writexl")
library(writexl)

write_xlsx(cleaned_data, "C:/Users/Sujon/Desktop/cleaned_data.xlsx")




# now check the eleanded_data dataset if there is any missing values

colSums(is.na(cleaned_data))

write.csv(cleaned_data, "D:/Capstone_Project/FinalDataset_RandomSampling.csv", row.names = FALSE)




# NOW LETS WORK ON GOAL_2


colnames(cleaned_data)


# Total number of toll violators across all records
total_violators <- sum(cleaned_data$VIOLATION, na.rm = TRUE)

total_violators

# so, there are 705671 violators 


# violators by facility

library(dplyr)
violations_by_facility <- cleaned_data %>%
  group_by(FAC_B) %>%
  summarise(
    Violators = sum(VIOLATION, na.rm = TRUE),
    Total_Vehicles = sum(TOTAL, na.rm = TRUE),
    Violation_Rate = round(100 * Violators / Total_Vehicles, 2)
  ) %>%
  arrange(desc(Violators))

View(violations_by_facility)

library(ggplot2)

ggplot(violations_by_facility, aes(x = reorder(FAC_B, -Violators), y = Violators)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Total Toll Violators by Facility",
       x = "Facility", y = "Number of Violators") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# violation by day of week

violations_by_day <- cleaned_data %>%
  group_by(Day_Name) %>%
  summarise(Violators = sum(VIOLATION, na.rm = TRUE)) %>%
  arrange(desc(Violators))

View(violations_by_day)


library(ggplot2)

# Optional: Set day order if needed

violations_by_day$Day_Name <- factor(violations_by_day$Day_Name,
                                     levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plot
ggplot(violations_by_day, aes(x = Day_Name, y = Violators)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(title = "Total Toll Violators by Day of the Week",
       x = "Day of the Week", y = "Number of Violators") +
  theme_minimal()




# violations by hour of a day

violations_by_hour <- cleaned_data %>%
  group_by(Hour) %>%
  summarise(Violators = sum(VIOLATION, na.rm = TRUE)) %>%
  arrange(Hour)

View(violations_by_hour)











library(dplyr)

# Create a labeled hour for easy understanding
violations_by_hour <- cleaned_data %>%
  group_by(Hour) %>%
  summarise(Violators = sum(VIOLATION, na.rm = TRUE)) %>%
  mutate(Hour_Label = paste0(Hour, ":00"))

# Then plot using Hour_Label
library(ggplot2)
ggplot(violations_by_hour, aes(x = Hour_Label, y = Violators)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Toll Violations by Hour of the Day",
       x = "Hour", y = "Number of Violators") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# creating regression model to predict the violation.
# target variable is "VIOLATION"
# features are Hour, Day_Name,Month,FAC_B,TOTAL,CASH,EZPASS,TMAX,TMIN,PRCP,SNOW,AWND,Autos,Small_T,Large_T,Buses

library(dplyr)
model_data <- cleaned_data %>%
  select(VIOLATION, Hour, Day_Name, Month, FAC_B, TOTAL, CASH, EZPASS,
         Autos, Small_T, Large_T, Buses, TMAX, TMIN, PRCP, SNOW, AWND) %>%
  filter(!is.na(VIOLATION)) 
# Convert Categorical variable to Factors 

model_data$Day_Name<-as.factor(model_data$Day_Name)
model_data$FAC_B<-as.factor(model_data$FAC_B)

#  Fit a Linear Regression Model
violation_model <- lm(VIOLATION ~ ., data = model_data)
summary(violation_model)
# Make Predictions (Optional)

predicted_violations <- predict(violation_model, newdata = model_data)
predicted_violations

# Compare actual vs predicted
head(data.frame(Actual = model_data$VIOLATION, Predicted = round(predicted_violations)))
# Evaluate Model Accuracy

library(Metrics)
rmse <- rmse(model_data$VIOLATION, predicted_violations)
mae <- mae(model_data$VIOLATION, predicted_violations)

print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))

# we can see the MAE is 0 and MAE is near to zero which is extremely rear in the real world data
# it could be because of overfitting of the dataset or the model only memorizing the dataset.

# To overcome the situation we will split the dataset in two seet as TRAIN and TEST


# Split into train (80%) and test (20%)
set.seed(123)
sample_index <- sample(1:nrow(model_data), 0.8 * nrow(model_data))
train_data <- model_data[sample_index, ]
test_data <- model_data[-sample_index, ]

# Fit model on training data
violation_model_split <- lm(VIOLATION ~ ., data = train_data)

# Predict on test data
predictions_test <- predict(violation_model_split, newdata = test_data)

# Evaluate
library(Metrics)
rmse_val <- rmse(test_data$VIOLATION, predictions_test)
mae_val <- mae(test_data$VIOLATION, predictions_test)

print(paste("Test RMSE:", round(rmse_val, 2)))
print(paste("Test MAE:", round(mae_val, 2)))
 
# still we can see that the RMSE is 0 and MAE is nearly 0 which typically means one of the following happening

# Target Leakage => there might be one or more variable they are correlated with target "VIOLATION" column

# after reviewing the dataset we found that TOTAL = CASH + EZPASS + VIOLATION

# that means there are some independent variable they corelated with target variable "VIOLATION"

# We will ignore that variable from the model

model_data_revised <- model_data %>% select(-TOTAL,-CASH,-EZPASS)

# split the the dataset again

set.seed(123)
sample_index <- sample(1:nrow(model_data_revised), 0.8 * nrow(model_data_revised))
train_data <- model_data_revised[sample_index, ]
test_data <- model_data_revised[-sample_index, ]

# Train model

violation_model_revised <- lm(VIOLATION ~ ., data = train_data)

# Predict
predictions_test <- predict(violation_model_revised, newdata = test_data)

# Evaluation

library(Metrics)
rmse_val <- rmse(test_data$VIOLATION, predictions_test)
mae_val <- mae(test_data$VIOLATION, predictions_test)

round(rmse_val,2)
round(mae_val,2)


# Comments:This result indicating it can reasonably estimate the number of toll violations while capturing key influencing factors
#like time, facility, and weather. 

# creating the classification role

model_classification_data <- model_data_revised %>%
  mutate(Will_Violate = ifelse(VIOLATION > 0, 1, 0)) %>%
  select(-VIOLATION) 
# Convert Categorical Variables to Factors
model_classification_data$Day_Name <- as.factor(model_classification_data$Day_Name)
model_classification_data$FAC_B <- as.factor(model_classification_data$FAC_B)

# Split into Train/Test Sets
set.seed(123)
sample_idx <- sample(1:nrow(model_classification_data), 0.8 * nrow(model_classification_data))
train_data <- model_classification_data[sample_idx, ]
test_data <- model_classification_data[-sample_idx, ]

# trained logistic regressionn model
logit_model <- glm(Will_Violate ~ ., data = train_data, family = binomial)
summary(logit_model)

# Predict probabilities on test set
pred_probs <- predict(logit_model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions (cutoff 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- table(Predicted = pred_labels, Actual = test_data$Will_Violate)
print(conf_matrix)

# Accuracy
accuracy <- mean(pred_labels == test_data$Will_Violate)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

#Optional: ROC Curve and AUC


library(pROC)

roc_obj <- roc(test_data$Will_Violate, pred_probs)
plot(roc_obj, col = "blue", main = "ROC Curve - Logistic Regression")
auc_val <- auc(roc_obj)
print(paste("AUC:", round(auc_val, 3)))
















library(ggplot2)
test_results <- data.frame(Actual = test_data$VIOLATION, Predicted = round(predictions_test))

ggplot(test_results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Toll Violations",
       x = "Actual Violators", y = "Predicted Violators") +
  theme_minimal()















