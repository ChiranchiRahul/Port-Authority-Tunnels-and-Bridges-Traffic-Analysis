# Install (if not already installed)
install.packages("xgboost", repos = "https://cloud.r-project.org/")
install.packages("SHAPforxgboost", repos = "https://cloud.r-project.org/")
install.packages("caret", repos = "https://cloud.r-project.org/")
install.packages("Metrics", repos = "https://cloud.r-project.org/")
install.packages("dplyr", repos = "https://cloud.r-project.org/")
install.packages("readxl", repos = "https://cloud.r-project.org/")

# Load libraries
library(readxl)
library(dplyr)
library(caret)
library(xgboost)
library(Metrics)
library(SHAPforxgboost)

# Load data
data <- read_excel("tbl_FinalDataset_PortAuthority.xlsx")

# Select only the required variables
selected_vars <- c(
  "Total_Traffic",       # target
  "FAC_B",               # categorical
  "Month_Name",          # categorical
  "Month",               # numeric
  "Yr",                  # numeric
  "Day_Name",            # categorical
  "Weekend_Weekday",     # categorical
  "Weekend_Flag",        # binary
  "Event_Flag",          # binary
  "TMAX", "TMIN",        # numeric
  "PRCP", "SNOW","SNWD",
 "AWND" # numeric
)

data_clean <- data %>% select(all_of(selected_vars))

# Convert necessary columns to factors
data_clean$FAC_B <- as.factor(data_clean$FAC_B)
data_clean$Month_Name <- as.factor(data_clean$Month_Name)
data_clean$Day_Name <- as.factor(data_clean$Day_Name)
data_clean$Weekend_Weekday <- as.factor(data_clean$Weekend_Weekday)
data_clean$Weekend_Flag <- as.integer(data_clean$Weekend_Flag)
data_clean$Event_Flag <- as.integer(data_clean$Event_Flag)

# One-hot encode categorical variables
dummies <- dummyVars(Total_Traffic ~ ., data = data_clean)
data_encoded <- data.frame(predict(dummies, newdata = data_clean))
data_encoded$Total_Traffic <- data_clean$Total_Traffic

# Train-test split
set.seed(123)
trainIndex <- createDataPartition(data_encoded$Total_Traffic, p = 0.8, list = FALSE)
train <- data_encoded[trainIndex, ]
test <- data_encoded[-trainIndex, ]

# Prepare matrices for XGBoost
train_x <- as.matrix(train[, -ncol(train)])
train_y <- train$Total_Traffic
test_x <- as.matrix(test[, -ncol(test)])
test_y <- test$Total_Traffic

# Train XGBoost model
xgb_model <- xgboost(
  data = train_x,
  label = train_y,
  nrounds = 100,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  verbose = 0
)

# Predict and evaluations(metrics)
pred_xgb <- predict(xgb_model, test_x)
rmse <- RMSE(pred_xgb, test_y)
mae <- MAE(pred_xgb, test_y)
r2 <- R2(pred_xgb, test_y)

cat("\n✅ XGBoost Model Performance (Refined Variables):\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE :", round(mae, 2), "\n")
cat("R²  :", round(r2, 4), "\n")

# SHAP analysis
shap_values <- shap.values(xgb_model = xgb_model, X_train = train_x)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = train_x)

# SHAP plot
shap.plot.summary(shap_long)

#For Power BI Insights(Adds predicted traffic and error to the test dataset)
test_results <- test
test_results$Predicted_Traffic <- pred_xgb
test_results$Error <- test_results$Total_Traffic - test_results$Predicted_Traffic

#Saving Predections as CSV
write.csv(test_results, "PowerBI_Traffic_Predictions.csv", row.names = FALSE)

