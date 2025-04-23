library(dplyr)
library(caTools)
library(Metrics)

head(data)

target_col <- "key_events"
feature_cols <- c("ads_cost",
                  "ads_impressions",
                  "ads_clicks",
                  "ads_cpc")

model_data <- data %>%
  select(all_of(c(target_col, feature_cols))) %>%
  na.omit()

set.seed(42)
sample <- sample.split(model_data[[target_col]], SplitRatio = 0.70)

train <- subset(model_data, sample == TRUE)
test  <- subset(model_data, sample == FALSE)

formula_str <- paste(target_col, "~", paste(feature_cols, collapse = " + "))
model_formula <- as.formula(formula_str)

lm_model <- lm(model_formula, data = train)
summary(lm_model)

test_predictions <- predict(lm_model, newdata = test)

SSE <- sum((test[[target_col]] - test_predictions)^2)  
SST <- sum((test[[target_col]] - mean(train[[target_col]]))^2)  
R2  <- 1 - (SSE/SST)

model_rmse <- rmse(actual = test[[target_col]],
                   predicted = test_predictions)
model_mae <- mae(actual = test[[target_col]],
                 predicted = test_predictions)

cat("\n---------------------------------\n")
cat("Model Performance on Test Set:\n")
cat("---------------------------------\n")
cat("R-squared:      ", round(R2, 3), "\n")
cat("RMSE:           ", round(model_rmse, 3), "\n")
cat("MAE:            ", round(model_mae, 3), "\n")

cat("\nModel Coefficients:\n")
print(coef(lm_model))

cat("\nIntercept (beta_0):\n")
print(coef(lm_model)[1])




