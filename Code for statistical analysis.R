library(stats)
library(ggplot2)
library(dplyr)
library(ggplotify)
library(carData)
library(latticeExtra)
library(pROC)

data <- read.csv("file.csv")

# Linear regression based independent feature screening########################
data <- read.csv("file.csv") # data loading, change the file name accordingly
features <- data[, 2:ncol(data)]  # feature were listed starting from the 2nd row
response <- data$Response 

# Univariate feature selection based feature screening#########################
regression_results <- data.frame(feature = character(0), slope = numeric(0), intercept = numeric(0), r_squared = numeric(0))

for (i in 1:ncol(features)) {
  model <- lm(response ~ features[, i])
  slope <- coef(model)[2]  # slope
  intercept <- coef(model)[1]  # intercept
  r_squared <- summary(model)$r.squared  # r-square
  feature_name <- colnames(features)[i]
  regression_results <- rbind(regression_results, c(feature_name, slope, intercept, r_squared))
}

colnames(regression_results) <- c("Feature", "Slope", "Intercept", "R-squared")
regression_results
print(regression_results)
# data output, set the name of the file accordingly
write.csv(regression_results, file = "file_name.csv", row.names = FALSE)

# Logistic regression based feature selection##################################
data <- read.csv("file_name.csv") # data loading, change the file name accordingly
features <- data[, 2:ncol(data)]  # feature were listed starting from the 2nd row
response <- data$Response  

regression_results <- data.frame(feature = character(0), coefficient = numeric(0), p_value = numeric(0), r_squared = numeric(0))


for (i in 1:ncol(features)) { # adopt logistic regression on each feature
  model <- glm(response ~ features[, i], family = binomial) 
  coefficient <- coef(model)[2]  
  p_value <- summary(model)$coefficients[2, 4]  
  r_squared <- NA 
  feature_name <- colnames(features)[i]
  regression_results <- rbind(regression_results, c(feature_name, coefficient, p_value, r_squared))
}


colnames(regression_results) <- c("Feature", "Coefficient", "P-Value", "R-Squared")

print(regression_results)

write.csv(regression_results, file = "regression_results_binary.csv", row.names = FALSE)

ggplot(data, aes(x = RTs., y = Response)) +
  geom_point(size = 4) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "black", size = 2) +
  labs(x = "RTs+", y = "Probability") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 2), 
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14)  
  ) # imaging on specific features, here we adopt RTS+ due to its lowest P-value

glm_model <- glm(Response ~ RTs., data = data, family = "binomial")

summary(glm_model)

predicted_prob <- predict(glm_model, type = "response")

roc_curve <- roc(data$Response, predicted_prob)
auc_value <- auc(roc_curve)

plot(roc_curve, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE,
     auc.polygon.col = "skyblue", auc.polygon.alpha = 0.3,
     auc.polygon.border = "blue", legacy.axes = TRUE)


# Batched linear regression based analysis#####################################
data <- read.csv("file.csv") # data loading, change the file name accordingly
feature_names <- names(data)[-1]

for (feature in feature_names) {
  plot_data <- data.frame(Response = data$Response, Feature = data[[feature]])

  print(
    ggplot(plot_data, aes(x = Feature, y = Response)) +
      geom_point(size = 4) +  
      geom_smooth(method = "lm", se = TRUE, color = "black", fill = "grey") + 
      labs(title = paste("Linear Regression:", feature)) +
      xlab(feature) +
      ylab("Response") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 2),  
        plot.title = element_text(hjust = 0.5),  
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15) 
      )
  )
}

################################################################################