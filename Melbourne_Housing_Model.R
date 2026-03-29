# ============================================================================
# Statistical Modelling of Melbourne Housing Prices
# Multiple Linear Regression Analysis in R
# Authors: Muhammad Owais Khan, Satyaki Mitra
# Course: Linear Regression & Time Series, Clark University
# ============================================================================

# Load libraries
library(ggplot2)
library(gridExtra)
library(scales)
library(leaps)
library(lmtest)
library(MASS)

# ============================================================================
# 1. Data Loading & Sampling
# ============================================================================

data1 <- read.csv("Melbourne_housing_FULL_cleaned.csv", header = TRUE)
set.seed(123)
data <- data1[sample(nrow(data1), 200), ]
attach(data)
names(data)

# ============================================================================
# 2. Feature Engineering
# ============================================================================

# Response variable
Y <- Price

# Centered variables (subtract mean for interpretability)
data$Rooms_c <- data$Rooms - mean(data$Rooms, na.rm = TRUE)
data$Distance_c <- data$Distance - mean(data$Distance, na.rm = TRUE)

# Binary indicators
data$Unit <- ifelse(data$Type == 'u', 1, 0)
data$Townhouse <- ifelse(data$Type == 't', 1, 0)
data$Southern_Metro <- ifelse(data$Regionname == 'Southern Metropolitan', 1, 0)

# Predictor aliases
data$X1 <- data$Rooms_c
data$X2 <- data$Distance_c
data$X3 <- data$Unit
data$X4 <- data$Townhouse
data$X5 <- data$Southern_Metro
data$X6 <- data$Bedroom2
data$X7 <- data$Bathroom
data$X8 <- data$Car
data$X9 <- data$Landsize
data$X10 <- data$BuildingArea

# Polynomial terms (additive)
data$X11 <- data$X1^2
data$X22 <- data$X2^2
data$X111 <- data$X1^3
data$X222 <- data$X2^3
data$X1111 <- data$X1^4
data$X2222 <- data$X2^4

# Pairwise interaction terms
data$X1X2 <- data$X1 * data$X2
data$X1X3 <- data$X1 * data$X3
data$X1X4 <- data$X1 * data$X4
data$X1X5 <- data$X1 * data$X5
data$X1X6 <- data$X1 * data$X6
data$X1X7 <- data$X1 * data$X7
data$X1X8 <- data$X1 * data$X8
data$X1X9 <- data$X1 * data$X9
data$X1X10 <- data$X1 * data$X10

data$X2X3 <- data$X2 * data$X3
data$X2X4 <- data$X2 * data$X4
data$X2X5 <- data$X2 * data$X5
data$X2X6 <- data$X2 * data$X6
data$X2X7 <- data$X2 * data$X7
data$X2X8 <- data$X2 * data$X8
data$X2X9 <- data$X2 * data$X9
data$X2X10 <- data$X2 * data$X10

data$X3X4 <- data$X3 * data$X4
data$X3X5 <- data$X3 * data$X5
data$X3X6 <- data$X3 * data$X6
data$X3X7 <- data$X3 * data$X7
data$X3X8 <- data$X3 * data$X8
data$X3X9 <- data$X3 * data$X9
data$X3X10 <- data$X3 * data$X10

data$X4X5 <- data$X4 * data$X5
data$X4X6 <- data$X4 * data$X6
data$X4X7 <- data$X4 * data$X7
data$X4X8 <- data$X4 * data$X8
data$X4X9 <- data$X4 * data$X9
data$X4X10 <- data$X4 * data$X10

data$X5X6 <- data$X5 * data$X6
data$X5X7 <- data$X5 * data$X7
data$X5X8 <- data$X5 * data$X8
data$X5X9 <- data$X5 * data$X9
data$X5X10 <- data$X5 * data$X10

data$X6X7 <- data$X6 * data$X7
data$X6X8 <- data$X6 * data$X8
data$X6X9 <- data$X6 * data$X9
data$X6X10 <- data$X6 * data$X10

data$X7X8 <- data$X7 * data$X8
data$X7X9 <- data$X7 * data$X9
data$X7X10 <- data$X7 * data$X10

data$X8X9 <- data$X8 * data$X9
data$X8X10 <- data$X8 * data$X10

data$X9X10 <- data$X9 * data$X10

# Triple interaction terms
data$X126 <- data$X1 * data$X2 * data$X6
data$X145 <- data$X1 * data$X4 * data$X5
data$X235 <- data$X2 * data$X3 * data$X5

# ============================================================================
# 3. Model Fitting
# ============================================================================

n <- length(Y)

# Full model (all predictors + interactions + polynomials)
model_final <- lm(Y ~
                    X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +
                    X11 + X22 + X111 + X222 + X1111 + X2222 +
                    X1X2 + X1X3 + X1X4 + X1X5 + X1X6 + X1X7 + X1X8 + X1X9 + X1X10 +
                    X2X3 + X2X4 + X2X5 + X2X6 + X2X7 + X2X8 + X2X9 + X2X10 +
                    X3X4 + X3X5 + X3X6 + X3X7 + X3X8 + X3X9 + X3X10 +
                    X4X5 + X4X6 + X4X7 + X4X8 + X4X9 + X4X10 +
                    X5X6 + X5X7 + X5X8 + X5X9 + X5X10 +
                    X6X7 + X6X8 + X6X9 + X6X10 +
                    X7X8 + X7X9 + X7X10 +
                    X8X9 + X8X10 +
                    X9X10 +
                    X126 + X145 + X235,
                  data = data)
summary(model_final)

# Additive model (no interactions)
model2 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X11 + X22 + X111 + X222 + X1111 + X2222,
             data = data)
summary(model2)

# Interaction model (no polynomial terms)
model1 <- lm(Y ~
               X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +
               X1X2 + X1X3 + X1X4 + X1X5 + X1X6 + X1X7 + X1X8 + X1X9 + X1X10 +
               X2X3 + X2X4 + X2X5 + X2X6 + X2X7 + X2X8 + X2X9 + X2X10 +
               X3X4 + X3X5 + X3X6 + X3X7 + X3X8 + X3X9 + X3X10 +
               X4X5 + X4X6 + X4X7 + X4X8 + X4X9 + X4X10 +
               X5X6 + X5X7 + X5X8 + X5X9 + X5X10 +
               X6X7 + X6X8 + X6X9 + X6X10 +
               X7X8 + X7X9 + X7X10 +
               X8X9 + X8X10 +
               X9X10 +
               X126 + X145 + X235,
             data = data)
summary(model1)

# ============================================================================
# 4. Model Comparison (ANOVA)
# ============================================================================

anova(model1)
anova(model2)
anova(model1, model2)

# ============================================================================
# 5. Visualization: Interaction vs Additive Model
# ============================================================================

data$Y_pred_model1 <- predict(model1, newdata = data)
data$Y_pred_model2 <- predict(model2, newdata = data)

p1 <- ggplot(data, aes(x = X1, y = Y, color = as.factor(X5))) +
  geom_point() +
  geom_line(aes(y = Y_pred_model1), size = 1) +
  labs(title = "Interaction Model",
       x = "Rooms (centered)", y = "Price (Millions)",
       color = "Southern Metro") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M", prefix = "$")) +
  theme_minimal()

p2 <- ggplot(data, aes(x = X1, y = Y, color = as.factor(X5))) +
  geom_point() +
  geom_line(aes(y = Y_pred_model2), size = 1) +
  labs(title = "Additive Model",
       x = "Rooms (centered)", y = "Price (Millions)",
       color = "Southern Metro") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M", prefix = "$")) +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)

# ============================================================================
# 6. Model Selection
# ============================================================================

# Check for linear dependencies
alias(model_final)

# Best subset selection
m3 <- regsubsets(Y ~
                   X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +
                   X11 + X22 + X111 + X222 + X1111 + X2222 +
                   X1X2 + X1X3 + X1X4 + X1X5 + X1X6 + X1X7 + X1X8 + X1X9 + X1X10 +
                   X2X3 + X2X4 + X2X5 + X2X6 + X2X7 + X2X8 + X2X9 + X2X10 +
                   X3X5 + X3X7 + X3X8 + X3X9 + X3X10 +
                   X4X5 + X4X7 + X4X8 + X4X9 + X4X10 +
                   X5X6 + X5X7 + X5X8 + X5X9 + X5X10 +
                   X6X7 + X6X8 + X6X9 + X6X10 +
                   X7X8 + X7X9 + X7X10 +
                   X8X9 + X8X10 +
                   X9X10 +
                   X126 + X145 + X235,
                 really.big = TRUE, data = data)
Leaps3 <- summary(m3)
with(Leaps3, round(cbind(which, rsq, adjr2, cp, bic), 3))

# Backward elimination (BIC)
backward.reg <- step(model_final, direction = "backward", k = log(n))

# Forward selection (AIC)
reg.null <- lm(Y ~ 1, data = data)
forward.reg <- step(reg.null, direction = "forward",
                    scope = list(upper = model_final, lower = reg.null))
summary(forward.reg)

# Stepwise regression (AIC)
stepwise_model <- step(model_final, direction = "both")

# ============================================================================
# 7. Final Model Fits from Selection Methods
# ============================================================================

backward_model <- lm(Y ~ X1 + X2 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X22 +
                       X2222 + X1X2 + X1X5 + X1X6 + X1X7 + X1X8 + X1X9 + X1X10 +
                       X2X3 + X2X4 + X2X6 + X3X5 + X5X6 + X6X7 + X6X8 + X6X9 +
                       X6X10 + X7X10,
                     data = data)
summary(backward_model)

forward_model <- lm(Y ~ X5X10 + X7 + X2X6 + X3X7 + X1111 + X7X10 + X22 + X1X7 +
                      X2X3 + X222 + X2X4 + X5X6 + X8 + X4X10 + X2X8 + X2X9 + X5X8,
                    data = data)
summary(forward_model)

stepwise_model <- lm(Y ~ X1 + X2 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X22 +
                       X111 + X1111 + X2222 + X1X2 + X1X5 + X1X6 + X1X7 + X1X8 +
                       X1X9 + X1X10 + X2X3 + X2X4 + X2X6 + X2X8 + X3X5 + X3X9 +
                       X4X5 + X4X7 + X4X10 + X5X6 + X5X9 + X5X10 + X6X7 + X6X8 +
                       X6X9 + X6X10 + X7X10 + X8X9 + X126,
                     data = data)
summary(stepwise_model)

anova(backward_model, forward_model, stepwise_model)

# ============================================================================
# 8. Residual Diagnostics (Stepwise Model)
# ============================================================================

# Normality check
residuals <- resid(stepwise_model)
qqnorm(residuals)
qqline(residuals, col = "red")
shapiro.test(residuals)

# Linearity check
plot(stepwise_model, which = 1)

# Homoscedasticity check
plot(stepwise_model, which = 3)
bptest(stepwise_model)

# Influential observations (Cook's distance)
plot(cooks.distance(stepwise_model),
     main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4 / (nrow(data) - length(coef(stepwise_model))), col = "red")

# ============================================================================
# 9. Box-Cox Transformation Analysis
# ============================================================================

boxcox(stepwise_model, lambda = seq(-2, 2, 0.1))

# Log transformation
stepwise_model_log <- lm(log(Y) ~ X1 + X2 + X5 + X6 + X7 + X8 + X9 + X10 + X11 +
                           X22 + X111 + X1111 + X2222 + X1X2 + X1X5 + X1X6 + X1X7 +
                           X1X8 + X1X9 + X1X10 + X2X3 + X2X4 + X2X6 + X2X8 +
                           X3X5 + X3X9 + X4X5 + X4X7 + X4X10 + X5X6 + X5X9 +
                           X5X10 + X6X7 + X6X8 + X6X9 + X6X10 + X7X10 + X8X9 + X126,
                         data = data)
summary(stepwise_model_log)
plot(stepwise_model_log, which = 3)

# Square root transformation
stepwise_model_sqrt <- lm(sqrt(Y) ~ X1 + X2 + X5 + X6 + X7 + X8 + X9 + X10 + X11 +
                            X22 + X111 + X1111 + X2222 + X1X2 + X1X5 + X1X6 + X1X7 +
                            X1X8 + X1X9 + X1X10 + X2X3 + X2X4 + X2X6 + X2X8 +
                            X3X5 + X3X9 + X4X5 + X4X7 + X4X10 + X5X6 + X5X9 +
                            X5X10 + X6X7 + X6X8 + X6X9 + X6X10 + X7X10 + X8X9 + X126,
                          data = data)
summary(stepwise_model_sqrt)

# Compare AIC across transformations
AIC(stepwise_model, stepwise_model_log, stepwise_model_sqrt)

# ============================================================================
# 10. Key Coefficient Analysis
# ============================================================================

summary(stepwise_model_log)$coefficients[c("X1", "X2", "X11", "X22", "X111", "X1111", "X2222", "X1X2"), ]
summary(stepwise_model_log)$coefficients[c("X1", "X2"), ]
summary(stepwise_model_log)$coefficients[c("X2X3", "X2X4", "X3X5", "X3X9"), ]
summary(stepwise_model_log)$coefficients[c("X7", "X6X7", "X7X10"), ]
summary(stepwise_model_log)$coefficients[c("X1X5"), ]

# ============================================================================
# 11. Predictions
# ============================================================================

pred_log <- predict(stepwise_model_log, newdata = data, interval = "prediction")
pred_price <- exp(pred_log)
data_with_predictions <- cbind(data, pred_price)
head(data_with_predictions[, c("Price", "fit", "lwr", "upr")])
