library(readxl)
library(Metrics)
library(e1071)
library(tseries)
library(openxlsx)
library(neuralnet)

data <- read_excel('Irish_data.xlsx')
data$MW_diff <- c(0, diff(data$MW, 1))
set.seed(01112001)

#train test split
train_unscaled <- data[1:17,]
test_unscaled <- data[18:21,]

#looking for correlation
cor(data[,2:4])

mean(data$MW)

plot(x=data$year, y=data$MW, type='b' ,xlab='Year', ylab='Waste (kg/capita)', 
     xaxt = "n", cex.lab=1.2)
axis(1, at = data$year, labels = FALSE, tcl = -0.5) #tick marks for every year
text(
  x=data$year[data$year%%2==0],      #positions for labels
  y=538,                             #slightly below the axis
  labels=data$year[data$year%%2==0], #adding the labels
  srt=45,                            #rotation angle in degrees
  adj=0.5,                           #alignment (0.5=centered)
  xpd=TRUE,                          #allow drawing outside the plot area
  cex=0.9                            #size of the text
)

#scaling
train_scaled <- as.data.frame(scale(train_unscaled))
train_mean <- sapply(train_unscaled, mean)
train_sd <- sapply(train_unscaled, sd)
test_scaled <- as.data.frame(scale(test_unscaled, center=train_mean, 
                                   scale=train_sd))

#fitting SVM ----
svm_model <- svm(MW~GDP+population+unemployment, 
                 data=train_scaled, type='eps-regression', kernal='radial')

predictions_svm <- predict(svm_model, train_scaled)

predictions_unscaled_svm <- predictions_svm*sd(train_unscaled$MW)+mean(train_unscaled$MW)
residuals_svm <- train_unscaled$MW-predictions_unscaled_svm
plot(predictions_unscaled_svm, residuals_svm, xlab='Fitted values (kg/capita)', 
      ylab='Residuals (kg/capita)', xaxt="n", yaxt="n", cex.lab=1.2)
axis(1, at = seq(575, 800, by = 25))
axis(2, at = seq(-60, 40, by = 20))

# Compute R^2
SSE <- sum((train_unscaled$MW-predictions_unscaled_svm)^2)
SST <- sum((train_unscaled$MW-mean(train_unscaled$MW))^2)
R2 <- 1-SSE/SST
cat("R^2:", R2, "\n") #very high

rmse(train_unscaled$MW, predictions_unscaled_svm)
mae(train_unscaled$MW, predictions_unscaled_svm)
mse(train_unscaled$MW, predictions_unscaled_svm)

plot(train_unscaled$MW, type='l')
lines(predictions_unscaled_svm, col='red')

#test set
predictions_test_svm <- predict(svm_model, test_scaled)
actual_test <- test_scaled$MW

predictions_unscaled_test_svm <- predictions_test_svm*sd(train_unscaled$MW)+mean(train_unscaled$MW)
residuals_test_svm <- test_unscaled$MW-predictions_unscaled_test_svm
plot(predictions_unscaled_test_svm, residuals_test_svm, xlab='Fitted', 
     ylab='Residuals', main = 'Fitted vs Residuals - Test (svm)')

# Compute R^2
SSE <- sum((test_unscaled$MW-predictions_unscaled_test_svm)^2)
SST <- sum((test_unscaled$MW-mean(test_unscaled$MW))^2)
R2 <- 1-SSE/SST
cat("R^2:", R2, "\n")

rmse(test_unscaled$MW,predictions_unscaled_test_svm)
mae(test_unscaled$MW,predictions_unscaled_test_svm)
mse(test_unscaled$MW,predictions_unscaled_test_svm)


plot(test_unscaled$MW, type='l')
lines(predictions_unscaled_test_svm, col='red')

#mean baseline model----

#training set
mean_baseline_train <- rep(mean(train_unscaled$MW), nrow(train_unscaled))

#train metrics
SSE <- sum((train_unscaled$MW - mean_baseline_train)^2)
SST <- sum((train_unscaled$MW - mean(train_unscaled$MW))^2)
R2 <- 1 - SSE/SST
cat("R^2:", R2, "\n")
rmse(train_unscaled$MW, mean_baseline_train)
mae(train_unscaled$MW, mean_baseline_train)
mse(train_unscaled$MW, mean_baseline_train)


#test set
mean_baseline_test <- rep(mean(train_unscaled$MW), nrow(test_unscaled))

#test metrics
SSE <- sum((test_unscaled$MW - mean_baseline_test)^2)
SST <- sum((test_unscaled$MW - mean(test_unscaled$MW))^2)
R2 <- 1 - SSE/SST
cat("R^2:", R2, "\n")
rmse(test_unscaled$MW, mean_baseline_test)
mae(test_unscaled$MW, mean_baseline_test)
mse(test_unscaled$MW, mean_baseline_test)





#Naive Baseline-----
#train - predict y_{t-1}
naive_baseline_train <- c(NA, head(train_unscaled$MW, -1))

#test - last value from training, then rolling forward
naive_baseline_test <- c(tail(train_unscaled$MW, 1), head(test_unscaled$MW, -1))

# Train metrics (excluding first NA)
SSE <- sum((train_unscaled$MW[-1] - naive_baseline_train[-1])^2)
SST <- sum((train_unscaled$MW[-1] - mean(train_unscaled$MW[-1]))^2)
R2 <- 1 - SSE/SST
cat("R^2:", R2, "\n")
rmse(train_unscaled$MW[-1], naive_baseline_train[-1])
mae(train_unscaled$MW[-1], naive_baseline_train[-1])
mse(train_unscaled$MW[-1], naive_baseline_train[-1])


# Test metrics
SSE <- sum((test_unscaled$MW - naive_baseline_test)^2)
SST <- sum((test_unscaled$MW - mean(test_unscaled$MW))^2)
R2 <- 1 - SSE/SST
cat("R^2:", R2, "\n")
rmse(test_unscaled$MW, naive_baseline_test)
mae(test_unscaled$MW, naive_baseline_test)
mse(test_unscaled$MW, naive_baseline_test)




#Neural Network------
set.seed(20229798)
nnet_model <- neuralnet(MW~GDP+population+unemployment,
                        data=train_scaled, hidden=2, stepmax=5000 , rep=10, 
                        threshold=0.01)

predictions_nnet <- predict(nnet_model, train_scaled)

predictions_unscaled_nnet <- predictions_nnet*sd(train_unscaled$MW)+mean(train_unscaled$MW)
residuals_nnet <- train_unscaled$MW-predictions_unscaled_nnet
plot(predictions_unscaled_nnet, residuals_nnet, xlab='Fitted values (kg/capita)', 
     ylab='Residuals (kg/capita)', xaxt="n", yaxt="n", cex.lab=1.2)
axis(1, at = seq(575, 800, by = 25))
axis(2, at = seq(-60, 40, by = 20))

# Compute R^2
SSE <- sum((train_unscaled$MW-predictions_unscaled_nnet)^2)
SST <- sum((train_unscaled$MW-mean(train_unscaled$MW))^2)
R2 <- 1-SSE/SST
cat("R^2:", R2, "\n") #very high

rmse(train_unscaled$MW, predictions_unscaled_nnet)
mae(train_unscaled$MW, predictions_unscaled_nnet)
mse(train_unscaled$MW, predictions_unscaled_nnet)


plot(train_unscaled$MW, type='l')
lines(predictions_unscaled_nnet, col='red')

#test set
predictions_test_nnet <- predict(nnet_model, test_scaled)
actual_test <- test_scaled$MW

predictions_unscaled_test_nnet <- predictions_test_nnet*sd(train_unscaled$MW)+mean(train_unscaled$MW)
residuals_test_nnet <- test_unscaled$MW-predictions_unscaled_test_nnet
plot(predictions_unscaled_test_nnet, residuals_test_nnet, xlab='Fitted', 
     ylab='Residuals', main = 'Fitted vs Residuals - Test (nnet)')

# Compute R^2
SSE <- sum((test_unscaled$MW-predictions_unscaled_test_nnet)^2)
SST <- sum((test_unscaled$MW-mean(test_unscaled$MW))^2)
R2 <- 1-SSE/SST
cat("R^2:", R2, "\n")

rmse(test_unscaled$MW,predictions_unscaled_test_nnet)
mae(test_unscaled$MW,predictions_unscaled_test_nnet)
mse(test_unscaled$MW,predictions_unscaled_test_nnet)


plot(test_unscaled$MW, type='l')
lines(predictions_unscaled_test_nnet, col='red')




#plots----
#dependent
png('dependent.png', width = 8, height = 6, units = 'in', res = 600)
# Better margins for rotated labels
par(mar = c(6, 5, 2, 1)) 
plot(
  x = data$year,
  y = data$MW,
  type = 'b',
  xlab = 'Year',
  ylab = 'Waste (kg/capita)',
  xaxt = "n",
  cex.lab = 1.5,       # larger axis labels
  cex.axis = 1.2,      # larger tick text
  cex = 1.2,           # larger points
  pch = 16,            # solid points look better in print
  lwd = 2              # thicker line improves visibility
)

# Year tick marks
axis(1, at = data$year, labels = FALSE, tcl = -0.4)

# Rotated labels for alternate years
text(
  x = data$year[data$year %% 2 == 0],
  y = min(data$MW) - (0.07 * diff(range(data$MW))), # auto-placement below axis
  labels = data$year[data$year %% 2 == 0],
  srt = 45,
  adj = 1,
  xpd = TRUE,
  cex = 1.2
)
dev.off()




#resids plot
png('resids.png', width = 8, height = 6, units = 'in', res = 600)
par(mar = c(5, 5, 2, 1))
plot(
  x = predictions_unscaled_svm,
  y = residuals_svm,
  xlab = 'Fitted values (kg/capita)',
  ylab = 'Residuals (kg/capita)',
  xaxt = "n",
  yaxt = "n",
  cex.lab = 1.5,         # larger axis labels
  cex.axis = 1.2,        # larger tick text
  cex = 1.2,             # bigger points
  pch = 16,              # solid points (better print visibility)
  lwd = 2                # heavier point borders if needed
)

# X-axis ticks
axis(1,
     at = seq(575, 800, by = 25),
     cex.axis = 1.2,
     tcl = -0.3)

# Y-axis ticks
axis(2,
     at = seq(-60, 40, by = 20),
     cex.axis = 1.2,
     tcl = -0.3)

# Add horizontal zero-line (important in residual plots)
abline(h = 0, lwd = 2, lty = 2)
dev.off()


# predicttions
# Set up a 2×2 grid
png("model_predictions_grid.png", width = 10, height = 10, units = "in", res = 600) 
par(mfrow = c(2, 2), mar = c(5, 5, 4, 1))

years <- test_unscaled$year

## --- 1. Mean Baseline (a) ---
plot(test_unscaled$year, test_unscaled$MW, type = "b", pch = 16, ylim = c(570, 670),
     xlab = "Year", ylab = "Waste (kg/capita)",
     main = "(a) Mean Baseline", cex.lab = 1.4, cex.main = 1.4, xaxt = "n")
axis(1, at = years, labels = years, cex.axis = 1.1)
lines(test_unscaled$year, mean_baseline_test, col = "red", lwd = 2, type = "b", pch = 16)
legend("bottomright", legend = c("Observed", "Predicted"), 
       col = c("black", "red"), pch = 16, lty = 1, bty = "n", cex = 1.1)

## --- 2. Naive Baseline (b) ---
plot(test_unscaled$year, test_unscaled$MW, type = "b", pch = 16, ylim = c(570, 670),
     xlab = "Year", ylab = "Waste (kg/capita)",
     main = "(b) Naive Baseline", cex.lab = 1.4, cex.main = 1.4, xaxt = "n")
axis(1, at = years, labels = years, cex.axis = 1.1)
lines(test_unscaled$year, naive_baseline_test, col = "red", lwd = 2, type = "b", pch = 16)
legend("bottomright", legend = c("Observed", "Predicted"), 
       col = c("black", "red"), pch = 16, lty = 1, bty = "n", cex = 1.1)

## --- 3. SVM (c) ---
plot(test_unscaled$year, test_unscaled$MW, type = "b", pch = 16, ylim = c(570, 670),
     xlab = "Year", ylab = "Waste (kg/capita)",
     main = "(c) SVM Model", cex.lab = 1.4, cex.main = 1.4, xaxt = "n")
axis(1, at = years, labels = years, cex.axis = 1.1)
lines(test_unscaled$year, predictions_unscaled_test_svm, col = "red", lwd = 2, type = "b", pch = 16)
legend("bottomright", legend = c("Observed", "Predicted"), 
       col = c("black", "red"), pch = 16, lty = 1, bty = "n", cex = 1.1)

## --- 4. Neural Network (d) ---
plot(test_unscaled$year, test_unscaled$MW, type = "b", pch = 16, ylim = c(570, 670),
     xlab = "Year", ylab = "Waste (kg/capita)",
     main = "(d) Neural Network", cex.lab = 1.4, cex.main = 1.4, xaxt = "n")
axis(1, at = years, labels = years, cex.axis = 1.1)
lines(test_unscaled$year, predictions_unscaled_test_nnet, col = "red", lwd = 2, type = "b", pch = 16)
legend("bottomright", legend = c("Observed", "Predicted"), 
       col = c("black", "red"), pch = 16, lty = 1, bty = "n", cex = 1.1)

dev.off()
