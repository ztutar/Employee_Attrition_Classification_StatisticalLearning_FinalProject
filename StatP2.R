# Libraries
library(corrplot)
library(pROC)
library(class)
library(e1071)
library(MASS)

# Dataset
data <- read.csv("C://Users//ayse-//Downloads//Employee Attrition Classification Dataset//Employee Attrition Classification Dataset//data.csv")

# EDA Basics
str(data)
summary(data)

# Copy Data
data_detailed<- data

# Drop Employee Id
data <- data[, !names(data) %in% "Employee.ID"]
data <- data[, !names(data) %in% "Company.Tenure"]
data_detailed <- data_detailed[, !names(data_detailed) %in% "Employee.ID"]
data_detailed <- data_detailed[, !names(data_detailed) %in% "Company.Tenure"]

# Apply the mapping to the Education.Level column --- for label encoding
education_mapping <- c("Associate Degree" = "Assoc",
                       "Bachelor???s Degree" = "Bsc",
                       "Master???s Degree" = "Msc",
                       "PhD" = "PhD",
                       "High School" = "HS")
data$Education.Level <- education_mapping[data$Education.Level]
data_detailed$Education.Level <- education_mapping[data_detailed$Education.Level]

# Even they are numeric they represents categories
data$Number.of.Promotions <- factor(data$Number.of.Promotions)
data$Number.of.Dependents <- factor(data$Number.of.Dependents)
data_detailed$Number.of.Promotions <- factor(data_detailed$Number.of.Promotions)
data_detailed$Number.of.Dependents <- factor(data_detailed$Number.of.Dependents)

# Numeric and categorical val. separation
numeric_vars <- sapply(data, is.numeric)
categoric_vars <- sapply(data, function(x) is.factor(x) || is.character(x))

# Taking names from numeric and categorical data for dist. graph
categoric_var_names <- names(data)[categoric_vars]
numeric_var_names <- names(data)[numeric_vars]

# Numeric val. summary
summary(data[, numeric_vars])

# Missing Values --- No null Values
na_summary <- sapply(data, function(x) sum(is.na(x)))
na_summary

# Categorical val. dist.
for (var in categoric_var_names) {
  cat("\nDistribution of", var, ":\n")
  print(table(data[[var]]))
}

# Categorical val. dist.--barplot
par(mfrow = c(2, 2))  
for (cat_var in categoric_var_names) {
  barplot(table(data[[cat_var]]), main=paste(cat_var, "Distribution"), xlab=cat_var, col="cyan")
}
par(mfrow = c(1, 1))  

# Numeric features--hist graph
plots_per_page <- 4
num_plots <- length(numeric_var_names)
num_pages <- ceiling(num_plots / plots_per_page)
plot_index <- 1
for (page in 1:num_pages) {
  par(mfrow = c(2, 2))  
  for (i in 1:plots_per_page) {
    if (plot_index > num_plots) break
    num_var <- numeric_var_names[plot_index]
    hist(data[[num_var]], main=paste(num_var, "Distribution"), xlab=num_var, col="cyan", breaks=30)
    
    plot_index <- plot_index + 1
  }
}
par(mfrow = c(1, 1))  

# Target dist
barplot(table(data$Attrition), main="Attrition Count", xlab="Attrition", ylab="Count", col="lightblue")

# Target dist - Pie chart
attrition_table <- table(data$Attrition)
attrition_df <- as.data.frame(attrition_table)
colnames(attrition_df) <- c("Attrition", "Count")
attrition_df$Percentage <- round(100 * attrition_df$Count / sum(attrition_df$Count), 1)

pie(attrition_df$Count, labels=paste(attrition_df$Attrition, " - ", attrition_df$Percentage, "%"), col=c("lightblue", "lightcoral"), main="Attrition Distribution", cex=1.5, radius=1)

# Outlier Analysis 
par(mfrow = c(2, 2))  
for (num_var in numeric_var_names) {
  boxplot(data[[num_var]], main=paste(num_var, "Boxplot"), xlab=num_var, col="lightblue",horizontal = TRUE)
}
par(mfrow = c(1, 1)) 

# Target Visualization with Numeric features Outlier Check --boxplot
cat_var <- "Attrition"
plots_per_page <- 4
num_plots <- length(numeric_var_names)
num_pages <- ceiling(num_plots / plots_per_page)
plot_index <- 1
for (page in 1:num_pages) {
  par(mfrow = c(2, 2))  
  for (i in 1:plots_per_page) {
    if (plot_index > num_plots) break
    num_var <- numeric_var_names[plot_index]
    boxplot(data[[num_var]] ~ data[[cat_var]], main=paste(num_var, "by", cat_var), xlab=cat_var, ylab=num_var, col="lightblue",horizontal = TRUE)
    plot_index <- plot_index + 1
  }
}
par(mfrow = c(1, 1))  

# Function to identify outliers using IQR
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  outliers <- x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)]
  return(outliers)
}

# Identify and show outliers for each numeric variable
outliers_list <- list()
for (var in numeric_var_names) {
  outliers <- identify_outliers(data[[var]])
  outliers_list[[var]] <- outliers
  cat("\nOutliers in", var, ":\n")
  print(outliers)
}

# Plot histograms and highlight outliers
plots_per_page <- 4
num_plots <- length(numeric_var_names)
num_pages <- ceiling(num_plots / plots_per_page)
plot_index <- 1
for (page in 1:num_pages) {
  par(mfrow = c(2, 2))  
  for (i in 1:plots_per_page) {
    if (plot_index > num_plots) break
    num_var <- numeric_var_names[plot_index]
    
    hist(data[[num_var]], main=paste(num_var, "Distribution"), xlab=num_var, col="cyan", breaks=30)
    outliers <- outliers_list[[num_var]]
    if (length(outliers) > 0) {
      points(outliers, rep(0, length(outliers)), col="red", pch=16)
    }
    
    plot_index <- plot_index + 1
  }
}
par(mfrow = c(1, 1))

# Define bins for numeric values
data_detailed$Age_Cat <- cut(data_detailed$Age, breaks = c(18, 25, 35, 45, 55, 60), labels = c("18-25", "25-35", "35-45", "45-55", "55-60"), right = FALSE)
data_detailed$MonthlyIncome_Cat <- cut(data_detailed$Monthly.Income, breaks = c(0, 3000, 6000, 9000, 12000, 15000,18000), labels = c("0-3000", "3000-6000", "6000-9000", "9000-12000", "12000-15000","15000+"), right = FALSE)
data_detailed$DistanceFromHome_Cat <- cut(data_detailed$Distance.from.Home, breaks = c(0, 20, 40, 60, 80, 100), labels = c("0-20", "20-40", "40-60", "60-80", "80-100"), right = FALSE)
data_detailed$YearsAtCompany_Cat <- cut(data_detailed$Years.at.Company, breaks = c(0, 10, 20, 30, 40, 50,60), labels = c("0-10", "10-20", "20-30", "30-40", "40-50","50-60"), right = FALSE)

exclude_columns <- c("Age", "Monthly.Income", "Distance.from.Home", "Years.at.Company", "Attrition")
exclude_columns1 <- c("Age", "Monthly.Income", "Distance.from.Home", "Years.at.Company")
data_corr <- data_detailed[, !names(data_detailed) %in% exclude_columns1]
data_corr <- data_corr[, c(setdiff(names(data_corr), "Attrition"), "Attrition")]

# Plotting with target feature after transform numeric features into categorical
par(mfrow = c(2, 2))  
for (col in setdiff(names(data_detailed), exclude_columns)) {
  table_left <- table(data_detailed[data_detailed$Attrition == "Left", col])
  table_stayed <- table(data_detailed[data_detailed$Attrition == "Stayed", col])
  
  barplot(rbind(table_left, table_stayed), beside = TRUE,
          main = paste(col, "Distribution by Attrition"),
          xlab = col,
          col = c("red", "green"))
}
par(mfrow = c(1, 1))

# Cor. and Cov. -- Cor.Matrix for All 

#Label Encoding for All
for (var in names(data_corr)) {
  data_corr[[var]] <- as.numeric(factor(data_corr[[var]]))
}

cov_matrix <- cov(data_corr)
cor_matrix <- cor(data_corr)
print("Covariance Matrix:")
print(cov_matrix)
print("Correlation Matrix:")
print(cor_matrix)

corrplot(cor_matrix, method = 'color', type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7,tl.cex = 0.6)

# Cor. Matrix by Target Feature
left_data <- data_corr[data_corr$Attrition == 1, ]
stayed_data <- data_corr[data_corr$Attrition == 2, ]
left_data <- left_data[, !names(left_data) %in% "Attrition"]
stayed_data <- stayed_data[, !names(stayed_data) %in% "Attrition"]

left_cor <- cor(left_data)
stayed_cor <- cor(stayed_data)

corrplot(left_cor, method = 'color', type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7,tl.cex = 0.6,
         title = "Correlation Matrix - Left Employees")
corrplot(stayed_cor, method = 'color', type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7,tl.cex = 0.6,
         title = "Correlation Matrix - Stayed Employees")



################## PREPARE MODELLING ##############################################################
data$Attrition <- ifelse(data$Attrition == "Left", 0, 1)
categorical_data <- data[categoric_vars]
categorical_data <- categorical_data[, !names(categorical_data) %in% "Attrition"]

#One-Hot Encoding
categorical_data_encoded <- model.matrix(~ . - 1, data = categorical_data)

# Normalize Numeric Features between 0 and 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
numeric_data <- data[numeric_vars]
numeric_data_normalized <- as.data.frame(lapply(data[, numeric_vars], normalize))

# Concat finalized data
data_model <- cbind(numeric_data_normalized, categorical_data_encoded, Attrition = data$Attrition)

# Split the data train+test
set.seed(123)
train_index <- sample(seq_len(nrow(data_model)), size = 0.8 * nrow(data_model))
train_data <- data_model[train_index, ]
test_data <- data_model[-train_index, ]

# Cross-validation with K-fold (10-fold in this case)
k <- 10
set.seed(17)
cv_error <- cv.glm(data, logistic_model, K = k)$delta[1]
cv_error

# Function to compute performance measures
cv_performance <- function(data, indices) {
  train_data <- data[indices, ]  # training set
  test_data <- data[-indices, ]  # testing set

################## MODELLING ##############################################################
# Logistic Regression
logistic_model <- glm(Attrition ~ ., data = train_data, family = binomial)
logistic_prob <- predict(logistic_model, newdata = test_data, type = "response")
logistic_pred <- ifelse(logistic_prob > 0.5, 1, 0)

# Confusion Matrix
conf_matrix_log <- table(logistic_pred, test_data$Attrition)
conf_matrix_log

conf_matrix_log <- conf_matrix_log[c(1, 0) + 1, c(1, 0) + 1]
conf_matrix_log <- addmargins(conf_matrix_log, margin = c(1, 2))
print("Rearranged Confusion Matrix with Margins:")
print(conf_matrix_log)

TP <- conf_matrix_log["1", "1"]  # True Positives
TN <- conf_matrix_log["0", "0"]  # True Negatives
FP <- conf_matrix_log["1", "0"]  # False Positives
FN <- conf_matrix_log["0", "1"]  # False Negatives


# Performance Measures
accuracy_log <- (TP + TN) / (TP + TN + FP + FN)
  #sum(diag(conf_matrix_log)) / sum(conf_matrix_log)

overall_error_rate_log <- 1 - accuracy_log  #(FP + FN) / sum(conf_matrix)

PPV <- TP / (TP + FP) # Positive Predictive Value (Precision)
TPR <- TP / (TP + FN) # True Positive Rate (Sensitivity, Recall)
F1 <- 2 * PPV * TPR / (PPV + TPR)
TNR <- TN / (TN + FP) # True Negative Rate (Specificity)
FPR <- FP / (FP + TN) # False Positive Rate

cat("Log Reg. Performance:\n")
cat("Accuracy: ", accuracy_log, "\n")
cat("Overall Error Rate: ", overall_error_rate_log, "\n")
cat("Precision (PPV): ", PPV, "\n")
cat("Recall (TPR): ", TPR, "\n")
cat("F1 Score: ", F1, "\n")
cat("Specificity (TNR): ", TNR, "\n")
cat("False Positive Rate (FPR): ", FPR, "\n")


# ROC curve
roc_out <- roc(test_data$Attrition, logistic_prob, levels = c("0", "1"))
plot(roc_out, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate", col = "blue", main = "ROC Curve")
auc(roc_out)


# LDA
lda_model <- lda(Attrition ~ ., data = train_data)
lda_pred <- predict(lda_model, newdata = test_data)$class

conf_matrix_lda <- table(lda_pred, test_data$Attrition)
conf_matrix_lda

TP <- conf_matrix_lda["1", "1"] 
TN <- conf_matrix_lda["0", "0"] 
FP <- conf_matrix_lda["1", "0"]  
FN <- conf_matrix_lda["0", "1"]

accuracy_lda <- (TP + TN) / (TP + TN + FP + FN)
overall_error_rate_lda <- 1 - accuracy_lda

PPV <- TP / (TP + FP)
TPR <- TP / (TP + FN)
F1 <- 2 * PPV * TPR / (PPV + TPR)
TNR <- TN / (TN + FP)
FPR <- FP / (FP + TN)

cat("LDA Performance:\n")
cat("Accuracy: ", accuracy_lda, "\n")
cat("Overall Error Rate: ", overall_error_rate_lda, "\n")
cat("Precision (PPV): ", PPV, "\n")
cat("Recall (TPR): ", TPR, "\n")
cat("F1 Score: ", F1, "\n")
cat("Specificity (TNR): ", TNR, "\n")
cat("False Positive Rate (FPR): ", FPR, "\n")

# Naive Bayes
nb_model <- naiveBayes(Attrition ~ ., data = train_data)
nb_pred <- predict(nb_model, newdata = test_data)

conf_matrix_nb <- table(nb_pred, test_data$Attrition)
conf_matrix_nb

TP <- conf_matrix_nb["1", "1"] 
TN <- conf_matrix_nb["0", "0"] 
FP <- conf_matrix_nb["1", "0"]  
FN <- conf_matrix_nb["0", "1"]

accuracy_nb <- (TP + TN) / (TP + TN + FP + FN)
overall_error_rate_nb <- 1 - accuracy_nb

PPV <- TP / (TP + FP)
TPR <- TP / (TP + FN)
F1 <- 2 * PPV * TPR / (PPV + TPR)
TNR <- TN / (TN + FP)
FPR <- FP / (FP + TN)

cat("Naive Bayes Performance:\n")
cat("Accuracy: ", accuracy_nb, "\n")
cat("Overall Error Rate: ", overall_error_rate_nb, "\n")
cat("Precision (PPV): ", PPV, "\n")
cat("Recall (TPR): ", TPR, "\n")
cat("F1 Score: ", F1, "\n")
cat("Specificity (TNR): ", TNR, "\n")
cat("False Positive Rate (FPR): ", FPR, "\n")


# KNN
train_X <- as.matrix(train_data[, -which(names(train_data) == "Attrition")])
test_X <- as.matrix(test_data[, -which(names(test_data) == "Attrition")])
train_y <- train_data$Attrition
knn_pred <- knn(train_X, test_X, train_y, k = 5)  # k adjust

conf_matrix_knn <- table(knn_pred, test_data$Attrition)
conf_matrix_knn

TP <- conf_matrix_knn["1", "1"] 
TN <- conf_matrix_knn["0", "0"] 
FP <- conf_matrix_knn["1", "0"]  
FN <- conf_matrix_knn["0", "1"]

accuracy_knn <- (TP + TN) / (TP + TN + FP + FN)
overall_error_rate_knn <- 1 - accuracy_knn

PPV <- TP / (TP + FP)
TPR <- TP / (TP + FN)
F1 <- 2 * PPV * TPR / (PPV + TPR)
TNR <- TN / (TN + FP)
FPR <- FP / (FP + TN)

cat("KNN Performance:\n")
cat("Accuracy: ", accuracy_knn, "\n")
cat("Overall Error Rate: ", overall_error_rate_knn, "\n")
cat("Precision (PPV): ", PPV, "\n")
cat("Recall (TPR): ", TPR, "\n")
cat("F1 Score: ", F1, "\n")
cat("Specificity (TNR): ", TNR, "\n")
cat("False Positive Rate (FPR): ", FPR, "\n")
