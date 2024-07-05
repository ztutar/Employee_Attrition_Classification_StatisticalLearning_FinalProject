# Libraries
library(dplyr)
library(corrplot)

# Dataset
data <- read.csv("C://Users//ayse-//Downloads//Employee Attrition Classification Dataset//Employee Attrition Classification Dataset//data.csv")

# EDA Basics
str(data)
summary(data)

# Drop Employee Id
data <- data[, !names(data) %in% "Employee.ID"]

# Numeric and categorical val. separation
numeric_vars <- sapply(data, is.numeric)
categoric_vars <- sapply(data, function(x) is.factor(x) || is.character(x))

# Taking names of features
categoric_var_names <- names(data)[categoric_vars]
numeric_var_names <- names(data)[numeric_vars]

# Numeric val. summary
summary(data[, numeric_vars])

# Missing Values --- No null Values
na_summary <- sapply(data, function(x) sum(is.na(x)))
na_summary

# Categorical val. dist.
categoric_var_names <- names(data)[categoric_vars]
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



# Outlier Analysis 
par(mfrow = c(2, 2))  
for (num_var in numeric_var_names) {
  boxplot(data[[num_var]], main=paste(num_var, "Boxplot"), xlab=num_var, col="lightblue")
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
    boxplot(data[[num_var]] ~ data[[cat_var]], main=paste(num_var, "by", cat_var), xlab=cat_var, ylab=num_var, col="lightblue")
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



# Cor. and Cov.
cov_matrix <- cov(data[, numeric_var_names])
cor_matrix <- cor(data[, numeric_var_names])

print("Covariance Matrix:")
print(cov_matrix)
print("Correlation Matrix:")
print(cor_matrix)

corrplot(cor_matrix, method = 'number', type = "upper", tl.col = "black", tl.srt = 45)














# Density plots
cat_var <- "Attrition"

plots_per_page <- 2
num_plots <- length(numeric_var_names)
num_pages <- ceiling(num_plots / plots_per_page)

plot_index <- 1
for (page in 1:num_pages) {
  par(mfrow = c(2, 1))  
  for (i in 1:plots_per_page) {
    if (plot_index > num_plots) break
    num_var <- numeric_var_names[plot_index]
    plot(density(data[[num_var]][data[[cat_var]] == "Left"], na.rm = TRUE), col="red", main=paste(num_var, "Density by", cat_var), xlab=num_var, ylab="Density")
    lines(density(data[[num_var]][data[[cat_var]] == "Stayed"], na.rm = TRUE), col="blue")
    legend("topright", legend=c("Left", "Stayed"), col=c("red", "blue"), lty=1, cex=0.8)
    plot_index <- plot_index + 1
  }
}
par(mfrow = c(1, 1))





