# Libraries:
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(grid)


bank <- read_delim("bank-full.csv", delim = ";")

#DATA MANIPULATION TASK

#select random subsample of data set;
random <- sample_n(bank, 100)

#filter desired rows using simple and more complex conditions;
filtered_simple <- random %>% 
  filter(age > 43)

filtered_complex <- random %>%
  filter(age > 43, 
         (balance > 1000 | y == "yes"),
         campaign > 1)

#drop unnecessary variables, rename some variables;
removed_var <- filtered_simple %>% 
  select(-month)

renamed_var <- removed_var %>%
  rename(occupation = job)

#calculate summarizing statistics (for full sample and by categorical variables as well);
# Full Sample Statistics
full_sample_stats <- bank %>%
  summarise(
    average_age = sprintf("%.2f", mean(age, na.rm = TRUE)),
    median_age = sprintf("%.2f", median(age, na.rm = TRUE)),
    average_balance = sprintf("%.2f", mean(balance, na.rm = TRUE)),
    median_balance = sprintf("%.2f", median(balance, na.rm = TRUE))
  )

#stat_table <- as.data.frame(t(full_sample_stats))
#table_grob <- tableGrob(stat_table, theme = ttheme_default(
#  core = list(fg_params = list(fontsize = 30, fontface = "bold")), 
#  rowhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
#  colhead = list(fg_params = list(fontsize = 30, fontface = "bold"))
#))
#jpeg("summary_statistics.jpg", width = 600, height = 400)
#grid.draw(table_grob)
#dev.off()

# Summarizing statistics by categorical variables
categorical_stats <- bank %>%
  group_by(job, education) %>%
  summarise(
    average_age = sprintf("%.2f", mean(age, na.rm = TRUE)),
    median_age = sprintf("%.2f", median(age, na.rm = TRUE)),
    average_balance = sprintf("%.2f", mean(balance, na.rm = TRUE)),
    median_balance = sprintf("%.2f", median(balance, na.rm = TRUE)),
    count = n(),
    .groups = 'drop'
  )
#categorical_table <- as.data.frame(categorical_stats)

#table_grob <- tableGrob(categorical_table, theme = ttheme_default(
#  core = list(fg_params = list(fontsize = 18, fontface = "bold")), 
#  colhead = list(fg_params = list(fontsize = 18, fontface = "bold")),  
#  rowhead = list(fg_params = list(fontsize = 18, fontface = "bold"))
#))
#jpeg("categorical_statistics.jpg", width = 1024, height = 768)  
#grid.draw(table_grob)
#dev.off()
3
#create new variables using simple transformation and custom functions;
# Create age groups
new_var <- renamed_var %>%
  mutate(age_group = cut(age,
                         breaks = c(0, 25, 35, 50, 65, 100),
                         labels = c("Under 25", "25-35", "36-50", "51-65", "Over 65"),
                         right = FALSE))


# Custom function to categorize balance
get_balance_category <- function(balance) {
  if (balance < 0) {
    return("Negative")
  } else if (balance <= 1000) {
    return("Low")
  } else if (balance <= 5000) {
    return("Medium")
  } else {
    return("High")
  }
}

# Apply custom function using mutate
custom_function <- new_var %>%
  mutate(balance_category = sapply(balance, get_balance_category))

#order data set by several variables.
sorted_data <- new_var %>%
  arrange(age, desc(balance), occupation)

# View the first few rows of the sorted dataset
print(head(sorted_data))

#DATA VISUALISATION TASK

# Correlation matrix
numeric_data <- bank %>%
  select(age, balance, day, duration, campaign, pdays, previous)

cor_matrix <- cor(numeric_data, use = "complete.obs")
#jpeg("correlation_plot.jpg", width = 800, height = 800)
corrplot(cor_matrix, method = "color", 
         type = "upper",  
         order = "hclust", 
         tl.col = "black", 
         tl.cex = 1.5, 
         tl.srt = 45,  
         addCoef.col = "black", 
         number.cex = 1.5)  
#dev.off()

# Age histogram plot
age_histogram <- ggplot(bank, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Distribution of Age") +
  xlab("Age") + ylab("Frequency")
#ggsave("age_histogram.jpg", plot = age_histogram, width = 8, height = 6, units = "in")

#Density histogram
density_histogram <- ggplot(bank, aes(x = balance)) +
  geom_density(fill = "blue", alpha = 0.5) +  
  ggtitle("Smoothed Histogram of Balance") +
  xlab("Balance") + ylab("Density")
#ggsave("density_histogram.jpg", plot = density_histogram, width = 8, height = 6, units = "in")

# Bar chart for 'job' types
bar_chart <- ggplot(bank, aes(x = job)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Count of Job Types") +
  xlab("Job") + ylab("Count")
#ggsave("bar_chart.jpg", plot = bar_chart, width = 8, height = 6, units = "in")

# Scatter plot for 'age' vs 'balance'
scatter_plot <- ggplot(bank, aes(x = age, y = balance)) +
  geom_point(color = "blue", alpha = 0.5) +
  ggtitle("Age vs Balance Scatter Plot") +
  xlab("Age") + ylab("Balance")
#ggsave("scatter_plot.jpg", plot = scatter_plot, width = 8, height = 6, units = "in")


#Mosaic Plot of Education vs Marital Status
tableMar <- table(bank$education, bank$marital)
rowProportions <- prop.table(tableMar, 1) * 100
colProportions <- prop.table(tableMar, 2) * 100
row_desc <- apply(rowProportions, 2, function(x) paste(round(x, 3), "%"))
col_desc <- apply(colProportions, 1, function(x) paste(round(x, 3), "%"))

#jpeg("mosaic_plot.jpg", width = 800, height = 600)  

mosaicplot(tableMar, main = "Mosaic Plot of Education vs Marital Status",
           xlab = "Education", ylab = "Marital Status", color = TRUE)

#dev.off()

#Box plot of  balance by education and marital factors

box_plot <- ggplot(bank, aes(x = education, y = balance)) + 
  geom_boxplot() +
  facet_wrap(~ marital, scales = "free_y") +  
  labs(title = "Balance by Education Levels", x = "Education", y = "Balance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("box_plot.jpg", plot = box_plot, width = 8, height = 6, units = "in")


#Balance and age group by y factor
balance <- bank %>%
mutate(age_group = cut(age, breaks = c(18, 25, 35, 50, 65, 100), labels = c("Under 25", "25-35", "36-50", "51-65", "Over 65")),
       y = factor(y, levels = c("no", "yes")))  
ggplot(balance, aes(x = age, y = balance, color = y)) + 
  geom_point() + 
  facet_wrap(~ age_group, scales = "free_y") + 
  labs(x = "Age", y = "Balance", title = "Balance by Age Group") +
  theme(axis.text.x = element_text(angle = 90))  
balance_age_y <- ggplot(bank, aes(x = age, y = balance, color = y)) + 
  geom_point() + 
  facet_wrap(~ age_group, scales = "free_y") + 
  labs(x = "Age", y = "Balance", title = "Balance by Age Group") +
  theme(axis.text.x = element_text(angle = 90))

ggsave("balance_age_y.jpg", plot = balance_age_y, width = 10, height = 8, units = "in")

g <- ggplot(bank, aes(x = job, y = percentage, fill = marital, label = scales::percent(percentage))) +
  geom_bar(stat = "identity") + 
  facet_wrap(~ education, scales = "free_y") +  
  labs(x = "Job", y = "Percentage", title = "Marital Status Distribution by Job and Education", fill = "Marital Status") +
  theme(axis.text.x = element_text(angle = 90))  

#ggsave("g.jpg", plot = g, width = 10, height = 8, units = "in")

# Marital Status Distribution by Job and Education
stacked_box <- ggplot(bank, aes(x = job, fill = marital)) +
  geom_bar(position = "fill") +  
  facet_wrap(~ education, scales = "free_y") + 
  labs(x = "Job", title = "Marital Status Distribution by Job and Education", fill = "Marital Status") +
  theme(axis.text.x = element_text(angle = 90))  

#ggsave("stacked_box.jpg", plot = stacked_box, width = 10, height = 8, units = "in")


# Distribution of Term Deposit Subscriptions Pie Chart
pie <- bank %>%
  group_by(y) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

pie_chart <- ggplot(pie, aes(x = "", y = Percentage, fill = y)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  labs(fill = "Term Deposit", title = "Distribution of Term Deposit Subscriptions") +
  theme_void() +  
  geom_text(aes(label = paste(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#AED6F1", "#F1948A"))
ggsave("pie_chart.jpg", plot = pie_chart, width = 8, height = 6, units = "in")



#Perform a logistic regression to obtain the predicted probability that a customer has subscribed for a term deposit.
#Use continuous variables and dummy variables created for categorical columns. Not necessarily all variables provided in data sample should be used.
#Evaluate model goodness of fit and predictive ability. If needed, data set could be split into training and test sets.

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(writexl)
library(pROC)
library(caret)  

set.seed(100)  

# Load and partition data
bank <- read.csv("bank-full.csv", sep = ";")
sample_index <- sample(c(TRUE, FALSE), nrow(bank), replace = TRUE, prob = c(0.8, 0.2))
train <- bank[sample_index, ]
test <- bank[!sample_index, ]

# Convert variables and response for modeling
train <- train %>%
  mutate(across(c(job, marital, education, housing, loan, contact, poutcome, default, month), as.factor),
         y = as.factor(ifelse(y == "no", 0, 1)))

test <- test %>%
  mutate(across(c(job, marital, education, housing, loan, contact, poutcome, default, month), as.factor),
         y = as.factor(ifelse(y == "no", 0, 1)))

# Fit logistic regression model
model <- glm(y ~ age + job + marital + education + default + balance + housing + loan +
               contact + day + month + duration + campaign + pdays + previous + poutcome,
             family = binomial(), data = train)

summary(model)

# Plot Cook's Distance
plot(cooks.distance(model), pch = 16, col = "black", cex = 1, ylab = "Cook's Distance", main = "Cook's Distance for Logistic Regression")
abline(h = 4/nrow(train), col = "red", lty = 2)  

# Calculate and print key performance metrics
test_predictions <- predict(model, newdata = test, type = "response")
prediction_class <- ifelse(test_predictions > 0.5, 1, 0)
conf_matrix <- table(Predicted = prediction_class, Actual = test$y)
predicted_classes <- ifelse(test_predictions > 0.5, 1, 0)

actual_classes <- as.factor(test$y)  
predicted_classes <- as.factor(predicted_classes)
conf_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
confusion_details <- confusionMatrix(predicted_classes, actual_classes)

sensitivity <- confusion_details$byClass['Sensitivity']
specificity <- confusion_details$byClass['Specificity']
precision <- confusion_details$byClass['Positive Predictive Value']

roc_curve <- roc(response = as.numeric(actual_classes), predictor = test_predictions)
plot(roc_curve, main = "ROC Curve for Logistic Regression")

roc_auc <- auc(roc_curve)

cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("AUC:", roc_auc, "\n")

# Save processed datasets
write_xlsx(test, "test_data.xlsx")
write_xlsx(train, "train_data.xlsx")

#Goodness of Fit
model <- glm(y ~ age + job + marital + education + default + balance + housing + loan +
               contact + day + month + duration + campaign + pdays + previous + poutcome,
             family = binomial(), data = train)

null_model <- glm(y ~ 1, family = binomial(), data = train)
mcFadden_R2 <- 1 - (model$deviance / null_model$deviance)
cat("McFadden's R-squared:", mcFadden_R2, "\n")

