# Credit Risk Model Validation

## Install necessary R packages if needed

```install.packages("dplyr")```

```install.packages("readr")```

```install.packages("ggplot2")```

```install.packages("corrplot")```

```install.packages("gridExtra")```

```install.packages("grid")```

```install.packages("car")```

```install.packages("writexl")```

```install.packages("pROC")```

```install.packages("caret")```

## Libraries Used

dplyr: For data manipulation.

readr: For reading CSV files.

ggplot2: For data visualization.

corrplot: To create correlation matrices.

gridExtra, grid: For arranging multiple plots.

car: For advanced data handling.

writexl: To write data frames to Excel.

pROC: For ROC curve analysis.

caret: For model validation and performance metrics.

## Data Manipulation

Select a Random Subsample: A subset of 100 records is sampled from the full dataset for focused analysis.

## Filter Data:

Simple condition: Filters for customers older than 43.

Complex condition: Additional criteria include balance over 1000 or a positive subscription response, and more than one campaign contact.

## Variable Adjustments:

The month variable was removed from the filtered dataset;

The job variable was renamed to occupation.

## Summarizing Statistics:

Calculates and presents basic statistics (mean, median) for age and balance both for the full dataset and grouped by job and education.

## Data Visualization

Correlation Matrix: Displays correlations between numeric variables like age, balance, and campaign-related metrics.

Histograms and Density Plots: Visualize distributions of age and balance.

Bar and Scatter Plots: Show distributions of job types and the relationship between age and balance.

Mosaic Plot: Examines the relationship between education and marital status.

Box Plot: Compares balance across different education levels and marital statuses.

Faceted Scatter Plot: Displays balance by age group segmented by subscription status.

## Modeling Task

Logistic Regression: A model is built to predict the likelihood of a customer subscribing to a term deposit based on a range of predictors including demographic and transactional variables.

Model Diagnostics: Cook's Distance is plotted to identify influential points.

Performance Metrics: Accuracy, sensitivity, specificity, precision, and AUC are calculated for the model based on test data predictions.

Goodness of Fit: McFadden's R-squared is computed to evaluate the model fit compared to a null model.
