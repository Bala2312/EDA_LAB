# Setup and Library Initialization

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)


# Question 1: Import a dataset into R

dataset <- read.csv("C:/Users/11c16/OneDrive/Desktop/HepatitisCdata.csv")
cat("Dataset Dimensions (Rows x Columns):", dim(dataset), "\n")


# Question 2: Display the first and last six records of the dataset

# Display first 6 records
cat("--- First 6 Records ---\n")
head(dataset, 6)

# Display last 6 records
cat("--- Last 6 Records ---\n")
tail(dataset, 6)


# Question 3: Identify the total number of missing values in the dataset

total_missing <- sum(is.na(dataset))
cat("Total number of missing values across all cells:", total_missing, "\n")

# Detailed column-wise missing count
colSums(is.na(dataset))


# Question 4: Display all records containing at least one missing value

missing_records <- dataset[!complete.cases(dataset), ]

cat("Number of rows with at least one missing value:", nrow(missing_records), "\n\n")

head(missing_records, 10)


# Question 5: Remove columns having more than 70 percentage of missing values

# Calculate percentage of missing values per column
missing_pct <- colMeans(is.na(dataset)) * 100

cat("Missing Percentage per Column:\n")

print(round(missing_pct, 2))

# Filter columns with <= 70% missing values
dataset_clean <- dataset[, missing_pct <= 70]

cat("\nNumber of columns before:", ncol(dataset),
    "| After removal:", ncol(dataset_clean), "\n")


# Question 6: Replace missing numerical values with the mean

dataset_mean_imp <- dataset %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

cat("Remaining missing numerical values after Mean Imputation:",
    sum(is.na(select(dataset_mean_imp, where(is.numeric)))), "\n")


# Question 7: Replace missing numerical values with the median

dataset_median_imp <- dataset %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

cat("Remaining missing numerical values after Median Imputation:",
    sum(is.na(select(dataset_median_imp, where(is.numeric)))), "\n")


# Question 8: Replace missing categorical values with the mode

# Define mode calculation function
get_mode <- function(v) {
  v_clean <- na.omit(v)
  tbl <- table(v_clean)
  names(tbl)[which.max(tbl)]
}

# Impute missing categorical variables with mode
dataset_mode_imp <- dataset %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), get_mode(.), .)))

cat("Remaining missing categorical values after Mode Imputation:",
    sum(is.na(select(dataset_mode_imp, where(is.character)))), "\n")


# Question 9: Identify duplicate rows if any

num_duplicates <- sum(duplicated(dataset))
cat("Total duplicate rows found:", num_duplicates, "\n")

if(num_duplicates > 0) {
  dataset[duplicated(dataset), ]
}


# Question 10: Remove the duplicate rows if any

dataset_dedup <- distinct(dataset)

cat("Rows before deduplication:", nrow(dataset),
    "| Rows after deduplication:", nrow(dataset_dedup), "\n")


# Question 11: Identify columns stored with incorrect data types

# Display internal structure and data types
str(dataset)


# Question 12: Convert character variables into factors if any

dataset <- dataset %>%
  mutate(across(where(is.character), as.factor))

# Verify updated structure
sapply(dataset, class)


# Question 13: Convert text values into lowercase / uppercase

# Convert 'Sex' to uppercase and 'Category' to lowercase strings
dataset <- dataset %>%
  mutate(
    Sex = as.factor(toupper(as.character(Sex))),
    Category = as.factor(tolower(as.character(Category)))
  )

# Display levels
cat("Levels of Sex:", levels(dataset$Sex), "\n")

cat("Levels of Category:", levels(dataset$Category), "\n")


# Question 14: Detect outliers using boxplots

ggplot(dataset, aes(y = ALT)) +
  geom_boxplot(
    fill = "#85C1E9",
    color = "#1B4F72",
    outlier.color = "red",
    outlier.shape = 18,
    outlier.size = 3
  ) +
  labs(
    title = "Boxplot Detection of Outliers in ALT (Alanine Transaminase)",
    y = "ALT Level (U/L)"
  ) +
  theme_minimal(base_size = 12)


# Question 15: Identify outliers using the interquartile range method

Q1 <- quantile(dataset$ALT, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$ALT, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

outliers <- dataset$ALT[!is.na(dataset$ALT) & (dataset$ALT < lower_bound | dataset$ALT > upper_bound)]

cat("Q1 (25th percentile):", Q1, "\n")

cat("Q3 (75th percentile):", Q3, "\n")

cat("IQR:", IQR_val, "\n")

cat("Lower Bound (1.5 * IQR):", lower_bound, "\n")

cat("Upper Bound (1.5 * IQR):", upper_bound, "\n")

cat("Total Outliers Detected in ALT:", length(outliers), "\n")


# Question 16: Remove extreme outliers from the dataset

# Extreme outliers are defined beyond Q3 + 3 * IQR or Q1 - 3 * IQR.

extreme_lower <- Q1 - 3 * IQR_val
extreme_upper <- Q3 + 3 * IQR_val

dataset_no_extreme <- dataset %>%
  filter(is.na(ALT) | (ALT >= extreme_lower & ALT <= extreme_upper))

cat("Row count before removing extreme outliers:", nrow(dataset), "\n")

cat("Row count after removing extreme outliers:", nrow(dataset_no_extreme), "\n")

cat("Extreme outliers removed:", nrow(dataset) - nrow(dataset_no_extreme), "\n")


# Question 17: Create a histogram for a numerical variable and interpret its distribution

ggplot(dataset, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#3498DB", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Patient Age",
    x = "Age (Years)",
    y = "Frequency Count"
  ) +
  theme_minimal(base_size = 12)


# Question 18: Add a density curve to the histogram

ggplot(dataset, aes(x = Age, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "#AEB6BF", color = "white", alpha = 0.6) +
  geom_density(color = "#C0392B", size = 1.2) +
  labs(
    title = "Age Distribution with Overlaid Kernel Density Estimation",
    x = "Age (Years)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)


# Question 19: Create a violin plot

ggplot(dataset, aes(x = Sex, y = Age, fill = Sex)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, fill = "white", color = "black", outlier.size = 1) +
  scale_fill_manual(values = c("F" = "#F1948A", "M" = "#85C1E9")) +
  labs(
    title = "Violin Plot of Age Distribution by Sex",
    x = "Sex",
    y = "Age (Years)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


# Question 20: Create a dot plot for a small numerical dataset

# Subset first 30 records for dot plot display
small_data <- head(dataset, 30)

ggplot(small_data, aes(x = Age)) +
  geom_dotplot(binwidth = 1, fill = "#E67E22", color = "black", dotsize = 0.8) +
  labs(
    title = "Dot Plot of Patient Age (Sample of 30 Patients)",
    x = "Age (Years)",
    y = "Proportion"
  ) +
  theme_minimal(base_size = 12) +
  scale_y_continuous(NULL, breaks = NULL)


# Question 21: Create a frequency table for a categorical variable

cat_freq <- table(dataset$Category)
print(cat_freq)


# Question 22: Calculate the proportion and percentage of each category

cat_prop <- prop.table(cat_freq)
cat_pct <- cat_prop * 100

summary_cat_df <- data.frame(
  Category = names(cat_freq),
  Frequency = as.vector(cat_freq),
  Proportion = round(as.vector(cat_prop), 4),
  Percentage = round(as.vector(cat_pct), 2)
)

print(summary_cat_df)


# Question 23: Create a bar chart showing the frequency of each category

ggplot(dataset, aes(x = Category)) +
  geom_bar(fill = "#2980B9", color = "black", width = 0.6) +
  labs(
    title = "Frequency Bar Chart of Diagnostic Categories",
    x = "Category",
    y = "Patient Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))


# Question 24: Create a horizontal bar chart for a variable with several categories

ggplot(dataset, aes(y = Category)) +
  geom_bar(fill = "#16A085", color = "black", height = 0.6) +
  labs(
    title = "Horizontal Bar Chart of Categories",
    x = "Patient Count",
    y = "Category"
  ) +
  theme_minimal(base_size = 12)


# Question 25: Create a percentage bar chart instead of a frequency bar chart

ggplot(dataset, aes(x = Category, y = (..count..) / sum(..count..) * 100)) +
  geom_bar(fill = "#27AE60", color = "black", width = 0.6) +
  labs(
    title = "Percentage Distribution Across Diagnostic Categories",
    x = "Category",
    y = "Percentage (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))


# Question 26: Create a pie chart for a categorical variable with a small number of categories

ggplot(dataset, aes(x = "", fill = Sex)) +
  geom_bar(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("F" = "#E74C3C", "M" = "#3498DB")) +
  labs(
    title = "Pie Chart of Gender Proportion",
    x = "",
    y = ""
  ) +
  theme_void(base_size = 12)


# Question 27: Create a lollipop chart for categorical frequencies

category_counts <- dataset %>%
  count(Category)

ggplot(category_counts, aes(x = Category, y = n)) +
  geom_segment(aes(x = Category, xend = Category, y = 0, yend = n), color = "#7F8C8D", size = 1) +
  geom_point(color = "#C0392B", size = 5) +
  labs(
    title = "Lollipop Chart of Patient Categories",
    x = "Category",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))


# Question 28: Create a dot chart

sorted_counts <- category_counts %>%
  mutate(Category = fct_reorder(Category, n))

ggplot(sorted_counts, aes(x = n, y = Category)) +
  geom_point(color = "#8E44AD", size = 4) +
  labs(title = "Cleveland Dot Plot of Category Counts", x = "Count", y = "Category") +
  theme_minimal(base_size = 12)


# Question 29: Create a density plot using ggplot2

ggplot(dataset, aes(x = CHOL, fill = Sex)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("F" = "#E74C3C", "M" = "#3498DB")) +
  labs(
    title = "Density Plot of Serum Cholesterol (CHOL) by Sex",
    x = "Cholesterol Level (mmol/L)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)


# Question 30: Use facet_wrap() to display the distribution of one variable across different groups while interpreting each distribution separately

ggplot(dataset, aes(x = Age, fill = Category)) +
  geom_histogram(binwidth = 5, color = "black", show.legend = FALSE) +
  facet_wrap(~ Category, scales = "free_y", ncol = 3) +
  labs(
    title = "Faceted Age Distribution Across Diagnostic Categories",
    x = "Age (Years)",
    y = "Patient Count"
  ) +
  theme_minimal(base_size = 12)
