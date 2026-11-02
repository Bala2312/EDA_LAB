# Import the Hepatitis C dataset
df <- read.csv(
  "C:/Users/11c16/OneDrive/Desktop/Machine Learning/HepatitisCdata.csv",
  header = TRUE
)

# Display first 10 records
head(df, 10)

# Display last 10 records
tail(df, 10)

# Total number of rows and columns
nrow(df)
ncol(df)

# Display column names
colnames(df)

# Display data structure
str(df)

# Summary statistics
summary(df)

# Missing values in each column
colSums(is.na(df))

# Total missing values
sum(is.na(df))

# Identify rows containing missing values
which(!complete.cases(df))

# Remove rows containing missing values
cleaned_data <- na.omit(df)
colSums(is.na(cleaned_data))

# Replace missing ALP values with mean
df$ALP[is.na(df$ALP)] <- mean(df$ALP, na.rm = TRUE)
sum(is.na(df$ALP))

# Replace missing CHOL values with median
df$CHOL[is.na(df$CHOL)] <- median(df$CHOL, na.rm = TRUE)
sum(is.na(df$CHOL))

# Forward filling
if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
library(zoo)
df <- na.locf(df)

# Backward filling
df_bfill <- na.locf(df, fromLast = TRUE)

# Replace remaining missing values with 0 if required
# df[is.na(df)] <- 0

# Remove duplicate records
df <- unique(df)
nrow(df)
ncol(df)

# Convert variables into factors
df$Category <- as.factor(df$Category)
df$Sex <- as.factor(df$Sex)

# Rename column
names(df)[1] <- "S.No"
colnames(df)

# Create derived column
df$Total <- df$ALB + df$ALP
colnames(df)

# Summary statistics
summary(df)

# Minimum and maximum values
min(df$ALB, na.rm = TRUE)
max(df$ALB, na.rm = TRUE)

# Range
range(df$ALB, na.rm = TRUE)

# Variance
var(df$ALB, na.rm = TRUE)

# Standard deviation
sd(df$ALB, na.rm = TRUE)

# Interquartile range
IQR(df$ALB, na.rm = TRUE)

# IQR of all numerical columns
sapply(df[sapply(df, is.numeric)], IQR, na.rm = TRUE)