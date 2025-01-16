
#Loading the data
app_data <- read.csv("application_data.csv")
prev_data <- read.csv("previous_application.csv")

#First look at the data
str(app_data)
summary(app_data)
str(prev_data)
summary(prev_data)

#Percentage of missing values in the data
(sum(is.na(app_data)) / (nrow(app_data) * ncol(app_data))) * 100
(sum(is.na(prev_data)) / (nrow(prev_data) * ncol(prev_data))) * 100
colSums(is.na(app_data)) / nrow(app_data) * 100
colSums(is.na(prev_data)) / nrow(prev_data) * 100

#Removing columns with more than 40% missing values
app_data <- app_data[, colMeans(is.na(app_data)) < 0.4]
prev_data <- prev_data[, colMeans(is.na(prev_data)) < 0.4]


# Converting character columns to factors
char_cols <- sapply(app_data, is.character)
app_data[char_cols] <- lapply(app_data[char_cols], as.factor)

# Impute missing values in categorical columns with "Missing" category
categorical_cols <- sapply(app_data, is.factor)
categorical_cols
app_data[categorical_cols] <- lapply(app_data[categorical_cols], function(x) {
  x[is.na(x)] <- "Missing"
  return(x)
})

# Finding numeric columns with missing values
numeric_cols <- sapply(app_data, is.numeric)
missing_numeric <- colSums(is.na(app_data[, numeric_cols])) / nrow(app_data) * 100

missing_numeric <- missing_numeric[missing_numeric > 0]
print(missing_numeric)


library(modeest)

cnt_fam_mode <- mfv(app_data$CNT_FAM_MEMBERS, na_rm = TRUE)
app_data$CNT_FAM_MEMBERS[is.na(app_data$CNT_FAM_MEMBERS)] <- cnt_fam_mode

obs_30_mode <- mfv(app_data$OBS_30_CNT_SOCIAL_CIRCLE, na_rm = TRUE)
def_30_mode <- mfv(app_data$DEF_30_CNT_SOCIAL_CIRCLE, na_rm = TRUE)
obs_60_mode <- mfv(app_data$OBS_60_CNT_SOCIAL_CIRCLE, na_rm = TRUE)
def_60_mode <- mfv(app_data$DEF_60_CNT_SOCIAL_CIRCLE, na_rm = TRUE)

app_data$OBS_30_CNT_SOCIAL_CIRCLE[is.na(app_data$OBS_30_CNT_SOCIAL_CIRCLE)] <- obs_30_mode
app_data$DEF_30_CNT_SOCIAL_CIRCLE[is.na(app_data$DEF_30_CNT_SOCIAL_CIRCLE)] <- def_30_mode
app_data$OBS_60_CNT_SOCIAL_CIRCLE[is.na(app_data$OBS_60_CNT_SOCIAL_CIRCLE)] <- obs_60_mode
app_data$DEF_60_CNT_SOCIAL_CIRCLE[is.na(app_data$DEF_60_CNT_SOCIAL_CIRCLE)] <- def_60_mode

app_data$DAYS_LAST_PHONE_CHANGE[is.na(app_data$DAYS_LAST_PHONE_CHANGE)] <- median(app_data$DAYS_LAST_PHONE_CHANGE, na.rm = TRUE)

# Imputing AMT_REQ_CREDIT_BUREAU columns with mode
amt_req_hour_mode <- mfv(app_data$AMT_REQ_CREDIT_BUREAU_HOUR, na_rm = TRUE)
app_data$AMT_REQ_CREDIT_BUREAU_HOUR[is.na(app_data$AMT_REQ_CREDIT_BUREAU_HOUR)] <- amt_req_hour_mode

amt_req_day_mode <- mfv(app_data$AMT_REQ_CREDIT_BUREAU_DAY, na_rm = TRUE)
app_data$AMT_REQ_CREDIT_BUREAU_DAY[is.na(app_data$AMT_REQ_CREDIT_BUREAU_DAY)] <- amt_req_day_mode

amt_req_week_mode <- mfv(app_data$AMT_REQ_CREDIT_BUREAU_WEEK, na_rm = TRUE)
app_data$AMT_REQ_CREDIT_BUREAU_WEEK[is.na(app_data$AMT_REQ_CREDIT_BUREAU_WEEK)] <- amt_req_week_mode

amt_req_mon_mode <- mfv(app_data$AMT_REQ_CREDIT_BUREAU_MON, na_rm = TRUE)
app_data$AMT_REQ_CREDIT_BUREAU_MON[is.na(app_data$AMT_REQ_CREDIT_BUREAU_MON)] <- amt_req_mon_mode

amt_req_qrt_mode <- mfv(app_data$AMT_REQ_CREDIT_BUREAU_QRT, na_rm = TRUE)
app_data$AMT_REQ_CREDIT_BUREAU_QRT[is.na(app_data$AMT_REQ_CREDIT_BUREAU_QRT)] <- amt_req_qrt_mode

amt_req_year_mode <- mfv(app_data$AMT_REQ_CREDIT_BUREAU_YEAR, na_rm = TRUE)
app_data$AMT_REQ_CREDIT_BUREAU_YEAR[is.na(app_data$AMT_REQ_CREDIT_BUREAU_YEAR)] <- amt_req_year_mode

app_data$AMT_ANNUITY[is.na(app_data$AMT_ANNUITY)] <- mean(app_data$AMT_ANNUITY, na.rm = TRUE)

app_data$AMT_GOODS_PRICE[is.na(app_data$AMT_GOODS_PRICE)] <- mean(app_data$AMT_GOODS_PRICE, na.rm = TRUE)

app_data$EXT_SOURCE_2[is.na(app_data$EXT_SOURCE_2)] <- mean(app_data$EXT_SOURCE_2, na.rm = TRUE)
app_data$EXT_SOURCE_3[is.na(app_data$EXT_SOURCE_3)] <- mean(app_data$EXT_SOURCE_3, na.rm = TRUE)



# Convert to positive values
app_data$DAYS_BIRTH <- abs(app_data$DAYS_BIRTH)
app_data$DAYS_EMPLOYED <- abs(app_data$DAYS_EMPLOYED)
app_data$DAYS_REGISTRATION <- abs(app_data$DAYS_REGISTRATION)
app_data$DAYS_ID_PUBLISH <- abs(app_data$DAYS_ID_PUBLISH)
app_data$DAYS_LAST_PHONE_CHANGE <- abs(app_data$DAYS_LAST_PHONE_CHANGE)


colSums(is.na(app_data))
sum(is.na(app_data))


str(app_data)
summary(app_data)
str(prev_data)
summary(prev_data)
head(app_data)
head(prev_data)
colSums(is.na(prev_data)) / nrow(prev_data) * 100

library(dplyr)

#Imputing Annuity with mean and Goods price with median
prev_data <- prev_data %>%
  mutate(
    AMT_ANNUITY = ifelse(is.na(AMT_ANNUITY), mean(AMT_ANNUITY, na.rm = TRUE), AMT_ANNUITY),
    AMT_GOODS_PRICE = ifelse(is.na(AMT_GOODS_PRICE), median(AMT_GOODS_PRICE, na.rm = TRUE), AMT_GOODS_PRICE)
  )


#Imputing `CNT_PAYMENT` with the median
prev_data$CNT_PAYMENT[is.na(prev_data$CNT_PAYMENT)] <- median(prev_data$CNT_PAYMENT, na.rm = TRUE)


#Imputing `AMT_CREDIT` with the median
prev_data$AMT_CREDIT[is.na(prev_data$AMT_CREDIT)] <- median(prev_data$AMT_CREDIT, na.rm = TRUE)



# Checking for duplicate rows in `app_data`
app_data_duplicates <- app_data[duplicated(app_data), ]
nrow(app_data_duplicates)

any(duplicated(app_data$SK_ID_CURR))

# Checking for duplicate rows in `prev_data`
prev_data_duplicates <- prev_data[duplicated(prev_data), ]
nrow(prev_data_duplicates)

any(duplicated(prev_data$SK_ID_CURR))

any(duplicated(prev_data$SK_ID_PREV))


# Exporting the cleaned `app_data`
write.csv(app_data, "cleaned_application_data.csv", row.names = FALSE)

# Exporting the cleaned `prev_data`
write.csv(prev_data, "cleaned_previous_application_data.csv", row.names = FALSE)







str(app_data)





# Creating an aggregated column for address mismatches
app_data <- app_data %>%
  mutate(ADDRESS_MISMATCH_COUNT = 
           REG_REGION_NOT_LIVE_REGION + 
           REG_REGION_NOT_WORK_REGION + 
           LIVE_REGION_NOT_WORK_REGION + 
           REG_CITY_NOT_LIVE_CITY + 
           REG_CITY_NOT_WORK_CITY + 
           LIVE_CITY_NOT_WORK_CITY) %>%
  # Drop the original columns
  select(-REG_REGION_NOT_LIVE_REGION, -REG_REGION_NOT_WORK_REGION, 
         -LIVE_REGION_NOT_WORK_REGION, -REG_CITY_NOT_LIVE_CITY, 
         -REG_CITY_NOT_WORK_CITY, -LIVE_CITY_NOT_WORK_CITY)


total_weight <- 6 + 6 + 5 + 5 + 4 + 3

# Applying weighted aggregation and normalizing by total weight
app_data <- app_data %>%
  mutate(WEIGHTED_CREDIT_BUREAU_INQUIRIES = 
           ((AMT_REQ_CREDIT_BUREAU_HOUR * 6) + 
              (AMT_REQ_CREDIT_BUREAU_DAY * 6) + 
              (AMT_REQ_CREDIT_BUREAU_WEEK * 5) + 
              (AMT_REQ_CREDIT_BUREAU_MON * 5) + 
              (AMT_REQ_CREDIT_BUREAU_QRT * 4) + 
              (AMT_REQ_CREDIT_BUREAU_YEAR * 3)) / total_weight) %>%
  # Dropping the original columns after aggregation
  select(-AMT_REQ_CREDIT_BUREAU_HOUR, -AMT_REQ_CREDIT_BUREAU_DAY, 
         -AMT_REQ_CREDIT_BUREAU_WEEK, -AMT_REQ_CREDIT_BUREAU_MON, 
         -AMT_REQ_CREDIT_BUREAU_QRT, -AMT_REQ_CREDIT_BUREAU_YEAR)




str(app_data)


app_data <- app_data %>%
  mutate(SOCIAL_CIRCLE_DEFAULT_RATE = ifelse(
    OBS_30_CNT_SOCIAL_CIRCLE + OBS_60_CNT_SOCIAL_CIRCLE > 0,
    (DEF_30_CNT_SOCIAL_CIRCLE + DEF_60_CNT_SOCIAL_CIRCLE) /
      (OBS_30_CNT_SOCIAL_CIRCLE + OBS_60_CNT_SOCIAL_CIRCLE),
    0)) %>%
  # Remove the original columns 
  select(-OBS_30_CNT_SOCIAL_CIRCLE, -DEF_30_CNT_SOCIAL_CIRCLE, 
         -OBS_60_CNT_SOCIAL_CIRCLE, -DEF_60_CNT_SOCIAL_CIRCLE)


# Counting the number of documents provided for each client
app_data <- app_data %>%
  mutate(NUM_DOCUMENTS_PROVIDED = rowSums(select(., starts_with("FLAG_DOCUMENT_")))) %>%
  select(-starts_with("FLAG_DOCUMENT_"))  # Dropping individual document flag columns

# Convert all FLAG columns to numeric first
app_data <- app_data %>%
  mutate(across(starts_with("FLAG_"), as.numeric)) %>%
  # Sum the FLAG columns into a single CONTACT_PROVIDED_SUM column
  mutate(CONTACT_PROVIDED_SUM = rowSums(select(., starts_with("FLAG_")))) %>%
  # Remove individual contact columns after aggregation
  select(-FLAG_MOBIL, -FLAG_EMP_PHONE, -FLAG_WORK_PHONE, -FLAG_CONT_MOBILE, 
         -FLAG_PHONE, -FLAG_EMAIL)

app_data <- app_data %>%
  select(-FONDKAPREMONT_MODE, -HOUSETYPE_MODE, -WALLSMATERIAL_MODE, 
         -EMERGENCYSTATE_MODE, -WEEKDAY_APPR_PROCESS_START, -HOUR_APPR_PROCESS_START)


str(app_data)
str(prev_data)


prev_data <- prev_data %>%
  mutate(
    NAME_CONTRACT_TYPE = as.factor(NAME_CONTRACT_TYPE),
    FLAG_LAST_APPL_PER_CONTRACT = as.factor(FLAG_LAST_APPL_PER_CONTRACT),
    NAME_CASH_LOAN_PURPOSE = as.factor(NAME_CASH_LOAN_PURPOSE),
    NAME_CONTRACT_STATUS = as.factor(NAME_CONTRACT_STATUS),
    NAME_PAYMENT_TYPE = as.factor(NAME_PAYMENT_TYPE),
    CODE_REJECT_REASON = as.factor(CODE_REJECT_REASON),
    NAME_TYPE_SUITE = as.factor(NAME_TYPE_SUITE),
    NAME_CLIENT_TYPE = as.factor(NAME_CLIENT_TYPE),
    NAME_GOODS_CATEGORY = as.factor(NAME_GOODS_CATEGORY),
    NAME_PORTFOLIO = as.factor(NAME_PORTFOLIO),
    NAME_PRODUCT_TYPE = as.factor(NAME_PRODUCT_TYPE),
    CHANNEL_TYPE = as.factor(CHANNEL_TYPE),
    NAME_SELLER_INDUSTRY = as.factor(NAME_SELLER_INDUSTRY),
    NAME_YIELD_GROUP = as.factor(NAME_YIELD_GROUP),
    PRODUCT_COMBINATION = as.factor(PRODUCT_COMBINATION),
    WEEKDAY_APPR_PROCESS_START = as.factor(WEEKDAY_APPR_PROCESS_START))


# Checking for -1 values in each column
sapply(prev_data, function(column) sum(column == -1, na.rm = TRUE))


# Replacing -1 with NA in SELLERPLACE_AREA
prev_data$SELLERPLACE_AREA[prev_data$SELLERPLACE_AREA == -1] <- NA


# Dropping rows where DAYS_DECISION is -1
prev_data <- prev_data %>% filter(DAYS_DECISION != -1)

sum(is.na(prev_data$SELLERPLACE_AREA)) / nrow(prev_data) * 100


# Dropping the SELLERPLACE_AREA column
prev_data <- prev_data %>% select(-SELLERPLACE_AREA)

# For DAYS_DECISION, we will take the absolute value if we only need the magnitude
prev_data$DAYS_DECISION <- abs(prev_data$DAYS_DECISION)



sapply(prev_data[, c("AMT_ANNUITY", "AMT_APPLICATION", "AMT_CREDIT", "AMT_GOODS_PRICE")], function(column) {
  sum(column < 0, na.rm = TRUE)
})


str(app_data)
summary(app_data)
str(prev_data)
summary(prev_data)



install.packages("dlookr")
library(dlookr)
install.packages("extrafontdb")
library(extrafontdb)

# Outlier diagnosis for all numeric columns in the app_data dataset
outlier_report_app <- diagnose_outlier(app_data)
# Display outliers summary
outlier_report_app




app_data$DAYS_EMPLOYED

# Replacing 365243 with NA in DAYS_EMPLOYED
app_data$DAYS_EMPLOYED <- ifelse(app_data$DAYS_EMPLOYED == 365243, NA, app_data$DAYS_EMPLOYED)

# Median imputation for DAYS_EMPLOYED
app_data$DAYS_EMPLOYED[is.na(app_data$DAYS_EMPLOYED)] <- median(app_data$DAYS_EMPLOYED, na.rm = TRUE)


app_data$REGION_RATING_CLIENT

unique(app_data$REGION_RATING_CLIENT)

app_data$REGION_RATING_CLIENT_W_CITY
unique(app_data$REGION_RATING_CLIENT_W_CITY)



app_data$SOCIAL_CIRCLE_DEFAULT_RATE
unique(app_data$SOCIAL_CIRCLE_DEFAULT_RATE)



unique(app_data$NUM_DOCUMENTS_PROVIDED)


#We can retain these columns as they are not actually outliers.



# Outlier diagnosis for all numeric columns in the app_data dataset
outlier_report_prev <- diagnose_outlier(prev_data)
# Display outliers summary
outlier_report_prev


summary(prev_data$AMT_ANNUITY)
prev_data$AMT_ANNUITY


summary(prev_data$AMT_APPLICATION)
prev_data$AMT_APPLICATION


summary(prev_data$AMT_CREDIT)
prev_data$AMT_CREDIT


summary(prev_data$AMT_GOODS_PRICE)
prev_data$AMT_GOODS_PRICE



summary(prev_data$CNT_PAYMENT)
prev_data$CNT_PAYMENT



# Calculate the upper cap value, including all values in the column
upper_cap <- quantile(prev_data$AMT_ANNUITY, 0.99)

# Apply the upper cap to the AMT_ANNUITY column
prev_data$AMT_ANNUITY <- ifelse(
  prev_data$AMT_ANNUITY > upper_cap, upper_cap, prev_data$AMT_ANNUITY
)




# Filtering rows where AMT_APPLICATION is 0
amt_application_zero_snapshot <- subset(prev_data, AMT_APPLICATION == 0)

# Display the first few rows  
head(amt_application_zero_snapshot)


# Filter out rows with both AMT_APPLICATION and AMT_CREDIT as 0
prev_data <- prev_data %>%
  filter(!(AMT_APPLICATION == 0 & AMT_CREDIT == 0))
prev_data



str(app_data)
str(prev_data)


write.csv(app_data, "app_data_cleaned_2.csv", row.names = FALSE)
write.csv(prev_data, "prev_data_cleaned_2.csv", row.names = FALSE)

library(dplyr)
library(tidyverse)
library(tidymodels)

merged_data <- read.csv("merged_data.csv")


str(merged_data)
summary(merged_data)

colSums(is.na(merged_data)) / nrow(merged_data) * 100

# Replacing missing values with appropriate placeholders
merged_data <- merged_data %>%
  mutate(
    # Replacing missing numerical values with 0
    across(
      c(PREVIOUS_AMT_ANNUITY, AMT_APPLICATION, PREVIOUS_AMT_CREDIT, PREVIOUS_AMT_GOODS_PRICE, CNT_PAYMENT, SK_ID_PREV),
      ~ replace_na(., 0)
    ),
    # Replacing missing categorical values with "Unknown"
    across(
      c(PREVIOUS_NAME_CONTRACT_TYPE, NAME_CASH_LOAN_PURPOSE, NAME_CONTRACT_STATUS, NAME_CLIENT_TYPE,
        NAME_GOODS_CATEGORY, NAME_PORTFOLIO, NAME_PRODUCT_TYPE, CHANNEL_TYPE, NAME_SELLER_INDUSTRY,
        NAME_YIELD_GROUP, PRODUCT_COMBINATION),
      ~ replace_na(., "Unknown")
    ),
    # Creating a flag for customers without previous loans
    NO_PREVIOUS_LOANS = if_else(SK_ID_PREV == 0, 1, 0)
  )

colSums(is.na(merged_data))

str(merged_data)
summary(merged_data)


merged_data <- merged_data %>%
  mutate(
    # Converting specified columns to factors
    across(
      c(
        "CURRENT_NAME_CONTRACT_TYPE", "CODE_GENDER", "CURRENT_NAME_TYPE_SUITE",
        "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", 
        "NAME_HOUSING_TYPE", "OCCUPATION_TYPE", "ORGANIZATION_TYPE", 
        "PREVIOUS_NAME_CONTRACT_TYPE", "NAME_CASH_LOAN_PURPOSE", 
        "NAME_CONTRACT_STATUS", "NAME_CLIENT_TYPE", "NAME_GOODS_CATEGORY", 
        "NAME_PORTFOLIO", "NAME_PRODUCT_TYPE", "CHANNEL_TYPE", 
        "NAME_SELLER_INDUSTRY", "NAME_YIELD_GROUP", "PRODUCT_COMBINATION", 
        "FLAG_OWN_CAR", "FLAG_OWN_REALTY", "REGION_RATING_CLIENT", 
        "REGION_RATING_CLIENT_W_CITY", "NFLAG_LAST_APPL_IN_DAY", 
        "FLAG_LAST_APPL_PER_CONTRACT"
      ),
      as.factor
    ),
    # Converting binary and ID columns to appropriate numeric types
    NO_PREVIOUS_LOANS = as.integer(NO_PREVIOUS_LOANS),
    SK_ID_PREV = as.numeric(SK_ID_PREV)
  )



str(merged_data)
summary(merged_data)
head(merged_data)

library(dplyr)
library(tidyverse)

# Converting all character columns to factors
merged_data <- merged_data %>%
  mutate(across(where(is.character), as.factor))

# TARGET Distribution
ggplot(merged_data, aes(x = factor(TARGET, labels = c("Non-Default", "Default")))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Target Variable", x = "Loan Status", y = "Count") +
  theme_minimal()



# Plotting histogram for AMT_INCOME_TOTAL
ggplot(merged_data, aes(x = AMT_INCOME_TOTAL)) +
  geom_histogram(binwidth = 50000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of AMT_INCOME_TOTAL", x = "Income Amount", y = "Count") +
  theme_minimal()


ggplot(merged_data, aes(x = log1p(AMT_INCOME_TOTAL))) +  # log1p handles log(0) safely
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Log-Transformed Distribution of AMT_INCOME_TOTAL", 
       x = "Log of Income Amount", 
       y = "Count") +
  theme_minimal()


# Boxplot to visualize outliers
ggplot(merged_data, aes(y = log1p(AMT_INCOME_TOTAL))) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Boxplot of Log-Transformed AMT_INCOME_TOTAL", 
       y = "Log of Income Amount") +
  theme_minimal()


ggplot(merged_data, aes(x = log1p(AMT_INCOME_TOTAL), fill = factor(TARGET))) +
  geom_density(alpha = 0.7) +
  labs(title = "Income Distribution by Loan Status",
       x = "Log of Income Amount",
       fill = "Loan Status") +
  theme_minimal()

library(dlookr)

# Generate a diagnostic report for numeric columns
outlier_report <- diagnose_outlier(merged_data)

# View the summary of the outlier report
print(outlier_report)

# Display only columns with outliers
outlier_report_filtered <- outlier_report %>% filter(outliers_ratio > 0)
print(outlier_report_filtered)

merged_data <- merged_data %>%
  mutate(
    LOG_AMT_INCOME_TOTAL = log1p(AMT_INCOME_TOTAL),
    LOG_CURRENT_AMT_CREDIT = log1p(CURRENT_AMT_CREDIT),
    LOG_CURRENT_AMT_GOODS_PRICE = log1p(CURRENT_AMT_GOODS_PRICE),
    LOG_AMT_APPLICATION = log1p(AMT_APPLICATION),
    LOG_PREVIOUS_AMT_CREDIT = log1p(PREVIOUS_AMT_CREDIT),
    LOG_PREVIOUS_AMT_GOODS_PRICE = log1p(PREVIOUS_AMT_GOODS_PRICE)
  )


outlier_report_log <- diagnose_outlier(merged_data)

outlier_report_log


plot_outlier(merged_data, LOG_AMT_INCOME_TOTAL)
plot_outlier(merged_data, LOG_CURRENT_AMT_CREDIT)
plot_outlier(merged_data, LOG_CURRENT_AMT_GOODS_PRICE)
plot_outlier(merged_data, LOG_AMT_APPLICATION)
plot_outlier(merged_data, TARGET)
plot_outlier(merged_data, DAYS_EMPLOYED)
plot_outlier(merged_data, ADDRESS_MISMATCH_COUNT)
plot_outlier(merged_data, SOCIAL_CIRCLE_DEFAULT_RATE)
plot_outlier(merged_data, NUM_DOCUMENTS_PROVIDED)
plot_outlier(merged_data, CONTACT_PROVIDED_SUM)
plot_outlier(merged_data, CNT_PAYMENT)



merged_data %>% filter(LOG_AMT_APPLICATION == 0)




# Creating new features
merged_data$debt_to_income_ratio <- merged_data$CURRENT_AMT_CREDIT / merged_data$AMT_INCOME_TOTAL
merged_data$age_in_years <- abs(merged_data$DAYS_BIRTH) / 365


str(merged_data)
summary(merged_data)



columns_to_remove <- c(
  "SK_ID_CURR", "CURRENT_NAME_TYPE_SUITE", "DAYS_REGISTRATION", "DAYS_ID_PUBLISH", 
  "REGION_RATING_CLIENT_W_CITY", "DAYS_LAST_PHONE_CHANGE", "SK_ID_PREV", 
  "PREVIOUS_AMT_GOODS_PRICE", "WEEKDAY_APPR_PROCESS_START", "HOUR_APPR_PROCESS_START",
  "FLAG_LAST_APPL_PER_CONTRACT", "NFLAG_LAST_APPL_IN_DAY", "NAME_CASH_LOAN_PURPOSE", 
  "DAYS_DECISION", "NAME_PAYMENT_TYPE", "CODE_REJECT_REASON", "PREVIOUS_NAME_TYPE_SUITE", 
  "NAME_GOODS_CATEGORY", "NAME_PRODUCT_TYPE", "NAME_SELLER_INDUSTRY", 
  "NAME_YIELD_GROUP", "PRODUCT_COMBINATION", "LOG_PREVIOUS_AMT_GOODS_PRICE"
)

# Remove the unnecessary columns
filtered_data <- merged_data %>% 
  select(-all_of(columns_to_remove))

# Check the structure of the filtered dataset
str(filtered_data)

write.csv(filtered_data, "filtered_data.csv", row.names = FALSE)

# Creating dummies for in preparation for modeling
library(fastDummies)


# Creating dummy variables and then adding back the TARGET column
final_data <- filtered_data %>%
  select(-TARGET) %>% # Removing TARGET temporarily
  dummy_cols(remove_first_dummy = TRUE, # Removing the first dummy to avoid multicollinearity
             remove_selected_columns = TRUE) %>% # Removing original categorical columns
  mutate(TARGET = filtered_data$TARGET) # Adding TARGET back using mutate

# View the structure of the dataset
str(final_data)


library(glmnet)

# Preparing the feature matrix (X) and response vector (y)
X <- as.matrix(final_data %>% select(-TARGET))
y <- final_data$TARGET


# Running cross-validated Lasso regression
set.seed(123) # Ensuring reproducibility
lasso_model <- cv.glmnet(X, y, alpha = 1, family = "binomial", standardize = TRUE)

# Displaying the optimal lambda value
optimal_lambda <- lasso_model$lambda.min

optimal_lambda


# Extracting coefficients for the optimal lambda as matrix
lasso_coefficients <- as.matrix(coef(lasso_model, s = "lambda.1se"))
print(lasso_coefficients)


# Extracting non-zero coefficients (selected features)
selected_features <- rownames(lasso_coefficients)[lasso_coefficients != 0]
selected_features <- selected_features[-1] # Excluding the intercept
selected_features


# Summarizing ORGANIZATION_TYPE variables
organization_type_summary <- final_data %>%
  select(starts_with("ORGANIZATION_TYPE")) %>%
  summarise(across(everything(), ~ sum(.))) %>%
  gather(key = "Variable", value = "Count") %>%
  arrange(desc(Count))

# Viewing the summary
print(organization_type_summary)

final_dataset <- final_data %>%
  select(all_of(selected_features), TARGET)

str(final_dataset)


library(caret)

# Setting a seed for reproducibility
set.seed(123)

# Splitting the dataset: 80% training, 20% testing
split <- createDataPartition(final_dataset$TARGET, p = 0.8, list = FALSE)

# Creating training and testing datasets
training_data <- final_dataset[split, ]
testing_data <- final_dataset[-split, ]

nrow(training_data)
nrow(testing_data)

# Using the training_data to fit our logistic regression model.
logistic_model <- glm(TARGET ~ ., data = training_data, family = "binomial")
summary(logistic_model)


# Extracting summary of the logistic model
model_summary <- summary(logistic_model)
model_summary
# Extracting coefficients and p-values
coefficients <- model_summary$coefficients
coefficients
p_values <- coefficients[, 4]  # Extracting p-values (4th column)

# Getting names of significant variables
significant_vars <- rownames(coefficients)[p_values < 0.05 & rownames(coefficients) != "(Intercept)"]
significant_vars


refined_dataset <- final_data %>%
  select(all_of(significant_vars), TARGET)

# Removing backticks from the column names in significant_vars
cleaned_significant_vars <- gsub("`", "", significant_vars)

# Selecting only the significant variables and the TARGET column
refined_dataset <- final_data %>%
  select(all_of(cleaned_significant_vars), TARGET)
str(refined_dataset)


# Removing the TARGET column for correlation analysis
numeric_vars <- refined_dataset %>% select(-TARGET)

# Computing the correlation matrix
cor_matrix <- cor(numeric_vars)
cor_matrix

# Identifying highly correlated pairs (absolute correlation > 0.9)
high_cor_pairs <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor_pairs

#Removing goods price and its log column as it is highly correlated with AMT_CREDIT
refined_dataset <- refined_dataset %>%
  select(-CURRENT_AMT_GOODS_PRICE, -LOG_CURRENT_AMT_GOODS_PRICE)



# Setting a seed for reproducibility
set.seed(123)

# Splitting the dataset: 80% training, 20% testing
split <- createDataPartition(refined_dataset$TARGET, p = 0.8, list = FALSE)

# Creating training and testing datasets
training_data <- refined_dataset[split, ]
testing_data <- refined_dataset[-split, ]

nrow(training_data)
nrow(testing_data)

# Using the training_data to fit our logistic regression model.
logistic_model <- glm(TARGET ~ ., data = training_data, family = "binomial")
summary(logistic_model)



# Predicting probabilities for the test data
predicted_probs <- predict(logistic_model, testing_data, type = "response")
predicted_probs

# Converting probabilities to binary predictions using a threshold of 0.5
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

predicted_classes


# Confusion matrix
conf_matrix <- table(Predicted = predicted_classes, Actual = testing_data$TARGET)
conf_matrix

# Calculating accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy: ", round(accuracy * 100, 2), "%\n")


# Loading pROC package for ROC and AUC calculation
library(pROC)

# Generating ROC curve and calculating AUC
roc_curve <- roc(testing_data$TARGET, predicted_probs)
auc_score <- auc(roc_curve)
auc_score



# Calculate the optimal threshold based on Youden's Index
optimal_threshold <- coords(roc_curve, "best", ret = "threshold", best.method = "youden")
optimal_threshold



# Converting probabilities to binary predictions using a threshold of 0.07844993
predicted_classes <- ifelse(predicted_probs > 0.07844993, 1, 0)

predicted_classes


# Confusion matrix
conf_matrix <- table(Predicted = predicted_classes, Actual = testing_data$TARGET)
conf_matrix

# Calculating accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
sensitivity
specificity


weight_ratio <- nrow(training_data[training_data$TARGET == 0, ]) / 
  nrow(training_data[training_data$TARGET == 1, ])
weight_ratio


# Adjusting weights for imbalanced classes
weights <- ifelse(training_data$TARGET == 1, 11, 1)
logistic_model_weighted <- glm(TARGET ~ ., 
                               data = training_data, 
                               family = "binomial", 
                               weights = weights)

summary(logistic_model_weighted)

# Predicting probabilities for the test data
predicted_probs <- predict(logistic_model_weighted, testing_data, type = "response")
predicted_probs

# Converting probabilities to binary predictions using a threshold of 0.5
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

predicted_classes


# Confusion matrix
conf_matrix <- table(Predicted = predicted_classes, Actual = testing_data$TARGET)
conf_matrix

# Calculating accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
sensitivity
specificity

# Generating ROC curve and calculating AUC
roc_curve <- roc(testing_data$TARGET, predicted_probs)
auc_score <- auc(roc_curve)
auc_score

write.csv(final_dataset, "final_dataset.csv", row.names = FALSE)

write.csv(refined_dataset, "refined_dataset.csv", row.names = FALSE)


library(dplyr)
library(caret)

str(refined_dataset)

filtered_data <- read.csv("filtered_data.csv")
str(filtered_data)
# Selecting features for Decision tree with high significance and diverse representation
dt_features <- c(
  "TARGET",                         # Target variable
  "CURRENT_AMT_ANNUITY",            # Loan-related
  "DAYS_BIRTH",                     # Demographics
  "DAYS_EMPLOYED",                  # Employment-related
  "CNT_FAM_MEMBERS",                # Family structure
  "EXT_SOURCE_2",                   # External credit source
  "EXT_SOURCE_3",                   # External credit source
  "WEIGHTED_CREDIT_BUREAU_INQUIRIES", # Credit inquiries
  "SOCIAL_CIRCLE_DEFAULT_RATE",     # Social default rate
  "NUM_DOCUMENTS_PROVIDED",         # Loan-related
  "AMT_APPLICATION",                # Loan application
  "LOG_AMT_INCOME_TOTAL",           # Income-related
  "debt_to_income_ratio",           # Financial health
  "NO_PREVIOUS_LOANS"               # Previous loan indicator
)
dt_data <- filtered_data[, dt_features]

str(dt_data)
summary(dt_data)

# Converting identified features to factors using dplyr's mutate function
dt_data <- dt_data %>%
  mutate(
    NUM_DOCUMENTS_PROVIDED = as.factor(NUM_DOCUMENTS_PROVIDED),
    NO_PREVIOUS_LOANS = as.factor(NO_PREVIOUS_LOANS),
    TARGET = as.factor(TARGET)
  )

# Verify the changes
str(dt_data)

# Splitting the data into training and testing sets
set.seed(123)
dt_split <- createDataPartition(dt_data$TARGET, p = 0.8, list = FALSE)

# Create training and testing datasets
dt_train <- dt_data[dt_split, ]
dt_test <- dt_data[-dt_split, ]

# Loading necessary libraries
library(rpart)
library(rpart.plot)

# Building the decision tree
dt_model <- rpart(TARGET ~ ., data = dt_train, method = "class", control = rpart.control(cp = 0.001))


# Visualize the decision tree
rpart.plot(dt_model, type = 3, extra = 102, fallen.leaves = TRUE)



# Building a weighted decision tree
dt_model_weighted <- rpart(
  TARGET ~ ., 
  data = dt_train, 
  method = "class", 
  parms = list(
    loss = matrix(c(0, 11, 1, 0), nrow = 2)  # Loss matrix
  ), 
  control = rpart.control(cp = 0.001)  # Complexity parameter
)



# Visualizing the decision tree
rpart.plot(dt_model_weighted, type = 3, extra = 102, fallen.leaves = TRUE)



# Predicting class labels for the test dataset
predictions_weighted <- predict(dt_model_weighted, newdata = dt_test, type = "class")
str(predictions_weighted)

# Predicting probabilities for ROC/AUC analysis
probs_weighted <- predict(dt_model_weighted, newdata = dt_test, type = "prob")[, 2]  # Probabilities for class "1"


library(caret)

# Confusion matrix
conf_matrix <- confusionMatrix(predictions_weighted, dt_test$TARGET, positive = "1")
print(conf_matrix)



library(pROC)

# Generate ROC curve
roc_weighted <- roc(dt_test$TARGET, probs_weighted, levels = rev(levels(dt_test$TARGET)))

# Plot the ROC curve
plot(roc_weighted, col = "blue", main = "ROC Curve for Weighted Decision Tree", lwd = 2)

# Compute AUC
auc_weighted <- auc(roc_weighted)
auc




# Loading the randomForest library
library(randomForest)

library(ranger)

# Building the Random Forest model using ranger
set.seed(123)
rf_model <- ranger(
  TARGET ~ ., 
  data = dt_train, 
  num.trees = 100,       # Number of trees
  importance = "impurity", # To track feature importance
  probability = TRUE,    # For classification probabilities
  verbose = TRUE         # To display progress
)


rf_model


# Predicting on the test data
rf_probs <- predict(rf_model, data = dt_test)$predictions[, 2] # Probability for class 1
rf_probs

# Evaluating model performance using ROC and AUC
library(pROC)
roc_rf <- roc(dt_test$TARGET, rf_probs, levels = rev(levels(dt_test$TARGET)))

# Plotting the ROC curve
plot(roc_rf, col = "blue", main = "ROC Curve for Random Forest", lwd = 2)

# Computing AUC
auc_rf <- auc(roc_rf)
auc_rf

# Feature Importance
importance <- rf_model$variable.importance
print(importance)


importance_df <- data.frame(Feature = names(importance), Importance = importance)
importance_df <- importance_df[order(-importance_df$Importance), ] # Sort by importance
print(importance_df)


write.csv(data.frame(Feature = names(rf_model$variable.importance), 
                     Importance = rf_model$variable.importance), 
          file = "Feature_Importance.csv", row.names = FALSE)





#Visualizing feature importance
library(ggplot2)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance in Random Forest", x = "Features", y = "Importance")



library(pdp)
pdp_ext_source_2 <- partial(rf_model, pred.var = "EXT_SOURCE_2", train = dt_train, prob = TRUE)
plotPartial(pdp_ext_source_2, main = "Partial Dependence of EXT_SOURCE_2")



calibration_data <- calibration(TARGET ~ rf_probs, data = dt_test, class = "1", cuts = 10)
plot(calibration_data)


library(Metrics)
brier_score <- mean((rf_probs - as.numeric(as.character(dt_test$TARGET)))^2)
print(brier_score)


# Converting probabilities to binary predictions using a threshold of 0.5
rf_preds <- ifelse(rf_probs > 0.5, 1, 0)
rf_preds
# Creating a confusion matrix
conf_matrix_rf <- confusionMatrix(
  factor(rf_preds), 
  factor(dt_test$TARGET), 
  positive = "1"
)

# Print the confusion matrix
print(conf_matrix_rf)





#Exporting files

write.csv(data.frame(TARGET = dt_test$TARGET, Predicted_Prob_Default = rf_probs), 
          file = "Predicted_Probabilities.csv", row.names = FALSE)


write.csv(dt_data, file = "Final_dt_Data.csv", row.names = FALSE)

rf_probs

# Exporting test data with predicted probabilities
write.csv(
  cbind(dt_test, Predicted_Probs = rf_probs), 
  "test_data_with_probs.csv", 
  row.names = FALSE
)

