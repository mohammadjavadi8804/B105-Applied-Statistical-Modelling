

#4-Data Preparation:

# Load dataset with correct quote syntax
churn_data <- read.csv("/Users/mohammad/Downloads/WA_Fn-USeC_-Telco-Customer-churn.csv")

# Convert TotalCharges to numeric
churn_data$TotalCharges <- as.numeric(as.character(churn_data$TotalCharges))

# Remove rows with NA values:
churn_data <- churn_data[!is.na(churn_data$TotalCharges), ]

# Add a binary churn variable
churn_data$ChurnBinary <- ifelse(churn_data$Churn == "Yes", 1, 0)

###################

#5-Exploratory Data Analysis

charges_churn_yes <- churn_data[churn_data$Churn == "Yes", ]$MonthlyCharges

charges_churn_no <- churn_data[churn_data$Churn == "No", ]$MonthlyCharges

#For having histogram

#For pink part
hist(charges_churn_yes, col = rgb(1,0,0,0.5), xlab = "Monthly Charges", main = "Monthly Charges: Churn vs No Churn")

#For blue part
hist(charges_churn_no, col = rgb(0,0,1,0.5), add = TRUE)

#For legend and it's colors
legend("topright", legend = c("Churned", "Not Churned"), fill = c("red", "blue"))

###################

#6-Statistical Testing and Results

#For T.test:
t.test(charges_churn_yes, charges_churn_no)

#For chi-square test:
chisq.test(table(churn_data$Contract, churn_data$Churn))

