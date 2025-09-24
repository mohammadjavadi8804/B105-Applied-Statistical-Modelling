

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


#For third hypothesis: payment Method X Churn:chi-square test
install.packages("vcd")
library(vcd)
table_payment <- table(telco$PaymentMethod, telco$Churn)
#Run chi square test and get cramer's V
assocstats(table_payment)


#For forth hypothesis: Internet Service x Churn
table_internet <- table(telco$InternetService, telco$Churn)
#Run chi square test and get cramer's V
assocstats(table_internet)







##############
#figure 1



#Install packages if not already installed

if(!require(ggplot2)) install.packages("ggplot2")

if(!require(dplyr)) install.packages("dplyr")


# Load libraries

library(ggplot2)

library(dplyr)


#Loading the dataset


telco <- read.csv("/Users/mohammad/Downloads/WA_Fn-UseC_-Telco-Customer-Churn.csv",
                  stringsAsFactors = TRUE)


#Cleaning the data:


#Converting TotalCharges to numeric (some entries may be blank)

telco$TotalCharges <- suppressWarnings(as.numeric(telco$TotalCharges))


#Removing rows with missing TotalCharges

telco <- telco[!is.na(telco$TotalCharges), ]


#Creating binary churn variable


telco$ChurnBinary <- ifelse(telco$Churn == "Yes", 1, 0)


#Boxplot of MonthlyCharges by churn:
p1 <- ggplot(telco, aes(x = Churn, y = MonthlyCharges, fill = Churn)) + 
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Figure 1: Boxplot of Monthly Charges by churn status",
    x = "Churn",
    y = "Monthly Charges"
  ) +
  theme_minimal() + 
  theme(legend.position = "none")

print(p1)




#Figure 2:
# Bar chart of contract type by churn:
p2 <- ggplot(telco, aes(x = Contract, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Figure 2: contract Type by churn",
    x = "Contract Type",
    y = "Number of Customers",
    fill = "Churn"
  ) +
  theme_minimal()

print(p2)
#Saving plots automatically as PNG:
ggsave("/Users/mohammad/Downloads/Figure1_Boxplot.png", plot = p1, width = 7, height = 5)
ggsave("/Users/mohammad/Downloads/Figure2_Barchart.png", plot = p2, width = 7, height = 5)



