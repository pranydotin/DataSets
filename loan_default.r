library(dplyr)
loan_data <- read.csv("./Loan_Default.csv")
head(loan_data)
loan_data$Status <- factor(loan_data$Status, levels = c(0, 1))

class(loan_data$income)
attach(loan_data)
loan_data$income_cat <- NA
# income[income < 1000] <- "<1Lakh"
# income[income >= 1000 & income < 8000] <- "1-8 Lakhs"
# income[income >= 8000 & income < 10000] <- "8-10 Lakhs"
# income[income >= 10000] <- "More than 10 lakhs"
# loan_data$income_cat <- as.factor(income)


loan_data$income_cat[loan_data$income < 1000] <- "<1 Lakh"
loan_data$income_cat[loan_data$income >= 1000 & loan_data$income < 8000] <- "1-8 Lakhs"
loan_data$income_cat[loan_data$income >= 8000 & loan_data$income < 10000] <- "8-10 Lakhs"
loan_data$income_cat[loan_data$income >= 10000] <- "More than 10 lakhs"

summary(loan_data)

loan_data$income_cat
head(loan_data)

income_status_summary <- loan_data %>%
    group_by(income_cat) %>% # Group by the income categories
    summarise(
        Total = n(), # Total rows in each category
        Loan_Default = sum(Status == 1), # Count where Status == 1
        Loan_Default_Percentage = (Loan_Default / Total) * 100 # Percentage of defaults
    )
income_status_summary
# loan_data$Gender

gender_status_summary <- loan_data %>%
    group_by(Gender) %>% # Group by the income categories
    summarise(
        Total = n(), # Total rows in each category
        Loan_Default = sum(Status == 1), # Count where Status == 1
        Loan_Default_Percentage = (Loan_Default / Total) * 100 # Percentage of defaults
    )

gender_status_summary
