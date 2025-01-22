library(dplyr)
data <- read.csv("./Loan_Default.csv")


loan_df <- data[, c("income", "Gender", "Status", "Credit_Score")]
attach(loan_df)

# making new colums for categorized income
loan_df$income_cat <- ifelse(
    income < 1000, "< 1 lakh",
    ifelse(
        income >= 1000 & income < 8000, "1-8 lakhs",
        ifelse(
            income >= 8000 & income < 10000, "8-10 lakhs",
            ifelse(
                income >= 10000, ">10 lakhs",
                "NA"
            )
        )
    )
)
loan_df$income_cat <- as.factor(loan_df$income_cat)
str(loan_df)
loan_df$Status <- as.factor(loan_df$Status)
loan_data$income_cat <- NA

# summary by income
income_status_summary <- loan_df %>%
    group_by(income_cat) %>%
    summarise(
        Total = n(),
        Loan_Default = sum(Status == 1),
        Loan_Default_Percentage = round((Loan_Default / Total) * 100, 2)
    ) %>%
    mutate(
        Variables = "Income",
        Categories = as.character(income_cat),
        `Loan Default (%)` = paste0(Loan_Default, " (", Loan_Default_Percentage, "%)")
    ) %>%
    select(Variables, Categories, Total, `Loan Default (%)`)
print(income_status_summary)


# summary by gender
gender_status_summary <- loan_df %>%
    group_by(Gender) %>% # Group by the income categories
    summarise(
        Total = n(), # Total rows in each category
        Loan_Default = sum(Status == 1), # Count where Status == 1
        Loan_Default_Percentage = round((Loan_Default / Total) * 100, 2) # Percentage of defaults
    ) %>%
    mutate(
        Variables = "Gender",
        Categories = as.character(Gender),
        `Loan Default (%)` = paste0(Loan_Default, " (", Loan_Default_Percentage, "%)")
    ) %>%
    select(Variables, Categories, Total, `Loan Default (%)`)

print(gender_status_summary)


# credit category
loan_df$credit_cat <- ifelse(
    Credit_Score < 700, "<700",
    ifelse(
        Credit_Score >= 700 & Credit_Score < 800, "700-800",
        ifelse(
            Credit_Score >= 800, ">800",
            "NA"
        )
    )
)
loan_df$credit_cat <- as.factor(loan_df$credit_cat)

# summary by credit score
credit_score_summary <- loan_df %>%
    group_by(credit_cat) %>% # Group by the income categories
    summarise(
        Total = n(), # Total rows in each category
        Loan_Default = sum(Status == 1), # Count where Status == 1
        Loan_Default_Percentage = round((Loan_Default / Total) * 100, 2) # Percentage of defaults
    ) %>%
    mutate(
        Variables = "Credit Score",
        Categories = as.character(credit_cat),
        `Loan Default (%)` = paste0(Loan_Default, " (", Loan_Default_Percentage, "%)")
    ) %>%
    select(Variables, Categories, Total, `Loan Default (%)`)

print(credit_score_summary)


final <- bind_rows(income_status_summary, gender_status_summary, credit_score_summary)
print(final, n = 50)
knitr::kable(final)

write.csv(final, "Loan_default_by_categories.csv", fileEncoding = "UTF-8")
