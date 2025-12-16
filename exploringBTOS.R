---
  title: "Final Project - Cleaning & Tidying Data"
author: "Faith"
date:"15th December"

---

#Load packages and data

library(tidyverse)
library(dplyr)
library(ggplot2)

btosdataset <-read.csv("~/Desktop/Modern Data Science/Final Project/State.csv")

View(btosdataset)


#DataCleaning

clean_btos <- btosdataset %>%
  mutate(
    across(starts_with("X20"), ~ na_if(.x, "S")),
    across(starts_with("X20"), parse_number)
  ) %>%
  pivot_longer(
    starts_with("X20"),
    names_to = "Period",
    values_to = "Value"
  ) %>%
  mutate(
    Period = factor(Period),
    Survey.Year = as.integer(substr(Period, 2, 5)),
    Survey.WeekNum = as.integer(substr(Period, 6, 7))
  ) %>%
  select(-Question)


#DataPreparation

performance_weights <- tibble(
  Answer = c("Excellent", "Above average", "Average", "Below average", "Poor"),
  Weight = c(2, 1, 0, -1, -2)
)

final_btos_features <- clean_btos %>%
  filter(Question.ID == 3) %>%
  left_join(performance_weights, by = "Answer") %>%
  mutate(
    Value = replace_na(Value, 0),
    Weighted_Component = (Value / 100) * Weight
  ) %>%
  group_by(State, Period, Survey.Year, Survey.WeekNum) %>%
  summarise(
    Performance.Score = sum(Weighted_Component),
    .groups = "drop"
  )

View(final_btos_features)



#Calculate Net Change Index for (Q4,Q5,Q6, Q10, Q11, Q12) Increased -Decreased)

create_net_index_df <- function(data, question_id, index_name) {
  
  data %>%
    filter(Question.ID == question_id) %>%
    pivot_wider(
      id_cols    = c(State, Period, Survey.Year, Survey.WeekNum),
      names_from = Answer,
      values_from = Value,
      values_fill = 0
    ) %>%
    mutate(
      !!index_name := (Increased - Decreased) / 100
    ) %>%
    select(State, Period, Survey.Year, Survey.WeekNum, !!index_name)
  
}



# Calculate Net Change Q7 (Yes-No)

create_ai_adoption_index_df <- function(data, question_id, index_name) {
  
  data %>%
    filter(Question.ID == question_id) %>%
    pivot_wider(
      id_cols    = c(State, Period, Survey.Year, Survey.WeekNum),
      names_from = Answer,
      values_from = Value,
      values_fill = 0
    ) %>%
    mutate(
      !!index_name := (Yes - No) / 100
    ) %>%
    select(State, Period, Survey.Year, Survey.WeekNum, !!index_name)
  
}

#Create and merge all features


# QUESTION 4: Change in operating revenue
net_q4_revenue <- create_net_index_df(
  clean_btos,
  question_id = 4,
  index_name  = "Net.Revenue.Q4.Index"
)

final_btos_features <- final_btos_features %>%
  left_join(net_q4_revenue,
            by = c("State", "Period", "Survey.Year", "Survey.WeekNum"))



# QUESTION 5:Change in number of paid employees
net_q5_employee <- create_net_index_df(
  clean_btos,
  question_id = 5,
  index_name  = "Net.Employee.Q5.Index"
)

final_btos_features <- final_btos_features %>%
  left_join(net_q5_employee,
            by = c("State", "Period", "Survey.Year", "Survey.WeekNum"))



# QUESTION 6: Change in total hours worked
net_q6_hours <- create_net_index_df(
  clean_btos,
  question_id = 6,
  index_name  = "Net.Hours.Q6.Index"
)

final_btos_features <- final_btos_features %>%
  left_join(net_q6_hours,
            by = c("State", "Period", "Survey.Year", "Survey.WeekNum"))


# QUESTION 7: Past AI Usage (Yes-No)
net_q7_ai <- create_ai_adoption_index_df(
  clean_btos,
  question_id = 7,
  index_name  = "Net.AI.Q7.Index"
)

final_btos_features <- final_btos_features %>%
  left_join(net_q7_ai,
            by = c("State", "Period", "Survey.Year", "Survey.WeekNum"))


# QUESTION 10:Change in demand for this business’s goods or services 
net_q10_demand <- create_net_index_df(
  clean_btos,
  question_id = 10,
  index_name  = "Net.Demand.Q10.Index"
)

final_btos_features <- final_btos_features %>%
  left_join(net_q10_demand,
            by = c("State", "Period", "Survey.Year", "Survey.WeekNum"))


# QUESTION 11: Change in prices charged for goods and services
net_q11_prices <- create_net_index_df(
  clean_btos,
  question_id = 11,
  index_name  = "Net.Prices.Q11.Index"
)

final_btos_features <- final_btos_features %>%
  left_join(net_q11_prices,
            by = c("State", "Period", "Survey.Year", "Survey.WeekNum"))



# QUESTION 12: Change in prices paid for goods or services
net_q12_suppliesprices <- create_net_index_df(
  clean_btos,
  question_id = 12,
  index_name  = "Net.SuppliesPrices.Q12.Index"
)

final_btos_features <- final_btos_features %>%
  left_join(net_q12_suppliesprices,
            by = c("State", "Period", "Survey.Year", "Survey.WeekNum"))



# QUESTION 24: Expected change in AI usage
net_q24_ai <- create_ai_adoption_index_df(
  clean_btos,
  question_id = 24,
  index_name  = "Net.AI.Q24.Index"
)

final_btos_features <- final_btos_features %>%
  left_join(net_q24_ai,
            by = c("State", "Period", "Survey.Year", "Survey.WeekNum"))




#Replace NA values with 0
final_btos_features <- final_btos_features %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

View(final_btos_features)
write.csv(final_btos_features, "final_btos_features.csv", row.names = FALSE)





  