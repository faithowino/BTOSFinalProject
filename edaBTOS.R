---
  title: "Final Project - Exploratory data analysis"
author: "Faith"
date:"15th December" 

---
  
#Load packages and data
  
library(tidyverse)
library(dplyr)
library(ggplot2)


btos <- read.csv("final_btos_features.csv")
View(btos)


# Q3: Overall Business Performance

# National Trend Plot
national_wpi <- btos %>%
  group_by(Period) %>%
  summarise(Avg_WPI = mean(Performance.Score, na.rm = TRUE), .groups = 'drop')

ggplot(national_wpi, aes(x = Period, y = Avg_WPI, group = 1)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(title = "National Trend in Business Performance between 2023-2025 (WPI) - Q3",
       x = "Survey Period", y = "Weighted Performance Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# State Ranking
state_ranking_wpi <- btos %>%
  group_by(State) %>%
  summarise(Avg_WPI = mean(Performance.Score, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Avg_WPI) %>%
  mutate(State = factor(State, levels = State))

ggplot(state_ranking_wpi, aes(x = Avg_WPI, y = State, fill = Avg_WPI)) +
  geom_col() +
  scale_fill_gradient2(low = "darkred", mid = "grey80", high = "#0072B2", midpoint = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "State Ranking by Average Business Performance between 2023-2025 (WPI) - Q3",
       x = "Weighted Performance Index", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")





# Q5: Expected Change in Paid Employees
# National Trend
national_employees <- btos %>%
  group_by(Period) %>%
  summarise(Avg_Employees = mean(Net.Employee.Q5.Index, na.rm = TRUE), .groups = 'drop')

ggplot(national_employees, aes(x = Period, y = Avg_Employees, group = 1)) +
  geom_line(color = "#CC79A7", linewidth = 1.2) +
  geom_point(color = "#CC79A7", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "National Trend in number of paid employees between 2023-2025 (Q5)",
       x = "Survey Period", y = "Net Change Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# State Ranking
state_ranking_employees <- btos %>%
  group_by(State) %>%
  summarise(Avg_Employees = mean(Net.Employee.Q5.Index, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Avg_Employees) %>%
  mutate(State = factor(State, levels = State))

ggplot(state_ranking_employees, aes(x = Avg_Employees, y = State, fill = Avg_Employees)) +
  geom_col() +
  scale_fill_gradient2(low = "darkred", mid = "grey80", high = "#CC79A7", midpoint = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "State Ranking by Average Change in number of paid employees between 2023-2025 (Q5)",
       x = "Net Change Index", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")



# Q6: Total Number of Hours Worked
# National Trend
national_hours <- btos %>%
  group_by(Period) %>%
  summarise(Avg_Hours = mean(Net.Hours.Q6.Index, na.rm = TRUE), .groups = 'drop')

ggplot(national_hours, aes(x = Period, y = Avg_Hours, group = 1)) +
  geom_line(color = "#D55E00", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "National Trend in Change in Hours Worked by Employees (Q6) between 2023–2025",
       x = "Survey Period", y = "Net Change Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# State Ranking
state_ranking_hours <- btos %>%
  group_by(State) %>%
  summarise(Avg_Hours = mean(Net.Hours.Q6.Index, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Avg_Hours) %>%
  mutate(State = factor(State, levels = State))

ggplot(state_ranking_hours, aes(x = Avg_Hours, y = State, fill = Avg_Hours)) +
  geom_col() +
  scale_fill_gradient2(low = "darkred", mid = "grey80", high = "#D55E00", midpoint = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "State Ranking by Change in Hours Worked (Q6)",
       x = "Net Change Index", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")




# Q7: Change in AI Usage
# National Trend
national_ai <- btos %>%
  group_by(Period) %>%
  summarise(Avg_AI = mean(Net.AI.Q7.Index, na.rm = TRUE), .groups = 'drop')

ggplot(national_ai, aes(x = Period, y = Avg_AI, group = 1)) +
  geom_line(color = "#D55E00", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "National Trend in AI Adoption between 2023-2025, Q7",
       x = "Survey Period", y = "Net AI Adoption Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# State Ranking for AI Usage Change (Q7)
state_ranking_ai_q7 <- btos %>%
  group_by(State) %>%
  summarise(Avg_Net_AI_Q7 = mean(Net.AI.Q7.Index, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Avg_Net_AI_Q7) %>%
  mutate(State = factor(State, levels = State))  # Order bars by value

# Horizontal Bar Chart
ggplot(state_ranking_ai_q7, aes(x = Avg_Net_AI_Q7, y = State, fill = Avg_Net_AI_Q7)) +
  geom_col() +
  scale_fill_gradient2(low = "darkred", mid = "grey80", high = "#D55E00", midpoint = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "State Ranking by Change in AI Adoption (Q7) Between 2023 to 2025",
    x = "Net AI Adoption Index",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")



#Q24: Expected Change in AI Usage
# National Trend
national_ai_expect <- btos %>%
  group_by(Period) %>%
  summarise(Avg_Net_AI = mean(Net.AI.Q24.Index, na.rm = TRUE), .groups = 'drop')

ggplot(national_ai_expect, aes(x = Period, y = Avg_Net_AI, group = 1)) +
  geom_line(color = "#56B4E9", linewidth = 1.2) +
  geom_point(color = "#56B4E9", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "National Trend of Expected Change in AI Usage (Q24) Between 2023–2025",
       x = "Survey Period", y = "Net Change Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# State Ranking
state_ranking_ai <- btos %>%
  group_by(State) %>%
  summarise(Avg_Net_AI = mean(Net.AI.Q24.Index, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Avg_Net_AI) %>%
  mutate(State = factor(State, levels = State))

ggplot(state_ranking_ai, aes(x = Avg_Net_AI, y = State, fill = Avg_Net_AI)) +
  geom_col() +
  scale_fill_gradient2(low = "darkred", mid = "grey80", high = "#56B4E9", midpoint = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "State Ranking of Expected Change in AI Usage (Q24) Between 2023-2025",
       x = "Net Change Index", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")






