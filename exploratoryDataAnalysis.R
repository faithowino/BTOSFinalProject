---
  title: "Final Project - Exploratory data analysis"
author: "Faith"
date: "17th November"

---
  
  ## Load packages and data
  
  
  library(tidyverse)
library(dplyr)
library(dbplyr)
library(ggplot2)
library(scales)


btosdataset <-read.csv("~/Desktop/Modern Data Science/Final Project/State.csv")

#Clean data
clean_btos <- btosdataset %>%
  mutate(across(starts_with("X20"), ~ na_if(.x, "S"))) %>%  
  mutate(across(starts_with("X20"), ~ parse_number(.x))) 


clean_btos <- clean_btos %>%
  pivot_longer(
    cols = starts_with("X20"),
    names_to = "Period",
    values_to = "Value"
  ) %>%
  mutate(
    Period = as.factor(Period)     
  )


#1.Overall, how are Businesse's currently performing ?

# Select Question #3 (Overall Performance) 
performance <- clean_btos %>%
  filter(Question.ID == 3) %>%         
  select(State, Answer,Period,Value) 

performance_summary <- performance %>%
  mutate(Performance = case_when(
    Answer %in% c("Excellent", "Good") ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  group_by(State, Period, Performance) %>%
  summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop")

performance_national <- performance_summary %>%
  group_by(Period, Performance) %>%
  summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop")

ggplot(performance_national, aes(x = Period, y = Value, color = Performance, group = Performance)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "BTOS – Overall National Business Performance",
       x = "Period", y = "% of Businesses")

ggplot(performance_summary, aes(x = Period, y = Value, color = Performance, group = Performance)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  facet_wrap(~ State, scales = "free_y") +
  labs(title = "BTOS – Business Performance Trends by State",
       x = "Period", y = "% of Businesses")





#2. How did the total number of hours worked by  paid employees change between 2024-2025?

hours_worked <- clean_btos %>%
  filter(`Question.ID` == 6) %>%    
  select(State, Answer, Period, Value) %>%
  mutate(Answer_Other = ifelse(is.na(Value), "Other", Answer))  

hours_worked_summary <- hours_worked %>%
  group_by(Period, Answer_Other) %>%
  summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop")

ggplot(hours_worked_summary, aes(x = Period, y = Value, fill = Answer_Other)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +
  labs(
    title = "Change in Total Hours Worked by Paid Employees (Q6)",
    x = "Period",
    y = "Percent of Businesses",
    fill = "Category"
  )



#3. How did the businesses use of AI in producing goods or services change between 2024-2025?
ai_usage <- clean_btos %>%
  filter(`Question.ID` == 7) %>%
  mutate(Answer_Other = ifelse(is.na(Value), "Other", Answer))

ai_usage_summary <- ai_usage %>%
  group_by(Period, Answer_Other) %>%
  summarize(Average = mean(Value, na.rm = TRUE), .groups = "drop")

ggplot(ai_usage_summary, aes(x = Period, y = Average, fill = Answer_Other)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Changes in Business AI Usage (2024–2025)",
    x = "Period",
    y = "Percent of Businesses",
    fill = "AI Usage"
  ) 


#4.How do businesses think their number of paid employees is going to change ?
employees_change <- clean_btos %>%
  filter(`Question.ID` == 17) %>%
  select(State, Answer, Period, Value) %>%
  mutate(Answer_Other = ifelse(is.na(Value), "Other", Answer))

employees_change_summary <- employees_change %>%
  group_by(Period, Answer_Other) %>%
  summarize(Average = mean(Value, na.rm = TRUE), .groups = "drop")

ggplot(employees_change_summary, aes(x = Period, y = Average, fill = Answer_Other)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Expected Change in Number of Paid Employees (Q4)",
    x = "Period",
    y = "Percent of Businesses",
    fill = "Category"
  )

#5. How do businesses think the number of hours worked by their paid employees is going to change ?
hours_change <- clean_btos %>%
  filter(`Question.ID` == 18) %>%
  select(State, Answer, Period, Value) %>%
  mutate(Answer_Other = ifelse(is.na(Value), "Other", Answer)) 

hours_change_summary <- hours_change %>%
  group_by(Period, Answer_Other) %>%
  summarize(Average = mean(Value, na.rm = TRUE), .groups = "drop")

ggplot(hours_change_summary, aes(x = Period, y = Average, fill = Answer_Other)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Expected Change in Hours Worked by Paid Employees (Q5)",
    x = "Period",
    y = "Percent of Businesses",
    fill = "Category"
  )


#6. How do businesses think their use of Artificial Intelligence is going to change ?

ai_change <- clean_btos %>%
  filter(`Question.ID` == 6) %>%
  select(State, Answer, Period, Value) %>%
  mutate(Answer_Other = ifelse(is.na(Value), "Other", Answer))

ai_change_summary <- ai_change %>%
  group_by(Period, Answer_Other) %>%
  summarize(Average = mean(Value, na.rm = TRUE), .groups = "drop")

ggplot(ai_change_summary, aes(x = Period, y = Average, fill = Answer_Other)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Expected Change in AI Usage by Businesses (Q6)",
    x = "Period",
    y = "Percent of Businesses",
    fill = "Category"
  ) 
