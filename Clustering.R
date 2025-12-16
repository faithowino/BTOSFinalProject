---
  title: "Final Project - Clustering States based on performance "
author: "Faith"
date: "15th December"

---

#Load packages and data
library(factoextra)
library(tibble)
library(dplyr)
library(readr) 

btos_data <- read_csv("final_btos_features.csv")

#Average data by State
numeric_cols <- names(btos_data)[!names(btos_data) %in% c("State", "Period", "Survey.Year", "Survey.WeekNum")]

state_avg_data <- btos_data %>%
  group_by(State) %>%
  summarise(across(all_of(numeric_cols), mean, .names = "Avg.{.col}")) %>%
  ungroup()

# Extract State column to use as row names later
state_names <- state_avg_data$State

# Remove non-numeric columns
btos_data_num_aggregated <- state_avg_data %>%
  select(-State)


btos_data_num_aggregated <- btos_data_num_aggregated %>%
  column_to_rownames(var = "Avg.Performance.Score") 

#kmeans algorithm
fviz_nbclust(btos_data_num_aggregated, kmeans, method = "wss") 

k_btos <- kmeans(btos_data_num_aggregated, centers = 3, nstart = 25)

# summary
#Cluster sizes
print(k_btos$size)

#Total Within-Cluster Sum of Squares
print(k_btos$tot.withinss)

#Average profile for each cluster
print(k_btos$centers)

# Assign the cluster results back to the state names
cluster_assignment <- k_btos$cluster

state_cluster_final <- data.frame(State = state_names, Cluster = as.factor(cluster_assignment))

print(head(state_cluster_final))



# Filter for the best-performing group (Cluster 1)
cluster_1_states <- state_cluster_final %>%
  filter(Cluster == 1) %>%
  select(State)

print(cluster_1_states$State)

# Filter for the best-performing group (Cluster 2)
cluster_2_states <- state_cluster_final %>%
  filter(Cluster == 2) %>%
  select(State)

print(cluster_2_states$State)

# Filter for the best-performing group (Cluster 3)
cluster_3_states <- state_cluster_final %>%
  filter(Cluster == 3 ) %>%
  select(State)

print(cluster_3_states$State)
