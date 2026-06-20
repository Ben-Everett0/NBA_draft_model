### NBA Draft Model
#### Using Jeremias Engelmann methodology
#### Random forest + adding player role

### SETUP ---------------------------------------

# Libraries
library(tidyverse)
library(randomForest)
library(readxl)

# Loading data
data <- read_excel("draftmodeldata.xlsx")

### DATA PREPROCESSING --------------------------

# Generating metrics from raw data
mod_data <- data %>%
  mutate(ft_perc = ftm / fta, # Free throw percentage (efficiency)
         fta_per_100 = (fta / NCAA_possessions_est) * 100, # Free throw attempts per 100 possessions
         X3fg_perc = X3fgm / X3fga, # 3-point percentage (efficiency)
         X3fga_per_100 = (X3fga / NCAA_possessions_est) * 100, # 3-point attempts per 100 possessions
         rim_perc = rim_made / (rim_made + rim_miss), # Rim percentage (efficiency)
         rim_per_100 = ((rim_made + rim_miss) / NCAA_possessions_est) * 100, # Rim attempts per 100 possessions
         mid_perc = mid_made / (mid_made + mid_miss), # Mid-range percentage (efficiency)
         mid_per_100 = ((mid_made + mid_miss) / NCAA_possessions_est) * 100, # Mid-range attempts per 100 possessions
         oreb_per_100 = (oreb / NCAA_possessions_est) * 100, # Offensive rebounds per 100 possessions
         dreb_per_100 = (dreb / NCAA_possessions_est) * 100, # Defensive rebounds per 100 possessions
         ast_per_100 = (ast / NCAA_possessions_est) * 100, # Assists per 100 possessions
         stl_per_100 = (stl / NCAA_possessions_est) * 100, # Steals per 100 possessions
         blks_per_100 = (blks / NCAA_possessions_est) * 100, # Blocks per 100 possessions
         stocks_per_100 = stl_per_100 + blks_per_100, # Stocks per 100 possessions
         pts_per_100 = (pts / NCAA_possessions_est) * 100, # Points per 100 possessions
         tov_per_100 = (tov / NCAA_possessions_est) * 100) # Turnovers per 100 possessions

all_possible_roles <- unique(data$player_role)

# Separate the data
train_raw <- mod_data %>% filter(season < 2026) %>% drop_na(NBA_impact)
test_raw <- mod_data %>% filter(season == 2026)

# Clean up percentages to prevent NaN crashes
train_raw <- train_raw %>% mutate(across(c(X3fg_perc, ft_perc, rim_perc, mid_perc), ~replace_na(., 0)))
test_raw <- test_raw %>% mutate(across(c(X3fg_perc, ft_perc, rim_perc, mid_perc), ~replace_na(., 0)))

# Core predictors
features <- c("height", "rec_rank", "age", "sos", "ft_perc", "X3fga_per_100", "rim_perc", "rim_per_100", "mid_per_100", "ast_per_100",
              "stocks_per_100", "tov_per_100", "international")

# Convert to numeric
train_data <- as.data.frame(lapply(train_raw[,c("NBA_impact",features)], as.numeric))
train_data$player_role <- factor(train_raw$player_role, levels = all_possible_roles)
test_data  <- as.data.frame(lapply(test_raw[,features], as.numeric))
test_data$player_role <- factor(test_raw$player_role, levels = all_possible_roles)

train_data <- train_data %>%
  mutate(NBA_impact = replace_na(NBA_impact, min(train_data$NBA_impact, na.rm = TRUE))) # Replace the NA values of NBA impact with minimum value

test_data <- test_data %>%
  mutate(rec_rank = replace_na(rec_rank, mean(test_data$rec_rank, na.rm = TRUE))) # Replace the NA values of rec_rank with average value


### RANDOM FOREST MODEL WITH ROLES -------------------------

set.seed(123)
rf_model <- randomForest(
  NBA_impact ~ player_role + height + rec_rank + age + sos + ft_perc + X3fga_per_100 + rim_perc + rim_per_100 + mid_per_100 + 
    ast_per_100 + stocks_per_100 + tov_per_100 + international,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Viewing results
test_2026 <- mod_data %>%
  filter(season == 2026)

test_2026$predicted_impact_rf <- predict(rf_model, newdata = test_data)

test_2026 %>%
  select(name, player_role, predicted_impact_rf) %>%
  arrange(desc(predicted_impact_rf)) %>%
  print(n = 35)

# Probability outcomes
rf_predictions <- predict(rf_model, newdata = test_data, predict.all = TRUE)
individual_tree_votes <- rf_predictions$individual

# Function to calculate tier percentages for a single player's tree votes
calculate_tier_probs <- function(player_votes) {
  total_trees <- length(player_votes)
  
  prob_below_0 <- sum(player_votes < 0) / total_trees
  prob_0_to_15 <- sum(player_votes >= 0 & player_votes < 1.5) / total_trees
  prob_15_to_3  <- sum(player_votes >= 1.5 & player_votes <= 3) / total_trees
  prob_above_3 <- sum(player_votes > 3) / total_trees
  
  return(c(Bust = prob_below_0, Starter = prob_0_to_15, AllStar = prob_15_to_3, Superstar = prob_above_3))
}

# Apply the function across all 2026 prospects
tier_probabilities_matrix <- t(apply(individual_tree_votes, 1, calculate_tier_probs))

# Merge results back into a readable draft board
prob_draft_board_rf <- cbind(name = test_2026$name, as.data.frame(tier_probabilities_matrix)) %>%
  arrange(desc(Superstar), desc(AllStar))

print(head(prob_draft_board_rf, 10))

