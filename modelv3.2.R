### NBA Draft Model
#### Using Jeremias Engelmann methodology
#### Adding Win Shares as target

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
         tov_per_100 = (tov / NCAA_possessions_est) * 100, # Turnovers per 100 possessions
         NBA_contributions = (NBA_WS / NBA_minutes) * 48) # NBA win shares per 48 minutes

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
train_data <- as.data.frame(lapply(train_raw[,c("NBA_impact","NBA_contributions",features)], as.numeric))
train_data$player_role <- factor(train_raw$player_role, levels = all_possible_roles)
test_data  <- as.data.frame(lapply(test_raw[,features], as.numeric))
test_data$player_role <- factor(test_raw$player_role, levels = all_possible_roles)

train_data <- train_data %>%
  mutate(NBA_impact = replace_na(NBA_impact, min(train_data$NBA_impact, na.rm = TRUE))) # Replace the NA values of NBA impact with minimum value

test_data <- test_data %>%
  mutate(rec_rank = replace_na(rec_rank, mean(test_data$rec_rank, na.rm = TRUE))) # Replace the NA values of rec_rank with average value


### RANDOM FOREST MODEL WITH ROLES -------------------------

## Impact model
set.seed(123)
rf_model_impact <- randomForest(
  NBA_impact ~ player_role + height + rec_rank + age + sos + ft_perc + X3fga_per_100 + rim_perc + rim_per_100 + mid_per_100 + 
    ast_per_100 + stocks_per_100 + tov_per_100 + international,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

## Contributions model
set.seed(123)
rf_model_cont <- randomForest(
  NBA_contributions ~ player_role + height + rec_rank + age + sos + ft_perc + X3fga_per_100 + rim_perc + rim_per_100 + mid_per_100 + 
    ast_per_100 + stocks_per_100 + tov_per_100 + international,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Viewing results
test_2026 <- mod_data %>%
  filter(season == 2026)



pred_impact <- predict(rf_model_impact, newdata = test_data)
pred_contrib <- predict(rf_model_cont, newdata = test_data)

# Normalize both predictions using Min-Max scaling (0 to 1 scale)
norm_impact  <- (pred_impact - min(pred_impact)) / (max(pred_impact) - min(pred_impact))
norm_contrib <- (pred_contrib - min(pred_contrib)) / (max(pred_contrib) - min(pred_contrib))

# Weighting
weight_impact <- 0.80
weight_contrib <- 0.20

final_blended_score <- (norm_impact * weight_impact) + (norm_contrib * weight_contrib)

# Draft Board
final_board <- test_2026 %>%
  mutate(
    raw_pred_impact = pred_impact,
    raw_pred_contrib = pred_contrib,
    draft_index = final_blended_score * 100
  ) %>%
  arrange(desc(draft_index)) %>%
  select(name, player_role, raw_pred_impact, raw_pred_contrib, draft_index)


train_view <- mod_data %>%
  filter(season != 2026)


# Generate historical predictions on the training data from both models
train_pred_impact  <- predict(rf_model_impact, newdata = train_data)
train_pred_contrib <- predict(rf_model_cont, newdata = train_data)

# Extract the actual historical targets to evaluate model error later
actual_impact  <- train_data$NBA_impact
actual_contrib <- train_data$NBA_contributions

# Scale and blend the predictions (70% Impact / 30% Contributions)
norm_train_impact  <- (train_pred_impact - min(train_pred_impact)) / (max(train_pred_impact) - min(train_pred_impact))
norm_train_contrib <- (train_pred_contrib - min(train_pred_contrib)) / (max(train_pred_contrib) - min(train_pred_contrib))

weight_impact  <- 0.70
weight_contrib <- 0.30
blended_train_score <- (norm_train_impact * weight_impact) + (norm_train_contrib * weight_contrib)

# Compile the historical draft board dataframe
historical_board <- train_view %>%
  filter(NBA_minutes != 0) %>%
  mutate(
    pred_impact   = round(train_pred_impact, 2),
    actual_impact = round(actual_impact, 2),
    pred_contrib  = round(train_pred_contrib, 3),
    actual_contrib = round(actual_contrib, 3),
    # Scale final index score out of 100 for clean grading views
    draft_grade   = round(blended_train_score * 100, 1),
    # Calculate a simple residual variance metric to flag model blindspots
    impact_error  = round(pred_impact - actual_impact, 2)
  ) %>%
  # Sort by your custom blended evaluation metric
  arrange(desc(draft_grade)) %>%
  # Select the crucial evaluation identifiers
  select(name, season, player_role, draft_grade, pred_impact, actual_impact, pred_contrib, actual_contrib, impact_error)

# Print the top 25 historical model hits to the console
print("=== HISTORICAL TRAINING DATA DRAFT BOARD ===")
print(head(historical_board, 25))


