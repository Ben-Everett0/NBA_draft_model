### NBA Draft Model
#### Using Jeremias Engelmann methodology
#### Adding Win Shares as target + final weightings

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
test_data  <- as.data.frame(lapply(test_raw[,c(features, "expert_mock")], as.numeric))
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
rf_model_contrib <- randomForest(
  NBA_contributions ~ player_role + height + rec_rank + age + sos + ft_perc + X3fga_per_100 + rim_perc + rim_per_100 + mid_per_100 + 
    ast_per_100 + stocks_per_100 + tov_per_100 + international,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

test_2026 <- mod_data %>%
  filter(season == 2026)

# Saving predictions for both models
pred_impact <- predict(rf_model_impact, newdata = test_data)
pred_contrib <- predict(rf_model_contrib, newdata = test_data)

# Normalize both predictions using Min-Max scaling (0 to 1 scale)
norm_impact  <- (pred_impact - min(pred_impact)) / (max(pred_impact) - min(pred_impact))
norm_contrib <- (pred_contrib - min(pred_contrib)) / (max(pred_contrib) - min(pred_contrib))

# We use a log-transform on expert mocks because pick values drop off exponentially.
mod_data_clean <- test_2026 %>%
  mutate(
    # Handle missing expert mocks safely by pushing them to the end of the draft (e.g., 65)
    expert_mock_clean = ifelse(is.na(expert_mock) | expert_mock == 0, 65, as.numeric(expert_mock)),
    # Log-transform and invert Expert Mock (Higher score = higher projected pick)
    norm_expert_mock = (log(65) - log(expert_mock_clean)) / (log(65) - log(1))
  )

# Define your Master Weighting Architecture (Must sum to 1.0)
w_impact   <- 0.40  # Pure talent/ceiling baseline
w_contrib  <- 0.20  # Role efficiency baseline
w_expert   <- 0.40  # Scout/market consensus prior

# Compute the Unified Stabilized Grade
final_stabilized_score <- (norm_impact * w_impact) + 
  (norm_contrib * w_contrib) +
  (mod_data_clean$norm_expert_mock * w_expert)

# Compile the Final Stabilized 2026 Draft Board
stabilized_board <- mod_data_clean %>%
  mutate(
    raw_pred_impact   = round(pred_impact, 2),
    raw_pred_contrib  = round(pred_contrib, 3),
    # Map cleanly to your target 0-100 index scale
    draft_grade       = round(final_stabilized_score * 100, 1)
  ) %>%
  arrange(desc(draft_grade)) %>%
  select(name, player_role, draft_grade, raw_pred_impact, raw_pred_contrib, expert_mock)

print("=== MARKET-STABILIZED 2026 DRAFT BOARD ===")
print(head(stabilized_board, 30))

