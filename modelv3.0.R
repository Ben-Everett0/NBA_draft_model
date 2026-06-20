### NBA Draft Model
#### Using Jeremias Engelmann methodology

### SETUP ---------------------------------------

# Libraries
library(tidyverse)
library(corrplot)
library(car)

# Loading data
data <- read.csv2("jedraftdata.csv", header = TRUE, sep = ",")

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

mod_data$X3fg_perc[is.nan(mod_data$X3fg_perc)] <- 0 # Turning NaN to 0 for players with no attempted 3's

# Separating training data from testing data
train_raw <- mod_data %>% filter(season < 2026) %>%
  select(height, rec_rank, mpg, age, sos, team_strength, # general stats
         ft_perc, fta_per_100, X3fg_perc, X3fga_per_100, rim_perc, rim_per_100, mid_perc, mid_per_100, # shooting stats
         oreb_per_100, dreb_per_100, ast_per_100, stocks_per_100, pts_per_100, tov_per_100, # other box score stats
         international, NBA_impact) # binary variable and target variable

test_raw <- mod_data %>% filter(season == 2026) %>%
  select(height, rec_rank, mpg, age, sos, team_strength, # general stats
         ft_perc, fta_per_100, X3fg_perc, X3fga_per_100, rim_perc, rim_per_100, mid_perc, mid_per_100, # shooting stats
         oreb_per_100, dreb_per_100, ast_per_100, stocks_per_100, pts_per_100, tov_per_100, # other box score stats
         international) # binary variable and target variable

# Convert columns to numeric
train_data <- as.data.frame(lapply(train_raw, as.numeric))
test_data  <- as.data.frame(lapply(test_raw, as.numeric))

# Checking for NA's
colSums(is.na(train_data)) # Checking for Na's - we have some for NBA impact

train_data <- train_data %>%
  mutate(NBA_impact = replace_na(NBA_impact, min(train_data$NBA_impact, na.rm = TRUE))) # Replace the NA values of NBA impact with minimum value

colSums(is.na(test_data)) # Checking for Na's - we have some for rec_rank

test_data <- test_data %>%
  mutate(rec_rank = replace_na(rec_rank, mean(test_data$rec_rank, na.rm = TRUE))) # Replace the NA values of rec_rank with average value

# Scaling the data
cols_to_scale <- c(1:20)
scaled_train_matrix <- scale(train_data[,cols_to_scale]) # Normalizing all continuous predictor variables
train_means <- attr(scaled_train_matrix, "scaled:center") # Extracting means from scaled training data
train_sds <- attr(scaled_train_matrix, "scaled:scale") # Extracting sds from scaled training data

train_data[, cols_to_scale] <- scaled_train_matrix # Placing scaled data into our data frame

test_data[, cols_to_scale] <- scale(test_data[, cols_to_scale], 
                                    center = train_means, 
                                    scale = train_sds)

### REGRESSION MODEL ---------------

# Checking for multicollinearity
numeric_predictors <- train_data %>%
  select(1:20) %>%
  drop_na()

corrplot(cor(numeric_predictors), type = "upper") # Potential issues: height with rebounding and stocks, ft rate and points

# Fitting the initial model
mod1 <- lm(NBA_impact ~ ., data = train_data)
summary(mod1)

# Checking for VIF
vif(mod1) # No variables > 10, closest is pts_per_100 with 9.464 - no variables will be removed for this purpose

# With 0.1 significance level, keeping the following variables:
# height + rec_rank + age + sos + ft_perc + X3fga_per_100 + rim_perc + rim_per_100 + mid_per_100 + ast_per_100 + stocks_per_100 + tov_per_100 + international

red_mod1 <- lm(NBA_impact ~ height + rec_rank + age + sos + ft_perc + X3fga_per_100 + rim_perc + rim_per_100 + mid_per_100 + 
                 ast_per_100 + stocks_per_100 + tov_per_100 + international, data = train_data)
summary(red_mod1)

# Viewing results
test_2026 <- mod_data %>%
  filter(season == 2026)

test_2026$predicted_impact <- predict(red_mod1, newdata = test_data)
test_2026 %>%
  select(name, predicted_impact) %>%
  arrange(desc(predicted_impact))







