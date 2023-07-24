data <- readRDS("data/USvideos.RDS")
View(data) # trending_date = year/day/month

# Libraries ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(rjson)
library(lubridate)
library(parsnip)
library(rsample)
library(yardstick)
library(reshape2)
library(MLmetrics)
library(Metrics)


# Research Checkpoint 1: Cleaning the Data Set ####

# Renaming Columns
renamed_data <- rename(data,
                   channel = channel_title,
                   videos_removed = video_error_or_removed)
View(renamed_data)


# Videos ranked by Views
VidView <- renamed_data |>
  arrange(views) |>
  select(title, channel, likes, dislikes, comment_count, views)
View(VidView)

# New Variables for Categorical_ID ####
new_data <- mutate(renamed_data, category_id = case_when(
  category_id == 1 ~ "Film & Animation",
  category_id == 2 ~ "Autos & Vehicles",
  category_id == 10 ~ "Music",
  category_id == 15 ~ "Pets & Animals",
  category_id == 16 ~ "name",
  category_id == 17 ~ "Sports",
  category_id == 18 ~ "Short Movies",
  category_id == 19 ~ "Travel & Events",
  category_id == 20 ~ "Gaming",
  category_id == 21 ~ "Videoblogging",
  category_id == 22 ~ "People & Blogs",
  category_id == 23 ~ "Comedy",
  category_id == 24 ~ "Entertainment",
  category_id == 25 ~ "News & Politics",
  category_id == 26 ~ "Howto & Style",
  category_id == 27 ~ "Education",
  category_id == 28 ~ "Science & Technology",
  category_id == 29 ~ "Nonprofits & Activism",
  category_id == 30 ~ "Movies",
  category_id == 31 ~ "Anime/Animation",
  category_id == 32 ~ "Action/Adventure",
  category_id == 33 ~ "Classics",
  category_id == 34 ~ "Comedy",
  category_id == 35 ~ "Documentary",
  category_id == 36 ~ "Drama",
  category_id == 37 ~ "Family",
  category_id == 38 ~ "Foreign",
  category_id == 39 ~ "Horror",
  category_id == 40 ~ "Sci-Fi/Fantasy",
  category_id == 41 ~ "Thriller",
  category_id == 42 ~ "Shorts",
  category_id == 43 ~ "Shows",
  category_id == 44 ~ "Trailers"))
View(new_data)

# Separate publish_time into 2 Column: Month_Publish and Time_Publish



# Research Checkpoint 2: Analyze Variables of Data & Create Testable Research Question ####

# Categorical Variables: videos_id, title, channel, tags, comment/ratings/videos_disabled/removed, description
# Numeric Variables: views, likes, dislikes, comment_count
# Both: trending_date, category_id?, publish_time, 

## Notes and Questions ##
# What do the Numbers in the category_id Variable mean?
  # No information about Category ID Numbers
# How is the trending_date's are Organized?
  # Is it from Year / Day / Month?
# How is the publish_time Organized?
  # What do the T represents?

# Research Questions
  # How do the Title of the Video and the Time it was Published influences the View Counts?
  # How do the Title of the Videos influences the View Counts of it?
  # How does the Time it was Published of the Videos affect the View Counts of it?
  # How does the Category of the Videos affect the View Counts of it? (Probably the Best)


## Plots ####
ggplot()

# Histogram Plot for Views (1 Numeric Variable)
ggplot(data = renamed_data, aes(x = views)) +
  geom_histogram()

# Research Question Plot?
# Bar Plot between Views and Category ID (1 Categorical Variable & 1 Numeric Variable) 
ggplot(data = renamed_data, aes(x = title, y = views)) +
  geom_bar(stat = "summary",
              fun = "mean")

## BEST BARPLOT ##
ggplot(data = renamed_data, aes(x = category_id, y = views)) +
  geom_bar(stat = "summary",
           fun = "mean")

# Scatter Plot for Most Common between Likes and Dislikes (1 Numeric Variable vs 1 Numeric Variables)
ggplot(data = renamed_data, aes(x = likes, y = dislikes)) +
  geom_point()

# Line Plot between Likes and Dislikes (1 Numeric Variable vs 1 Numeric Variables)
ggplot(data = renamed_data, aes(x = likes, y = dislikes)) +
  geom_line(stat = "summary",
            fun = "mean")


# Research Checkpoint: 3 ####
# Formatting Data


# Conduct Statistical Analysis
# Machine Learning Model

# Step 1 and 2:
VidOnlDat <- select(new_data, -c(video_id, trending_date, publish_time, tags, thumbnail_link:description))
View(VidOnlDat)

str(VidOnlDat)
VidOnlDat_nochr <- mutate(VidOnlDat, title = as.integer(as.factor(title)),
                          channel = as.integer(as.factor(channel)),
                          category_id = as.integer(as.factor(category_id)))
VidOnlDat_nochr
str(VidOnlDat_nochr)

# Step 3:
VidOnlDat_Cors <- VidOnlDat_nochr |>
  cor() |>
  melt() |>
  as.data.frame()

VidOnlDat_Cors
View(VidOnlDat_Cors)

ggplot(VidOnlDat_Cors, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", high = "white", mid = "darkgreen",
                       midpoint = 0)

# Step 4 and 5:
set.seed(72423)

# Regression Data Split (Predict Numeric Variables) (Predict View Counts)
VidOnlDat_split <- initial_split(VidOnlDat_nochr, prop = .75) # Use 75% of data for Training
VidOnlDat_train <- training(VidOnlDat_split)
VidOnlDat_test <- testing(VidOnlDat_split)


# Step 6 and 7:

#Linear Model
VidOnlDat_lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(views ~ .,
      data = VidOnlDat_train)

VidOnlDat_lm_fit
VidOnlDat_lm_fit$fit
summary(VidOnlDat_lm_fit$fit)


# Boosted Decision Trees
VidOnlDat_boost_reg_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(views ~ ., data = VidOnlDat_train)

VidOnlDat_boost_reg_fit
VidOnlDat_boost_reg_fit$fit
summary(VidOnlDat_boost_reg_fit$fit)
VidOnlDat_boost_reg_fit$fit$evaluation_log


# Step 8:
VidOnlDat_reg_results <- VidOnlDat_test

#Linear Regression
VidOnlDat_reg_results$lm_pred <- predict(VidOnlDat_lm_fit, VidOnlDat_test)$.pred

# Error for LR
yardstick::mae(VidOnlDat_reg_results, views, lm_pred)
yardstick::rmse(VidOnlDat_reg_results, views, lm_pred)


# Boosted Tree Regression
VidOnlDat_reg_results$boost_pred <- predict(VidOnlDat_boost_reg_fit, VidOnlDat_test)$.pred

# Error for BT
yardstick::mae(VidOnlDat_reg_results, views, boost_pred)
yardstick::rmse(VidOnlDat_reg_results, views, boost_pred)























































