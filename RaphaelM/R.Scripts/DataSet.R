data <- readRDS("data/USvideos.RDS")
datacatgjson <- fromJSON(file="data/US_category_id.json")
View(data) # trending_date = year/day/month
View(datacatgjson)

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
library(gganimate)
library(geomtextpath)
library(gghighlight)


# Research Checkpoint 1: Cleaning the Data Set ####

# Renaming Columns
renamed_data <- rename(data,
                   channel = channel_title,
                   videos_removed = video_error_or_removed,
                   genre = category_id,
                   comments = comment_count)
View(renamed_data)



# Separate publish_time into 2 Column: Month_Publish and Time_Publish
newvar_data <- mutate(renamed_data,
       month_publish = month(publish_time),
       time_published_hrs = hour(publish_time))
View(newvar_data)



# Videos ranked by Views
VidView <- renamed_data |>
  arrange(views) |>
  select(title, channel, likes, dislikes, comments, views)
View(VidView)

# New Variables for Categorical_ID ####
# First Version
new_data <- mutate(newvar_data, genre = case_when(
  genre == 1 ~ "Film & Animation",
  genre == 2 ~ "Autos & Vehicles",
  genre == 10 ~ "Music",
  genre == 15 ~ "Pets & Animals",
  genre == 16 ~ "name",
  genre == 17 ~ "Sports",
  genre == 18 ~ "Short Movies",
  genre == 19 ~ "Travel & Events",
  genre == 20 ~ "Gaming",
  genre == 21 ~ "Videoblogging",
  genre == 22 ~ "People & Blogs",
  genre == 23 ~ "Comedy",
  genre == 24 ~ "Entertainment",
  genre == 25 ~ "News & Politics",
  genre == 26 ~ "Howto & Style",
  genre == 27 ~ "Education",
  genre == 28 ~ "Science & Technology",
  genre == 29 ~ "Nonprofits & Activism",
  genre == 30 ~ "Movies",
  genre == 31 ~ "Anime/Animation",
  genre == 32 ~ "Action/Adventure",
  genre == 33 ~ "Classics",
  genre == 34 ~ "Comedy",
  genre == 35 ~ "Documentary",
  genre == 36 ~ "Drama",
  genre == 37 ~ "Family",
  genre == 38 ~ "Foreign",
  genre == 39 ~ "Horror",
  genre == 40 ~ "Sci-Fi/Fantasy",
  genre == 41 ~ "Thriller",
  genre == 42 ~ "Shorts",
  genre == 43 ~ "Shows",
  genre == 44 ~ "Trailers"))
View(new_data)



# Research Checkpoint 2: Analyze Variables of Data & Create Testable Research Question ####

# Categorical Variables: videos_id, title, channel, tags, comment/ratings/videos_disabled/removed, description
# Numeric Variables: views, likes, dislikes, comment_count
# Both: trending_date, category_id?, publish_time, 

# Research Questions ####
  # How does the Time it was Published of the Videos affect the View Counts of it?
  # How does the Category of the Videos affect the View Counts of it? (Probably the Best)


## Plots ####
ggplot()

# Histogram Plot for Views (1 Numeric Variable)
ggplot(new_data, aes(x = views)) +
  geom_histogram()

# Research Question Plot?
# Bar Plot between Views and title (1 Categorical Variable & 1 Numeric Variable) 
ggplot(new_data, aes(x = title, y = views, fill = genre)) +
  geom_bar(stat = "summary",
           fun = "mean")



## BEST BARPLOT ##
ggplot(renamed_data, aes(x = genre, y = views, fill = genre)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Genre", y = "Views",
       title = "Most Viewed Genre on Youtube")


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
VidOnlDat <- select(new_data, -c(video_id, trending_date, publish_time, tags, thumbnail_link:time_published_hrs))
View(VidOnlDat)

str(VidOnlDat)
VidOnlDat_nochr <- mutate(VidOnlDat, title = as.integer(as.factor(title)),
                          channel = as.integer(as.factor(channel)),
                          genre = as.integer(as.factor(genre)))
VidOnlDat_nochr
str(VidOnlDat_nochr)
View(VidOnlDat_nochr)

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
# High Correlation = Views to Likes or Comments to Likes

# Regression Data ####
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
  fit(views ~ genre,
      data = VidOnlDat_train)

VidOnlDat_lm_fit
VidOnlDat_lm_fit$fit
summary(VidOnlDat_lm_fit$fit)

# Boosted Decision Trees
VidOnlDat_boost_reg_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(views ~ genre, data = VidOnlDat_train)

VidOnlDat_boost_reg_fit
VidOnlDat_boost_reg_fit$fit
summary(VidOnlDat_boost_reg_fit$fit)
VidOnlDat_boost_reg_fit$fit$evaluation_log


# Step 8:
VidOnlDat_reg_results <- VidOnlDat_test

summary(VidOnlDat_reg_results)
View(VidOnlDat_reg_results)

# Regression Results ####
#Linear Regression
VidOnlDat_reg_results$lm_pred <- predict(VidOnlDat_lm_fit, VidOnlDat_test)$.pred

table(VidOnlDat_reg_results$lm_pred)

# Error for LR
yardstick::mae(VidOnlDat_reg_results, views, lm_pred)
yardstick::rmse(VidOnlDat_reg_results, views, lm_pred)


# Boosted Tree Regression
VidOnlDat_reg_results$boost_pred <- predict(VidOnlDat_boost_reg_fit, VidOnlDat_test)$.pred

# Error for BT
yardstick::mae(VidOnlDat_reg_results, views, boost_pred)
yardstick::rmse(VidOnlDat_reg_results, views, boost_pred)





# Hypothesis Testing ####

#Information ####
# Research Question: How does the Category of the Videos affect the View Counts 
# Null Hypothesis: The Genre has no effect the View Counts. (Ave Number of Views does not change in category)
# Alternative Hypothesis: The Genre has an affect to the View Counts. (there is a change in the categories) (the Ave number of views does change in Categories)
# Dependent Var = Views
# Independent = Genre
# Significant if lower than 0.05

music <- filter(new_data, genre == "Music", views > 0)
gaming <- filter(new_data, genre == "Gaming", views > 0)
View(music)
View(gaming)

# T-Test ####
t.test(music$views, gaming$views, paired = F, alternative = "less")
# Not Significant because p-value is 1 > 0.5

# Anova Testing ####
## anova_results <- aov(Num.Var ~ Catg.Var, Data)
VidOnlDat_aov <- aov(views ~ genre, new_data)

summary(VidOnlDat_aov)
# Threshold is 0.05
TukeyHSD(VidOnlDat_aov)
# NULL
MusicHSD <- TukeyHSD(VidOnlDat_aov)$Music
GamingHSD <- TukeyHSD(VidOnlDat_aov)$Gaming


ggplot(new_data, aes(x = views, y = genre)) +
  geom_count() +
  theme_minimal() +
  labs(x = "Views", y = "Genre", title = "Observations between Video's Genre and Views") +
  scale_size_continuous(name = "Observations")







































