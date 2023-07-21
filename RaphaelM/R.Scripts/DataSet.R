data <- readRDS("data/USvideos.RDS")
View(data) # trending_date = year/day/month

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(rjson)


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

















































































































