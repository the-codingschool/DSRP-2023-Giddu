data <- readRDS("data/USvideos.RDS")
View(data) # trending_date = year/day/month

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)

# Research Checkpoint 1: Cleaning the Data Set ####

# Renaming Columns
renamed_data <- rename(data,
                   channel = channel_title,
                   videos_removed = video_error_or_removed)
View(renamed_data)


# Videos ranked by Views
VidView <- renamed_data |>
  arrange(views) |>
  select(title, channel, likes, dislikes, comment_count)
View(VidView)

# New Variables for Categorical_ID
new_data <- mutate(renamed_data, category_id = case_when(
  category_id == 1 ~ "name",
  category_id == 2 ~ "name",
  category_id == 3 ~ "name",
  category_id == 4 ~ "name",
  category_id == 5 ~ "name",
  category_id == 6 ~ "name",
  category_id == 7 ~ "name",
  category_id == 8 ~ "name",
  category_id == 9 ~ "name",
  category_id == 10 ~ "name",
  category_id == 11 ~ "name",
  category_id == 12 ~ "name",
  category_id == 13 ~ "name",
  category_id == 14 ~ "name",
  category_id == 15 ~ "name",
  category_id == 16 ~ "name",
  category_id == 17 ~ "name",
  category_id == 18 ~ "name",
  category_id == 19 ~ "name",
  category_id == 20 ~ "name",
  category_id == 21 ~ "name",
  category_id == 22 ~ "name",
  category_id == 23 ~ "name",
  category_id == 24 ~ "name",
  category_id == 25 ~ "name",
  category_id == 26 ~ "name",
  category_id == 27 ~ "name",
  category_id == 28 ~ "name",
  category_id == 29 ~ "name",
  category_id == 30 ~ "name",
  category_id == 31 ~ "name",
  category_id == 32 ~ "name",
  category_id == 33 ~ "name",
  category_id == 34 ~ "name",
  category_id == 35 ~ "name",
  category_id == 36 ~ "name",
  category_id == 37 ~ "name",
  category_id == 38 ~ "name",
  category_id == 39 ~ "name",
  category_id == 40 ~ "name",
  category_id == 41 ~ "name",
  category_id == 42 ~ "name",
  category_id == 43 ~ "name"))



dude <- mutate(iris, Petal.Width = case_when(
  Petal.Width == 0.2 ~ "172"))
View(dude)
  
  
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

## Plots ##
ggplot()

# Histogram Plot for Views (1 Numeric Variable)
ggplot(data = renamed_data, aes(x = views)) +
  geom_histogram()

# Barplots between Views and Category ID (1 Numeric Variable & 1 Categorical Variable) 
ggplot(data = renamed_data, aes(x = views, y = likes, fill = category_id)) +
  geom_bar(stat = "summary" ,
           fun = "mean")

























































































































