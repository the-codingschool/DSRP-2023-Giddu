data <- readRDS("data/USvideos.RDS")
View(data) # trending_date = year/day/month

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)

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










































































































































