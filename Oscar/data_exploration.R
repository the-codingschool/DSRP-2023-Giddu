usvide <- readRDS("data/USvideos.RDS")
head(usvid)
str(usvid)

## Packages
install.packages("tidyvesre")
library("dplyr")
library(readr)
library(ggplot2)
install.packages("installr")
library(installr)
updateR()

library(jsonlite)
data <- fromJSON("data/US_category_id.json")

View(data)

#Select
usvid <- usvide %>% 
  select("trending_date", "publish_time", "channel_title", "category_id", "views", "likes", "dislikes",
                   "comment_count")

