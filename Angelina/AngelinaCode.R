library(readr)
USvideos_USvideos <- read_csv("Angelina/videos/USvideos - USvideos.csv")
View(USvideos_USvideos)

install.packages("rjson")
library("rjson")
my_category_data <- fromJSON(file="data/US_category_id.json")
View(my_category_data)

install.packages(“lubridate”)
library(lubridate)



library(ggplot2)
str(USvideos_USvideos)

ggplot(data = USvideos_USvideos, aes(x=views, y=hour(publish_time))) +
  geom_point ()+
  labs(x= "Views",
       y= "Release Time",
       title= "Trending Youtube Videos Release Time VS. its correlating views(popularity) "
  )
