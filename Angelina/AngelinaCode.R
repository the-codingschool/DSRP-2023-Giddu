library(readr)
USvideos_USvideos <- read_csv("Angelina/videos/USvideos - USvideos.csv")
View(USvideos_USvideos)

install.packages("rjson")
library("rjson")
my_category_data <- fromJSON(file="data/US_category_id.json")
View(my_category_data)
