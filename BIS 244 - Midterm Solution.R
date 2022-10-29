# First, clear memory and the Console 

rm(list=ls(all=TRUE))
cat("\014")

library(here)
library(tidyverse)

# Import data into dataframe
Netflix <- read_csv("netflix_titles.csv")

# Size of data frame
length(Netflix$show_id)

# Filter to movies only

Netflix <- filter(Netflix,type=="Movie")
n <- length(Netflix$show_id)
n

for (i in 1:n) {
  if (is.na(Netflix$rating[i]) | Netflix$rating[i]=="84 min"|
      Netflix$rating[i]=="74 min" | Netflix$rating[i]=="66 min" |
      Netflix$rating[i]=="NR" | Netflix$rating[i]=="Unrated" |
      Netflix$rating[i]=="UR") {
    Netflix$rating[i] <- "Unrated"
  }
}

table(Netflix$rating)


Netflix$For_Kids <- NA

for (i in 1:n) {
  if (Netflix$rating[i]=="PG"| Netflix$rating[i]=="PG-13" |
      Netflix$rating[i]=="G"| Netflix$rating[i]=="TV-G" |
      Netflix$rating[i]=="TV-PG" | Netflix$rating[i]=="TV-Y" |
      Netflix$rating[i]=="TV-Y7" | Netflix$rating[i]=="TV-Y7-FV")
    {
    Netflix$For_Kids[i] <- "Appropriate for 13 and Under"
  } else{Netflix$For_Kids[i] <- "NOT Appropriate for 13 and Under"}
}  

table(Netflix$For_Kids)

p <- ggplot(data=Netflix, mapping = aes(x = Netflix$For_Kids,fill = Netflix$rating))
p + geom_bar() +
  labs(x = "Appropriateness", 
       y = "Movies",
       title = "Breakdown of Netflix Movies Appropriateness for 13-Year-Old")
