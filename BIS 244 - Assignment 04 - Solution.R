# Solution file for BIS 244 Assignment 04 Spring 2022
# Grading Rubric:
# In general, if they get separate box plots for each nation and type, give them credit. However, if:
#     They have a single flat line for all their boxplots, subtract 20 points: they failed to convert to numeric as we went over in class.
# If they failed to order each graph from lowest to highest averages (as shown in the assignment, there will be some where the box plot averages are out of order because the ordering uses ALL the data of each nation/type, but the boxplots leave out outliers), subtract 10 points
# Other than that, use your judgement on subtracting points. Total points are 100 for the assignment.

# First, clear memory and the Console 

rm(list=ls(all=TRUE))
cat("\014")

library(here)
library(tidyverse)
library(patchwork)

# Import data into dataframe
WOT <- read_csv("WOT.csv")

WOT$WR <- as.numeric(sub("%", "",WOT$WR,fixed=TRUE))
# Filter down to PA and Lehigh only
WOT <- WOT %>% filter(Battles>=4)

# Original way I arranged data, grouping by 2 levels

WOT_by_Nation <- WOT %>% group_by(Nation, Class) %>%
  summarize(AVG = mean(WR)) 


p <- ggplot(WOT_by_Nation, aes(x = reorder(Nation, AVG, na.rm=TRUE), y = AVG))
a1 <- p + geom_boxplot()  +
  labs(x = "Nation Played",y = "Win Rate (WR)",
       title = "Stats by Nation Played")

WOT_by_Class <- WOT %>% group_by(Class, Nation) %>%
  summarize(AVG = mean(WR)) 


p <- ggplot(WOT_by_Class, aes(x = reorder(Class, AVG, na.rm=TRUE), y = AVG))
b1 <- p + geom_boxplot()  +
  labs(x = "Type Played",y = "Win Rate (WR)",
       title = "Stats by Type Played")

a1+b1

# Equally acceptable way, grouping only by single levels

p <- ggplot(WOT, aes(x = reorder(Nation, WR, na.rm=TRUE), y = WR))
a2 <- p + geom_boxplot()  +
  labs(x = "Nation Played",y = "Win Rate (WR)",
       title = "Stats by Nation Played")

p <- ggplot(WOT, aes(x = reorder(Class, WR, na.rm=TRUE), y = WR))
b2 <- p + geom_boxplot()  +
  labs(x = "Type Played",y = "Win Rate (WR)",
       title = "Stats by Type Played")

a2+b2

# Comparison of two sets of graphs
(a1+b1)/(a2+b2)


