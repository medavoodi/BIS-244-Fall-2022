library(tidyverse)
AWI <- read_csv("AWI Changes.csv")
str(AWI)

# Substituting blanks for %
AWI$"Growth_in_AWI" <- as.numeric(sub("%","",AWI$"Growth_in_AWI"))
str(AWI)

# Making graphs and using pathwork

p <- ggplot(data=AWI, mapping = aes(x = Work_Year, y = Growth_in_AWI))
p + geom_col()
a <- p + geom_col()
p + geom_line(color="red")
b <- p + geom_line(color="red")
a + b

library(patchwork)
a+b
