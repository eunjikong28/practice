install.packages("here")
install.packages("tidyverse")
install.packages("ggplot2")



library(here)
library(tidyverse)
library(ggplot2)


### The first command tells R where the script is located in relationship to the "root" directory of your project
# Using this command you can then use shortened versions of file pathways that will work across different users' systems
# A non-preferred alternative is to read the data in using the full filepath

i_am("slides/EDUC641_3_code.r")

# Let's first access the data

df <- read.csv(here("data/deathpenalty.csv"))
# it is common in R to name our datasets short names 
# so as to reduce typing (df for dataframe)

###############################################################################
####                    UNIT 1 Describing Categorical Data
###############################################################################

# Understanding data structure

df                 # typing the dataframe name spits out the full data file
names(df)          # names() lists the column names
head(df)           # head() prints to the console the first six records (you can alter this to show more or fewer)
str(df)            # str() provides basic details on the columns, their data type, etc.

# We know from our codebook what this long list of numbers means, so let's make our dataset a little more readable...

df$rvictim <- factor(df$rvictim,
                     levels = c(1,2), labels=c("Black", "White"))
df$rdefend <- factor(df$rdefend,
                     levels = c(1,2), labels=c("Black", "White"))
df$deathpen <- factor(df$deathpen, 
                      levels = c(0,1), labels = c("No", "Yes"))

# Look at the first 6 observations
head(df)

# The default is to see 6, but you can change that!!
head(df, 20)

#How many were sentenced to the death penalty?
table(df$deathpen)


# Summarizing data: Charts

counts <- table(df$deathpen)
barplot(counts, xlab = "Sentenced to Death?")


# Calculating proportions and visualizing
prop <- prop.table(table(df$rvictim))
barplot(prop, xlab = "Race of Victim")



# Another way to visualize:  Let's start to get familiar with the beauty of `ggplot`

# This is the basic version of the graph in the lecture slides
rd <- ggplot(df, aes(rdefend)) + geom_bar() +
         xlab("Race of Defendant")
rd

# Here we add bar value labels to the bars
rd + geom_text(aes(label = ..count..), stat='count', vjust = -0.5)

# And now can make it a little prettier
rd + geom_text(aes(label = ..count..), stat='count', vjust = -0.5, size = 6) +
  ylim(0,1550) +
  theme_minimal(base_size = 16)

# Now I can save the image I've created
ggsave(filename = "slides/rdefend.png")


