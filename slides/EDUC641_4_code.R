library(here)
library(tidyverse)
library(ggplot2)


###This tells R where the script is located in relationship to the "root" directory of your project
# Using this command you can then use shortened versions of file pathways that will work across different users' systems
# A non-preferred alternative is to read the data in using the full filepath

i_am("slides/EDUC641_4_code.r")

# Let's first access the data

df <- read.csv(here("data/deathpenalty.csv"))
# it is common in R to name our datasets short names 
# so as to reduce typing (df for dataframe)

# Label our variables
df$rvictim <- factor(df$rvictim,
                     levels = c(1,2), labels=c("Black", "White"))
df$rdefend <- factor(df$rdefend,
                     levels = c(1,2), labels=c("Black", "White"))
df$deathpen <- factor(df$deathpen, 
                      levels = c(0,1), labels = c("No", "Yes"))

###############################################################################
####                    UNIT 2 Relationships Between Categorical Data
###############################################################################

# Two-way tables

table(df$deathpen, df$rvictim)

# Another way to do a two-way table
xtabs(formula = ~ deathpen + rvictim, data = df)

# Two-way table of percents
# margin=2 asks for the column-wise percents
# if you specify margin=1, you'll get the row percents

round(prop.table(table(df$deathpen, df$rvictim), margin=2)*100, 2)

# A grouped chart
ggplot(df, aes(x = rvictim,
               fill = deathpen)) +
  geom_bar(position = "dodge") + 
  xlab("Race of victim") +
  theme_minimal(base_size = 16) +
  scale_fill_discrete(name = "Death sentence?")

# A two-way table with "marginal" values
two_way <- table(df$deathpen, df$rvictim)
addmargins(two_way)

## Constructing the "expected" values in R -- YOU DO NOT NEED TO KNOW HOW TO DO ANY OF THIS!!!
# df <- df %>% group_by(rvictim) %>% mutate(sum_rvictim = n())
# df <- df %>% group_by(deathpen) %>% mutate(sum_deathpen = n())
# 
# df <- ungroup(df) %>% mutate(prop_rvictim = sum_rvictim / n())
# df <- ungroup(df) %>% mutate(prop_deathpen = sum_deathpen / n())
# 
# df <- df %>% group_by(rvictim) %>% mutate(expec_death = sum_rvictim * prop_deathpen)
# 
# df2 <- df %>% group_by(deathpen, rvictim) %>% summarise(expec_death = round(mean(expec_death), 0))
# sum_ref <- df %>% group_by(deathpen) %>% summarise(Sum = n())
# df2 <- left_join(df2, sum_ref, by = c("deathpen"))
# 
# df3 <- df2 %>% spread(rvictim, expec_death) %>% relocate(Sum, .after = last_col())
# 
# sum_vict <- df %>% group_by(rvictim) %>% summarise(sum_rvictim = n())
# sum_vict <- sum_vict %>% spread(rvictim, sum_rvictim)
# tot <- ungroup(df) %>% summarise(Sum = n())
# death <- c("Sum") %>% as.data.frame() %>% setNames(c("deathpen"))
# 
# sum_vict <- cbind(death, sum_vict, tot)
# 
# df3 <- rbind(df3, sum_vict)
# 
# kable(df3, format='html')

## Sub-setting the data to convicted Black defendants

df_b <- filter(df, rdefend == "Black")
table(df_b$deathpen, df_b$rvictim)

## Conduct the chi-square test
chi_b <- chisq.test(df_b$deathpen, df_b$rvictim)
chi_b

## Two-way table of observed frequencies
chi_b$observed

## Two-way table of expected frequencies
round(chi_b$expected, 0) # rounded to an even number

## Display the chi^2 statistic:
chi_b$statistic

## Display the p-value
chi_b$p.value

# or rounded to 10-significant digits...
round(chi_b$p.value, 10)