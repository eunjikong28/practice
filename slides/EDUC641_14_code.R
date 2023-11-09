library(here)
library(tidyverse)


###This tells R where the script is located in relationship to the "root" directory of your project
# Using this command you can then use shortened versions of file pathways that will work across different users' systems
# A non-preferred alternative is to read the data in using the full filepath

i_am("slides/EDUC641_14_code.R")

# Let's first access the data
who <- read.csv(here("data/life_expectancy.csv")) 

# We start with some simple data-cleaning steps that you will learn via practice on applied research projects
# It is not necessary for you to know how to do this now

who <- who %>%  janitor::clean_names() %>% 
        filter(year == 2015) %>%
        select(country, schooling, year, life_expectancy) %>% 
        mutate(life_expectancy = round(life_expectancy, digits = 0))

#####################################
### Identifying missingness

sum(is.na(who$life_expectancy))
sum(is.na(who$schooling))

### For the really ambitious, can do this for all columns in the data
sapply(who, function(x) sum(is.na(x)))

# We apply listwise deletion and filter out any observations with missing values of schooling
who <- filter(who, !is.na(schooling))
# We count the number of rows in our data
nrow(who)


#######################################################
### Univariate statistics

# Create a stem-and-leaf plot of LIFE_EXPECTANCY
stem(who$life_expectancy)

# Create a histogram of LIFE_EXPECTANCY by single year
ggplot(who, aes(x = life_expectancy)) +
  geom_histogram(binwidth = 1) 

# Do the same for SCHOOLING
stem(who$schooling)
ggplot(who, aes(x = schooling)) +
  geom_histogram(binwidth = 1) 

# Generate univariate statistics
summary(who$life_expectancy)
summary(who$schooling)

#########################################
### Visualizing bivariate relationships


# Visualize bivariate relationship (with observation label)
ggplot(data = who, aes(x = schooling, y = life_expectancy)) + 
  geom_label(aes(label=country)) + 
  # the commands below are for formatting purposes only
  xlim(0, 22) +
  ylab("Life Expectancy (Yrs)") + xlab("Schooling (Yrs)") +
  scale_y_continuous(breaks = seq(40, 90, 10), limits = c(40, 90))

# Visualize bivariate relationship in scatterplot form
biv <- ggplot(data = who, aes(x = schooling, y = life_expectancy)) + 
  geom_point() + 
  xlim(0, 22) +
  ylab("Life Expectancy (Yrs)") + xlab("Schooling (Yrs)") +
  scale_y_continuous(breaks = seq(40, 90, 10), limits = c(40, 90))

biv

# Scatterplot PLUS fitted regression line estimate

biv + geom_smooth(method = lm, se = F)
  # SE = F removes the confidence intervals. We'll learn about those in EDUC 643


####################################################################
## Bivariate regression

# Fitting a relationship using Ordinary Least Squares (OLS)
fit <- lm(life_expectancy ~ schooling, data=who)
summary(fit)

####################################################################
##  Constructing tables


## First, install the modelsummary package
install.packages("modelsummary")
library(modelsummary)

# Generate an un-modified summary statistics table
datasummary_skim(who)

# Remove the histogram
datasummary_skim(who, histogram = F)

# Export the table to Word
datasummary_skim(who, histogram = F,
                 output="slides/table.docx") # <- note that you should direct R to save the table to a defined folder

# Generate an un-modified regression output table
modelsummary(fit)

# Remove unnecessary elements
modelsummary(fit,
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|F",  # <- removes goodness-of-fit (gof) statistics containing these strings; the | indicates OR
             coef_rename = c("schooling" = "Yrs. Schooling")) # use interpretable coefficient names

# Export this table to word
modelsummary(fit,
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|F",  
             coef_rename = c("schooling" = "Yrs. Schooling"),
             output = "slides/table2.docx")


####################################################################
##  Residual analysis

# 'predict' asks for the predicted values
who$predict <- predict(fit)

# 'resid' asks for the raw residual
who$resid <- residuals(fit)

# Can examine the standard deviation of the residuals
sd(who$resid)

# Boxplot of residuals
boxplot(resid(fit))

# Histogram of the residuals
ggplot(who, aes(x = resid)) + 
  geom_histogram(binwidth = 1)

# Residuals vs. fitted values plot -- a key diagnostic tool
ggplot(who, aes(x = predict, y = resid)) + 
  geom_point() +
  # The below commands are for stylistic purposes
  geom_hline(yintercept = 0, color = "red", linetype="dashed") +
  ylab("Residuals") + xlab("Fitted values") +
  scale_y_continuous(limits=c(-20, 20))

