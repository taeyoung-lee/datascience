# clear environment

rm(list=ls(all=TRUE))

# getting working directory (default)

getwd()

# set the working directory

setwd("/Users/mymacbook/Desktop/OneDrive - The University of Texas at Austin/UT/5.2020 SUMMER")

# Basic Math

7 + 4 #addition
7 - 4 #subtraction
7 * 4 #multiplication
7 / 4 #division

## Vectors, Sequences, and Data Classes/Types

# creating a country character vector
country <- c("france","france","france","france","france","france")

# create a vector for year
year <-c(2000, 2005, 2010, 2015, 2020, 2025)

# create a year variable via sequence
year2 = seq(2000, 2025, 5)

rm(year2) #remove year2

# inspect the year vector
print(year)

# poverty rate 
poverty_rate <- c(13.6, 13.1, 14, 14.2, NA, NA)
rm(poverty_rate)

# GDP per capita
# Consumption (C) + Income (I) + Government Expenditure (G) + (Exports - Imports)
# C + I + G + (X - M) / population
gdp_per_capita <- c(22364, 34760, 40368, 36613, NA, NA)

# classfy the GDP by low or high
low_high <- c("low", "low", "high", "high", "NA", "NA")

# Create a factor variable
# so R knows low is less than high
gdp_levels <- factor(low_high, levels=c("low", "high"))

# remove the low_high string variable
rm(low_high)

#check the class/type of our vectors
class(country)
class(poverty_rate)
class(gdp_per_capita)
class(gdp_levels)
class(year)

## Dataframes, Variables, Basic Statistics, and Missing Data

# convert vectors to dataframe
df <- data.frame(country, year, poverty_rate, gdp_per_capita, gdp_levels)

# remove vectors,so we just have a data frame
rm(country, gdp_levels, gdp_per_capita, poverty_rate, year)

# Inspect the dataframe
print(df)
View(df)
head(df)
summary(df)
dim(df)

# inspect the variables individually
summary(df$gdp_per_capita)
summary(df$poverty_rate)

# count the observations
length(df$poverty_rate)

# count the number of unique obersavtions
length(unique(df$poverty_rate))

# check the class of the country variable
class(df$country)

# change country back to a character
df$country_char <-as.character(df$country)
class(df$country_char)
class(df$country)

# get descriptive statistics of our variables
mean(df$gdp_per_capita, na.rm = TRUE)

# Assign the mean gdp per capita as another variable in the df
mean_gdp <- mean(df$gdp_per_capita, na.rm = TRUE)
print(mean_gdp)

# Correlation between poverty rate and GDP per capita
cor(df$gdp_per_capita, df$poverty_rate, use="complete.obs")



## instralling Packages, Making Tables with Stargazer, and Getting help

# Load the stargazer
library(stargazer)


# Output our dataset with some descriptive stats with Stargazer
stargazer(df, type="text")
stargazer(df, type="latex")
stargazer(df, type="html")


# help function for Stargazer
?stargazer


## Importing Data and Intro to the V-dem Dataset
library(rio)
vdem <- import("V-Dem-CY-Core-v10.dta")
View(vdem)

# Internet source doesn't work, so I used the downloaded data
# V_Dem_CY_Core_v10 <- read_dta("V-Dem-CY-Core-v10.dta")
# View(V_Dem_CY_Core_v10)

# vdem <- V_Dem_CY_Core_v10
# rm(V_Dem_CY_Core_v10)


# Which countries are include in V-Dem?
table(vdem$country_name)
summary(vdem$country_name)
# Which years are in V-Dem?
summary(vdem$year)
table(vdem$year)


## Subsetting and Removing Data Frames

# subset vdem
vdem2 <- subset(vdem, select = c("country_name", "year", 
                                 "COWcode", "v2x_polyarchy",
                                 "v2x_corr"))
                                 
# na.omit (remove na)
vdem3 <- na.omit(vdem2, select = c("v2x_polyarchy", "v2x_corr"))

vdem4 = na.omit(vdem)

# correlation between democracy and corruption
cor(vdem3$v2x_polyarchy,vdem3$v2x_corr)


## Creating New Variables and Cross-Tabs

# get the mean democracy score
mean(vdem3$v2x_polyarchy)

# create a new variable democracy dummy variable (0 or 1)
vdem3$democracy = NA
vdem3$democracy [vdem3$v2x_polyarchy >= .27] = 1
vdem3$democracy [vdem3$v2x_polyarchy < .27] = 0

# create a new variable autocracy dummy variable (0 or 1)
vdem3$autocracy = NA
vdem3$autocracy [vdem3$v2x_polyarchy < .27] = 1
vdem3$autocracy [vdem3$v2x_polyarchy >= .27] = 0

# creaate a politica regime variable
vdem3$political_regime = "NA"
vdem3$political_regime[vdem3$democracy ==1] = "democracy"
vdem3$political_regime[vdem3$democracy ==0] = "autocracy"

#create a political regime variable in another way
vdem3$political_regime = "NA"
vdem3$political_regime[vdem3$autocracy ==1] = "autocracy"
vdem3$political_regime[vdem3$autocracy ==0] = "democracy"

vdem3$autoocracy <- NULL

# table the political regime
table(vdem3$political_regime, exclude = TRUE)

# Run the cross tab
library(doBy)

summaryBy(v2x_corr ~ political_regime, data = vdem3, FUN = c(mean, length)) #why it doesn't work?


## Saving/Export

library(rio)
?rio
export(vdem3, "new_vdem.csv") # csv file
export(vdem3, "new_vdem.dta") # Stata dataset
export(vdem3, "new_vdem.xlsx") # Excel\