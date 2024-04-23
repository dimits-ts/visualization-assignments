library(countrycode)
library(ggplot2)
library(dplyr)
library(tidyr)

load("pisa2018.Rdata")

# select and rename columns
df = subset(newdata, select=c("CNT", "MATH", "READ", "SCIE", "GLCM", "ST004D01T"))
names(df) = c("country", "math", "reading", "science", "glcm", "gender")

# add continent information
df$continent = countrycode(sourcevar = df[,"country"],
                           origin = "country.name",
                           destination="continent")
df[df$country == "Kosovo", ]$continent = "Europe"
df[df$country == "Moscow Region (RUS)", ]$continent = "Europe"
df[df$country == "Tatarstan (RUS)", ]$continent = "Asia"
df$continent = as.factor(df$continent)
levels(df$continent)
any(is.na(df$continent))

# Note: GLCM data do not exist for Oceania


