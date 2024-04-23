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

# greece compared to continents
avg_scores <- df %>%
  group_by(continent) %>%
  summarise(
    avg_math = mean(math, na.rm = TRUE),
    avg_reading = mean(reading, na.rm = TRUE),
    avg_science = mean(science, na.rm = TRUE),
    avg_glcm = mean(glcm, na.rm = TRUE)
  )
greece_df = df[df$country=="Greece",]
avg_scores = cbind(list("Greece",
                       mean(greece_df$math),
                       mean(greece_df$reading),
                       mean(greece_df$science),
                       mean(greece_df$glcm)),
                  avg_scores)

avg_scores_long <- avg_scores %>%
  pivot_longer(cols = c(avg_math, avg_reading, avg_science, avg_glcm),
               names_to = "subject",
               values_to = "average_score")

ggplot(avg_scores_long, aes(x = continent, y = average_score, fill = subject)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  labs(title = "Average Scores by Continent",
       x = "Continent",
       y = "Average Score",
       fill = "Subject") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
