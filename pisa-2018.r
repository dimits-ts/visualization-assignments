library(countrycode)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(sf)

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

# ======== Greece compared to continents ======== 

avg_scores <- df %>%
  group_by(continent) %>%
  summarise(
    avg_math = mean(math, na.rm = TRUE),
    avg_reading = mean(reading, na.rm = TRUE),
    avg_science = mean(science, na.rm = TRUE),
    avg_glcm = mean(glcm, na.rm = TRUE)
  )
greece_df = df[df$country=="Greece",]
avg_scores = rbind(data.frame(continent="Greece",
                              avg_math=mean(greece_df$math),
                              avg_reading=mean(greece_df$reading),
                              avg_science=mean(greece_df$science),
                              avg_glcm=mean(greece_df$glcm)),
                  avg_scores)
names(avg_scores) <- c("Continent", "Mathematics", "Reading", "Science", "GLCM")

avg_scores_long <- avg_scores %>%
  pivot_longer(cols = c(Mathematics, Reading, Science, GLCM),
               names_to = "subject",
               values_to = "average_score")

ggplot(avg_scores_long, aes(x = Continent, y = average_score, fill = subject)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  labs(title = "Greece Scores compared to Continents",
       x = "Greece/Continent",
       y = "Average Score",
       fill = "Subject") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #+
  #scale_x_discrete(labels = function(x) {
    # Make the first label bold
    #ifelse(x == unique(avg_scores_long$continent)[1], paste0("<b>", x, "</b>"), x)
  #}


# ======== Map on average score ======== 

overall_df = data.frame(df)
overall_df$score = rowMeans(overall_df[, c("math", "reading", "science", "glcm")],
                            na.rm = TRUE)
avg_country_scores <- overall_df %>%
  group_by(country) %>%
  summarise(avg_score = mean(score))

# Load world map data from maps package
world_map <- map_data("world")

world_map$iso_code <- countrycode(sourcevar = world_map$region,
                                  origin = "country.name",
                                  destination = "iso3c")
avg_country_scores$iso_code <- countrycode(sourcevar = avg_country_scores$country,
                         origin = "country.name",
                         destination = "iso3c")

# Merge map data with average scores data
# Merge while retaining polygon order to prevent map from having aneurism
# TODO: Overlay map for unknown countries?
Total <- world_map[world_map$iso_code %in% avg_country_scores$iso_code, ]
Total$avg_score <- avg_country_scores$avg_score[match(Total$iso_code, avg_country_scores$iso_code)]

ggplot(Total, aes(x=long, y=lat, group = group, fill = avg_score)) + 
  geom_polygon(colour = "white") +
  scale_fill_continuous(low = "thistle2", 
                        high = "darkred", 
                        guide="colorbar") +
  scale_fill_gradient(low = "black", high = "lightgreen", na.value = "grey") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  labs(fill="Average score (4 tests)") +
  theme_minimal() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "right") +
  theme_void()



# ======== Map on F/M divergence ======== 

gender_country_df <- overall_df %>%
  group_by(country, gender) %>%
  summarise(
    avg_math = mean(math, na.rm = TRUE),
    avg_reading = mean(reading, na.rm = TRUE),
    avg_science = mean(science, na.rm = TRUE),
    avg_glcm = mean(glcm, na.rm = TRUE)
  )

# Calculate the total scores for each country and gender
gender_country_perc_df <- gender_country_df %>%
  group_by(country, gender) %>%
  summarize(total_math = sum(avg_math, na.rm = TRUE),
            total_reading = sum(avg_reading, na.rm = TRUE),
            total_science = sum(avg_science, na.rm = TRUE),
            total_glcm = sum(avg_glcm, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(female_total = sum(ifelse(gender == "Female", total_math + total_reading + total_science + total_glcm, 0), na.rm = TRUE),
         male_total = sum(ifelse(gender == "Male", total_math + total_reading + total_science + total_glcm, 0), na.rm = TRUE)) %>%
  summarise(female_to_male_perc = (female_total / (female_total + male_total) - 0.5) * 100)
# remove duplicates
gender_country_perc_df <- gender_country_perc_df[seq(1, nrow(gender_country_perc_df), 2), ]

gender_country_perc_df$iso_code <- countrycode(sourcevar = gender_country_perc_df$country,
                                           origin = "country.name",
                                           destination = "iso3c")

# Merge map data with average scores data
# Merge while retaining polygon order to prevent map from having aneurism
# TODO: Overlay map for unknown countries?
Total <- world_map[world_map$iso_code %in% avg_country_scores$iso_code, ]
Total$avg_score <- gender_country_perc_df$female_to_male_perc[
                      match(Total$iso_code, gender_country_perc_df$iso_code)]

ggplot(Total, aes(x=long, y=lat, group = group, fill = avg_score)) + 
  geom_polygon(colour = "white") +
  scale_fill_continuous(low = "thistle2", 
                        high = "darkred", 
                        guide="colorbar") +
    scale_fill_gradient2(high = "yellow",
                         low = "#964B00",
                         mid="lightgrey",
                         na.value = "grey") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  labs(title="Difference between female (yellow) and male (brown) test scores",
       fill="Female/Male Difference (%)") +
  theme_minimal() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "right") +
  theme_void()


# ======== Continents F/M divergence ======== 

gender_continent_df <- overall_df %>%
  group_by(continent, gender) %>%
  summarise(
    avg_math = mean(math, na.rm=T),
    avg_reading = mean(reading, na.rm=T),
    avg_science = mean(science, na.rm=T),
    avg_glcm = mean(glcm, na.rm=T)
  )
gender_continent_df <- gender_continent_df[!is.na(gender_continent_df$gender),] 

# Calculate the total scores for each country and gender
gender_continent_perc_df <- gender_continent_df %>%
  group_by(continent, gender) %>%
  summarize(total_math = sum(avg_math, na.rm = TRUE),
            total_reading = sum(avg_reading, na.rm = TRUE),
            total_science = sum(avg_science, na.rm = TRUE),
            total_glcm = sum(avg_glcm, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(continent) %>%
  mutate(fem_math = sum(ifelse(gender == "Female", total_math, 0), na.rm = TRUE),
         male_math = sum(ifelse(gender == "Male", total_math, 0), na.rm = TRUE),
         fem_science = sum(ifelse(gender == "Female", total_science, 0), na.rm = TRUE),
         male_science = sum(ifelse(gender == "Male", total_science, 0), na.rm = TRUE),
         fem_reading = sum(ifelse(gender == "Female", total_reading, 0), na.rm = TRUE),
         male_reading = sum(ifelse(gender == "Male", total_reading, 0), na.rm = TRUE),
         fem_glcm = sum(ifelse(gender == "Female", total_glcm, 0), na.rm = TRUE),
         male_glcm = sum(ifelse(gender == "Male", total_glcm, 0), na.rm = TRUE)) %>%
  summarise(math_perc = (fem_math / (fem_math + male_math)-0.5) * 100,
            science_perc = (fem_science / (fem_science + male_science)-0.5) * 100,
            reading_perc = (fem_reading / (fem_reading + male_reading)-0.5) * 100,
            glcm_perc = (fem_glcm / (fem_glcm + male_glcm)-0.5) * 100)
# remove duplicates
gender_continent_df <- gender_continent_df[seq(1, nrow(gender_country_perc_df), 2), ]
gender_continent_perc_df

names(gender_continent_perc_df) <- c("Continent", "Mathematics", "Reading", "Science", "GLCM")

gender_continent_perc_df_long <- gender_continent_perc_df %>%
  pivot_longer(cols = c(Mathematics, Reading, Science, GLCM),
               names_to = "subject",
               values_to = "average_score")

custom_yticks <- seq(-1, 2, by = 0.5) 
custom_ylabels <- paste(custom_yticks, "%", sep = "")  # Convert tick positions to labels with percentage symbol
ggplot(gender_continent_perc_df_long, aes(x = Continent, y = average_score, fill = subject)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  labs(title = "Difference between female (top) and male (bottom) test scores",
       x = "Continent",
       y = "Score Difference Ratio (%)",
       fill = "Subject") +
  scale_y_continuous(breaks = custom_yticks, labels = custom_ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

