library(countrycode)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(sf)
library(grid)
library(gridExtra)
library(forcats)


# ===== HELPER FUNCTIONS FOR SAVING PLOTS AND TABLES =====

RESOURCE_PATH = "output"

# Utility function to get a relative file path (including file extension)
# from a file name.
filepath_png <- function(name) {
  return(file.path(RESOURCE_PATH, paste(name, ".png", sep = "")))
}


GREECE_COLOR = "red"
OTHER_COUNTRIES_COLOR = "steelblue"
FEMALE_COLOR = "darkgoldenrod1"
MALE_COLOR = "#964B00"
CONTINENT_COLORS =  c("Asia" = "orange", "Europe" = "blue", "Africa" = "red", "Americas" = "purple", "Oceania"= "green", "red"= "red")

SUBJECT_NAME_LIST <- c("Mathematics", "Reading", "Science", "GLCM")


# Import pisa data
load("data/pisa2018.Rdata")

# select and rename columns
df = subset(newdata, select=c("CNT", "MATH", "READ", "SCIE", "GLCM", "ST004D01T"))
names(df) = c("country", "math", "reading", "science", "glcm", "gender")

# add russian regions to russia
df[df$country == "Moscow Region (RUS)", ]$country = "Russian Federation"
df[df$country == "Tatarstan (RUS)", ]$country = "Russian Federation"


# add continent information
df$continent = countrycode(sourcevar = df[,"country"],
                           origin = "country.name",
                           destination="continent")
df[df$country == "Kosovo", ]$continent = "Europe"
df$continent = as.factor(df$continent)
levels(df$continent)
any(is.na(df$continent))


# Import extra data
# Data sources:
# https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2018&start=2018
# https://data.worldbank.org/indicator/SP.POP.TOTL?end=

pop_df <- read.csv("data/pops.csv", header=TRUE, skip=4)[,c("Country.Code", "X2018")]
names(pop_df) <- c("iso_code", "population")

gdp_percap_df <- read.csv("data/gdp_percap.csv", header=TRUE, skip=4)[,c("Country.Code", "X2018")]
names(gdp_percap_df) <- c("iso_code", "gdp_percap")

extra_data_df <- merge(pop_df, gdp_percap_df, by="iso_code")
extra_data_df$continent = countrycode(sourcevar = extra_data_df[,"iso_code"],
                        origin = "iso3c",
                        destination="continent")


# ======== Greece compared to all others ======== 

greece_global_df <- drop_na(df) %>%
                    pivot_longer(cols = c(math, reading, science, glcm),
                                 names_to = "subject", 
                                 values_to = "score")

plot_list <- list()
i=1

for (subject in unique(greece_global_df$subject)) {
  temp_subject_df <- greece_global_df[greece_global_df$subject == subject,]
  p <- ggplot(df, aes(x = math, fill = country != "Greece")) +
    geom_density(alpha = 0.7) +
    labs(x=NULL, y=NULL, title=SUBJECT_NAME_LIST[i]) +
    scale_fill_manual(values=c(GREECE_COLOR, OTHER_COUNTRIES_COLOR), labels = c("Greece", "Other Countries")) +
    theme_minimal() +
    theme(legend.title = element_blank(), 
          axis.title.y = element_text(vjust = 1)) 
  
  plot_list[[subject]] <- p
  i <- i + 1
}

grid_plot <- grid.arrange(grobs = plot_list, 
             ncol = 2, 
             nrow = 2, 
             top=textGrob("Greek vs global scores", gp=gpar(fontsize=20,font=3)),
             bottom=textGrob("Average Score"),
             left="Density")
ggsave(filename=filepath_png("greek_global"), plot=grid_plot)


# ======== Greece compared to continents ======== 

# Note: GLCM data do not exist for Oceania

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
ggsave(filename=filepath_png("greece_continents"))

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

ggsave(filename=filepath_png("avg_scores_world"))


# ======== Horizontal barplots for EU grades ========

eu_avg_df <- df[df$continent=="Europe",]  %>%
  group_by(country) %>%
  summarise(
    avg_math = mean(math, na.rm = TRUE),
    avg_reading = mean(reading, na.rm = TRUE),
    avg_science = mean(science, na.rm = TRUE),
    avg_glcm = mean(glcm, na.rm = TRUE)
  )

# Reshape the data from wide to long format
eu_avg_df_long <- eu_avg_df %>%
  pivot_longer(cols = starts_with("avg"), names_to = "subject", values_to = "average_value")

# Create a list to store individual plots
plot_list <- list()
i=1

# Loop through each subject and create a separate barplot
for (subject in unique(eu_avg_df_long$subject)) {
  # Subset data for the current subject
  plot_data <- eu_avg_df_long[eu_avg_df_long$subject==subject,]
  plot_data <- drop_na(plot_data)
  plot_data <- plot_data[order(plot_data$average_value),]
  plot_data$country <- fct_reorder(plot_data$country, plot_data$average_value)
  
  
  country_colors <- ifelse(plot_data$country == "Greece", "red", "steelblue")
  p <- ggplot(plot_data, aes(x = average_value, y = country, fill=country)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = paste(SUBJECT_NAME_LIST[i])) +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  guides(fill = FALSE) +
  xlab(NULL) + 
  ylab(NULL)
  
  plot_list[[subject]] <- p
  i <- i + 1
}

grid_plot <- grid.arrange(grobs = plot_list, 
             ncol = 2, 
             nrow = 2, 
             top=textGrob("European Average Scores", gp=gpar(fontsize=20,font=3)),
             bottom=textGrob("Average Score"))

ggsave(filename=filepath_png("avg_eu"), plot=grid_plot)

# ======== Pop, GDP, avg_score plot ======== 

enhanced_avg_scores_df <- merge(avg_country_scores, extra_data_df, by="iso_code")

ggplot(enhanced_avg_scores_df, aes(x=gdp_percap, y=avg_score, size=population, color=continent)) +
  geom_point(data = enhanced_avg_scores_df, alpha = 0.6) +
  geom_text(data = subset(enhanced_avg_scores_df, country == "Greece"),
            aes(label = "Greece"), 
            nudge_y=3,
            size = 4, 
            fontface = "bold",
            show.legend = F) +
  scale_color_manual(values=CONTINENT_COLORS) +
  scale_size(range = c(2, 20), guide=F) +
  labs(x = "GDP per Capita (U.S. $ 2024)",
       y = "Average Score (all tests)", 
       title="Average Scores by GDP per capita and population",
       color="Continent") +
  theme_minimal() +
  theme(legend.title = element_text(size = 12),  
        legend.text = element_text(size = 10),   
        legend.key.size = unit(1.5, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 10)))  

ggsave(filename=filepath_png("avg_bubble"))

# ======== F/M Distribution ======== 

gender_grades_df <- drop_na(df) %>%
                    pivot_longer(cols = c(math, reading, science, glcm),
                                 names_to = "subject", 
                                 values_to = "score")

plot_list <- list()
i=1

for (subject in unique(gender_grades_df$subject)) {
  temp_subject_df <- gender_grades_df[gender_grades_df$subject == subject,]
  p <- ggplot(temp_subject_df, aes(x = score, color = gender, fill = gender)) +
      geom_density(alpha = 0.5) +  # Add transparency
      labs(x=NULL, y=NULL, title = SUBJECT_NAME_LIST[i]) +
      scale_color_manual(values = c(FEMALE_COLOR, MALE_COLOR)) +  
      scale_fill_manual(values = c(FEMALE_COLOR, MALE_COLOR)) +
      theme_minimal() +
      theme(legend.title = element_blank(), 
            axis.title.y = element_text(vjust = 1)) 
  
  plot_list[[subject]] <- p
  i <- i + 1
}

grid_plot <- grid.arrange(grobs = plot_list, 
             ncol = 2, 
             nrow = 2, 
             top=textGrob("Global scores by gender", gp=gpar(fontsize=20,font=3)),
             bottom=textGrob("Average Score"),
             left="Density")

ggsave(filename=filepath_png("gender_distplot"), plot=grid_plot)


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
    scale_fill_gradient2(high = FEMALE_COLOR,
                         low = MALE_COLOR,
                         mid="lightgrey",
                         na.value = "grey") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  labs(title="Difference between female (yellow) and male (brown) test scores",
       fill="Female/Male score diff (%)") +
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

ggsave(filename=filepath_png("gender_world_map"))



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
gender_continent_perc_df <- gender_continent_perc_df[seq(1, nrow(gender_country_perc_df), 2), ]
gender_continent_perc_df <- gender_continent_perc_df[1:5,]

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

ggsave(filename=filepath_png("gender_continents"))



# ======== Pop, GDP, M/F diff plot ======== 
enhanced_gender_country_perc_df <- merge(gender_country_perc_df, extra_data_df, by="iso_code")

ggplot(enhanced_gender_country_perc_df, aes(x=gdp_percap, y=female_to_male_perc, size=population, color=continent)) +
  geom_point(data = enhanced_gender_country_perc_df, alpha = 0.6) +
  geom_text(data = subset(enhanced_gender_country_perc_df, country == "Greece"),
            aes(label = "Greece"),
            nudge_y=0.1,
            size = 4, 
            fontface = "bold",
            show.legend = F) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  annotate("text", y = 0.05, x = max(enhanced_gender_country_perc_df$gdp_percap)*4/5,
           label = "Gender Equality", color = "red", size = 4, fontface = "bold")  +
  scale_color_manual(values=CONTINENT_COLORS) +
  scale_size(range = c(2, 20), guide=F) +
  labs(x = "GDP per Capita (U.S. $ 2024)",
       y = "Female/Male score diff (%)", 
       title="Gender gap in average scores\n(Higher: Female-dominated scores)",
       color="Continent") +
  theme_minimal() +
  theme(legend.title = element_text(size = 12),  
        legend.text = element_text(size = 10),   
        legend.key.size = unit(1.5, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 10)))  

ggsave(filename=filepath_png("gender_bubble"))



# ======== GDP, M/F diff plot for all subjects ======== 

gender_country_df$iso_code <-  countrycode(sourcevar = gender_country_df$country,
                            origin = "country.name",
                            destination = "iso3c")
enhanced_gender_country_df <- merge(gender_country_df,
                                    extra_data_df, 
                                    by="iso_code"
                                    )[,c("avg_math", "avg_reading", "avg_science", "avg_glcm", "gender", "gdp_percap")]

enhanced_gender_country_df <- drop_na(enhanced_gender_country_df) %>%
                                pivot_longer(cols = c(avg_math, avg_reading, avg_science, avg_glcm),
                                             names_to = "subject", 
                                             values_to = "score")
plot_list <- list()
i=1
for (subject in unique(enhanced_gender_country_df$subject)) {
  custom_yticks <- seq(-1, 2, by = 0.5) 
  custom_ylabels <- paste(custom_yticks, "%", sep = "")  # Convert tick positions to labels with percentage symbol
   p <- ggplot(data = enhanced_gender_country_df[enhanced_gender_country_df$subject==subject,], 
         aes(x = gdp_percap, y = score, color=gender, size=10)) +
        geom_point() +
        scale_color_manual(values = c(FEMALE_COLOR, MALE_COLOR)) +
        scale_y_continuous(breaks = custom_yticks, labels = custom_ylabels) +
        labs(x=NULL, y=NULL, title = SUBJECT_NAME_LIST[i], color=NULL) +
        theme_minimal() +
        theme(legend.text = element_text(size = 10),   
              legend.key.size = unit(1.5, "cm")) +
        guides(color = guide_legend(override.aes = list(size = 10)), size = "none")  
  
  plot_list[[subject]] <- p
  i <- i + 1
}

grid_plot <- grid.arrange(grobs = plot_list, 
             ncol = 2, 
             nrow = 2, 
             top=textGrob("Country average scores by gender and GDP per capita"),
             bottom=textGrob("GDP per Capita (U.S. $ 2024)"),
             left="Average Score")

ggsave(filename=filepath_png("gender_scatterplot"), plot=grid_plot)


             