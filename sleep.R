library(tidyverse) # data manipulation
library(here) # allocate file
library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(magrittr) # string manipulation
library(padr) # insert a new row for each missing day
library(zoo) # assign mean to NaN values
library(VIM) # tools for the visualization of missing or imputed values
library(sqldf) # using SQL
library(patchwork) # merge plots

# I. Data Wrangling ----
## load sleep dataset
sleep <- read.csv(here('data','sleepdata.csv'))
sleep <- sleep[-c(10,11,12)] %>% filter(Is.nap == " No") %>% 
  dplyr::rename(Duration = Time.in.Bed..mins., 
                 Awake = Awake.duration..mins.,
                 REM = REM.sleep.duration..mins.,
                 Light = Light.sleep.duration..mins.,
                 Deep = Deep.sleep.duration..mins.)

## string manipulation
sleep$Start.Time <- sleep$Start.Time %>% 
                  gsub("Optional", "", .) %>%
                  gsub("\\(", "", .) %>% 
                  gsub("\\)", "", .) %>% 
                  gsub("\\+", "", .) %>% 
                  gsub("0000", "", .)

sleep$End.Time <- sleep$End.Time %>% 
                  gsub("Optional", "", .) %>%
                  gsub("\\(", "", .) %>% 
                  gsub("\\)", "", .) %>% 
                  gsub("\\+", "", .) %>% 
                  gsub("0000", "", .)

# Add necessary column for analysis
# create new column for date
sleep$Date <- as.Date(sleep$Start.Time)

# create new column for DOW
sleep$DOW <- weekdays(sleep$Date)
sleep$DOW  <- ordered(sleep$DOW, # order Day Of Week chronically
                      levels=c("Monday", "Tuesday", "Wednesday", 
                               "Thursday", "Friday", "Saturday", "Sunday"))

# create new column for weekend vs weekday
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

sleep$wDay <- factor(
  (weekdays(sleep$Date) %in% weekdays1), 
  levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

## load mood dataset
mood <- read.csv(here('data','mood.csv')) %>% filter(Aliases == "Tyrion")
### standardize data
mood$Morning <- trimws(mood$Morning) %>% tolower()
### change date format
mood$Date <- strptime(as.character(mood$Date), "%d/%m/%Y") %>% as.Date()

## load screentime dataset
screentime <- read.csv(here('data','screentime.csv')) %>% filter(Aliases == "Tyrion")
glimpse(screentime)
## change date format
screentime$Date <- strptime(as.character(screentime$Date), "%d/%m/%Y") %>% as.Date()

# merge data
sleep <- sqldf(
  "SELECT
    sleep.*,
    ROUND(screentime.Total_STime*60) AS screen_time,
    mood.Morning AS mood,
    CASE
      WHEN mood.Morning = 'awful' THEN 1
      WHEN mood.Morning = 'bad' THEN 2
      WHEN mood.Morning = 'neutral' THEN 3
      WHEN mood.Morning = 'good' THEN 4
      WHEN mood.Morning = 'happy' THEN 5
    ELSE 0
    END AS mood_scale  
  FROM sleep
  INNER JOIN screentime
  ON sleep.Date = screentime.Date
  INNER JOIN mood
  ON sleep.Date = mood.Date
  "
)

## Retain only hour
sleep$Start.Time <- 
  sub(".+? ", "", sleep$Start.Time) %>% 
  substr(start = 1, stop = 2) %>% 
  as.numeric()

sleep$End.Time <- 
  sub(".+? ", "", sleep$End.Time) %>% 
  substr(start = 1, stop = 2) %>% 
  as.numeric()

# insert a new row for each missing day
pad(sleep)

## Check for NA values
missing_data <- summary(aggr(
  sleep,prop=TRUE,combined=TRUE, cex.axis=0.4, sortVars=TRUE)
)

# numeric variables
sleep[c(1:3,5:9)] <- 
  lapply(sleep[c(1:3,5:9)], as.numeric) # convert columns to numeric

sleep[c(1:3,5:9)] <- 
  na.aggregate(sleep[c(1:3,5:9)]) # replace NA values with mean


# II. Data Visualization ----

## 1. Duration of Sleep Overtime ----
ggplot(sleep) +
  aes(x = Date, y = Duration, fill = Sleep.quality) +
  geom_point(
    shape = "triangle down filled",
    size = 2.8,
    colour = "#112446"
  ) +
  geom_smooth(span = 1L) +
  scale_fill_distiller(palette = "OrRd", direction = -1) +
  labs(
    x = "Date",
    y = "Duration of Sleep (Minutes)",
    title = "Duration of Sleep Overtime",
    fill = "quality"
  ) +
  ggthemes::theme_par() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

## 2. Duration of Sleep Stages by Days of Week ----
# create new variable
sleep1 <- sleep %>% 
  tidyr::gather(key = "Type", value = 'Duration', 'Awake', 'REM', 'Light','Deep')
sleep1[is.na(sleep1)] <- 0

# grouped bar plot
ggplot(sleep1) +
  aes(x = DOW, fill = Type, weight = Duration) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(
    values = c(Awake = "#000000",
               Deep = "#0CB747",
               Light = "#C7E79E",
               REM = "#F230AF")
  ) +
  labs(
    x = "Sleep Stages",
    y = "Sleep Duration (Minutes)",
    title = "Duration of Sleep Stages by Days of Week",
    fill = "sleep stages"
  ) +
  coord_flip() +
  ggthemes::theme_par() +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 15L, hjust = 0.5))

## 3. Correlation ----
## a. Relationship Between Duration & Sleep Quality
corr1 <- ggplot(sleep, aes(x = Sleep.quality, y = Duration)) + 
  geom_point(alpha = .6, colour="#710193") + 
  geom_smooth(method = 'lm', formula = y ~ x, colour="red") +
  labs(title = "Sleep Quality vs Sleep Duration",
       x = "Sleep Quality (%)",
       y = "Sleep Duration (minutes)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
corr1

## b. Relationship Between Total Screen time & Sleep Quality
corr2 <- ggplot(sleep, aes(x = Sleep.quality, y = screen_time)) + 
  geom_point(alpha = .6, , colour="#710193") + 
  geom_smooth(method = 'lm', formula = y ~ x, colour="red") +
  labs(title = "Sleep Quality vs Total Screentime",
       x = "Sleep Quality (%)",
       y = "Total Screen time (minutes)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
corr2

## c. Relationship Between Mood & Sleep Quality 
corr3 <- ggplot(sleep, aes(x = Sleep.quality, y = mood_scale)) + 
  geom_point(alpha = .6, colour="#710193") + 
  geom_smooth(method = 'lm', formula = y ~ x, colour="red") +
  labs(title = "Sleep Quality vs Mood",
       x = "Sleep Quality (%)",
       y = "Wake-up Mood Scale") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
corr3

## d. Relationship Between Mood & Sleep Duration 
corr4 <- ggplot(sleep, aes(x = Duration, y = mood_scale)) + 
  geom_point(alpha = .6, colour="#710193") + 
  geom_smooth(method = 'lm', formula = y ~ x, colour="red") +
  labs(title = "Sleep Duration vs Mood",
       x = "Sleep Quality (%)",
       y = "Sleep Duration (minutes)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
corr4

### plot multiple plots
(corr1 + corr2) / (corr3 + corr4) 

