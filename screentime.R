library(tidyverse) # data manipulation
library(here) # allocate file
library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(ggrepel)
library(lubridate)
library(VIM) # tools for the visualization of missing or imputed values
library(lubridate)
library(patchwork) # merge plots
library(xts) # a powerful library that makes work with time series pretty easy
library(sqldf) # using SQL
library(reshape)
library(tibble)
library(ggpubr) # plot bubble plot
library(treemapify) #plot treemap visualization

# I. Data Wrangling ----

## Load the dataset
df <- read.csv(here('data','screentime.csv'))
glimpse(df)

## convert column format
df <- df[-c(1)] # remove index column
df$Date <- as.Date(strptime(df$Date, "%d/%m/%Y")) # convert column format to date
df$Day_of_Week <- as.factor(df$Day_of_Week) # convert column format to factor
df$First_Pickup[which(df$First_Pickup == "0:13")] <- "12:13 AM"
df$First_Pickup[which(df$First_Pickup == "0:05")] <- "12:05 AM"
df$First_Pickup[which(df$First_Pickup == "1:03")] <- "1:03 AM"
df$First_Pickup <- format(strptime(df$First_Pickup, "%I:%M %p"), "%H")

## convert string columns
df$Aliases <- trimws(df$Aliases)

low_col <-c(13,15,17,19,21,25,27,29,32,34,36)
df2 <- df %>%
  mutate_at(vars(low_col), funs(tolower(.))) # convert app name columns to lower case

## Check for NA values
missing_data <- summary(aggr(
  df2,prop=TRUE,combined=TRUE, cex.axis=0.4, sortVars=TRUE)
  )

## convert hour to minute column
h_col <- c(4:12,14,16,18,20,22)
df2[h_col] <- round(df2[h_col] * 60)

## Sum values for Entertainment and Games
df2$Entertainment <- df2$Games + df2$Entertainment

## Add column for number of week
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

df2$DayType <- factor(
  (weekdays(df2$Date) %in% weekdays1), 
  levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))

df2$week_number<-ifelse(df2$Date >= as.Date("2021-08-10") & 
                          df2$Date <= as.Date("2021-08-17")
                        ,"Week 1",
                        ifelse(df2$Date >= as.Date("2021-08-18") & 
                                 df2$Date <= as.Date("2021-08-25")
                               ,"Week 2",
                               ifelse(df2$Date >= as.Date("2021-08-26") & 
                                        df2$Date <= as.Date("2021-09-02")
                                      ,"Week 3",
                                      ifelse(df2$Date >= as.Date("2021-09-03") & 
                                               df2$Date <= as.Date("2021-09-10")
                                             ,"Week 4",
                                             ifelse(df2$Date >= as.Date("2021-09-11") & 
                                                      df2$Date <= as.Date("2021-09-18")
                                                    ,"Week 5",NA)))))


## Create variables for each member
cersei <- df2 %>% filter(Aliases == "Cersei")
melisandre <- df2 %>% filter(Aliases == "Melisandre")
tyrion <- df2 %>% filter(Aliases == "Tyrion")
tormund <- df2 %>% filter(Aliases == "Tormund")
jaqen <- df2 %>% filter(Aliases == "Jaqen")
oberyn <- df2 %>% filter(Aliases == "Oberyn")

# II. Data Visualization ----
## Daily screen time by participant
ggplot(df2) +
  aes(x = Date, y = Total_STime, colour = Aliases) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    y = "Total Screentime (minutes)",
    title = "Daily screen time by participant",
    color = "Participant"
  ) +
  ggthemes::theme_par() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 15L,
                              hjust = 0.5)
  )

## Time spent on Social
c1 <- ggplot(df2) +
  aes(x = Date, y = Social, colour = Aliases) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    y = "Total Screentime (minutes)",
    title = "Time spent on Social",
    color = "Participant"
  ) +
  ggthemes::theme_par() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 15L,
                              hjust = 0.5)
  )
c1

## Time spent on Games/Entertainment
c2 <- ggplot(df2) +
  aes(x = Date, y = Entertainment, colour = Aliases) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    y = "Total Screentime (minutes)",
    title = "Time spent on Games/Entertainment",
    color = "Participant"
  ) +
  ggthemes::theme_par() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 15L,
                              hjust = 0.5)
  )
c2

## Time spent on Productivity & Finance
c3 <- ggplot(df2) +
  aes(x = Date, y = Productivity, colour = Aliases) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    y = "Total Screentime (minutes)",
    title = "Time spent on Productivity",
    color = "Participant"
  ) +
  ggthemes::theme_par() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 15L,
                              hjust = 0.5)
  )
c3

## Time spent on Shopping
c4 <- ggplot(df2) +
  aes(x = Date, y = Shopping, colour = Aliases) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    y = "Total Screentime (minutes)",
    title = "Time spent on Shopping",
    color = "Participant"
  ) +
  ggthemes::theme_par() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 15L,
                              hjust = 0.5)
  )
c4

## Time spent on Creativity
c5 <- ggplot(df2) +
  aes(x = Date, y = Creativity, colour = Aliases) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    y = "Total Screentime (minutes)",
    title = "Time spent on Creativity",
    color = "Participant"
  ) +
  ggthemes::theme_par() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 15L,
                              hjust = 0.5)
  )
c5

## Time spent on Reading
c6 <- ggplot(df2) +
  aes(x = Date, y = Reading, colour = Aliases) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    y = "Total Screentime (minutes)",
    title = "Time spent on Creativity",
    color = "Participant"
  ) +
  ggthemes::theme_par() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 15L,
                              hjust = 0.5)
  )
c6

grid.arrange(c1,c2,c3,c4,c5,c6, ncol=3, nrow=2)

## 1. Time spent in each category by participant ----
mdata <- melt(data=df2,
              id.vars=
                c("Aliases","Day_of_Week","Total_STime","Total_Pickups"),
              measure.vars =
                c("Entertainment","Social","Reading","Productivity",
                  "Creativity","Shopping", "Other")
              )

### order Day Of Week chronically
mdata$Day_of_Week <- ordered(mdata$Day_of_Week, 
                             levels=c("Mon", "Tue", "Wed", 
                                      "Thu", "Fri", "Sat", "Sun"))

### Stacked barplots
ggplot(mdata) +
  aes(x = Aliases, fill = variable, weight = Total_STime) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3", direction = 1) +
  labs(
    x = "Participant",
    y = "Total Screentime (minutes)",
    title = "Total Time Spent by Participant",
    fill = "Category"
  ) +
  ggthemes::theme_calc() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))


## 2. Total_STime Vs. Total_Pickups ----

### a. Cersei ----
### ST and PC trend overtime
g1 <- ggplot(cersei, aes(x = Date, y = Total_STime)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Screen Minutes Over Time ",
       x = "Date",
       y = "Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

g2 <- ggplot(cersei, aes(x = Date, y = Total_Pickups)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Phone Pickups Over Time ",
       x = "Date",
       y = "Pickups") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Average Screen Time/Phone pick ups
a1 <- ggplot(cersei, aes(x = Total_STime)) +
  geom_density(alpha=.2, fill="blue") +
  labs(title = "Screen Time Minutes",
       x = "Minutes",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

a2 <- ggplot(cersei, aes(x = Total_Pickups)) +
  geom_density(alpha=.2, fill="red") +
  labs(title = "Phone Pickups",
       x = "Pickups",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

### plotting multiple plots
cersei_SPUtime <- (a1 + a2) / (g1 + g2) + 
  plot_annotation(title = "Cersei") & 
  theme(plot.title = element_text(hjust = 0.5))
cersei_SPUtime

### b. Melisandre ----
### ST and PC trend overtime
g3 <- ggplot(melisandre, aes(x = Date, y = Total_STime)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Screen Minutes Over Time ",
       x = "Date",
       y = "Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

g4 <- ggplot(melisandre, aes(x = Date, y = Total_Pickups)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Phone Pickups Over Time ",
       x = "Date",
       y = "Pickups") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Average Screen Time/Phone pick ups
a3 <- ggplot(melisandre, aes(x = Total_STime)) +
  geom_density(alpha=.2, fill="blue") +
  labs(title = "Screen Time Minutes",
       x = "Minutes",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

a4 <- ggplot(melisandre, aes(x = Total_Pickups)) +
  geom_density(alpha=.2, fill="red") +
  labs(title = "Phone Pickups",
       x = "Pickups",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

### plotting multiple plots
melisandre_SPUtime <- (a3 + a4) / (g3 + g4) + 
  plot_annotation(title = "Melisandre") & 
  theme(plot.title = element_text(hjust = 0.5))
melisandre_SPUtime


### c. Tyrion ----
### ST and PC trend overtime
g5 <- ggplot(tyrion, aes(x = Date, y = Total_STime)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Screen Minutes Over Time ",
       x = "Date",
       y = "Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

g6 <- ggplot(tyrion, aes(x = Date, y = Total_Pickups)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Phone Pickups Over Time ",
       x = "Date",
       y = "Pickups") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(g5, g6, nrow=2)

### Average Screen Time/Phone pick ups
a5 <- ggplot(tyrion, aes(x = Total_STime)) +
  geom_density(alpha=.2, fill="blue") +
  labs(title = "Screen Time Minutes",
       x = "Minutes",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

a6 <- ggplot(tyrion, aes(x = Total_Pickups)) +
  geom_density(alpha=.2, fill="red") +
  labs(title = "Phone Pickups",
       x = "Pickups",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

### plotting multiple plots
tyrion_SPUtime <- (a5 + a6) / (g5 + g6) + 
  plot_annotation(title = "Tyrion") & 
  theme(plot.title = element_text(hjust = 0.5))
tyrion_SPUtime

### d. Oberyn ----
### ST and PC trend overtime
g7 <- ggplot(oberyn, aes(x = Date, y = Total_STime)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Screen Minutes Over Time ",
       x = "Date",
       y = "Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

g8 <- ggplot(oberyn, aes(x = Date, y = Total_Pickups)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Phone Pickups Over Time ",
       x = "Date",
       y = "Pickups") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Average Screen Time/Phone pick ups
a7 <- ggplot(oberyn, aes(x = Total_STime)) +
  geom_density(alpha=.2, fill="blue") +
  labs(title = "Screen Time Minutes",
       x = "Minutes",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

a8 <- ggplot(oberyn, aes(x = Total_Pickups)) +
  geom_density(alpha=.2, fill="red") +
  labs(title = "Phone Pickups",
       x = "Pickups",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

### plotting multiple plots
oberyn_SPUtime <- (a7 + a8) / (g7 + g8) + 
  plot_annotation(title = "Oberyn") & 
  theme(plot.title = element_text(hjust = 0.5))
oberyn_SPUtime

### e. Jaqen ----
### ST and PC trend overtime
g9 <- ggplot(jaqen, aes(x = Date, y = Total_STime)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Screen Minutes Over Time ",
       x = "Date",
       y = "Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

g10 <- ggplot(jaqen, aes(x = Date, y = Total_Pickups)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Phone Pickups Over Time ",
       x = "Date",
       y = "Pickups") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Average Screen Time/Phone pick ups
a9 <- ggplot(jaqen, aes(x = Total_STime)) +
  geom_density(alpha=.2, fill="blue") +
  labs(title = "Screen Time Minutes",
       x = "Minutes",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

a10 <- ggplot(jaqen, aes(x = Total_Pickups)) +
  geom_density(alpha=.2, fill="red") +
  labs(title = "Phone Pickups",
       x = "Pickups",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

### plotting multiple plots
jaqen_SPUtime <- (a9 + a10) / (g9 + g10) + 
  plot_annotation(title = "Jaqen") & 
  theme(plot.title = element_text(hjust = 0.5))
jaqen_SPUtime

### f. Tormund ----
### ST and PC trend overtime
g11 <- ggplot(tormund, aes(x = Date, y = Total_STime)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Screen Minutes Over Time ",
       x = "Date",
       y = "Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

g12 <- ggplot(tormund, aes(x = Date, y = Total_Pickups)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Phone Pickups Over Time ",
       x = "Date",
       y = "Pickups") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Average Screen Time/Phone pick ups
a11 <- ggplot(tormund, aes(x = Total_STime)) +
  geom_density(alpha=.2, fill="blue") +
  labs(title = "Screen Time Minutes",
       x = "Minutes",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

a12 <- ggplot(tormund, aes(x = Total_Pickups)) +
  geom_density(alpha=.2, fill="red") +
  labs(title = "Phone Pickups",
       x = "Pickups",
       y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

### plotting multiple plots
tormund_SPUtime <- (a11 + a12) / (g11 + g12) + 
  plot_annotation(title = "Tormund") & 
  theme(plot.title = element_text(hjust = 0.5))
tormund_SPUtime

## 3. Relationship Between Phone picks ups and Screen Time ----
ggplot(df2, aes(x = Total_Pickups, y = Total_STime)) + 
  geom_point(alpha = .6) + 
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  labs(title = "Minutes of Screen Time vs Phone Pickups",
       x = "Phone Pickups",
       y = "Minutes of Screen Time") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## 4. Time spent distribution along weekdays and weekends by participant ----

### create new variable
weekend_weekday <- melt(data=df2,
                        id.vars=
                          c("Aliases","DayType","Total_STime"),
                        measure.vars =
                          c("Entertainment","Social","Reading","Productivity",
                            "Creativity","Shopping", "Other")
)

## General ----
gen_weekdays <- sqldf( # weekdays
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE DayType = 'Weekday'
    GROUP BY category
      "
)
# calculate percentage
gen_weekdays <- gen_weekdays %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_gen_weekdays <- gen_weekdays %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

gen_weekend <- sqldf( # weekdays
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE DayType = 'Weekend'
    GROUP BY category
      "
)
# calculate percentage
gen_weekend <- gen_weekend %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_gen_weekend <- gen_weekend %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

### i. weekday - pie chart
gw1 <- gen_weekdays %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_gen_weekdays %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekdays Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### ii. weekend - pie chart
gw2 <- gen_weekend %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_gen_weekend %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekend Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
(gw1 + gw2) + plot_annotation(title = "General") & 
  theme(plot.title = element_text(hjust = 0.5))


### a. Cersei ----
# create weekdays variable
cersei_weekdays <- sqldf( # weekdays
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Cersei'
    AND DayType = 'Weekday'
    GROUP BY category
      "
)
# calculate percentage
cersei_weekdays <- cersei_weekdays %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_cersei_weekdays <- cersei_weekdays %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

# create weekend variable
cersei_weekend <- sqldf( # weekend
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Cersei'
    AND DayType = 'Weekend'
    GROUP BY category
      "
)
# calculate percentage
cersei_weekend <- cersei_weekend %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_cersei_weekend <- cersei_weekend %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

### i. weekday - pie chart
cw1 <- cersei_weekdays %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_cersei_weekdays %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekdays Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### ii. weekend - pie chart
cw2 <- cersei_weekend %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_cersei_weekend %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekend Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
(cw1 + cw2) + plot_annotation(title = "Cersei") & 
  theme(plot.title = element_text(hjust = 0.5))

### b. Melisandre ----
# create weekdays variable
melisandre_weekdays <- sqldf( # weekdays
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Melisandre'
    AND DayType = 'Weekday'
    GROUP BY category
      "
)
# calculate percentage
melisandre_weekdays <- melisandre_weekdays %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_melisandre_weekdays <- melisandre_weekdays %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

# create weekend variable
melisandre_weekend <- sqldf( # weekend
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Melisandre'
    AND DayType = 'Weekend'
    GROUP BY category
      "
)
# calculate percentage
melisandre_weekend <- melisandre_weekend %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_melisandre_weekend <- melisandre_weekend %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

### i. weekday - pie chart
cw3 <- melisandre_weekdays %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_melisandre_weekdays %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekdays Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### ii. weekend - pie chart
cw4 <- melisandre_weekend %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_melisandre_weekend %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekend Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
(cw3 + cw4) + plot_annotation(title = "Melisandre") & 
  theme(plot.title = element_text(hjust = 0.5))

### c. Tyrion ----
# create weekdays variable
tyrion_weekdays <- sqldf( # weekdays
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Tyrion'
    AND DayType = 'Weekday'
    GROUP BY category
      "
)
# calculate percentage
tyrion_weekdays <- tyrion_weekdays %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_tyrion_weekdays <- tyrion_weekdays %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

# create weekend variable
tyrion_weekend <- sqldf( # weekend
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Tyrion'
    AND DayType = 'Weekend'
    GROUP BY category
      "
)
# calculate percentage
tyrion_weekend <- tyrion_weekend %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_tyrion_weekend <- tyrion_weekend %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

### i. weekday - pie chart
cw5 <- tyrion_weekdays %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_tyrion_weekdays %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekdays Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### ii. weekend - pie chart
cw6 <- tyrion_weekend %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_tyrion_weekend %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekend Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
(cw5 + cw6) + plot_annotation(title = "Tyrion") & 
  theme(plot.title = element_text(hjust = 0.5))

### d. Oberyn ----
# create weekdays variable
oberyn_weekdays <- sqldf( # weekdays
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Oberyn'
    AND DayType = 'Weekday'
    GROUP BY category
      "
)
# calculate percentage
oberyn_weekdays <- oberyn_weekdays %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_oberyn_weekdays <- oberyn_weekdays %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

# create weekend variable
oberyn_weekend <- sqldf( # weekend
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Oberyn'
    AND DayType = 'Weekend'
    GROUP BY category
      "
)
# calculate percentage
oberyn_weekend <- oberyn_weekend %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_oberyn_weekend <- oberyn_weekend %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

### i. weekday - pie chart
cw7 <- oberyn_weekdays %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_oberyn_weekdays %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekdays Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### ii. weekend - pie chart
cw8 <- oberyn_weekend %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_oberyn_weekend %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekend Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
(cw7 + cw8) + plot_annotation(title = "Oberyn") & 
  theme(plot.title = element_text(hjust = 0.5))

### e. Jaqen ----
# create weekdays variable
jaqen_weekdays <- sqldf( # weekdays
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Jaqen'
    AND DayType = 'Weekday'
    GROUP BY category
      "
)
# calculate percentage
jaqen_weekdays <- jaqen_weekdays %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_jaqen_weekdays <- jaqen_weekdays %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

# create weekend variable
jaqen_weekend <- sqldf( # weekend
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Jaqen'
    AND DayType = 'Weekend'
    GROUP BY category
      "
)
# calculate percentage
jaqen_weekend <- jaqen_weekend %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_oberyn_weekend <- oberyn_weekend %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

### i. weekday - pie chart
cw9 <- jaqen_weekdays %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_jaqen_weekdays %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekdays Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### ii. weekend - pie chart
cw10 <- jaqen_weekend %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_jaqen_weekend %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekend Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
(cw9 + cw10) + plot_annotation(title = "Jaqen") & 
  theme(plot.title = element_text(hjust = 0.5))

### e. Tormund ----
# create weekdays variable
tormund_weekdays <- sqldf( # weekdays
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Tormund'
    AND DayType = 'Weekday'
    GROUP BY category
      "
)
# calculate percentage
tormund_weekdays <- tormund_weekdays %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_tormund_weekdays <- tormund_weekdays %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

# create weekend variable
tormund_weekend <- sqldf( # weekend
  "
  SELECT
      variable AS category,
      SUM(value) AS value
    FROM weekend_weekday
    WHERE Aliases = 'Tormund'
    AND DayType = 'Weekend'
    GROUP BY category
      "
)
# calculate percentage
tormund_weekend <- tormund_weekend %>% 
  arrange(desc(value)) %>%
  mutate(prop = round(100 * value / sum(value),1))
# get the position
pos_tormund_weekend <- tormund_weekend %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

### i. weekday - pie chart
cw11 <- tormund_weekdays %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_tormund_weekdays %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekdays Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### ii. weekend - pie chart
cw12 <- tormund_weekend %>% 
  filter(prop > 0.01) %>% # filter out 0% value
  ggplot(aes(x = "", y = value, fill = fct_inorder(category))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(data = pos_tormund_weekend %>% filter(prop > 0.01),
                   aes(y = pos, label = paste0(prop, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(
    title = "Weekend Distribution"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "categories")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
(cw11 + cw12) + plot_annotation(title = "Tormund") & 
  theme(plot.title = element_text(hjust = 0.5))

## 5. App with most Screen time Usage ----
## General ----
# create new variable
app <- data.frame(app_name = c(df2[,"Top1"], df2[,"Top2"], 
                               df2[,"Top3"], df2[,"Top4"], 
                               df2[,"Top5"]),
                         Total_STime = c(df2[,"Top1_T"], df2[,"Top2_T"], 
                                         df2[,"Top3_T"], df2[,"Top4_T"], 
                                         df2[,"Top5_T"]),
                         DOW = df2["Day_of_Week"])
# sum total screen time by app name
app <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime
    FROM app
    GROUP BY 
      app_name
    ORDER BY 
      Total_STime DESC
    LIMIT 10
      "
)

### Top 10 Apps with Highest Screentime Usage
app %>%
  group_by(app_name) %>%
  top_n(10, Total_STime)  %>% 
  ggplot(aes(area = Total_STime, 
             fill = Total_STime, 
             label = app_name)) +
  geom_treemap() +
  labs(title = "Top 10 Apps with Highest Screentime Usage") +
  geom_treemap_text(fontface = "italic", 
                    colour = "white", 
                    place = "topleft", 
                    reflow = T,
                    grow = TRUE) +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))


## a. Cersei ----
# create new variable
cersei_app <- data.frame(app_name = c(cersei[,"Top1"], cersei[,"Top2"], 
                                      cersei[,"Top3"], cersei[,"Top4"], 
                                      cersei[,"Top5"]),
                         Total_STime = c(cersei[,"Top1_T"], cersei[,"Top2_T"], 
                                         cersei[,"Top3_T"], cersei[,"Top4_T"], 
                                         cersei[,"Top5_T"]),
                         DOW = cersei["Day_of_Week"])
# sum total screen time by app name
cersei_app <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime,
      Day_of_Week
    FROM cersei_app
    GROUP BY 
      app_name,
      Day_of_Week
    ORDER BY 
      Day_of_Week,
      Total_STime DESC
      "
)

# order Day Of Week chronically
cersei_app$Day_of_Week <- ordered(cersei_app$Day_of_Week, 
                                  levels=c("Mon", "Tue", "Wed", 
                                           "Thu", "Fri", "Sat","Sun"))

cersei_top10 <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime
    FROM cersei_app
    GROUP BY app_name
    ORDER BY Total_STime DESC
    LIMIT 10
      "
)

### Top 10 Apps with Highest Screentime Usage
aw1 <- cersei_top10 %>%
  group_by(app_name) %>%
  top_n(10, Total_STime)  %>% 
  ggplot(aes(area = Total_STime, 
             fill = Total_STime, 
             label = app_name)) +
  geom_treemap() +
  labs(title = "Top 10 Apps with Highest Screentime Usage") +
  geom_treemap_text(fontface = "italic", 
                    colour = "white", 
                    place = "topleft", 
                    reflow = T,
                    grow = TRUE) +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))


### App Screentime Usage by Days of Week
aw2 <- ggballoonplot(cersei_app, x = "Day_of_Week", y = "app_name", 
                     size = "Total_STime", fill = "Total_STime",
                     ggtheme = theme_minimal()) +
  labs(title = "App Screentime Usage by Days of Week") +
  scale_fill_viridis_c(option = "C") +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
aw1 + aw2 + plot_annotation(title = "Cersei") & 
  theme(plot.title = element_text(hjust = 0.5))

## b. Melisandre ----
# create new variable
melisandre_app <- data.frame(app_name = c(melisandre[,"Top1"], 
                                          melisandre[,"Top2"], 
                                          melisandre[,"Top3"], 
                                          melisandre[,"Top4"], 
                                          melisandre[,"Top5"]),
                         Total_STime = c(melisandre[,"Top1_T"], 
                                         melisandre[,"Top2_T"], 
                                         melisandre[,"Top3_T"], 
                                         melisandre[,"Top4_T"], 
                                         melisandre[,"Top5_T"]),
                         DOW = melisandre["Day_of_Week"])
# sum total screen time by app name
melisandre_app <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime,
      Day_of_Week
    FROM melisandre_app
    GROUP BY 
      app_name,
      Day_of_Week
    ORDER BY 
      Day_of_Week,
      Total_STime DESC
      "
)

# order Day Of Week chronically
melisandre_app$Day_of_Week <- ordered(melisandre_app$Day_of_Week, 
                                  levels=c("Mon", "Tue", "Wed", 
                                           "Thu", "Fri", "Sat","Sun"))

melisandre_top10 <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime
    FROM melisandre_app
    GROUP BY app_name
    ORDER BY Total_STime DESC
    LIMIT 10
      "
)

### Top 10 Apps with Highest Screentime Usage
aw1 <- melisandre_top10 %>%
  group_by(app_name) %>%
  top_n(10, Total_STime)  %>% 
  ggplot(aes(area = Total_STime, 
             fill = Total_STime, 
             label = app_name)) +
  geom_treemap() +
  labs(title = "Top 10 Apps with Highest Screentime Usage") +
  geom_treemap_text(fontface = "italic", 
                    colour = "white", 
                    place = "topleft", 
                    reflow = T,
                    grow = TRUE) +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))


### App Screentime Usage by Days of Week
aw2 <- ggballoonplot(melisandre_app, x = "Day_of_Week", y = "app_name", size = "Total_STime", fill = "Total_STime",
                     ggtheme = theme_minimal()) +
  labs(title = "App Screentime Usage by Days of Week") +
  scale_fill_viridis_c(option = "C") +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
aw1 + aw2 + plot_annotation(title = "Melisandre") & 
  theme(plot.title = element_text(hjust = 0.5))

## c. Tyrion ----
# create new variable
tyrion_app <- data.frame(app_name = c(tyrion[,"Top1"], tyrion[,"Top2"], 
                                      tyrion[,"Top3"], tyrion[,"Top4"], 
                                      tyrion[,"Top5"]),
                         Total_STime = c(tyrion[,"Top1_T"], tyrion[,"Top2_T"], 
                                         tyrion[,"Top3_T"], tyrion[,"Top4_T"], 
                                         tyrion[,"Top5_T"]),
                         DOW = tyrion["Day_of_Week"])
# sum total screen time by app name
tyrion_app <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime,
      Day_of_Week
    FROM tyrion_app
    GROUP BY 
      app_name,
      Day_of_Week
    ORDER BY 
      Day_of_Week,
      Total_STime DESC
      "
)

# order Day Of Week chronically
tyrion_app$Day_of_Week <- ordered(tyrion_app$Day_of_Week, 
                                  levels=c("Mon", "Tue", "Wed", 
                                           "Thu", "Fri", "Sat","Sun"))

tyrion_top10 <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime
    FROM tyrion_app
    GROUP BY app_name
    ORDER BY Total_STime DESC
    LIMIT 10
      "
)

### Top 10 Apps with Highest Screentime Usage
aw5 <- tyrion_top10 %>%
  group_by(app_name) %>%
  top_n(10, Total_STime)  %>% 
  ggplot(aes(area = Total_STime, 
             fill = Total_STime, 
             label = app_name)) +
  geom_treemap() +
  labs(title = "Top 10 Apps with Highest Screentime Usage") +
  geom_treemap_text(fontface = "italic", 
                    colour = "white", 
                    place = "topleft", 
                    reflow = T,
                    grow = TRUE) +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))


### App Screentime Usage by Days of Week
aw6 <- ggballoonplot(tyrion_app, x = "Day_of_Week", y = "app_name", size = "Total_STime", fill = "Total_STime",
                     ggtheme = theme_minimal()) +
  labs(title = "App Screentime Usage by Days of Week") +
  scale_fill_viridis_c(option = "C") +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
aw5 + aw6 + plot_annotation(title = "Tyrion") & 
  theme(plot.title = element_text(hjust = 0.5))

## d. Oberyn ----
# create new variable
tyrion_app <- data.frame(app_name = c(tyrion[,"Top1"], tyrion[,"Top2"], 
                                      tyrion[,"Top3"], tyrion[,"Top4"], 
                                      tyrion[,"Top5"]),
                         Total_STime = c(tyrion[,"Top1_T"], tyrion[,"Top2_T"], 
                                         tyrion[,"Top3_T"], tyrion[,"Top4_T"], 
                                         tyrion[,"Top5_T"]),
                         DOW = tyrion["Day_of_Week"])
# sum total screen time by app name
tyrion_app <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime,
      Day_of_Week
    FROM tyrion_app
    GROUP BY 
      app_name,
      Day_of_Week
    ORDER BY 
      Day_of_Week,
      Total_STime DESC
      "
)

# order Day Of Week chronically
tyrion_app$Day_of_Week <- ordered(tyrion_app$Day_of_Week, 
                                  levels=c("Mon", "Tue", "Wed", 
                                           "Thu", "Fri", "Sat","Sun"))

tyrion_top10 <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime
    FROM tyrion_app
    GROUP BY app_name
    ORDER BY Total_STime DESC
    LIMIT 10
      "
)

### Top 10 Apps with Highest Screentime Usage
aw5 <- tyrion_top10 %>%
  group_by(app_name) %>%
  top_n(10, Total_STime)  %>% 
  ggplot(aes(area = Total_STime, 
             fill = Total_STime, 
             label = app_name)) +
  geom_treemap() +
  labs(title = "Top 10 Apps with Highest Screentime Usage") +
  geom_treemap_text(fontface = "italic", 
                    colour = "white", 
                    place = "topleft", 
                    reflow = T,
                    grow = TRUE) +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))


### App Screentime Usage by Days of Week
aw6 <- ggballoonplot(tyrion_app, x = "Day_of_Week", y = "app_name", size = "Total_STime", fill = "Total_STime",
                     ggtheme = theme_minimal()) +
  labs(title = "App Screentime Usage by Days of Week") +
  scale_fill_viridis_c(option = "C") +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
aw5 + aw6 + plot_annotation(title = "Tyrion") & 
  theme(plot.title = element_text(hjust = 0.5))
## e. Jaqen ----
# create new variable
jaqen_app <- data.frame(app_name = c(jaqen[,"Top1"], 
                                     jaqen[,"Top2"],
                                     jaqen[,"Top3"], 
                                     jaqen[,"Top4"], 
                                     jaqen[,"Top5"]),
                          Total_STime = c(jaqen[,"Top1_T"], 
                                          jaqen[,"Top2_T"], 
                                          jaqen[,"Top3_T"], 
                                          jaqen[,"Top4_T"], 
                                          jaqen[,"Top5_T"]),
                          DOW = jaqen["Day_of_Week"])
# sum total screen time by app name
jaqen_app <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime,
      Day_of_Week
    FROM jaqen_app
    GROUP BY 
      app_name,
      Day_of_Week
    ORDER BY 
      Day_of_Week,
      Total_STime DESC
      "
)

# order Day Of Week chronically
jaqen_app$Day_of_Week <- ordered(jaqen_app$Day_of_Week, 
                                   levels=c("Mon", "Tue", "Wed", 
                                            "Thu", "Fri", "Sat","Sun"))

jaqen_top10 <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime
    FROM jaqen_app
    GROUP BY app_name
    ORDER BY Total_STime DESC
    LIMIT 10
      "
)

### Top 10 Apps with Highest Screentime Usage
aw9 <- jaqen_top10 %>%
  group_by(app_name) %>%
  top_n(10, Total_STime)  %>% 
  ggplot(aes(area = Total_STime, 
             fill = Total_STime, 
             label = app_name)) +
  geom_treemap() +
  labs(title = "Top 10 Apps with Highest Screentime Usage") +
  geom_treemap_text(fontface = "italic", 
                    colour = "white", 
                    place = "topleft", 
                    reflow = T,
                    grow = TRUE) +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))


### App Screentime Usage by Days of Week
aw10 <- ggballoonplot(jaqen_app, x = "Day_of_Week", y = "app_name", size = "Total_STime", fill = "Total_STime",
                      ggtheme = theme_minimal()) +
  labs(title = "App Screentime Usage by Days of Week") +
  scale_fill_viridis_c(option = "C") +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
aw9 + aw10 + plot_annotation(title = "Jaqen") & 
  theme(plot.title = element_text(hjust = 0.5))

## f. Tormund ----
# create new variable
tormund_app <- data.frame(app_name = c(tormund[,"Top1"], 
                                       tormund[,"Top2"],
                                       tormund[,"Top3"], 
                                       tormund[,"Top4"], 
                                       tormund[,"Top5"]),
                         Total_STime = c(tormund[,"Top1_T"], 
                                         tormund[,"Top2_T"], 
                                         tormund[,"Top3_T"], 
                                         tormund[,"Top4_T"], 
                                         tormund[,"Top5_T"]),
                          DOW = tormund["Day_of_Week"])
# sum total screen time by app name
tormund_app <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime,
      Day_of_Week
    FROM tormund_app
    GROUP BY 
      app_name,
      Day_of_Week
    ORDER BY 
      Day_of_Week,
      Total_STime DESC
      "
)

# order Day Of Week chronically
tormund_app$Day_of_Week <- ordered(tormund_app$Day_of_Week, 
                                  levels=c("Mon", "Tue", "Wed", 
                                           "Thu", "Fri", "Sat","Sun"))

tormund_top10 <- sqldf(
  "
  SELECT
      app_name,
      SUM(Total_STime) AS Total_STime
    FROM tormund_app
    GROUP BY app_name
    ORDER BY Total_STime DESC
    LIMIT 10
      "
)

### Top 10 Apps with Highest Screentime Usage
aw11 <- tormund_top10 %>%
  group_by(app_name) %>%
  top_n(10, Total_STime)  %>% 
  ggplot(aes(area = Total_STime, 
             fill = Total_STime, 
             label = app_name)) +
  geom_treemap() +
  labs(title = "Top 10 Apps with Highest Screentime Usage") +
  geom_treemap_text(fontface = "italic", 
                    colour = "white", 
                    place = "topleft", 
                    reflow = T,
                    grow = TRUE) +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))


### App Screentime Usage by Days of Week
aw12 <- ggballoonplot(tormund_app, x = "Day_of_Week", y = "app_name", 
                      size = "Total_STime", fill = "Total_STime",
              ggtheme = theme_minimal()) +
  labs(title = "App Screentime Usage by Days of Week") +
  scale_fill_viridis_c(option = "C") +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

### Plot multiple plots
aw11 + aw12 + plot_annotation(title = "Tormund") & 
  theme(plot.title = element_text(hjust = 0.5))


