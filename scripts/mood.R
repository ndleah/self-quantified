library(tidyverse) # data manipulation
library(here) # allocate file
library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(patchwork) # merge visual plots to one
library(VIM) # tools for the visualization of missing or imputed values
library(sqldf) # using SQL
library(tm) # text mining
library(RColorBrewer)
library(wordcloud) # create wordcloud chart


# I. Data Wrangling ----
## load mood dataset
mood_df <- read.csv(here('data','mood.csv'))

### standardize data
mood_df$Morning <- trimws(mood_df$Morning) %>% tolower()
mood_df$Midday <- trimws(mood_df$Midday) %>% tolower()
mood_df$Afternoon <- trimws(mood_df$Afternoon) %>% tolower()
mood_df$Evening <- trimws(mood_df$Evening) %>% tolower()
mood_df <- mood_df[-1] # remove index column

### change date format
mood_df$Date <- strptime(as.character(mood_df$Date), "%d/%m/%Y") %>% 
                as.Date()

# check for missing values
missing_data <- summary(aggr(
  mood_df,prop=TRUE,combined=TRUE, cex.axis=0.4, sortVars=TRUE)
)

# II. Data Visualization ----
# create new variable
mem_mood <- data.frame(aliases = mood_df["Aliases"],
                        mood = c(mood_df[,"Morning"],
                                 mood_df[,"Midday"],
                                 mood_df[,"Afternoon"],
                                 mood_df[,"Evening"]
                                 ),
                        DOW = weekdays(mood_df$Date)
)

# text transformation
mem_mood$mood <- trimws(mem_mood$mood) %>% tolower()

# order Day Of Week chronically
mem_mood$DOW <- ordered(mem_mood$DOW, 
                        levels=c("Monday", "Tuesday", "Wednesday", 
                                 "Thursday", "Friday", "Saturday", 
                                  "Sunday"))
# order mood scale
mem_mood$mood <- ordered(mem_mood$mood,levels=c('awful','bad','neutral',
                                                'good','happy'))

# add mood scale

mem_mood <- sqldf(
  "SELECT
    *,
    CASE
      WHEN mood = 'awful' THEN 1
      WHEN mood = 'bad' THEN 2
      WHEN mood = 'neutral' THEN 3
      WHEN mood = 'good' THEN 4
      WHEN mood = 'happy' THEN 5
      ELSE NULL
    END AS mood_scale,
    CASE
      WHEN mood = 'awful' THEN 'negative'
      WHEN mood = 'bad' THEN 'negative'
      WHEN mood = 'neutral' THEN 'neutral'
      WHEN mood = 'good' THEN 'positive'
      WHEN mood = 'happy' THEN 'positive'
      ELSE NULL
    END AS category
  FROM mem_mood
  "
)

missing_data <- summary(aggr(
  mem_mood,prop=TRUE,combined=TRUE, cex.axis=0.4, sortVars=TRUE)
)

## 1. Mood Frequency by Participants ----
# count mood frequency of each member
mood_freq <- sqldf(
  "SELECT
      Aliases,
      mood,
      category,
      COUNT(*)
    FROM mem_mood
    WHERE mood IS NOT NULL
    AND mood_scale IS NOT NULL
    AND category IS NOT NULL
    GROUP BY 
      Aliases,
      mood,
      category
      "
)

### cersei
cersei_freq <- mood_freq %>%
  filter(Aliases %in% "Cersei") %>%
  ggplot() +
  aes(x = mood, fill = category, weight = `COUNT(*)`) +
  geom_bar(color = "black") +
  scale_fill_manual(
    values = c(negative = "#8E0000",
               neutral = "#80D991",
               positive = "#24B70D")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), 
                     breaks = c(0,10,20,30,40,50,60)) +
  coord_flip() +
  labs(x = "mood", y = "frequency", title = "Cersei") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

### Melisandre
melisandre_freq <- mood_freq %>%
  filter(Aliases %in% "Melisandre") %>%
  ggplot() +
  aes(x = mood, fill = category, weight = `COUNT(*)`) +
  geom_bar(color = "black") +
  scale_fill_manual(
    values = c(negative = "#8E0000",
               neutral = "#80D991",
               positive = "#24B70D")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), 
                     breaks = c(0,10,20,30,40,50,60)) +
  coord_flip() +
  labs(x = "mood", y = "frequency", title = "Melisandre") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

### tyrion
tyrion_freq <- mood_freq %>%
  filter(Aliases %in% "Tyrion") %>%
  ggplot() +
  aes(x = mood, fill = category, weight = `COUNT(*)`) +
  geom_bar(color = "black") +
  scale_fill_manual(
    values = c(negative = "#8E0000",
               neutral = "#80D991",
               positive = "#24B70D")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), 
                     breaks = c(0,10,20,30,40,50,60)) +
  coord_flip() +
  labs(x = "mood", y = "frequency", title = "Tyrion") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

### Oberyn
oberyn_freq <- mood_freq %>%
  filter(Aliases %in% "Oberyn") %>%
  ggplot() +
  aes(x = mood, fill = category, weight = `COUNT(*)`) +
  geom_bar(color = "black") +
  scale_fill_manual(
    values = c(negative = "#8E0000",
               neutral = "#80D991",
               positive = "#24B70D")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), 
                     breaks = c(0,10,20,30,40,50,60)) +
  coord_flip() +
  labs(x = "mood", y = "frequency", title = "Oberyn") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

### Jaqen
jaqen_freq <- mood_freq %>%
  filter(Aliases %in% "Jaqen") %>%
  ggplot() +
  aes(x = mood, fill = category, weight = `COUNT(*)`) +
  geom_bar(color = "black") +
  scale_fill_manual(
    values = c(negative = "#8E0000",
               neutral = "#80D991",
               positive = "#24B70D")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), 
                     breaks = c(0,10,20,30,40,50,60)) +
  coord_flip() +
  labs(x = "mood", y = "frequency", title = "Jaqen") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

### Tormund
tormund_freq <- mood_freq %>%
  filter(Aliases %in% "Tormund") %>%
  ggplot() +
  aes(x = mood, fill = category, weight = `COUNT(*)`) +
  geom_bar(color = "black") +
  scale_fill_manual(
    values = c(negative = "#8E0000",
               neutral = "#80D991",
               positive = "#24B70D")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), 
                     breaks = c(0,10,20,30,40,50,60)) +
  coord_flip() +
  labs(x = "mood", y = "frequency", title = "Tormund") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

### plot multiple plots
(cersei_freq + melisandre_freq)/
  (tyrion_freq + oberyn_freq)/ 
  (jaqen_freq + tormund_freq) +
    plot_annotation(title = "Mood Frequency by Participants") & 
    theme(plot.title = element_text(hjust = 0.5))

## 2. Mood Frequency by Day of Week ----
# count mood frequency of BY dow
mood_freq_week <- sqldf(
  "
  SELECT
      Aliases,
      mood,
      mood_scale,
      DOW,
      COUNT(*) AS freq
    FROM mem_mood
    WHERE mood IS NOT NULL
    GROUP BY 
      Aliases,
      mood,
      mood_scale,
      DOW

      "
)

mood_freq_week$mood<-ifelse(mood_freq_week$mood == "awful"
                        ,"1. awful",
                        ifelse(mood_freq_week$mood == "bad"
                               ,"2. bad",
                               ifelse(mood_freq_week$mood == "neutral"
                                      ,"3. neutral",
                                      ifelse(mood_freq_week$mood == "good"
                                             ,"4. good",
                                             ifelse(mood_freq_week$mood == "happy"
                                                    ,"5. happy",NA)))))
### Monday
t2 <- mood_freq_week %>%
  filter(DOW %in% "Monday") %>%
  ggplot() +
  aes(x = freq, fill = mood) +
  geom_density(alpha=.4, adjust = 1L) +
  scale_fill_manual(
    values = c(`1. awful` = "#A50026",
               `2. bad` = "#F85151",
               `3. neutral` = "#FFFFBF",
               `4. good` = "#8FDDA2",
               `5. happy` = "#21BB4E")
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12.5), 
                     breaks = c(0,2.5,5.0,7.5,10.0,12.5)) +
  labs(x = "frequency", title = "Monday") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

### Tuesday
t3 <- mood_freq_week %>%
  filter(DOW %in% "Tuesday") %>%
  ggplot() +
  aes(x = freq, fill = mood) +
  geom_density(alpha=.4, adjust = 1L) +
  scale_fill_manual(
    values = c(`1. awful` = "#A50026",
               `2. bad` = "#F85151",
               `3. neutral` = "#FFFFBF",
               `4. good` = "#8FDDA2",
               `5. happy` = "#21BB4E")
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12.5), 
                     breaks = c(0,2.5,5.0,7.5,10.0,12.5)) +
  labs(x = "frequency", title = "Tuesday") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

### Wednesday
t4 <- mood_freq_week %>%
  filter(DOW %in% "Wednesday") %>%
  ggplot() +
  aes(x = freq, fill = mood) +
  geom_density(alpha=.4, adjust = 1L) +
  scale_fill_manual(
    values = c(`1. awful` = "#A50026",
               `2. bad` = "#F85151",
               `3. neutral` = "#FFFFBF",
               `4. good` = "#8FDDA2",
               `5. happy` = "#21BB4E")
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12.5), 
                     breaks = c(0,2.5,5.0,7.5,10.0,12.5)) +
  labs(x = "frequency", title = "Wednesday") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

### Thursday
t5 <- mood_freq_week %>%
  filter(DOW %in% "Thursday") %>%
  ggplot() +
  aes(x = freq, fill = mood) +
  geom_density(alpha=.4, adjust = 1L) +
  scale_fill_manual(
    values = c(`1. awful` = "#A50026",
               `2. bad` = "#F85151",
               `3. neutral` = "#FFFFBF",
               `4. good` = "#8FDDA2",
               `5. happy` = "#21BB4E")
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12.5), 
                     breaks = c(0,2.5,5.0,7.5,10.0,12.5)) +
  labs(x = "frequency", title = "Thursday") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

### Friday
t6 <- mood_freq_week %>%
  filter(DOW %in% "Friday") %>%
  ggplot() +
  aes(x = freq, fill = mood) +
  geom_density(alpha=.4, adjust = 1L) +
  scale_fill_manual(
    values = c(`1. awful` = "#A50026",
               `2. bad` = "#F85151",
               `3. neutral` = "#FFFFBF",
               `4. good` = "#8FDDA2",
               `5. happy` = "#21BB4E")
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12.5), 
                     breaks = c(0,2.5,5.0,7.5,10.0,12.5)) +
  labs(x = "frequency", title = "Friday") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

### Saturday
t7 <- mood_freq_week %>%
  filter(DOW %in% "Saturday") %>%
  ggplot() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12.5), 
                     breaks = c(0,2.5,5.0,7.5,10.0,12.5)) +
  aes(x = freq, fill = mood) +
  geom_density(alpha=.4, adjust = 1L) +
  scale_fill_manual(
    values = c(`1. awful` = "#A50026",
               `2. bad` = "#F85151",
               `3. neutral` = "#FFFFBF",
               `4. good` = "#8FDDA2",
               `5. happy` = "#21BB4E")
  ) +
  labs(x = "frequency", title = "Saturday") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

### Sunday
t8 <- mood_freq_week %>%
  filter(DOW %in% "Sunday") %>%
  ggplot() +
  aes(x = freq, fill = mood) +
  geom_density(alpha=.4, adjust = 1L) +
  scale_fill_manual(
    values = c(`1. awful` = "#A50026",
               `2. bad` = "#F85151",
               `3. neutral` = "#FFFFBF",
               `4. good` = "#8FDDA2",
               `5. happy` = "#21BB4E")
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12.5), 
                     breaks = c(0,2.5,5.0,7.5,10.0,12.5)) +
  labs(x = "frequency", title = "Sunday") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )
    
### plot multiple plots
(t2+t3)/(t4+t5)/(t6+t7)/t8 +
  plot_annotation(title = "Mood Frequency by Days of Week") & 
  theme(plot.title = element_text(hjust = 0.5))

## 3. Sentimental Analysis ----
# assign variables to do sentimental analysis based on mood levels
sentiment <- data.frame(mood = c(mood_df[,"Morning"], 
                                     mood_df[,"Midday"],
                                     mood_df[,"Afternoon"],
                                     mood_df[,"Evening"]),
                              analysis = c(mood_df[,"Why."],
                                           mood_df[,"Why..1"],
                                           mood_df[,"Why..2"],
                                           mood_df[,"Why..3"])
)

# negative mood ----
negative_mood <- sqldf(
  "SELECT
    analysis
   FROM sentiment
   WHERE mood IN ('awful','bad')
  "
)

## Create corpus
negative_corpus = Corpus(VectorSource(negative_mood$analysis))

# Convert to lower-case
negative_corpus = tm_map(negative_corpus, tolower)

## Remove punctuation
negative_corpus = tm_map(negative_corpus, removePunctuation)

# Remove stopwords 
negative_corpus = tm_map(negative_corpus, removeWords, c("feeling", "feel", "and", "the", "have", "had", "with","about","really"))

## Create matrix
negative_frequencies = DocumentTermMatrix(negative_corpus)

# Convert to a data frame
allNegative = as.data.frame(as.matrix(negative_frequencies))

## How many unique words are there across all the documents?
ncol(allNegative)

wordcloud(colnames(allNegative), colSums(allNegative), 
          random.order=FALSE, max.words = 100, colors=brewer.pal(8, "Dark2"))


# positive mood ----
positive_mood <- sqldf(
  "SELECT
    analysis
   FROM sentiment
   WHERE mood IN ('good','happy')
  "
)

## Create corpus
positive_corpus = Corpus(VectorSource(positive_mood$analysis))

# Convert to lower-case
positive_corpus = tm_map(positive_corpus, tolower)

## Remove punctuation
positive_corpus = tm_map(positive_corpus, removePunctuation)

# Remove stopwords 
positive_corpus = tm_map(positive_corpus, removeWords, c("feeling", "feel", "and", "the", "have", "had", "with","about","really", "for"))

## Create matrix
positive_frequencies = DocumentTermMatrix(positive_corpus)

# Convert to a data frame
allPositive = as.data.frame(as.matrix(positive_frequencies))

## How many unique words are there across all the documents?
ncol(allPositive)

wordcloud(colnames(allPositive), colSums(allPositive), 
          random.order=FALSE, max.words = 100, colors=brewer.pal(8, "Dark2"))

