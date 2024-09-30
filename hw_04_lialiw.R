# Sources:
# https://edav.info/index.html
# https://livebook.datascienceheroes.com/
# https://r4ds.had.co.nz/

# https://medium.com/analytics-vidhya/spotify-music-data-analysis-part-3-9097829df16e
# https://stackoverflow.com/questions/47799182/grouping-data-in-r-and-summing-by-decade
# https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
# https://stackoverflow.com/questions/10866047/jitter-geom-line
# https://rdrr.io/cran/RColorBrewer/man/ColorBrewer.html

library(readr)
library(tidyverse)
library(dplyr)
library(funModeling)
library(minerva) # contains MIC statistic
# plotting
library(ggplot2) 
library(ggridges)
library(lattice)
library(GGally)
library(viridis)
library(gridExtra) # allow us to plot two plots in a row
library(corrplot)
library(RColorBrewer) 

library(reshape2) 


options(scipen=999) # disable scientific notation


# data source
url <- "https://raw.githubusercontent.com/akhopkar01/spotify-analyzed/master/data/data_by_year.csv"
spot_by_year <- read_csv(url)
#spot_by_year <- read_csv("~/hw_04_spotify_data_by_year.csv")

# Check for columns' datatypes
str(spot_by_year)
# Catch a glimpse of the data
#view(head(spot_by_year))

# Check for NAs
colSums(is.na(spot_by_year)) #; rowSums(is.na(spot_by_year))

# Check popularity range
range(spot_by_year$popularity)

# Converting milliseconds to minutes
spot_by_year$duration_ms <- spot_by_year$duration_ms * 10^(-5)


# UNIVARIATE ANALYSIS

time <- c("year")
music_categorical <- c("key","mode")
music_continuous <- c("acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "liveness", "loudness", "speechiness", "tempo", "valence", "popularity")


# HISTOGRAMS

# Histogram for acousticness 
ggplot(spot_by_year, aes(x = spot_by_year$acousticness, y = ..density..)) + 
  geom_histogram(bins = 20, colour = "#80593D", fill = "light blue", boundary = 0) + geom_density(color = "#3D6480") + 
  ggtitle("Histogram of acousticness") + labs(x = "Acousticness") +
  theme(plot.title = element_text(face = "bold")) + theme(plot.subtitle = element_text(face = "bold", color = "grey35")) + theme(plot.caption = element_text(color = "grey68"))

# Histogram for duration_ms
ggplot(spot_by_year, aes(x = spot_by_year$duration_ms, y = ..density..)) + 
  geom_histogram(bins = 20, colour = "#80593D", fill = "light blue", boundary = 0) + geom_density(color = "#3D6480") + 
  ggtitle("Histogram of duriation in minutes") + labs(x = "Minutes") +
  theme(plot.title = element_text(face = "bold")) + theme(plot.subtitle = element_text(face = "bold", color = "grey35")) + theme(plot.caption = element_text(color = "grey68"))


# BOXPLOTS

#Boxplots of speechiness by key
ggplot(spot_by_year, aes(x = reorder(spot_by_year$key, -spot_by_year$speechiness, median), y = spot_by_year$speechiness)) + 
  geom_boxplot(fill = "lightblue", color = "#473e2c") + 
  ggtitle("The key defines speechiness?", subtitle = "Boxplots of speechiness by key") + labs(x = "key i.e. pitch, notes, scale", y = "Speechiness") +
  theme_grey(16) + theme(plot.title = element_text(face = "bold")) + theme(plot.subtitle = element_text(face = "bold", color = "grey35")) + theme(plot.caption = element_text(color = "grey68"))


# VIOLIN PLOTS

# Violin Plot of instrumentalness by key
keys <- unique(spot_by_year$key)
ggplot(spot_by_year, aes(x = factor(spot_by_year$key, levels = keys),  y = spot_by_year$instrumentalness)) + 
  geom_violin(fill = "lightBlue", color = "#473e2c") + 
  labs(x = "key i.e. pitch, notes, scale", y = "Instumentalness") + 
  geom_boxplot(width=0.2) + geom_dotplot(binaxis='y', dotsize=0.05, stackdir='center')


# BARPLOTS

#Grouping by decades
spot_by_decade <- spot_by_year %>% mutate(decade = floor(year/10)*10) %>% group_by(decade)
#view(spot_by_decade)

# 
spot_by_key_mode_decade <- as.data.frame(spot_by_decade) %>%
  group_by(key) %>% select(decade, mode, key)
#view(spot_by_key_mode_decade)
temp <- count(spot_by_key_mode_decade, decade)


ggplot(temp, aes(x = decade, y = n)) + geom_bar(stat = "identity") + facet_wrap(~key)+
  ggtitle("Count of keys per decade") + labs(y = "key count") +
  theme(plot.title = element_text(face = "bold")) + theme(plot.subtitle = element_text(face = "bold", color = "grey35")) + theme(plot.caption = element_text(color = "grey68"))



# MULTIVARIATE


# SCATTERPLOTS

ggplot(data = spot_by_year) + 
  geom_point(mapping = aes(x = instrumentalness, y = liveness))

ggplot(spot_by_year, aes(spot_by_year$energy, spot_by_year$danceability)) + 
  geom_point() + 
  geom_smooth(mapping = aes(x = spot_by_year$energy, y = spot_by_year$danceability)) + 
  labs(x = "Energy", y = "Danceability") 

ggplot(spot_by_year, aes(spot_by_year$loudness, spot_by_year$acousticness)) + 
  geom_point() +  geom_density_2d(bins = 7) + labs(x = "Loudness", y = "Acousticness") 


subsetVars <- spot_by_year %>% dplyr::select(energy, danceability, valence, liveness, tempo, loudness)
#plot(subsetVars)  
splom(subsetVars)

# PARALLEL COORDINATES PLOTS
ggparcoord(spot_by_year, columns=9:10,  groupColumn = 13, scale = "robust",alphaLines = 0.8, title = "Parallel coordinate plot for Keys, Speechiness and Tempo")


# HEATMAPS

ggplot(spot_by_year, aes(year, tempo)) + 
  geom_bin2d(binwidth = c(11, 11))+
  ggtitle("Tempos over the years")+
  scale_fill_viridis() 

ggplot(spot_by_year, aes(year, valence))  + 
  geom_hex( alpha = .4)+
  #geom_point(size = 2, alpha = 0.8)+
  ggtitle("Valence over the years")+
  scale_fill_viridis() 


## TREAT CONTINUOUS VARIABLE AS CATEGORICAL --> BOXPLOTS

ggplot(data = spot_by_year, mapping = aes(x = valence, y = danceability)) + 
  geom_boxplot(mapping = aes(group = cut_width(valence, 0.01)))

ggplot(data = spot_by_year, mapping = aes(x = loudness, y = instrumentalness)) + 
  geom_boxplot(mapping = aes(group = cut_number(loudness, 10)))


# CORRELATIONS

correlation_table(data=spot_by_year, target="popularity")
#visualize
spot_by_year_cor = cor(spot_by_year)
corrplot(spot_by_year_cor)

temp <- subset(spot_by_year, select = -c(mode))
#view(temp)
spot_by_year_cor = cor(temp)
corrplot(spot_by_year_cor)

palette = colorRampPalette(c("lightblue", "white", "red")) (20)
heatmap(x = spot_by_year_cor, col = palette, symm = TRUE)

#dim(spot_by_year_cor)



# TIME SERIES

# YEAR
# excluding mode since it is always 1
temp <- subset(spot_by_year, select = -c(mode))
spot_time_series = melt(temp, id="year")

ggplot(data=spot_time_series, aes(x=year, y=value, colour=variable)) +
  geom_line(position=position_jitter(w=0, h=5)) + 
  theme_minimal() + 
  ggtitle("Songs' characteristics over the years")+
  scale_color_brewer(palette="Set3")

# Instrumentalness, Duration in minutes, loudness --> YEAR
temp <- subset(spot_by_year, select = c(year, instrumentalness, duration_ms, loudness))

df <- temp %>% gather(key = attribute, value = Value, -year) %>%
    mutate(TYPE = forcats::fct_reorder2(attribute, year, Value)) # puts legend in correct order
#df
ggplot(df, aes(year, Value, color = attribute)) + geom_line() +
  ggtitle("Instrumentalness, Duration in minutes, loudness over the years") +
  labs (x = "", y = "percent") +
  theme_grey(16) +
  theme(legend.title = element_blank())

# Key --> YEAR
temp <- subset(spot_by_year, select = c(year, key))
ggplot(data = temp, aes(x = year, y = key))+ geom_line(color = "blue") + ggtitle("Keys over the years") + labs(x = "year", y = "key")

# Danceability, Energy, Liveness, Speechiness, Valence --> YEAR
temp <- subset(spot_by_year, select = -c(acousticness, mode, popularity, tempo, instrumentalness, duration_ms, loudness, key))
df <- temp %>% gather(key = attribute, value = Value, -year) %>%
  mutate(TYPE = forcats::fct_reorder2(attribute, year, Value)) # puts legend in correct order
#df
ggplot(df, aes(year, Value, color = attribute)) + geom_line() +
  ggtitle("Instrumentalness, Duration in minutes, loudness over the years") +
  labs (x = "", y = "percent") +
  theme_grey(16) +
  theme(legend.title = element_blank())

# DECADE
# excluding mode since it is always 1 and year since decades are examined
temp <- subset(spot_by_decade, select = -c(mode,year))
spot_time_series = melt(temp, id="decade")

ggplot(data=spot_time_series, aes(x=decade, y=value, colour=variable)) +
  geom_line(position=position_jitter(w=0, h=5)) + 
  theme_minimal() + 
  ggtitle("Songs' characteristics over the decades")+
  scale_color_brewer(palette="Set3")

# Instrumentalness, Duration in minutes, loudness --> DECADE
temp <- subset(spot_by_decade, select = c(decade, instrumentalness, duration_ms, loudness))

df <- temp %>% gather(key = attribute, value = Value, -decade) %>%
  mutate(TYPE = forcats::fct_reorder2(attribute, decade, Value)) # puts legend in correct order
#df
ggplot(df, aes(decade, Value, color = attribute)) + geom_line() +
  ggtitle("Instrumentalness, Duration in minutes, loudness over the decades") +
  labs (x = "", y = "percent") +
  theme_grey(16) +
  theme(legend.title = element_blank())

# Key --> DECADE
temp <- subset(spot_by_decade, select = c(decade, key))
ggplot(data = temp, aes(x = decade, y = key))+ geom_line(color = "blue") + ggtitle("Keys over the decades") + labs(x = "decade", y = "key")

# Danceability, Energy, Liveness, Speechiness, Valence --> DECADE
temp <- subset(spot_by_decade, select = -c(year,acousticness, mode, popularity, tempo, instrumentalness, duration_ms, loudness, key))
df <- temp %>% gather(key = attribute, value = Value, -decade) %>%
  mutate(TYPE = forcats::fct_reorder2(attribute, decade, Value)) # puts legend in correct order
#df
ggplot(df, aes(decade, Value, color = attribute)) + geom_line() +
  ggtitle("Danceability, Energy, Liveness, Speechiness, Valence over the decades") +
  labs (x = "", y = "percent") +
  theme_grey(16) +
  theme(legend.title = element_blank())


## MIC and MAS

# Calculating, printing, plotting MAS and MIC values for time series --> YEAR

# excluding mode since it is always 1 
temp <- subset(spot_by_year, select = -c(mode))
mine_ts=mine(temp)
mine_ts$MAS 
mine_ts$MIC

diag(mine_ts$MIC)=0
corrplot(mine_ts$MIC, method="circle", col=brewer.pal(n=10, name="PuOr"),
         type="lower",# only display upper diagonal 
         tl.col="red", # label color
         tl.cex = 0.9, # label size
         tl.srt=90, # label rotation
         diag=FALSE, # don't print diagonal (var against itself)
         is.corr = F # accept a any matrix, mic in this case  (not a correlation element) 
)

diag(mine_ts$MAS)=0
corrplot(mine_ts$MAS, method="color", type="lower",  number.cex=0.7,
         addCoef.col = "black", # add coefficient of correlation 
         tl.col="red",  tl.srt=90,  tl.cex = 0.9, diag=FALSE,  is.corr = F )


# Calculating, printing, plotting MAS and MIC values for time series --> DECADE

# excluding mode since it is always 1 and year since decades are examined
temp <- subset(spot_by_decade, select = -c(mode, year))
mine_ts=mine(temp)
mine_ts$MAS 
mine_ts$MIC

diag(mine_ts$MIC)=0
corrplot(mine_ts$MIC, method="circle", col=brewer.pal(n=10, name="PuOr"),
         type="lower", tl.col="red",  tl.cex = 0.9,  tl.srt=90,  diag=FALSE, # don't print diagonal (var against itself)
         is.corr = F # accept a any matrix, mic in this case  (not a correlation element) 
)

diag(mine_ts$MAS)=0
corrplot(mine_ts$MAS, method="color", type="lower",  number.cex=0.7,
         addCoef.col = "black", # add coefficient of correlation 
         tl.col="red",  tl.srt=90,  tl.cex = 0.9, diag=FALSE,  is.corr = F )


## POPULARITY, based on MIC --> YEAR

# Getting the index of the variable to predict: popularity, based on MIC 
target="popularity"
# excluding mode since it is always 1 
temp <- subset(spot_by_year, select = -c(mode))
index_target <- grep(target, colnames(temp))

# master takes the index column number to calculate all the correlations
mic_predictive <- mine(temp, master = index_target)$MIC

# creating the data frame containing the results, ordering descently by 
# its correlation and excluding the correlation of target vs itself
df_predictive <-  
  data.frame(variable=rownames(mic_predictive), mic=mic_predictive[,1], stringsAsFactors = F) %>% 
  arrange(-mic) %>% filter(variable!=target)

ggplot(df_predictive,  aes(x=reorder(variable, mic),y=mic, fill=variable) ) + 
  geom_bar(stat='identity') +  coord_flip() +  theme_bw() +  xlab("") +  
  ylab("Variable Importance (based on MIC) - years") + guides(fill=FALSE)


## POPULARITY, based on MIC --> DECADE

# Getting the index of the variable to predict: popularity, based on MIC 
target="popularity"
# excluding mode since it is always 1 and year since decades are examined
temp <- subset(spot_by_decade, select = -c(mode, year))
index_target <- grep(target, colnames(temp))

# master takes the index column number to calculate all the correlations
mic_predictive <- mine(temp, master = index_target)$MIC

# creating the data frame containing the results, ordering descently by 
# its correlation and excluding the correlation of target vs itself
df_predictive <-  
  data.frame(variable=rownames(mic_predictive), mic=mic_predictive[,1], stringsAsFactors = F) %>% 
  arrange(-mic) %>% filter(variable!=target)

ggplot(df_predictive,  aes(x=reorder(variable, mic),y=mic, fill=variable) ) + 
  geom_bar(stat='identity') +  coord_flip() +  theme_bw() +  xlab("") +  
  ylab("Variable Importance (based on MIC) - decades") + guides(fill=FALSE)

