# hw2b

library(tidyverse)
# You will use the EconomistData dataset.
# The data is from the Economist magazine and pertains to two indices:
# HDI = Human Development Index (http://hdr.undp.org/en/content/human-development-index-hdi)
# CPI = Corruption Perceptions Index (https://www.transparency.org/)
dat <- read_csv("~/EconomistData.csv")  # if you are on igreed
dat <- read_csv("EconomistData.csv")  # if you are running RStudio on your computer
glimpse(dat)

# The HDI index ranges from 0 to 1, with 1 being the best
# The CPI index ranges from 0 to 10, with 10 being the best

# See which Regions exist
unique(dat$Region)

# Acronym explanations:
# MENA = Middle East and North Africa
# SSA = Sub-Saharan Africa
# East EU Cemt Asia = Eastern Europe and Central Asian countries that have CEMT permits,
# where CEMT = Conférence Européenne des Ministres des Transports

# See which countries are in the "EU W. Europe" Region
dat %>% filter(Region=="EU W. Europe") %>% view()

# Here is a comparison of the Regions with respect to the two indices
ggplot(dat2, aes(x = avgHDI, y = avgCPI, color=Region)) +
  geom_point()

# This is how we achieve the same with dplyr functions
(dat3 <- dat %>% 
    group_by(Region) %>% summarise(avgHDI = mean(HDI), avgCPI = mean(CPI)))

ggplot(dat3, aes(x = avgHDI, y = avgCPI, color=Region)) +
  geom_point()

# To answer the following problems, consult the ggplot2 cheat sheet (https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-visualization.pdf)
# There isn't a single answer as there are many ways to represent the required information for each problem. Experiment with different plots and improvise.

# 1. Provide a plot that describes the distribution of CPI.

ggplot(dat, aes(x=CPI))+
  geom_density(kernel = "gaussian", linetype=6)

ggplot(dat, aes(x=CPI))+
   geom_freqpoly(kernel = "gaussian", color="red")
   
# 2. Provide a plot that shows the number of countries per Region.

dat_Reg_groupBy <- dat %>%  
                  group_by(Region)%>% count()

ggplot(data = dat_Reg_groupBy) +
  geom_bar(mapping = aes(x = Region, y = n), stat = "identity")
  
# 3. Provide a plot that shows the percentage of countries per Region.

ggplot(data = dat) + 
  geom_bar(mapping = aes(x = Region, y = ..count.. / sum(..count..)))

ggplot(data = dat) +
  geom_bar(mapping = aes(x = Region, y = ..prop.., group = 1))
  
# 4. Provide a plot that shows the average HDI per Region.

dat_avgHDI_Reg <- dat %>% 
    group_by(Region) %>% summarise(avgHDI = mean(HDI))

ggplot(dat_avgHDI_Reg, aes(dat_avgHDI_Reg$avgHDI))+ 
        geom_dotplot(fill="cyan")
        
# 5. Provide a plot that shows the correlation between HDI and CPI for all countries.

ggplot(dat, aes(x=HDI, y=CPI))+ 
  geom_smooth( color="red")+ 
  geom_point() 

# 6. Provide a set of plots (facet) that shows the correlation between HDI and CPI for all the countries in each Region.

ggplot(data = dat) + 
  geom_point(mapping = aes(x = HDI, y = CPI), color="blue") + 
  facet_grid(. ~ Region)

ggplot(data = dat) + 
  geom_point(mapping = aes(x = HDI, y = CPI), color ="blue") + 
  facet_grid(Region ~ . )+ coord_flip()

