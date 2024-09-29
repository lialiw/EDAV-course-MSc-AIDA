# hw2a

library(tidyverse)
library(statisticalModeling)

# Study the structure of the Birth_weight dataset
head(Birth_weight) # Display the first few entries of the Birth_weight dataset
str(Birth_weight) # Display the structure of the Birth_weight dataset

# Provide a diagram for each of the following:

# Study the distribution of the 'income' variable values
ggplot(data = Birth_weight) + 
  stat_count(mapping = aes(x = income))

# Study the distribution of the 'baby_wt' (baby weight) variable values
ggplot(data = Birth_weight) + 
  geom_bar(mapping = aes(x = baby_wt),color="cyan")


# Study the distribution of the 'mother_age' variable values
ggplot(data = Birth_weight) + 
  geom_bar(mapping = aes(x = mother_age),fill="red")


# Study the relationship between the variables 'mother_age' and 'income'
ggplot(Birth_weight, aes(x = mother_age, y = income)) +
  geom_point()
ggplot(data = Birth_weight) + 
  geom_smooth(mapping = aes(x = mother_age, y = income))
# From the diagrams above, it is apparent that a barplot could better depict the relationship:
ggplot(data = Birth_weight) +
  geom_bar(mapping = aes(x = mother_age, color = income))


# Study the relationship between the variables 'baby_wt' and 'smoke'
ggplot(data = Birth_weight) +
  geom_bar(mapping = aes(x = baby_wt, color = smoke))

# Study the relationship between the variables 'mother_age' and 'mother_wt' (mother weight)
ggplot(data = Birth_weight) +
  geom_bar(mapping = aes(x = mother_age, fill = mother_wt))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = Birth_weight,aes(x=mother_wt, y=baby_wt))+
          geom_point(color="red")+
          geom_smooth(fill="yellow")


# Study the relationship between the variables 'mother_wt', 'baby_wt' and 'smoke'
ggplot(data = Birth_weight) + 
  geom_point(mapping = aes(x = mother_wt, y = baby_wt)) + 
  facet_grid(. ~ smoke)


# Study the distribution (boxplot) of the 'baby_wt' variable in relation to the 'smoke' variable
ggplot(data = Birth_weight, aes(x = baby_wt, y = smoke)) +
  geom_boxplot()+
  coord_flip()

# Study the distribution (boxplot) of the 'baby_wt' variable in relation to the 'smoke' and 'income' variables
ggplot(data = Birth_weight, aes(x = baby_wt, y = smoke, fill = income)) +
  geom_boxplot()
ggplot(data = Birth_weight, aes(x = baby_wt, y = smoke, colour = income)) +
  geom_boxplot(position = "identity")

