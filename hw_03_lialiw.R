#hw3
#In the following exercises we are going to use the dirty_iris dataset. You can download this dataset from: https://raw.github.com/edwindj/datacleaning/master/data/dirty_iris.csv


# Exercise 3.1. Reading and manually checking.

# a. View the file in a text-editor to determine its format and read the file into R. Make sure that strings are not converted to factor.
iris_data <- read.csv(url("https://raw.github.com/edwindj/datacleaning/master/data/dirty_iris.csv"), header=TRUE, sep=",")
str(iris_data) 

# b. Calculate the number and percentage of observations that are complete.
#1st way:
num_complete <- nrow(na.omit(iris_data))
#2nd way:
# num_complete <- sum(complete.cases(iris_data))
perc_complete <- num_complete/nrow(iris_data)

# c. Does the data contain other special values? If it does, replace them with NA 
is.special <- function(x){
  if (is.numeric(x) && !is.finite(x)  && !is.na(x)){
    x <- NA
  }
}
sapply(iris_data, is.special)
view(iris_data)

# Exercise 3.2. Checking with rules

# a. Besides missing values, the data set contains errors. We have the following background knowledge:
# – Species should be one of the following values: setosa, versicolor or virginica.
# – All measured numerical properties of an iris should be positive.
# – The petal length of an iris is at least 2 times its petal width.
# – The sepal length of an iris cannot exceed 30 cm.
# – The sepals of an iris are longer than its petals.
# Define these rules in a separate text file and read them into R using editfile (package editrules). Print the resulting constraint object.
library(editrules)
E <- editfile("~/hw_03_lialiw_edits.txt")
E

# b. Determine how often each rule is broken (violatedEdits). Also summarize and plot the result.
ve <- violatedEdits(E, iris_data)
summary(ve)
plot(ve)

# c. What percentage of the data has no errors?
#From structure of summary(ve) it is known that the first row concerns all records with zero errors. Hence:
summ_ve <- summary(ve)
summ_ve$rel[1]

# d. Find out which observations have too long petals using the result of violatedEdits.
# If an observation has too long petals, num7 is the one violated
library(dplyr)
num7 <- na.omit(filter(iris_data, iris_data$Sepal.Length<=iris_data$Petal.Length))
num7

# e. Find outliers in sepal length using boxplot and boxplot.stats. Retrieve the corresponding observations and look at 
#    the other values. Any ideas what might have happened? Set the outliers to NA (or a value that you find more appropiate)
library(ggplot2)
ggplot(iris_data, aes(y=Sepal.Length)) +
  geom_boxplot(color="black", fill="lightblue") + 
  ggtitle("Boxplot for Sepal Length in iris dataset")

boxplot.stats(iris_data$Sepal.Length)

seLe_out <- boxplot.stats(iris_data$Sepal.Length)$out
sepalLength_outliers <- filter(iris_data, iris_data$Sepal.Length %in% seLe_out)
sepalLength_outliers

iris_data[iris_data$Sepal.Length %in% seLe_out, "Sepal.Length"] <- NA

ggplot(iris_data, aes(y=Sepal.Length)) + geom_boxplot(color="black", fill="lightblue") + ggtitle("Boxplot for Sepal Length in iris dataset")
boxplot.stats(iris_data$Sepal.Length)


# Exercise 3.3. Correcting

# a. Replace non positive values from Petal.Width with NA using correctWithRules from the library deducorrect.
library(deducorrect)
R <- correctionRules("hw_03_lialiw_correctionRules.txt")
cor <- correctWithRules(R, iris_data)
summary(iris_data)

# b. Replace all erronous values with NA using (the result of) localizeErrors
le <- localizeErrors(E, iris_data, method = "mip")
#le$adapt
iris_data <- replace(iris_data, le$adapt,NA)
summary(iris_data)
summary(violatedEdits(E, iris_data))


# Exercise 3.4. Imputing

# a. Use kNN imputation (VIM) to impute all missing values.
library(VIM)
kNN_iris_data <- kNN(iris_data)
summary(kNN_iris_data)

# b. Use sequential hotdeck imputation to impute Petal.Width by sorting the dataset on Species. Compare the
#     imputed Petal.Width with the sequential hotdeck imputation method. Note the ordering of the data!
library(imputeTS)
# x : vector to be imputed
# last : value to use if last value of x is empty
iris_data_sortSpe<- iris_data[order(iris_data$Species),]
seqImpute <- function(x,last){
  n <- length(x)
  x <- c(x,last)
  i <- is.na(x)
  while(any(i)){
    x[i] <- x[which(i) + 1]
    i <- is.na(x)
  }
  x[1:n]
}
x_val <- na_mean(iris_data_sortSpe$Petal.Width) 
seq_hotdeck_peWiSpe <- seqImpute(iris_data_sortSpe$Petal.Width, x_val)
#Comparing:
summary(kNN_iris_data$Petal.Width)
summary(seq_hotdeck_peWiSpe)

# c. Do the same but now by sorting the dataset on Species and Sepal.Length.
x_val <- na_mean(iris_data_sortSpe$Sepal.Length)
seq_hotdeck_seLeSpe <- seqImpute(iris_data_sortSpe$Sepal.Length, x_val)
#Comparing:
summary(kNN_iris_data$Sepal.Length)
summary(seq_hotdeck_seLeSpe)

# Sources:
# https://datatofish.com/data-type-dataframe-r/
# https://www.geeksforgeeks.org/get-the-number-of-rows-of-an-object-in-r-programming-nrow-function/
# https://stackoverflow.com/questions/5391124/select-rows-of-a-matrix-that-meet-a-condition
# http://www.endmemo.com/r/boxplot.stats.php
# https://community.rstudio.com/t/how-do-i-filter-a-variable-by-a-value-and-replace-all-these-values/3222
# https://www.journaldev.com/39695/replace-in-r
# https://www.statmethods.net/management/sorting.html