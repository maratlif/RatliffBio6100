# Homework 8 R Code
# 20 March 2024
# Molly Ratliff

library(tidyverse)

# Load in data
data("iris")
str(iris)

# Sort species and sepal lengths
# One option. Does not work if | is a column (because it treats it as an &)
iris1 <- filter(iris, Species=="virginica"|Species=="versicolor", Sepal.Length > 6, Sepal.Width > 2.5)
# Another option
iris1 <- filter(iris, Species %in% c("virginica","versicolor"), Sepal.Length > 6, Sepal.Width > 2.5)
str(iris1)

# iris2 with only the columns I want
iris2 <- select(iris1, 1,2,5)
str(iris2)

#iris3
iris3 <- arrange(iris2, by=Sepal.Length)
head(iris3)

#iris4
iris4 <- mutate(iris3, Sepal.Area=Sepal.Length*Sepal.Width)
str(iris4)

#iris5
iris5 <- summarize(iris4, meanSepalLength=mean(Sepal.Length), meanSepalWidth=mean(Sepal.Width), number=n())
print(iris5)

#iris6
irisSpecies <- group_by(iris4, Species)
iris6 <- summarize(irisSpecies, meanSepalLength=mean(Sepal.Length), meanSepalWidth=mean(Sepal.Width), number=n())
print(iris6)

#Same as above but with pipe statements
irisFinal <- iris %>%
  filter(Species %in% c("virginica","versicolor"), Sepal.Length > 6, Sepal.Width > 2.5) %>%
  select(1,2,5) %>%
  arrange(by=Sepal.Length) %>%
  mutate(Sepal.Area=Sepal.Length*Sepal.Width) %>%
  group_by(Species) %>%
  summarize(meanSepalLength=mean(Sepal.Length), meanSepalWidth=mean(Sepal.Width), number=n())
print(irisFinal)  

# Longer dataset from iris
iris %>%
  pivot_longer(cols=1:4, names_to = "Measure", values_to= "Values", values_drop_na=T)


  
  
