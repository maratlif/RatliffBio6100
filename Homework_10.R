# Homework 10: For loops
# 03 April 2024
# Molly Ratliff

library(tidyverse)

# Question 1
myVec <- (rbinom(n=5,size=1,prob=0.5))
counter <- 0

for (i in seq_along(myVec)){
  if(myVec[i]==0) {
  counter <- counter + 1
  }
}
print(counter)

# Question 2
myVec <- rbinom(n = 5, size = 1, prob = 0.5)
counter <- sum(myVec == 0)
print(counter)

# Question 3
m <- matrix(round(runif(12),digits=1),nrow=3)
multiplyMatrix <- function(m) {
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      m[i,j] <- m[i,j] * m[i,j]
    }
  }
return(m)
}
multiplyMatrix(m)

# Question 4
#4a
plotApH <- rnorm(n=10, mean=9, sd=0.5)
plotBpH <- rnorm(n=10, mean=7, sd=0.5)
plotCpH <- rnorm(n=10, mean=6, sd=0.5)
pH <- data.frame(plotApH, plotBpH, plotCpH)
pH <- pH %>%
  pivot_longer(cols=1:3, names_to="TreatmentGroup",values_to="MeanpH",values_drop_na=T)
print(pH)

#4b
shuffle <- function(pH){
  plotApH <- rnorm(n=10, mean=9, sd=0.5)
  plotBpH <- rnorm(n=10, mean=7, sd=0.5)
  plotCpH <- rnorm(n=10, mean=6, sd=0.5)
  pH <- data.frame(plotApH, plotBpH, plotCpH)
  pH <- pH %>%
    pivot_longer(cols=1:3, names_to="TreatmentGroup",values_to="MeanpH",values_drop_na=T)
  pHGroups <- group_by(pH, TreatmentGroup)
  pHdata <- c(summarize(pHGroups, meanpH=mean(MeanpH, na.rm=TRUE)))
  means <- pHdata$meanpH
  return(means)
}
shuffle(pH)

#4c
results <- data.frame(replicate = 1:100)
for (i in 1:100) {
  results[i, 2:4] <- shuffle()
}
print(results)


#4d
qplot(data = results, x = V2, geom = "histogram")
qplot(data = results, x = V3, geom = "histogram")
qplot(data = results, x = V4, geom = "histogram")

