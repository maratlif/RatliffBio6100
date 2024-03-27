# Homework nine R code
# 27 March 2024
# Molly Ratliff

library(ggplot2)
library(tidyverse)

# Create a function to generate a data frame with soil organic carbon values for high
# and low topographic depressions in an agricultural field using sample size, mean, and SD.
###############################################################################
# function: genSOC
# input: nLow, nHigh are sample sizes
#        mLow is the mean SOC for high plots
#        mHigh is the mean SOC for low plots
#        SDLow, SDHigh are standard deviations
# output: lowSOC is random numbers generated for soil organic carbon in low plots
#         highSOC is random numbers generated for soil organic carbon in high plots
#------------------------------------------------------------------------
genSOC <- function(nLow=10,nHigh=10, mLow=3, mHigh=2, SDLow=0.5, SDHigh=0.5){
  ID <- 1:nLow
  lowSOC <- rnorm(n=nLow, mean=mLow, sd=SDLow)
  highSOC <- rnorm(n=nHigh, mean=mHigh, sd=SDHigh)
  SOC <- data.frame(highSOC, lowSOC)
  return(SOC)
}
####################################################################################
x <- genSOC()


# Create a function to determine summary stats of genSOC()
# input: x = genSOC
# output: stats
###################################################################################
statsSOC <- function(x=x){
  stats <- summary(x)
  return(stats)
}
###################################################################################
statsSOC(x)

# Create a function to make a boxplot for low vs high 
# input: y=x
# output: plot
##################################################################################
plotSOC <- function(x=x){
 x <- x %>%
    pivot_longer(cols=1:2, names_to = "Treatment", values_to= "MeanSOC", values_drop_na=T)
  plot <- ggplot(x, aes(x=Treatment, y=MeanSOC)) + geom_boxplot()
  return(plot)
}
#################################################################################
plotSOC(x)

# Create a function to run a t-test comparing low and high SOC means
# input: z=z
# output
#################################################################################
ttestSOC <- function(x=x){
  ttest <- t.test(x$highSOC, x$lowSOC, var.equal = TRUE)
  return(ttest)
}
#################################################################################
ttestSOC(x)
