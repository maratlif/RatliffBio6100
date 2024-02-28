# Homework seven R code
# 28 Feb 2024
# Molly Ratliff

library(ggplot2)

# size, mean, and variance of data
nName <- c("LowTopo","HighTopo") 
nSize <- c(9,9)
nMean <- c(3.397,2.685) 
nSD <- c(0.75,0.75)

# data frame setup
ID <- 1:(sum(nSize))
SOC <- c(rnorm(n=nSize[1],mean=nMean[1],sd=nSD[1]),
            rnorm(n=nSize[2],mean=nMean[2],sd=nSD[2]))
TGroup <- rep(nName,nSize)
ANOdata <- data.frame(ID,TGroup,SOC)
str(ANOdata)
print(ANOdata)

# model
ANOmodel <- aov(SOC~TGroup,data=ANOdata)
print(summary(ANOmodel))
z <- summary(ANOmodel)
str(z)
aggregate(SOC~TGroup,data=ANOdata,FUN=mean)

# plot
ANOPlot <- ggplot(data=ANOdata) +
  aes(x=TGroup,y=SOC,fill=TGroup) +
  geom_boxplot()
print(ANOPlot)


