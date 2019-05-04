happiness <- read.table (file = "2017.csv", header = TRUE, sep=",")

##################################### Categorical
##################################### Frequency Table
count<-table(happiness$Region)
percent<-count/sum(count)*100
cbind(count, percent)

##################################### Bar Diagram
library(ggplot2)
lbls <- c("ANZ", "CEE ", "EA", "LAC", "MENA", "NA", "SEA", "SA", "SSA", "WE")

barplot(percent,ylim = c(0,30),names.arg = lbls, ylab = "Number of Countries", xlab = "Region",
        main = "Percentage of Countries per region", col='lightblue')

lbls <- c("Australia and New Zealand", "Central and Eastern Europe ","Southeastern Asia", "Eastern Asia", "Latin America and Caribbean", "Middle East and Northern Africa", "North America", "Southern Asia", "Sub-Saharan Africa", "Western Europe")
lbls <- paste(lbls, round(percent, digits=2), "%") # add percents to labels 
#lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(percent,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of regions")

library(lattice)
histogrm(happiness$Region, happiness$Happiness.Score)
dotplot(happiness$Trust..Government.Corruption.)

#Confidence interval for single mean
mean(happiness$Happiness.Score)
sd(happiness$Happiness.Score)

t.test(happiness$Happiness.Score,  conf.level = 0.95)

# ************************************************** #
#         Bootstrapping for Single Mean              #
# ************************************************** #
data <- rnorm(155, mean = 5.354, sd = 1.131) # random number generation
B <- 10000 # number of bootstrap sample
BootMean <- numeric(B) # creating emply vector of size B
for(i in 1:B){
  bootsample <- sample(data, length(data), replace = TRUE)
  BootMean[i] <- mean(bootsample)
}
hist(BootMean, col = 'purple') 
require(mosaic)
dotPlot(BootMean,n=155,col = 'lightblue')
# To obtain 95% CI for the mean, find quantiles 
quantile(BootMean, c(.025, .975))  # percentiles bootstarp confidence interval

#rm(list = ls())
############################################ Bootstarp Confidence interval using "boot" pacakage
library(boot)
summary(happiness$Happiness.score)
bootmean<-function(x,i){
  return(mean(x[i]))
}
bootmean<-boot(happiness$Happiness.Score, bootmean,10000)
boot.ci(bootmean)
hist(BootMean)

# ************************************************** #
#  Bootstrapping for Two Means/ Difference of Proportions  #
# ************************************************** #

n1<-200
s1 <- 20
n2<-300
s2 <- 35
sample1 <- c(rep(1, s1), rep(0,(n1-s1)))
sample2 <- c(rep(1, s2), rep(0,(n2-s2)))

B <- 10000 
BootPropDiff <- numeric(B)
for(i in 1:B){
  bootsample1 <- sample(happiness$Happiness.Score, length(happiness$Happiness.Score), replace = TRUE)
  bootsample2 <- sample(happiness$Hemisphere, length(happiness$Hemisphere), replace = TRUE)
  prop1 <- sum(bootsample1)/length(bootsample1)
  prop2 <- sum(bootsample2)/length(bootsample2)
  BootPropDiff[i] <- prop1 - prop2
} 

hist(BootPropDiff)
dotPlot(BootPropDiff, n = 200)
quantile(BootPropDiff, c(.025, .975))

# Part4 Scatterplot
library(lessR)

plot(happiness$Economy..GDP.per.Capita., happiness$Health..Life.Expectancy.,
     xlab = "Economy",
     ylab = "Life Expectancy", 
     main = "Relationship Between Economy and Life Expectancy")
abline(lm(happiness$Health..Life.Expectancy. ~ happiness$Economy..GDP.per.Capita.))

#Summary Stats
summary(happiness$Hemisphere)
summary(happiness$Economy..GDP.per.Capita.)
summary(happiness$Health..Life.Expectancy.)
SummaryStats(Economy..GDP.per.Capita., data = happiness)
SummaryStats(Health..Life.Expectancy., data = happiness)

#Correlation
plot(happiness$Economy..GDP.per.Capita., happiness$Health..Life.Expectancy.,
     xlab = "Economy",
     ylab = "Life Expectancy", 
     main = "Relationship Between Economy and Life Expectancy")
abline(lm(happiness$Health..Life.Expectancy. ~ happiness$Economy..GDP.per.Capita.))

cor(happiness$Economy..GDP.per.Capita., happiness$Health..Life.Expectancy.)
cor.test(happiness$Economy..GDP.per.Capita., happiness$Health..Life.Expectancy.)

fit <- lm(happiness$Health..Life.Expectancy. ~ happiness$Economy..GDP.per.Capita.)

summary(fit)
plot(fit)
anova(fit) # anova table
TukeyHSD(happiness)
mean(happiness$Happiness.Score)
SummaryStats(Happiness.Score, data = happiness)

count<-table(happiness$Trust..Government.Corruption.)
barplot(count,ylim = c(0,30), ylab = "Number of countries", xlab = "Trust Score",
        main = "Government Trust", col='lightblue')

plot(happiness$Happiness.Rank, happiness$Trust..Government.Corruption.,
     xlab = "Happiness Score",
     ylab = "Trust", 
     main = "Relationship Between Happiness Score and Trust")
abline(lm(happiness$Trust..Government.Corruption. ~ happiness$Happiness.Score))

cor(happiness$Happiness.Score, happiness$Trust..Government.Corruption.)
cor.test(happiness$Happiness.Score, happiness$Trust..Government.Corruption.)

fit <- lm(happiness$Health..Life.Expectancy. ~ happiness$Economy..GDP.per.Capita.)

summary(fit)
plot(fit)
anova(fit) # anova table
TukeyHSD(happiness)
mean(happiness$Happiness.Score)
SummaryStats(Happiness.Score, data = happiness)
