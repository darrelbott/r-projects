#Code used for project

#Libraries needed
library(mosaic)
library(lessR)
library(ggplot2)

####Section 2
happy <- read.table (file = "2017.csv", header = TRUE, sep=",")#Reads in dataset from .csv file
head(happy)#displays data to see if read correctly

hist(happy$Happiness.Score, main="Distribution of Happiness", 
     xlab="Happiness",ylab = "Frequency",col = "blue")      #Creates a histogram for variable Happiness

barplot(table(round(happy$Happiness.Score)), main="Distribution of Happiness", ylab="Frequency", xlab="Happiness") #Barplot for variable Happiness

dotPlot(happy$Happiness.Score, main="Distribution of Happiness", ylab="Frequency", xlab="Happiness") #Dotplot for variable Happiness

SummaryStats(Happiness.Score, data=happy) #Summary statistics for variable Happiness

bwplot(Happiness.Score~Region, data = happy, main="Happiness by Region", 
       xlab=c("A&NZ", "C&EE", "EA", "LA&C", "ME&NA", "NA", "SEA", "SA", "SSA", "WE")) #Boxplot for happiness by region

dotplot(Happiness.Score~Region, data = happy, ylab="Happiness", 
        xlab=c("A&NZ", "C&EE", "EA", "LA&C", "ME&NA", "NA", "SEA", "SA", "SSA", "WE"), main="Happiness by Region") #Dotplot for happiness by region

ggplot2.barplot(data=happy, xName="Happiness.Score", groupName="Region")

barplot(happy$Happiness.Score~happy$Region)

#Section 3

t.test(happy$Happiness.Score, conf.level = 0.95)#Confidence interval for mean of Happiness

t.test(Happiness.Score~Hemisphere, alternative = "two.sided", var.equal=FALSE, data=happy)#Confidence interval for difference in mean of Happiness between hemispheres

B <- 10000
BootMean <- numeric(B) # creating emply vector of size B
for(i in 1:B){
  bootsample <- sample(happy$Happiness.Score, length(happy), replace = TRUE)
  BootMean[i] <- mean(bootsample) 
}
hist(BootMean) #Bootstrap inference on single mean, StatKey used for difference

#Section 4

plot(happy$Freedom, happy$Trust,
     xlab = "Freedom",
     ylab = "Trust")                    #Scatterplot for Trust and Freedom
abline(lm(happy$Trust ~ happy$Freedom)) #fitted line for the data

cor.test(happy$Trust,happy$Freedom)#correlation test

SummaryStats(Happiness.Score, by = Region, data = happy)#Summary stats for Happiness grouped by region
fit1 <- aov(Happiness.Score~Region, data = happy)#ANOVA for Happiness by region
summary(fit1)#Display ANOVA table
TukeyHSD(fit1)#post-hoc pairwise comparison

fit <- lm(Health..Life.Expectancy.~Economy..GDP.per.Capita., data=happy)
summary(fit) #summary stats for linear regression model
plot(fit) #residual plots for linear regression model
anova(fit) #ANOVA for linear regression model
