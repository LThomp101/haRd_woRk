title: "Exploratory Data Analysis"
author: "Laniya Thompson"
date: "September 14, 2020"
output: word_document


install.packages("EnvStats")
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(EnvStats)
data("Refinery.CO.df")
Refinery.CO.df

refinery = Refinery.CO.df[Refinery.CO.df$Source == "refinery",]

#1

install.packages("EnvStats")

ggplot(refinery,aes(CO.ppm))+ geom_histogram(bins= 4,fill = "pink") + labs(x = "carbon monoxide", title = "Histogram of the CO readings ")
#Skewed Right

#2
qqnorm(refinery$CO.ppm, pch = 19)
qqline(refinery$CO.ppm, col = "pink")

# Not normally distributed. 

#Transofrmation of the data.
x = log(Refinery.CO.df$CO.ppm)
qqnorm(x, pch = 19)
qqline(x, col = "pink")


#3
y = log(Refinery.CO.df$CO.ppm)
co_summary = EnvStats::summaryFull(y)
print(co_summary)

#Boxplot
boxplot(y ~ Refinery.CO.df$Source)
abline(h = 3.401:4.349,col = "red")


#4
boxplot(y ~ Refinery.CO.df$Source)
    # 
#5
lamda_1 = EnvStats::boxcoxTransform(metals_in_fish$Hg,lambda = 1)
qqnorm(lamda_1, pch = 19)
qqline(lamda_1, col = "green")

lamda_2 = EnvStats::boxcoxTransform(metals_in_fish$Hg,lambda = 0.75)
qqnorm(lamda_2, pch = 19)
qqline(lamda_2, col = "green")

lamda_3 = EnvStats::boxcoxTransform(metals_in_fish$Hg,lambda = 0.5)
qqnorm(lamda_3, pch = 19)
qqline(lamda_3, col = "green")

lamda_4 = EnvStats::boxcoxTransform(metals_in_fish$Hg,lambda = 0.25)
qqnorm(lamda_4, pch = 19)
qqline(lamda_4, col = "green")

lamda_5 = EnvStats::boxcoxTransform(metals_in_fish$Hg,lambda = 0.0)
qqnorm(lamda_5, pch = 19)
qqline(lamda_5, col = "green")


#6
optimal = EnvStats::boxcox(metals_in_fish$Hg,optimize = T)
print(optimal)


lamda_optimal = EnvStats::boxcoxTransform(metals_in_fish$Hg,lambda = 0.2695396)
qqnorm(lamda_optimal, pch = 19)
qqline(lamda_optimal, col = "green")

lamda_optimal_25 = EnvStats::boxcoxTransform(metals_in_fish$Hg,lambda = 0.25)
qqnorm(lamda_optimal, pch = 19)
qqline(lamda_optimal, col = "green")

#Lambda vs PPCC = appears the same.

