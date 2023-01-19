library(dplyr)#used for data manipulation
library(readxl)#used to read excel
library(qcc)

#Read Data
Gold = read_excel("C:/Users/HP/Documents/monthlygold.xlsx")                                                                            
dfgold = data.frame(Gold)
dfgold

#Knowing Data
group_by(dfgold,dfgold$date)%>%
  summarise(
    count = n(),
    mean = mean(dfgold$value, na.rm = TRUE),
    IQR = IQR(dfgold$value, na.rm = TRUE))

#factor
Gold$group <- as.factor(Gold$group)

#ANOVA TEST
#oneway
one.way.gold = aov(Gold$value ~ Gold$group,data = Gold)
summary(one.way.gold)

#twoway
two.way.gold = aov(Gold$value+high ~ Gold$group,data = Gold)
summary(two.way.gold)

#Check for homoscedasticity
par(mfrow=c(2,2))
plot(two.way.gold)
par(mfrow=c(1,1))

#tukey honest significant differences
TukeyHSD(one.way.gold)
plot(TukeyHSD(one.way.gold))

#boxplot
boxplot(Gold$value ~ Gold$group,data = Gold, main="Gold Rates of 15 Years ", xlab="Years",ylab = "Rates",  col=rainbow(7))

#NON-PARAMETRIC TESTS
#kruskal
kruskal.test(high ~group , data = dfgold)
#chi-squared
chivalue <- chisq.test(dfgold$value)
chivalue
#wilcox
wilcox <- wilcox.test(dfgold$value)
wilcox

#CONTROL CHARTS
date <- c(Gold$date)
value <- c(Gold$value)
#p chart
qcc(dfgold$value, type = 'p',size=10000)
#c chart
qcc(dfgold$value,type = "c")


#Time Series

Gold = read_excel("C:/Users/HP/Documents/monthlytime.xlsx")    
goldrate <- ts(Gold, frequency=12, start=c(2006,1))
goldrate
plot.ts(goldrate)
goldrateseriescomponents <- decompose(goldrate)
plot(goldrateseriescomponents)

#statistical concepts
#Mean
dfgold %>% summarise_if(is.numeric, mean)
#Median
dfgold %>% summarise_if(is.numeric, median)
#Mode
dfgold %>% summarise_if(is.numeric, mode)
#Min
dfgold %>% summarise_if(is.numeric, min)
#Max
dfgold %>% summarise_if(is.numeric, max)
#Sum
dfgold %>% summarise_if(is.numeric, sum)
#sd
dfgold %>% summarise_if(is.numeric, sd)
#var
dfgold %>% summarise_if(is.numeric, var)