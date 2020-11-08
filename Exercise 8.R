library(tidyverse)
library(here)
library(datasets)
here() #use here to set wd 

#1 load data
data("USArrests")

view(USArrests)

#name dataset as data 
data <- USArrests
#assign x and y variables 
x <- data$Murder
y <- data$Assault

#2
#Base R
plot(x, y, main = "Relationship between Murder and Assault Arrests", 
     xlab = "Murder Arrests",
     ylab = "Assault Arrests")
abline(lm(y ~ x, data = data), col="red")

#GGplot
ggplot(data = data, aes(x = Murder, y = Assault)) + geom_point() + geom_smooth(method = "lm")

#3

#R
boxplot(data$Rape)


#GG
ggplot(data = data, aes(y = Rape)) + geom_boxplot()


#4
#needed to add new column of state names since the other was not a variable in 
#the dataframe
states <- c(state.name) #use state.name data
data$states <- states #apply new column 

#R
barplot(data$Rape, main="Rape Arrests by State",
        xlab="State",
        ylab="Rape Arrests",
        names.arg = c(states), #apply names to x label bars
        cex.names = .5, #change font size
        las=2) #las changes orientation of x-labels

#GG
ggplot(data, aes(x=factor(states), y=Rape, label=Rape)) + 
  geom_bar(stat='identity', width=.5) +
  labs(title="Rape Arrests by State", y="Rape Arrests", x="State") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#5
#R
hist(data$UrbanPop, main = "Urban Population Crime Data", 
     xlab = "Urban Population (%)")

#GG

ggplot(data, aes(x=UrbanPop)) + 
  geom_histogram(binwidth = 2,
  color="black", fill="white")




