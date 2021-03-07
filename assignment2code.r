#installing the required packages
install.packages("arules")
install.packages("arulesViz")
install.packages("knitr")
install.packages("lubridate")
install.packages("plyr")
install.packages("RColorBrewer")

#loading necessary libraries
library(ggplot2)   
library(tidyr)
library(dplyr)
library(tidyverse) 
library(arulesViz)
library(arules)
library(RColorBrewer)

ds2 <- read.csv("exercise_WS2020_data.csv",header=TRUE)   #loading dataset into the variable ds2
ds <- ds2[complete.cases(ds2), ]  #(4556252 obs, 7 variables)

#converting as factor type
ds %>% mutate(article_name = as.factor(article_name))  
ds %>% mutate(article_name = as.factor(article_group))


#converting ids into numeric type
article_id <- as.numeric(as.character(ds$article_id))
basket_id <- as.numeric(as.character(ds$basket_id))

#binding new column ids into the dataframe ds
cbind(ds,article_id)
cbind(ds,basket_id)

glimpse(ds)

shopping_basket <- ddply(ds,c("basket_id","date"),function(df1)paste(df1$article_name,collapse = ","))


#setting column basket_id of dataframe transaction data  
shopping_basket$basket_id <- NULL

#setting column Date of dataframe transaction data
shopping_basket$date <- NULL

#Renaming column to items
colnames(shopping_basket) <- c("items")


#storing the basket format data into a csv file
write.csv(shopping_basket,"D:/Data Mining/assignment 2/tr_data.csv", quote = FALSE, row.names = FALSE)

retail_transactions <- read.transactions('D:/Data Mining/assignment 2/tr_data.csv', format = 'basket', sep=',')

itemFrequencyPlot(retail_transactions,topN=20,type="relative",col=brewer.pal(8,'Accent'),main="Item Frequency Plot")


#Min Support as 0.001, confidence as 0.2.
association.rules <-  apriori(retail_transactions, parameter = list(supp=0.001, conf=0.2,maxlen=100))
#Viewing top 10 rules
inspect(association.rules[1:10])


#removing subset-rules
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1)
length(subset.rules) 

subset.association.rules. <- association.rules[-subset.rules] # remove subset rules.

#identify the customers who bought hooks
hooks.association.rules <- apriori(retail_transactions, parameter = list(supp=0.001, conf=0.1),appearance = list(default="lhs",rhs="hooks"))
inspect(head(hooks.association.rules))

#identify the customers who bought hooks also bought
hooks.association.rules <- apriori(retail_transactions, parameter = list(supp=0.001, conf=0.1),appearance = list(lhs="hooks",default="rhs"))
inspect(head(hooks.association.rules))


# Filtering the rules with confidence greater than 0.3 or 30%
subRules<-association.rules[quality(association.rules)$confidence>0.3]
#Plot Sub-rules
plot(subRules)


#top 10 graph-based rule
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")
#group matrix
plot(association.rules, method="grouped", control = list(k=8))
