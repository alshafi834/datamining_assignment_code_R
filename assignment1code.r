#installing and loading the required packages and libraries
install.packages("tidyverse")
library(tidyverse) 
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)  
install.packages("dplyr")
library(dplyr)
library(stringr)
install.packages("tibble")
library(tibble)
install.packages("data.table")
library(data.table)
library(plyr)


dataset <- read.csv("Assignment1Data.csv",header=TRUE)   #loading dataset into a variable

rc <- str_detect(dataset$region, "Kreis$", negate = FALSE) #to find the region with kreis

#dataset 1
dataset1 <- split(dataset, rc)[['TRUE']] #separating dataset with the region Kreis
dim(dataset1)
write.csv(dataset1,"dataset1.csv")  #exporting the dataset into csv file

#dataset 2
dataset2 <- split(dataset, rc)[['FALSE']] #separating dataset without the Kreis regions
dim(dataset2)
write.csv(dataset2, "dataset2.csv")  #exporting the dataset into csv file


#visualization with ggplot

ggplot(dataset2, aes(x=age, y=employed_male, label=employed_male)) + 
  geom_point(stat='identity', aes(col=region), size=9)+ 
  geom_text(color="black", size=3) +
  labs(title="employment rate", 
       subtitle="region wise employed male separated by ages") + 
  ylim(500,4500) +
  coord_flip()

ggplot(dataset2, aes(x=age, y=inactive_male)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Inactive Population") +ylim(500,4500)+ 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(dataset2, aes(x=age, y=employed_male)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Employment rate") +ylim(500,4500)+ 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
