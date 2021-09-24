# Assignment: ASSIGNMENT 1
# Name: Mahaldar, Arti
# Date: 2021-09-21

scores <- read.csv('dsc520/data/scores.csv')
library(dplyr)
(scores)


# Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?

Count -> Quantitative
Section -> categorical
Score -> Quantitative

# Create one variable to hold a subset of your data set that contains only the Regular Section and one variable for the Sports Section.

Sports_Data <- subset(scores, Section=="Sports",
                  select=Count:Section)
Sports_Data

Regular_Data <- subset(scores, Section=="Regular",
                  select=Count:Section)

Regular_Data

table(Sports_Data$Section)
str(Sports_Data)
plot(Sports_Data$Count,Sports_Data$Score)

str(Regular_Data)
mean(Regular_Data$Score)
mean(Sports_Data$Score)
           
plot(Regular_Data$Count,Regular_Data$Score)


#Comparing and contrasting the point distributions between the two section, looking at both tendency and consistency: Can you say that one section tended to score more points than the other? Justify and explain your answer.

#Answer : Sports has more points than Regular the density at of the points at the higher scores is more in Sports graph.
#It feels like bimodal, peaks for the sections are different.

#Did every student in one section score more points than every student in the other section? If not, explain what a statistical tendency means in this context.

#yes looking the mean to see the central tendency, it looks likes that. 

#What could be one additional variable that was not mentioned in the narrative that could be influencing the point distributions between the two sections?  
#Prior knowledge could be one more factor of consideration.