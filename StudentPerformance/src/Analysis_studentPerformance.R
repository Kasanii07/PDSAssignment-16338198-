library(tidyverse) 
library(dplyr)
yield_data <- read.csv("C:/Users/nagas/OneDrive/Documents/Rishika/Rishika/StudentPerformance/data_raw/StudentsPerformance.csv")
clean_yield_data<-na.omit(yield_data[yield_data != "N", ])
write.csv(clean_yield_data,"C:/Users/nagas/OneDrive/Documents/Rishika/Rishika/StudentPerformance/data_clean/clean_yield_data.csv")

#Loading  clean Data

clean_yield_data<-read.csv2("C:/Users/nagas/OneDrive/Documents/Rishika/Rishika/StudentPerformance/data_clean/clean_yield_data.csv")
clean_yield_data


Summ<-summary(clean_yield_data)
capture.output(Summ,file="C:/Users/nagas/OneDrive/Documents/Rishika/Rishika/StudentPerformance/results/summary.txt")



data <- read.csv("C:/Users/nagas/OneDrive/Documents/Rishika/Rishika/StudentPerformance/data_clean/clean_yield_data.csv") 



#count the groups and start to analyze this. Group numbers may differ and could manuplate our results.
#That's why, the avarages could help us to analyze better.

data %>%
  group_by(race.ethnicity) %>%
  summarise(mathMean = mean(math.score),readingMean =  mean(reading.score),writingMean = mean(writing.score)) %>%
  ggplot(data = ., aes(x = race.ethnicity, y = mathMean, 
                       fill = race.ethnicity)) + geom_bar(stat = "identity")

Count_Race.Ethnicity <- data %>% 
  group_by(race.ethnicity) %>%
  filter(math.score > 80) 
table(Count_Race.Ethnicity$race.ethnicity)
Count_Race.Ethnicity %>% ggplot() + geom_bar(aes(Count_Race.Ethnicity$race.ethnicity,fill = Count_Race.Ethnicity$race.ethnicity))



#Analysis Based on Gender
totalGender <- table(data$gender)
plot(table(data$gender))
totalGender


# Generating MathScore Based on Gender
data %>% 
  group_by(gender) %>%
  select(math.score,reading.score,writing.score) %>%
  filter(math.score > 80, reading.score > 80, writing.score > 80) %>%
  ggplot(data = ., aes(x = gender, y = math.score, 
                       fill = gender)) + geom_bar(stat = "identity")


#reading Score Based on Gender
data %>% 
  group_by(gender) %>%
  select(math.score,reading.score,writing.score) %>%
  filter(math.score > 80, reading.score > 80, writing.score > 80) %>%
  ggplot(data = ., aes(x = gender, y = reading.score, 
                       fill = gender)) + geom_bar(stat = "identity")

#writing Score based on Gender
data %>% 
  group_by(gender) %>%
  select(math.score,reading.score,writing.score) %>%
  filter(math.score > 80, reading.score > 80, writing.score > 80) %>%
  ggplot(data = ., aes(x = gender, y = writing.score, 
                       fill = gender)) + geom_bar(stat = "identity")


#Education Difference in parents
totalLevel <- table(data$parental.level.of.education)
plot(table(data$parental.level.of.education))
totalLevel


#student Math Score comparision for parent level of education

level1 <- data %>% 
  group_by(parental.level.of.education) %>%
  summarise(mathTotal = sum(math.score))

level1 %>% ggplot(data = ., aes(x = parental.level.of.education, y = mathTotal, 
                                fill = parental.level.of.education)) + geom_bar(stat = "identity") + 
  labs(title="Education Level Chart") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))




#PieChart for Reading and Parent Education Level

GroupingBasedONStudy <- data %>% 
  group_by(parental.level.of.education) %>%
  summarise(readingTotal =  sum(reading.score))

GroupingBasedONStudy %>%
  ggplot(data = ., aes(x = parental.level.of.education, y = readingTotal, 
                       fill = parental.level.of.education)) + geom_bar(stat = "identity") + 
  labs(title="Education Level Chart",
       subtitle="Reading Scores") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


reading_parentEL = data.frame(GroupingBasedONStudy$readingTotal / totalLevel)
lbls <- reading_parentEL$Var1
pct <- round(reading_parentEL$Freq/sum(reading_parentEL$Freq)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(reading_parentEL$Freq,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Reading Education Level")

#math Vs Reading Score Graph
plot(data$reading.score, data$math.score,xlab="Read Score",ylab="Math Score",main="Math vs Read", col="red")


