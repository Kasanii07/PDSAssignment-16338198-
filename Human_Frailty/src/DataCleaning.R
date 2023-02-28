yield_data <- read.csv("raw_yield_data.csv")
clean_yield_data<-na.omit(yield_data[yield_data$Height..Inches. != "N", ])
write.csv(clean_yield_data,"C:/Users/nagas/OneDrive/Documents/Rishika/Rishika/Human_Frailty/data_clean/clean_yield_data.csv")

#Loading  clean Data

clean_yield_data<-read.csv2("C:/Users/nagas/OneDrive/Documents/Rishika/Rishika/Human_Frailty/data_clean/clean_yield_data.csv")
clean_yield_data


typeof(clean_yield_data$Height..Inches.)

Summ<-summary(clean_yield_data)
capture.output(Summ,file="C:/Users/DELL/Documents/Rishika/Human_Frailty/results/summary.txt")

##ploting between Age and Grip Strength
plot(clean_yield_data$Age, clean_yield_data$Grip.Strength,xlab="Age",ylab="GripStrength",main="Age vs GripStrength", col="red")


