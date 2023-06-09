library(zoo)
install.packages("ggplot2")
library(ggplot2)
#set the work directory
setwd("G:\\FCI\\level4\\semester2\\Genomic Analytics\\project\\labs")

# read the data file
data <- read.csv("G2_anthropometry.csv")

# print the data dimension 
dim(data)   

# print number of rows and columns 
print(nrow(data))
print(ncol(data))

# print data columns name
print(colnames(data))

# print data sturcture of data
print(str(data))

#print data summary
print(summary(data))

# round age column to delete the dicimal number and get clear age by year
data$age <- round(data$age)

# convert cm word to M to represent the male
data$gender[data$gender == "cm"] = 'M'

# print all rows that contian missing data
data[ ! complete.cases(data), ]


# fill missing data by last observation
data$foot_length <- na.locf(data$foot_length)

# removing cm text from height column and convert it to numeric
data$height <- gsub(" cm","", data$height)

data$height <- as.numeric(data$height)

# NOW DATA BECAME CLEANED AND READY FOR VISUALIZATION


#Q1 : what is the popular age?

ggplot(data, aes(x = age)) +
  geom_bar( fill = "black", color = "white") +
  ggtitle("Childern Ages") +
  xlab("Age") +
  ylab("Frequency")

#Q2 : what is the populart gender?

ggplot(data, aes(x = gender)) +
  geom_bar( fill = "black", color = "white") +
  ggtitle("Childern genders") +
  xlab("Gender") +
  ylab("Frequency")

#Q3 : what is the relation between age and foot length


ggplot(data, aes(x = age, y = foot_length)) +
  geom_point() +
  ggtitle("Scatter plot of x and y") +
  xlab("x") +
  ylab("y")



#Q 4: what is the highest gender 
# Calculate the mean height for each gender
mean_height <- aggregate(height ~ gender, data = data, mean)

# Create a bar chart of mean height by gender
ggplot(mean_height, aes(x = gender, y = height)) +
  geom_bar(stat = "identity", fill = "black") +
  ggtitle("Mean Height by Gender") +
  xlab("Gender") +
  ylab("Mean Height (cm)")


#Q 4: what is the gender that has the more length foot
# Calculate the mean foot length for each gender
mean_height <- aggregate(foot_length ~ gender, data = data, mean)

# Create a bar chart of mean foot length by gender
ggplot(mean_height, aes(x = gender, y = foot_length)) +
  geom_bar(stat = "identity", fill = "black") +
  ggtitle("Mean foot length by Gender") +
  xlab("Gender") +
  ylab("Mean foot length")


#Q5 : what is the relation of height and foot length


ggplot(data, aes(x= height, y = foot_length))+
  geom_point()+
  ggtitle("Relation Between height and foot length")+
  xlab("Height")+
  ylab("Foot length")



