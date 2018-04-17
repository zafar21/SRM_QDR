# R file for the Questionnaire Development Report 
# Submitted as part of a team project for our SRM II course.
# (C) Aniruddha Singh Jafa, Mayukh Nair, Shruthisagar RS, Zafar Ali

# Load necessary libraries
library(readr)
library(psych)

# Load and pre-process data.  
# ATTENTION: Before running this command please ensure that you change the first parameter in the command below to the path where the CSV file is loaded on your computer.

data <- read_csv("C:/Users/Zafar/Google Drive/Ashoka/Semester 4/SRM II/SRM II QDR (Responses).csv", 
                 col_types = cols(Gender = col_factor(levels = c("Male", 
                                                                 "Female", "Other")), Year = col_factor(levels = c("1st year", 
                                                                                                          "2nd year", "3rd year", "4th year"))))
View(data)

str(data)
attach(data)

# Put variables into a data frame for combined processing, if needed
fulldata<- data.frame(v1,v2,v3,v4,v5,v6,v7)

# View data as seen by R
View(fulldata)

# Compute reliability alpha for the entire data 
alpha(fulldata)

# Divide the data into their respective factors
PerfAnxiety<- data.frame(v4,v5,v1)
Rumination<- data.frame(v2,v3,v6)
JudgePVal<- data.frame(v7)

# Compute reliability alphas for each of the factors
alpha(PerfAnxiety)
alpha(Rumination)
alpha(JudgePVal)  #Single Variable

# Compute means for each of the factors 
data$m.PAnx<- rowMeans(PerfAnxiety, na.rm = TRUE)
data$m.Rumin<- rowMeans(Rumination, na.rm = TRUE)
data$m.Judge<- rowMeans(JudgePVal, na.rm = TRUE)

# View dataframe to check if row means have been computed successfully
View(data)

# Reattach the new data
attach(data)

# Create new dataframe to analyse the new variables
anxMeans<- data.frame(CGPA,m.PAnx,m.Rumin,m.Judge)
anxGen<- data.frame(Gender,CGPA,m.PAnx,m.Rumin,m.Judge)

# Generate correlation matrix for each factor with CGPA (generate r coefficient, sample size, p-values) 
corr.test(anxMeans)

# Generate correlation matrix for each factor with CGPA (generate r coefficient, prints lower triangle matrix)
lowerCor(anxMeans)

# Create a dataframe with only Male and Female gender data to run t-tests
# Copy the full dataframe to a new dataframe without the entries with "Other" as a gender
datamf<- data.frame(data[ ! paste(Gender) %in% c("Other"), ]) 


# Runs t-test to compare differences for gender in the factors
t.test(data=datamf, m.PAnx~Gender, paired=FALSE, alternative="two.sided", var.equal=TRUE)
t.test(data=datamf, m.Rumin~Gender, paired=FALSE, alternative="two.sided", var.equal=TRUE)

t.test(data=datamf, m.Judge~Gender, paired=FALSE, alternative="two.sided", var.equal=TRUE)
t.test(data=datamf, CGPA~Gender, paired=FALSE, alternative="two.sided", var.equal=TRUE)


# Generate correlation matrices & alpha for overall anxiety score with CGPA 
anxMeans$overall = rowMeans(subset(anxMeans, select = c(m.PAnx,m.Rumin)), na.rm = TRUE)
attach(anxMeans)
corr.test(anxMeans)
lowerCor(anxMeans)

# Generate scatterplot to compare overall scores with CGPA
plot(anxMeans$overall, anxMeans$CGPA, main="Grade anxiety score distribution", xlab="Overall score on anxiety questionnaire", ylab="Cumulative GPA")

# Generate scatterplot with gender labeling
plot(anxMeans$overall, anxMeans$CGPA, pch=16, col=c("dark blue","green","red")[Gender], main="Grade anxiety score by gender", xlab="Overall score on anxiety questionnaire", ylab="Cumulative GPA")
legend(1.2,3, pch=16, legend=levels(data$Gender), col=c("dark blue", "green", "red"))

# Generate scatterplot with academic year labeling
plot(anxMeans$overall, anxMeans$CGPA, pch=16, col=c("red","green","navy blue","yellow")[Year], main="Grade anxiety score by academic year", xlab="Overall score on anxiety questionnaire", ylab="Cumulative GPA")
legend(1.2,3, pch=16, legend=levels(data$Year), col=c("red","green","navy blue","yellow"))
