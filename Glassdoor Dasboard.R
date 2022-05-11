#####################################################################
#####################################################################
######### Data Visualisation 
######### Computer Workshop: Employee Diversity and gender pay-gap
######### People Analytics: Strategy and Practice 
#####################################################################
#####################################################################

#####################
# Install the package
#####################

# this installs the ggplot2 packages (dependencies = TRUE is optional)
# don't have to run this code is ggplot2 is already installed.

install.packages("ggplot2") 

# to load the package

library("ggplot2")

######################
# Load the sample data 
######################

# get the list of files in the current/home directory
getwd() 

# Change the directory to where you have stored the data.  
setwd("C:/Users/User/OneDrive - University of Leeds/Documents/gender_pay_gap.csv")

# header = TRUE means to keep the header as it is. 
# as.is = TRUE means to read the data as it is, that is, in its original form. 
# stringAsFactors = FALSE means that we don#t want string related data such as Department or Education to be factors
# check.names = FALSE means that we don't want the period symbol,'.', appearing in the variable names. 
# You can give any name to the data. I am going to call it 'df'.

df <- read.csv("gender_pay_gap.csv", header = TRUE, as.is = TRUE,  check.names = FALSE, stringsAsFactors = FALSE)

##################
# Explore the data 
##################

# See the structure of the data
str(df)

# See the names of the variables
colnames(df)

# see the top 5 observations of the data
head(df)

# See the top 10 observations of the data
head(df, 10)

# Basic summary statisitcs
summary(df$basePay)

########################
# Creating New Variables
########################

# In R/RStudio, we use the dollar function, $, to select variables from the data. 
# If we want to select variable x from data, df, we would use the following: df$x

# Create the variable total salary 
df$totalSalary <- df$basePay + df$bonus

# Create the log of the variable, total salary
df$logSalary <- log(df$totalSalary)

###############
# Scatter plots
###############

# This is a basic scatter plot
ggplot(data = df, mapping = aes(x = age, y = totalSalary)) + 
  geom_point()

# If I want to change the theme
ggplot(data = df, mapping = aes(x = age, y = totalSalary)) +
  geom_point() + 
  theme_classic()

# More theme options available 
# from: https://ggplot2-book.org/polishing.html

# Change the colour to blue 
ggplot(data = df, mapping = aes(x = age, y = totalSalary)) + 
  geom_point(color = "blue") 

# Change the colour and theme
ggplot(data = df, mapping = aes(x = age, y = totalSalary)) + 
  geom_point(color = "blue") +
  theme_classic()

# Change the names in the axes and title
ggplot(data = df, mapping = aes(x = age, y = totalSalary)) + 
  geom_point(color = "blue") +
  labs(x = "Age", y = "Total Salary", title = "Scatter plot of salary against gender") +
  theme_classic()

# Add Subtitle
ggplot(data = df, mapping = aes(x = age, y = totalSalary)) + 
  geom_point(color = "blue") +
  labs(x = "Age", y = "Total Salary", title = "Scatter plot of salary against gender",
       subtitle = "Glassdoor gender pay-gap data") +
  theme_classic()

# Suppose I want to plot the relationship by gender
ggplot(data = df, mapping = aes(x = age, y = totalSalary, color = gender)) +
  geom_point()

# Combining all together
ggplot(data = df, mapping = aes(x = age, y = totalSalary, color = gender)) + 
  geom_point(color = "blue") +
  labs(x = "Age", y = "Total Salary", title = "Scatter plot of salary against gender") +
  theme_classic()

##... Adding Multiple Plots (geometric objects)

# Adding multiple geoms or geom objects 
# You can use the geom_smooth layer to look for patterns in your data.
# Aids the eye in seeing patterns in the presence of over-plotting

ggplot(data = df, mapping = aes(x = age, y = totalSalary)) + 
  geom_point() + 
  geom_smooth()

# Change the colours
ggplot(data = df, mapping = aes(x = age, y = totalSalary)) + 
  geom_point(color = "red") + 
  geom_smooth(color = "blue")

# color aesthetic passed to each geom layer
ggplot(data = df, mapping = aes(x = age, y = totalSalary, color = gender)) + 
  geom_point() + 
  geom_smooth()

# The line of best fit. 
ggplot(data = df, mapping = aes(x = age, y = totalSalary, color = gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# color aesthetic specified for only the geom_point layer
ggplot(data = df, mapping = aes(x = age, y = totalSalary)) + 
  geom_point(aes(color = gender)) + 
  geom_smooth(method = "lm", se = FALSE)

# Example
ggplot(data = df, mapping = aes(x = age, y = totalSalary, color = gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(x = "Age", y = "Total Salary ($/per year)", color = "Gender") # changing the name of the legend

##################
# Bar plots/graphs 
##################

unique(df$dept)

ggplot(data = df, mapping = aes(x = dept)) + 
  geom_bar()

ggplot(data = df, mapping = aes(x = dept)) + 
  geom_bar(fill = "blue")

ggplot(data = df, mapping = aes(x = dept)) + 
  geom_bar(fill = "blue", color = "green")

ggplot(data = df, mapping = aes(x = dept, fill = dept)) + 
  geom_bar()

ggplot(data = df, mapping = aes(x = dept, fill = dept)) + 
  geom_bar() + 
  coord_flip()

ggplot(data = df, mapping = aes(x = dept, fill = dept)) + 
  geom_bar() + 
  theme_minimal()

ggplot(data = df, mapping = aes(x = dept, fill = dept)) + 
  geom_bar() + 
  theme_minimal() +
  labs(x = "Department", y =  "Count", fill = "Depratment")

########################################
# Statistical Transformation & Bar plots 
########################################

ggplot(data = df, mapping = aes(x = gender)) + 
  geom_bar()

ggplot(data = df, mapping = aes(x = gender)) + 
  geom_bar(width = 0.7)

ggplot(data = df, mapping = aes(x = gender)) + 
  geom_bar(width = 0.7, fill = "steelblue") +
  theme_minimal() +
  labs(x = "Gender", y = "")

ggplot(data = df, mapping = aes(x = gender, y = totalSalary)) + 
  geom_bar(stat = "identity", width = 0.7)

ggplot(data = df, mapping = aes(x = gender, y = totalSalary)) + 
  geom_bar(stat = "summary", fun = "mean", width = 0.7)

ggplot(data = df, mapping = aes(x = gender, y = totalSalary)) + 
  geom_bar(stat = "summary", fun = "mean", width = 0.7, fill = "steelblue") +
  labs(x = "Gender", y = "Mean Total Salary") +
  theme_minimal()

ggplot(data = df, mapping = aes(x = gender, y = totalSalary, fill = gender)) + 
  geom_bar(stat = "summary", fun = "mean", width = 0.7) +
  labs(x = "Gender", y = "Mean Total Salary") +
  theme_minimal()

####################################
# Bar plots with Multiple Categories 
####################################

# This is known as stacked barplot 
ggplot(data = df, mapping = aes(x = dept, fill = gender)) + 
  geom_bar() 

ggplot(data = df, mapping = aes(x = dept, fill = gender)) + 
  geom_bar() +
  theme_minimal() +
  labs(x = "Department", y = "Count", fill = "Gender")

# position = "dodge": values next to each other
ggplot(data = df, mapping = aes(x = dept, fill = gender)) + 
  geom_bar(position = "dodge")

ggplot(data = df, mapping = aes(x = dept, fill = gender)) + 
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Department", y = "Count", fill = "Gender")

# position = "fill": percentage chart
ggplot(data = df, mapping = aes(x = dept, fill = gender)) + 
  geom_bar(position = "fill")

ggplot(data = df, mapping = aes(x = dept, fill = gender)) + 
  geom_bar(position = "fill") +
  theme_minimal() + 
  labs(x = "Department", y = "Proportion", fill = "Gender")

# Percentage bar plot with dodge
ggplot(data = df, mapping = aes(x = dept, y = gender, fill = gender)) + 
  geom_bar(aes(y = ..count../sum(..count..) * 100), width = 0.7, position = "dodge")

ggplot(data = df, mapping = aes(x = dept, y = gender, fill = gender)) + 
  geom_bar(aes(y = ..count../sum(..count..) * 100), width = 0.7, position = "dodge") + 
  theme_minimal() +
  labs(x = "Department", y = "Percentage", fill = "Gender")

############
# Histograms 
############
  
ggplot(data = df, mapping = aes(x = totalSalary)) +
  geom_histogram()

ggplot(data = df, mapping = aes(x = totalSalary)) +
  geom_histogram(bins = 25)

ggplot(data = df, mapping = aes(x = totalSalary)) +
  geom_histogram(bins = 25, fill = "lightblue", color = "green") +
  theme_minimal() +
  labs(x = "Total Salary ($/per)", y = "")

###################
# Box-Whisker Plots 
###################

ggplot(data = df, mapping = aes(x = dept, y = totalSalary)) +
  geom_boxplot()

ggplot(data = df, mapping = aes(x = dept, y = totalSalary, fill = dept)) +
  geom_boxplot()

ggplot(data = df, mapping = aes(x = dept, y = totalSalary, fill = dept)) +
  geom_boxplot() + 
  theme_minimal() + 
  labs(x = "Department", y = "Total Salary", fill = "Department")

ggplot(data = df, mapping = aes(x = dept, y = totalSalary, fill = gender)) +
  geom_boxplot() + 
  theme_minimal() + 
  labs(x = "Department", y = "Total Salary", fill = "Gender")

##########
# Faceting 
##########

# Facets are ways of grouping a data plot into multiple 
# different pieces (subplots).

ggplot(data = df, mapping = aes(x = gender, y = totalSalary, fill = gender)) + 
  geom_bar(stat = "summary", fun = "mean", width = 0.7) +
  labs(x = "Gender", y = "Mean Total Salary", fill = "Gender") +
  theme_minimal()

ggplot(data = df, mapping = aes(x = gender, y = totalSalary, fill = gender)) + 
  geom_bar(stat = "summary", fun = "mean", width = 0.7) +
  labs(x = "Gender", y = "Mean Total Salary", fill = "Gender") +
  theme_minimal() +
  facet_grid(~ edu) 

##########
# Dasboard 
##########

# install.packages("gridExtra")
library("gridExtra")

p1 <- ggplot(data = df, mapping = aes(x = totalSalary)) +
  geom_histogram(bins = 25, fill = "lightblue", color = "green") +
  theme_minimal() +
  labs(x = "Total Salary ($/per)", y = "")

p2 <- ggplot(data = df, mapping = aes(x = age, y = totalSalary, color = gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(x = "Age", y = "Total Salary ($/per year)", color = "Gender") # changing the name of the legend

p3 <- ggplot(data = df, mapping = aes(x = dept, y = gender, fill = gender)) + 
  geom_bar(aes(y = ..count../sum(..count..) * 100), width = 0.7, position = "dodge") + 
  theme_minimal() +
  labs(x = "Department", y = "Percentage", fill = "Gender")


p4 <- ggplot(data = df, mapping = aes(x = gender, y = totalSalary, fill = gender)) + 
  geom_bar(stat = "summary", fun = "mean", width = 0.7) +
  labs(x = "Gender", y = "Mean Total Salary", fill = "Gender") +
  theme_minimal() +
  facet_grid(~ edu) 

grid.arrange(p3, p1, p2, p4)
  















