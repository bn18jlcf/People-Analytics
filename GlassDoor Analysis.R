########################################
########################################
##... Data Visualisation
##... Diversity
##... Gender-Pay Gap
########################################
########################################


#######################
##... Load the packages
#######################

library("dplyr")
library("ggplot2")
library("ggmosaic")
library("scales")
library("ggpubr")

###################
##... Read the data
###################

# get the list of files in the current/home directory
getwd() 

# Change the directory to where you have stored the data.  
setwd("C:/Users/User/Documents/University of Leeds Teaching/People Analytics_Strategy and Practice")

# You can view the list of all the files and folders in a directory 
list.files("")

df <- read.csv("gender_pay_gap.csv", header = TRUE, as.is = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

######################
##... Create Variables
######################

df$totalSalary <- df$basePay + df$bonus
df$logTotalSalary <- log(df$totalSalary) 

################################
##... Histogram of Pay Variables
################################

# Base Salary

ggplot(data = df, mapping = aes(x = basePay)) + 
  geom_histogram(fill = "lightblue", color = "white", bins = 25) + 
  labs(x = "Base Salary ($)", y = "Count") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12, color = "red"),
        axis.text = element_text(size = 10))

# Bonus 

ggplot(data = df, mapping = aes(x = bonus)) + 
  geom_histogram(fill = "lightblue", color = "white", bins = 20) + 
  labs(x = "Bonus ($)", y = "Count") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12, color = "red"),
        axis.text = element_text(size = 10))

# Total Salary 

ggplot(data = df, mapping = aes(x = totalSalary)) + 
  geom_histogram(fill = "lightblue", color = "white", bins = 25) + 
  labs(x = "Total Salary ($)", y = "Count") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12, color = "red"),
        axis.text = element_text(size = 10))

# Log of Total Salary 

ggplot(data = df, mapping = aes(x = logTotalSalary)) + 
  geom_histogram(fill = "lightblue", color = "white", bins = 25) + 
  labs(x = "Log of Total Salary ($)", y = "Count") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12, color = "red"),
        axis.text = element_text(size = 10))

########################
##... Overlaid Histogram
########################

ggplot(data = df, mapping = aes(x = totalSalary, fill = gender)) +
  geom_histogram(alpha = 0.6, bins = 20) +
  theme_minimal() +
  labs(x = "Total Salary ($)", y = "Count", fill = "Gender") +
  theme(axis.title = element_text(size = 12, color = "red"),
        axis.text = element_text(size = 10)) +
  annotate("text", x = 100000, y = 20, label = "Female: Mean = 96417, SD = 24202", col = "red") +
  annotate("text", x = 100000, y = 75, label = "Men: Mean = 104919, SD = 25330", col = "red")

df %>%
  group_by(gender) %>%
  summarise(mean_salary = mean(totalSalary),
            sd_salary = sd(totalSalary))

###########################
##... Mean Salary by Gender
###########################

ggplot(data = df, mapping = aes(x = gender, y = totalSalary)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.6, fill = "steelblue", color = "black") +
  theme_bw() +
  labs(x = "Gender", y = "Mean Total Salary") +
  theme(axis.title = element_text(size = 14, color = "blue"),
        axis.text = element_text(size = 12))

###########################################################
##... Gender by Department, Education, Title, and Seniority
###########################################################

# ggplot(data = df, mapping = aes(x = dept, y = gender, fill = gender)) + 
#   geom_bar(aes(y = ..count../sum(..count..) * 100), width = 0.7, position = "dodge") +
#   labs(x = "Department", y = "% of Employees", fill = "Gender") +
#   theme_minimal() +
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
#             stat = "count", vjust = -0.25, color = "white", position = position_dodge(width = .7)) +
#   theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#         legend.position = "top")

##... Number of Male and Female Employees at each Department

ggplot(data = df, mapping = aes(x = dept, y = gender, fill = gender)) + 
  geom_bar(aes(y = ..count../sum(..count..) * 100), width = 0.7, position = "dodge") +
  labs(x = "Department", y = "% of Employees", fill = "Gender") +
  theme_classic() +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count../sum(..count..) * 100), 
            stat = "count", vjust = -15, color = "white", position = position_dodge(width = 0.7)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "top")

##... Education by Gender

df$edu <- factor(df$edu, levels = c("High School", "College", "Masters", "PhD"))

p1 <- ggplot(data = df, mapping = aes(x = edu, y = gender, fill = gender)) + 
  geom_bar(aes(y = ..count../sum(..count..) * 100), width = 0.7, position = "dodge") +
  labs(x = "Education", y = "% of Employees", fill = "Gender") +
  theme_classic() +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count../sum(..count..) * 100), 
            stat = "count", vjust = -15, color = "white", position = position_dodge(width = 0.7)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "top")
p1

##... Job Titles by Gender

p2 <- ggplot(data = df, mapping = aes(x = jobTitle, y = gender, fill = gender)) + 
  geom_bar(aes(y = ..count../sum(..count..) * 100), width = 0.7, position = "dodge") +
  labs(x = "Job Title", y = "% of Employees", fill = "Gender") +
  theme_classic() +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count../sum(..count..) * 100), 
            stat = "count", vjust = -1, color = "white", position = position_dodge(width = 0.7)) +
  theme(axis.title = element_text(size = 14), axis.text.x = element_text(size = 8, angle = 45, face = "bold"),
        legend.position = "top")
p2

##... Seniority by Gender

p3 <- ggplot(data = df, mapping = aes(x = seniority, y = gender, fill = gender)) + 
  geom_bar(aes(y = ..count../sum(..count..) * 100), width = 0.7, position = "dodge") +
  labs(x = "Seniority (Number of Years Worked)", y = "% of Employees", fill = "Gender") +
  theme_classic() +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count../sum(..count..) * 100), 
            stat = "count", vjust = -1, color = "white", position = position_dodge(width = 0.7)) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12, face = "bold"),
        legend.position = "top")
p3

ggpubr::ggarrange(p2, ggarrange(p1, p3, ncol = 2), nrow = 2)

##################
##... Mosaic Plots
##################

df$seniority <- as.factor(df$seniority)

ggplot(df) +
  geom_mosaic(aes(x = product(seniority), fill = gender)) +
  facet_wrap(~dept) +
  labs(x = "Seniority (Number of Years Worked)", y = "Gender", fill = "Gender") 


#################################################
##... Salary by Gender and Performance Evaluation
#################################################

ggplot(data = df, mapping = aes(x = as.factor(perfEval), y = totalSalary, fill = gender)) + 
  geom_boxplot(color = "steelblue") +
  theme_bw() +
  labs(x = "Performance Evaluation Score", y = "Total Salary", fill = "Gender") +
  theme(axis.title = element_text(size = 14, color = "red"), axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position = "top") +
  scale_fill_manual(values = c("#FF66CC", "blue"))

########################################################
##... Scatter Plot of Total Salary Against Age by Gender
########################################################

ggplot(data = df, mapping = aes(x = age, y = logTotalSalary)) + 
  geom_point()

ggplot(data = df, mapping = aes(x = age, y = logTotalSalary)) + 
  geom_point(size = 2, aes(color = gender)) +
  geom_smooth(method = "lm", aes(fill = gender, color = gender)) +
  theme_light() +
  labs(x = "Age", y = "Log of Total Salary", fill = "Gender", color = "Gender") + 
  theme(axis.title = element_text(size = 14, color = "red"),
        axis.text = element_text(size = 12, color = "blue")) +
  scale_color_manual(values = c("#FF66CC", "blue")) +
  scale_fill_manual(values = c("#FF66CC", "blue"))
  
  
############################################################
##... Mean Salary by Job Title for Male and Female Employees
############################################################

ggplot(data = df, mapping = aes(x = jobTitle, y = totalSalary, fill = jobTitle)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.6, color = "black") +
  theme_bw() +
  labs(x = "Job Title", y = "Mean Total Salary", fill = "Job Title") +
  theme(axis.title = element_text(size = 14, color = "blue"),
        axis.text = element_text(size = 12)) +
  coord_flip() +
  facet_wrap( ~ gender)

ggplot(data = df, mapping = aes(x = jobTitle, y = totalSalary, fill = gender)) +
  geom_boxplot(color = "steelblue") +
  theme_minimal() +
  labs(x = "Job Title", y = "Total Salary", fill = "Gender") +
  theme(axis.title = element_text(size = 14, color = "red"),
        axis.text = element_text(size = 12, color = "blue")) +
  scale_fill_manual(values = c("#FF66CC", "blue")) +
  coord_flip()
  
###################################################################################
##... Mean Salary by Job Title and Performance Scores for Male and Female Employees
###################################################################################

# This plot is difficult to read. Too much clutter? 

ggplot(data = df, mapping = aes(x = jobTitle, y = totalSalary, fill = jobTitle)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.6, color = "black") +
  theme_bw() +
  labs(x = "Job Title", y = "Mean Total Salary", fill = "Job Title") +
  theme(axis.title = element_text(size = 14, color = "blue"),
        axis.text = element_text(size = 12)) +
  coord_flip() +
  facet_grid(perfEval ~ gender)

# df <- df %>%
#   dplyr::group_by(jobTitle, gender) %>%
#   dplyr::mutate(mean_salary = mean(totalSalary))

# A slight improvement of the previous graph

ggplot(data = df, mapping = aes(fill = gender, y = totalSalary, x = jobTitle)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.9, color = "black") +
  theme_bw() +
  labs(x = "Job Title", y = "Mean Total Salary", fill = "Gender") +
  theme(axis.title = element_text(size = 10, color = "blue"),
        axis.text = element_text(size = 8),
        legend.position = "top") +
  # geom_col() +
  # geom_text(aes(label = totalSalary), position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(values = c("#FF66CC", "blue")) +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  facet_wrap( ~ perfEval)

# With labels. Slight improvement. 

df_summary <- df %>% 
  group_by(gender, jobTitle, perfEval) %>%
  summarize(totalcomp = mean(basePay + bonus),
            totalcomp_label = paste0(round(totalcomp * 1e-3, 0), "k")) %>%
  ungroup() 


df_plot <- df_summary %>% 
  left_join(
    # the messy part to find appropriate label positions - there may be a solution with less pivoting steps
    df_summary %>%
      tidyr::pivot_wider(id_cols = c(jobTitle, perfEval), 
                         values_from = "totalcomp", names_from = "gender", values_fill = 0) %>%
      dplyr::mutate(labelpos_M = Male/2, labelpos_F = Male + Female/2) %>% 
      tidyr::pivot_longer(c(Female, Male), names_to = "gender") %>%
      dplyr::mutate(
        labelpos = case_when(gender == "Male" ~ labelpos_M,
                             gender == "Female" ~ labelpos_F,
                             TRUE ~ NA_real_)
      ) %>%
      dplyr::select(jobTitle, perfEval, gender, labelpos),
    by = c("jobTitle", "perfEval", "gender")
  ) 


df_plot %>%
  ggplot() +
  geom_col(aes(y = jobTitle, x = totalcomp, fill = gender), width = 0.9, color = "black") +
  theme_bw() +
  labs(x = "Job Title", y = "Mean Total Salary", fill = "Gender") +
  theme(axis.title = element_text(size = 10, color = "blue"),
        axis.text = element_text(size = 8),
        legend.position = "top") +
  scale_fill_manual(values = c("#FF66CC", "blue")) +
  scale_x_continuous(labels = scales::comma) +
  facet_wrap( ~ perfEval) +
  # positioning the labels
  geom_text(aes(x = labelpos, y = jobTitle, label = totalcomp_label), 
            color = "white")

##############
##... Dot Plot
##############

##... Basic Dot Plot

df2 <- df %>%
  group_by(jobTitle, gender) %>%
  summarise(mean_salary = mean(totalSalary, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(jobTitle = factor(jobTitle))

ggplot(data = df2, mapping = aes(x = mean_salary, y = jobTitle, label = round(mean_salary, 0))) + 
  geom_line(aes(group = jobTitle)) +
  geom_point(aes(color = gender)) +
  labs(x = "Mean Total Salary", y = "Job Title", color = "Gender") +
  theme(axis.title = element_text(size = 10, color = "blue"),
        axis.text = element_text(size = 8),
        legend.position = "top") +
  scale_color_manual(values = c("#FF66CC", "blue")) +
  geom_text(aes(color = gender), size = 2.5)

##... Basic Dot Plot (labels aligned)

right_label <- df2 %>%
  group_by(jobTitle) %>%
  arrange(desc(mean_salary)) %>%
  top_n(1)

left_label <- df2 %>%
  group_by(jobTitle) %>%
  arrange(desc(mean_salary)) %>%
  slice(2)

summary(df2$mean_salary)

ggplot(df2, aes(mean_salary, jobTitle)) +
  geom_line(aes(group = jobTitle)) +
  geom_point(aes(color = gender), size = 1.5) +
  geom_text(data = right_label, aes(color = gender, label = round(mean_salary, 0)),
            size = 2.5, hjust = -.5) + # hjust = -0.5
  geom_text(data = left_label, aes(color = gender, label = round(mean_salary, 0)),
            size = 2.5, hjust = 2) + # hjust = 1.5
  labs(x = "Mean Total Salary", y = "Job Title", color = "Gender") +
  scale_x_continuous(limits = c(70000, 135000)) +
  theme(axis.title = element_text(size = 10, color = "blue"),
        axis.text = element_text(size = 8),
        legend.position = "top") +
  scale_color_manual(values = c("#FF66CC", "blue"))


# create data frame that identifies revenue differences over 5%
big_diff <- df2 %>% 
  tidyr::spread(gender, mean_salary) %>% 
  dplyr::group_by(jobTitle) %>% 
  dplyr::mutate(Max = max(Female, Male),
         Min = min(Female, Male),
         Diff = Max / Min - 1) %>% 
  dplyr::arrange(desc(Diff)) %>%
  dplyr::filter(Diff > 0.05)

# filter the label data frames to only include those cities where the
# difference exceeds 5%
right_label <- filter(right_label, jobTitle %in% big_diff$jobTitle)
left_label <- filter(left_label, jobTitle %in% big_diff$jobTitle)

# filter the main data frame to only include those cities where the 
# difference exceeds 5%.
highlight <- filter(df2, jobTitle %in% big_diff$jobTitle)

ggplot(df2, aes(mean_salary, jobTitle)) +
  geom_line(aes(group = jobTitle), alpha = .3) +
  geom_point(aes(color = gender), size = 1.5, alpha = .3) +
  geom_line(data = df2, aes(group = jobTitle)) +
  geom_point(data = highlight, aes(color = gender), size = 2) +
  geom_text(data = right_label, aes(color = gender, label = round(mean_salary, 0)),
            size = 3, hjust = -.5) +
  geom_text(data = left_label, aes(color = gender, label = round(mean_salary, 0)),
            size = 3, hjust = 1.5) +
  labs(x = "Mean Total Salary", y = "Job Title", color = "Gender") +
  scale_x_continuous(limits = c(90000, 120000)) +
  theme(axis.title = element_text(size = 10, color = "blue"),
        axis.text = element_text(size = 8),
        legend.position = "top") +
  scale_color_manual(values = c("#FF66CC", "blue"))

##... Dot plot percentage gap. 

big_diff <- df2 %>% 
  tidyr::spread(gender, mean_salary) %>% 
  group_by(jobTitle) %>% 
  mutate(Max = max(Female, Male),
         Min = min(Female, Male),
         Diff = Max / Min - 1) %>% 
  arrange(desc(Diff)) 

right_label <- df2 %>%
  group_by(jobTitle) %>%
  arrange(desc(mean_salary)) %>%
  top_n(1)

left_label <- df2 %>%
  group_by(jobTitle) %>%
  arrange(desc(mean_salary)) %>%
  slice(2)

plot_label <- big_diff %>%
  select(jobTitle, mean_salary = Max, Diff) %>%
  right_join(right_label)

p <- ggplot(df2, aes(mean_salary, jobTitle)) +
  geom_line(aes(group = jobTitle), alpha = .3) +
  geom_point(aes(color = gender), size = 1.5, alpha = .3) +
  geom_line(data = df2, aes(group = jobTitle)) +
  geom_point(data = df2, aes(color = gender), size = 2) +
  geom_text(data = plot_label, aes(color = gender, 
                                   label = paste0("+", scales::percent(round(Diff, 2)))),
            size = 3, hjust = -.5) +
  labs(x = "Mean Total Salary", y = "Job Title", color = "Gender") +
  # scale_x_continuous(limits = c(70000, 135000)) +
  theme(axis.title = element_text(size = 10, color = "blue"),
        axis.text = element_text(size = 8),
        legend.position = "top") +
  scale_color_manual(values = c("#FF66CC", "blue")) 
p


##... Dot plot percentage gap by performance evaluation. 

df3 <- df %>%
  select(jobTitle, gender, perfEval, totalSalary) %>%
  group_by(jobTitle, gender, perfEval) %>%
  summarise(mean_salary = mean(totalSalary, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(jobTitle = factor(jobTitle))

big_diff <- df3 %>% 
  tidyr::spread(gender, mean_salary) %>% 
  group_by(jobTitle, perfEval) %>% 
  mutate(Max = max(Female, Male),
         Min = min(Female, Male),
         Diff = Max / Min - 1) %>% 
  arrange(desc(Diff)) 

right_label <- df3 %>%
  group_by(jobTitle, perfEval) %>%
  arrange(desc(mean_salary)) %>%
  top_n(1)

left_label <- df3 %>%
  group_by(jobTitle, perfEval) %>%
  arrange(desc(mean_salary)) %>%
  slice(2)

plot_label <- big_diff %>%
  select(jobTitle, mean_salary = Max, Diff) %>%
  right_join(right_label)

p <- ggplot(df3, aes(mean_salary, jobTitle)) +
  geom_line(aes(group = jobTitle), alpha = .3) +
  geom_point(aes(color = gender), size = 1.5, alpha = .3) +
  geom_line(data = df3, aes(group = jobTitle)) +
  geom_point(data = df3, aes(color = gender), size = 2) +
  geom_text(data = plot_label[!is.na(plot_label$Diff),], aes(color = gender, 
                                   label = paste0("+", scales::percent(round(Diff, 2)))),
            size = 3, hjust = -.5) +
  labs(x = "Mean Total Salary", y = "Job Title", color = "Gender") +
  # scale_x_continuous(limits = c(70000, 135000)) +
  theme(axis.title = element_text(size = 10, color = "blue"),
        axis.text = element_text(size = 8),
        legend.position = "top") +
  scale_color_manual(values = c("#FF66CC", "blue")) +
  facet_wrap(~ perfEval)
p


#################
##... Regressions
#################

summary(lm(totalSalary ~ factor(gender), data = df))
summary(lm(totalSalary ~ factor(gender) + factor(jobTitle), data = df))
summary(lm(totalSalary ~ factor(gender) + factor(jobTitle) + factor(dept), data = df))

mymodel <- lm(totalSalary ~ factor(gender) + factor(jobTitle) + factor(dept), data = df)

# Percentage of variation explained

af <- anova(mymodel)
afss <- af$`Sum Sq`
print(cbind(af,PctExp = afss/sum(afss) * 100))

#################
##... Regressions
#################

summary(lm(logTotalSalary ~ factor(gender), data = df))
summary(lm(logTotalSalary ~ factor(gender) + factor(jobTitle), data = df))
summary(lm(logTotalSalary ~ factor(gender) + factor(jobTitle) + factor(dept), data = df))

mymodel <- lm(logTotalSalary ~ factor(gender) + factor(jobTitle) + factor(dept), data = df)

# Percentage of variation explained

af <- anova(mymodel)
afss <- af$`Sum Sq`
print(cbind(af,PctExp = afss/sum(afss) * 100))















