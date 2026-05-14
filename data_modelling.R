library(dplyr)

students <- read.csv('students_cleaned.csv')
View(students)

str(students)



students <- students %>%
  mutate(
    # Nominal variables
    school = factor(school),
    
    sex = factor(sex,
                 levels = c("Female", "Male")),
    
    address = factor(address,
                     levels = c("Urban", "Rural")),
    
    famsize = factor(famsize,
                     levels = c("Small", "Large")),
    
    parent_status = factor(parent_status,
                           levels = c("Together", "Apart")),
    
    Mjob = factor(Mjob),
    Fjob = factor(Fjob),
    reason = factor(reason),
    guardian = factor(guardian),
    
    subject = factor(subject),
    
    performance = factor(performance,
                         levels = c("Low", "Medium", "High"),
                         ordered = TRUE),
    
    absence_level = factor(absence_level,
                           levels = c("Low", "Moderate", "High"),
                           ordered = TRUE),
    
    
    # Binary variables
    schoolsup = factor(schoolsup,
                       levels = c("no", "yes")),
    
    famsup = factor(famsup,
                    levels = c("no", "yes")),
    
    paid = factor(paid,
                  levels = c("no", "yes")),
    
    activities = factor(activities,
                        levels = c("no", "yes")),
    
    nursery = factor(nursery,
                     levels = c("no", "yes")),
    
    higher = factor(higher,
                    levels = c("no", "yes")),
    
    internet = factor(internet,
                      levels = c("no", "yes")),
    
    romantic = factor(romantic,
                      levels = c("no", "yes")),
    
    
    # Ordinal variables
    mother_edu = factor(mother_edu,
                        levels = 0:4,
                        ordered = TRUE),
    
    father_edu = factor(father_edu,
                        levels = 0:4,
                        ordered = TRUE),
    
    studytime = factor(studytime,
                       levels = c("Low", "Medium", "High", "Very High"),
                       ordered = TRUE),
    
    failures = factor(failures,
                      levels = 0:3,
                      ordered = TRUE),
    
    traveltime = factor(traveltime,
                        levels = 1:4,
                        ordered = TRUE),
    
    famrel = factor(famrel,
                    levels = 1:5,
                    ordered = TRUE),
    
    freetime = factor(freetime,
                      levels = 1:5,
                      ordered = TRUE),
    
    goout = factor(goout,
                   levels = 1:5,
                   ordered = TRUE),
    
    weekday_alcohol_consume = factor(
      weekday_alcohol_consume,
      levels = 1:5,
      ordered = TRUE
    ),
    
    weekend_alcohol_consume = factor(
      weekend_alcohol_consume,
      levels = 1:5,
      ordered = TRUE
    ),
    
    health = factor(health,
                    levels = 1:5,
                    ordered = TRUE)
  )

str(students)

student_subset <- students %>%
  select(
    studytime,
    final_grade,
    sex,
    parent_edu,
    weekend_alcohol_consume,
    absences
  )

View(student_subset)

library(psych)
windows(20,10)

pairs.panels(student_subset,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


attach(student_subset)
windows(20,12)
par(mfrow= c(3,2))

scatter.smooth(x = studytime,
               y = final_grade,
               xlab = "Study  Time",
               ylab = "Final Grade", main = "Correlation of study time ~ final grade")

scatter.smooth(x = sex,
               y = final_grade,
               xlab = "Sex",
               ylab = "Final Grade", main = "Correlation of sex ~ final grade")

scatter.smooth(x = parent_edu,
               y = final_grade,
               xlab = "Parent Education Level",
               ylab = "Final Grade", main = "Correlation of Parent Education Level ~ final grade")

scatter.smooth(x = weekend_alcohol_consume,
               y = final_grade,
               xlab = "Alcohol Consume Level",
               ylab = "Final Grade", main = "Correlation of Alcohol Consume Level ~ final grade")

scatter.smooth(x = absences,
               y = final_grade,
               xlab = "Absences",
               ylab = "Final Grade", main = "Correlation of Absences ~ final grade")
detach(student_subset)


# Examining correlation between murder and Independent variables

cor_data <- student_subset %>%
  mutate(
    studytime = as.numeric(studytime),
    sex = ifelse(sex == "Male", 1, 0),
    weekend_alcohol_consume = as.numeric(weekend_alcohol_consume)
  )

round(cor(cor_data), 2)

attach(cor_data)

# Examining the other variables
paste("Correlation for final grade and study time: ", round(cor(final_grade, studytime),2))
paste("Correlation for final grade and sex: ", round(cor(final_grade, sex),2))
paste("Correlation for final grade and parent education level: ", round(cor(final_grade, parent_edu),2))
paste("Correlation for final grade and alcohol consume level: ", round(cor(final_grade, weekend_alcohol_consume),2))
paste("Correlation for final grade and absences: ", round(cor(final_grade, absences),2))

detach(cor_data)


windows(20,10)
par(mfrow = c(3, 2)) # divide graph area in 3 rows by 2 columns
attach(student_subset)

boxplot(final_grade,
        main = "Final Grade") # box plot for 'Final Grade'

boxplot(studytime,
        main = "Study Time") # box plot for 'Study Time'

boxplot(sex,
        main = "Gender") # box plot for 'Gender'

boxplot(parent_edu,
        main = "Parent Education Level") # box plot for 'Parent Education Level'

boxplot(weekend_alcohol_consume,
        main = "Alcohol Consume Level") # box plot for 'Alcohol Consume Level'

boxplot(absences,
        main = "Absences") # box plot for 'Absences'


# Check outliers for absences
outlier_values <- boxplot.stats(absences)$out # outlier values.
outlier_values


# Apply Winsorization / Capping
# replace extreme values with boundary values


Q1 <- quantile(student_subset$absences, 0.25)
Q3 <- quantile(student_subset$absences, 0.75)

IQR_value <- Q3 - Q1

lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value

student_subset$absences_capped <- pmin(
  pmax(student_subset$absences, lower),
  upper
)

# check outlier values for absences_capped
outlier_values <- boxplot.stats(student_subset$absences_capped)$out
outlier_values

#check the boxplot for any outliers 
windows(10,10)
boxplot(student_subset$absences_capped,
        main = "Absences")


student_subset$sex_num <- ifelse(student_subset$sex == "Male", 1, 0)
student_subset$alcohol_consume <-  as.numeric(weekend_alcohol_consume)
# convert ordinal study time into numeric for modelling purposes
student_subset$studytime_num <- as.numeric(studytime)


windows(20,20)
boxplot(final_grade ~ studytime, data = student_subset)


# Check outliers for final grade
outlier_values <- boxplot.stats(final_grade)$out
outlier_values

# Apply Winsorization / Capping
# replace extreme values with boundary values


Q1 <- quantile(student_subset$final_grade, 0.25)
Q3 <- quantile(student_subset$final_grade, 0.75)

IQR_value <- Q3 - Q1

lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value

student_subset$final_grades_capped <- pmin(
  pmax(student_subset$final_grade, lower),
  upper
)

# check outlier values for grades_capped
outlier_values <- boxplot.stats(student_subset$final_grades_capped)$out
outlier_values

#check the boxplot for any outliers 
windows(10,10)
boxplot(student_subset$final_grades_capped,
        main = "Final Grades")


# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
windows(30,20)
par(mfrow = c(3,2)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical


plot(density(student_subset$final_grades_capped),
     main = "Density plot : Final Grade",
     ylab = "Frequency", xlab = "Final Grade",
     sub = paste("Skewness : ", round(e1071::skewness(student_subset$final_grades_capped), 2)))
polygon(density(student_subset$final_grades_capped), col = "red")

plot(density(student_subset$studytime_num),
     main = "Density plot : Study Time",
     ylab = "Frequency", xlab = "Study Time",
     sub = paste("Skewness : ", round(e1071::skewness(student_subset$studytime_num), 2)))
polygon(density(student_subset$studytime_num), col = "red")

plot(density(student_subset$sex_num),
     main = "Density plot : Gender",
     ylab = "Frequency", xlab = "Gender",
     sub = paste("Skewness : ", round(e1071::skewness(student_subset$sex_num), 2)))
polygon(density(student_subset$sex_num), col = "red")

plot(density(student_subset$parent_edu),
     main = "Density plot : Parent Education",
     ylab = "Frequency", xlab = "Parent Education",
     sub = paste("Skewness : ", round(e1071::skewness(student_subset$parent_edu), 2)))
polygon(density(student_subset$parent_edu), col = "red")

plot(density(student_subset$alcohol_consume),
     main = "Density plot : Alcohol Consume",
     ylab = "Frequency", xlab = "Alcohol Consume",
     sub = paste("Skewness : ", round(e1071::skewness(student_subset$alcohol_consume), 2)))
polygon(density(student_subset$alcohol_consume), col = "red")

plot(density(student_subset$absences_capped),
     main = "Density plot : Absences",
     ylab = "Frequency", xlab = "Absences",
     sub = paste("Skewness : ", round(e1071::skewness(student_subset$absences_capped), 2)))
polygon(density(student_subset$absences_capped), col = "red")





