library(e1071)
library(psych)
library(MASS)
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
  dplyr::select(
    studytime,
    final_grade,
    sex,
    parent_edu,
    weekend_alcohol_consume,
    absences
  )

View(student_subset)


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



# Check normality of all variables using normality test
shapiro.test(student_subset$final_grades_capped)
shapiro.test(student_subset$studytime_num)
shapiro.test(student_subset$parent_edu)
shapiro.test(student_subset$alcohol_consume)
shapiro.test(student_subset$absences_capped)
shapiro.test(student_subset$sex_num)


hist(student_subset$final_grades_capped,
     breaks = 20,
     main = "Final Grade Distribution",
     xlab = "Final Grade")




model <- lm(final_grades_capped ~ 1,
            data = student_subset)

boxcox(model)

boxcox_result <-boxcox(model)


lambda <- boxcox_result$x[
  which.max(boxcox_result$y)
]

lambda

student_subset$final_grade_normalized <-
  (student_subset$final_grades_capped^lambda - 1) / lambda

shapiro.test(student_subset$final_grade_normalized)

hist(student_subset$final_grade_normalized,
     breaks = 20,
     main = "Final Grade Distribution",
     xlab = "Final Grade")


model_data <- student_subset %>%
  dplyr::select(
    final_grade_normalized,
    studytime_num,
    sex_num,
    parent_edu,
    alcohol_consume,
    absences_capped
  )

str(model_data)
summary(model_data)



# Model 1

model_1 <- lm(
  final_grade_normalized ~
    studytime_num +
    sex_num +
    parent_edu +
    alcohol_consume +
    absences_capped,
  data = model_data
)

summary(model_1)


# Remove sex from the variable list and develop model 2
# Since sex isn't significant in the list

model_2 <- lm(
  final_grade_normalized ~
    studytime_num +
    parent_edu +
    alcohol_consume +
    absences_capped,
  
  data = model_data
)

summary(model_2)


# model 3 

standardized_data <- model_data %>%
  mutate(across(everything(), scale))

standardized_model <- lm(
  final_grade_normalized ~
    
    studytime_num +
    parent_edu +
    alcohol_consume +
    absences_capped,
  
  data = standardized_data
)

summary(standardized_model)


# Check AIC and BIC 

AIC(
  model_1,
  model_2,
  standardized_model
)

BIC(
  model_1,
  model_2,
  standardized_model
)


# Compare the results in the table
comparison <- data.frame(
  
  Model = c(
    "Model 1 - Full Model",
    "Model 2- Reduced Model",
    "Standardized Model"
  ),
  
  AIC = c(
    AIC(model_1),
    AIC(model_2),
    AIC(standardized_model)
  ),
  
  BIC = c(
    BIC(model_1),
    BIC(model_2),
    BIC(standardized_model)
  ),
  
  Adjusted_R2 = c(
    summary(model_1)$adj.r.squared,
    summary(model_2)$adj.r.squared,
    summary(standardized_model)$adj.r.squared
  )
)

comparison

# FINAL MODEL DIAGNOSTICS and VALIDATION



# NORMALITY OF RESIDUALS

windows(15,6)

par(mfrow = c(1,2))

# Histogram
hist(
  residuals(model_2),
  main = "Histogram of Residuals",
  col = "skyblue",
  xlab = "Residuals"
)

# QQ Plot
qqnorm(residuals(model_2))
qqline(residuals(model_2),
       col = "red")

# Shapiro-Wilk test
shapiro.test(residuals(model_2))



# HOMOSCEDASTICITY

install.packages("lmtest")
library(lmtest)

bptest(model_2)


# MULTICOLLINEARITY

library(car)

vif(model_2)



# INDEPENDENCE OF ERRORS

durbinWatsonTest(model_2)


install.packages('sandwich')
library(sandwich)
library(lmtest)

coeftest(
  model_2,
  vcov = vcovHC(model_2, type = "HC1")
)



# TRAIN TEST SPLIT

library(caret)

set.seed(123)

train_index <- createDataPartition(
  model_data$final_grade_normalized,
  p = 0.8,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]


# TRAIN MODEL

train_model <- lm(
  final_grade_normalized ~
    
    studytime_num +
    parent_edu +
    alcohol_consume +
    absences_capped,
  
  data = train_data
)

summary(train_model)


# TEST PREDICTIONS

predictions <- predict(
  train_model,
  newdata = test_data
)

results <- data.frame(
  Actual = test_data$final_grade_normalized,
  Predicted = predictions
)

head(results)


# MODEL ACCURACY

install.packages("Metrics")
library(Metrics)

# RMSE
rmse(
  test_data$final_grade_normalized,
  predictions
)

# MAE
mae(
  test_data$final_grade_normalized,
  predictions
)

# R-squared
cor(
  test_data$final_grade_normalized,
  predictions
)^2


# ACTUAL VS PREDICTED

windows(10,10)

plot(
  test_data$final_grade_normalized,
  predictions,
  
  xlab = "Actual Grades",
  ylab = "Predicted Grades",
  
  main = "Actual vs Predicted Grades",
  
  pch = 19,
  col = "blue"
)

abline(0,1,col="red")


# FORECASTING SCENARIOS

new_students <- data.frame(
  studytime_num = c(4,1,3,2),
  parent_edu = c(5,1,4,2),
  alcohol_consume = c(1,5,2,4),
  absences_capped = c(1,20,5,15)
)

forecast_predictions <- predict(
  train_model,
  newdata = new_students
)

forecast_results <- cbind(
  new_students,
  predicted_grade = forecast_predictions
)

forecast_results
