library(tidyverse)
library(MASS)
library(RColorBrewer)
library(scales)
library(caret)
library(stats)
library(pscl)
library(LogicReg)
library(leaps)
library(bestglm)
library(performance)
library(DescTools)
library(yardstick)
library(pROC)
library(class)
library(car)
library(glmnet)


getwd()
setwd("..")
setwd("bana6610_statistics/final_project/")

diabetes_data = read.csv("diabetes_data.csv")

diabetes_data = diabetes_data %>% relocate(Diabetes_binary, .after = Income)

# Data cleaning
sum(is.null(diabetes_data))
duplicated(diabetes_data)
sum(duplicated(diabetes_data))
dim(diabetes_data)
dim(unique(diabetes_data))
str(diabetes_data)

# Checking distributions and outliers of numerical variables

hist(diabetes_data$BMI)
hist(diabetes_data$MentHlth)
summary(diabetes_data$BMI)
sum(diabetes_data$BMI>45)
summary(diabetes_data$MentHlth)
sum(diabetes_data$MentHlth>5)
summary(diabetes_data$PhysHlth)
sum(diabetes_data$PhysHlth>15)

head(diabetes_data)


#### EDA ####

summary_data = diabetes_data %>% 
  summarise(bmi_mean = mean(BMI), percent_male = mean(Sex), mean_age_cat = mean(Age),
            hvy_drinkers = mean(HvyAlcoholConsump),
            mean_gen_health = mean(GenHlth), mean_edu_level = mean(Education),
            mean_income = mean(Income), percent_w_healthcare = mean(AnyHealthcare),
            fruit_consumers = mean(Fruits), veggie_consumers = mean(Veggies),
            active_respondents = mean(PhysActivity), heart_disease = mean(HeartDiseaseorAttack),
            stroke = mean(Stroke), smoker = mean(Smoker), high_chol = mean(HighChol),
            chol_checked = mean(CholCheck), noDoc_bc_cost = mean(NoDocbcCost),
            days_poor_mntl_health = mean(MentHlth), days_poor_phys_health = mean(PhysHlth),
            unmobile = mean(DiffWalk))

summary_data = t(summary_data)
summary_data

plot(diabetes_data$BMI,diabetes_data$Diabetes_binary)
plot(diabetes_data$Age, diabetes_data$Diabetes_binary)

## plot of distribution of age

ggplot(diabetes_data, aes(x= Age, fill= Age)) +
  geom_bar() + 
  scale_x_discrete(breaks= c(1,2,3,4,5,6,7,8,9,10,11,12,13), labels=c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                               "55-59", "60-64", "65-69", "70-74", "75-79", "80+")) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(y = "Count") +
  ggtitle("Age Distribution of Diabetes Survey Respondents")

ggplot(diabetes_data, aes(x = Age, fill = Diabetes_binary)) +
  geom_bar() + 
  scale_x_discrete(labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                              "55-59", "60-64", "65-69", "70-74", "75-79", "80+")) + 
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(y = "Count") +
  ggtitle("Age Distribution of Diabetes Survey Respondents")



## plot of income distribution

ggplot(diabetes_data, aes(x = Income, fill= Diabetes_binary))+
  geom_bar() +
  scale_x_discrete(breaks= c(1,2,3,4,5,6,7,8), labels=c("< $10,000", "$10,000-15,000", "15,000-20,000",
                                                          "$20,000-25,000", "$25,000-35,000", "$35,000-50,000",
                                                          "$50,000-75,000", "$75,000+")) + 
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust=.75)) +
  labs(y = "Count") +
  ggtitle("Income Distribution of Diabetes Survey Respondents")+
  scale_fill_manual(values=c("0"= "skyblue", "1"="lightgreen"), name="Diabetes")


## plot of bmi distribution

ggplot(diabetes_data, aes(x=BMI, fill=as.factor(Diabetes_binary))) +
  geom_histogram(binwidth = 2, color="black") +
   labs(title = "Distribution of BMI for those with and without Diabetes")+
  theme_minimal()+ 
   scale_fill_manual(values=c("0"= "orange", "1"="blue"), name="Diabetes")

  


## BP plot

diabetes_data %>% 
  count(HighBP = factor(HighBP)) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = HighBP, y = pct, fill=as.factor(Diabetes_binary), label = scales::percent(pct))) + 
  geom_col(position = 'dodge', fill="coral",width = 0.5) + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels = c("No", "Yes"))+
  theme_minimal()+
  ggtitle("Percentage of Respondents with High Blood Pressure")+
  labs(x="High Blood Pressure", y="Percentage")


ggplot(diabetes_data, aes(x=HighBP, fill=as.factor(Diabetes_binary))) +
  geom_bar(width=.5) +
  labs(title = "HighBP Responses for those with and without Diabetes")+
  theme_minimal()+ 
  scale_fill_manual(values=c("0"= "darkblue", "1" = "forestgreen"), name="Diabetes")



w## High Cholesterol

diabetes_data %>% 
  count(HighChol = factor(HighChol)) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = HighChol, y = pct, label = scales::percent(pct))) + 
  geom_col(position = 'dodge', fill="maroon",width = 0.5) + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels = c("No", "Yes"))+
  theme_minimal()+
  ggtitle("Percentage of Respondents with High Cholesterol")+
  labs(x="High Cholesterol", y="Percentage")

ggplot(diabetes_data, aes(x=HighChol, fill=as.factor(Diabetes_binary))) +
  geom_bar(width=.5) +
  labs(title = "High Cholesterol Responses for those with and without Diabetes")+
  theme_minimal()+ 
  scale_fill_manual(values=c("0"= "darkblue", "1" = "maroon"), name="Diabetes")

## Alcohol consumption

diabetes_data %>% 
  count(HvyAlcoholConsump = factor(HvyAlcoholConsump)) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = HvyAlcoholConsump, y = pct, label = scales::percent(pct))) + 
  geom_col(position = 'dodge', fill="grey",width = 0.5) + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels = c("No", "Yes"))+
  theme_minimal()+
  ggtitle("Percentage of Respondents with Heavy Drinking Problem")+
  labs(x="Heavy Alcohol Consumption", y="Percentage")

## Heavy smoker
diabetes_data %>% 
  count(Smoker = factor(Smoker)) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = Smoker, y = pct, label = scales::percent(pct))) + 
  geom_col(position = 'dodge', fill="grey",width = 0.5) + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels = c("No", "Yes"))+
  theme_minimal()+
  ggtitle("Percentage of Respondents with Heavy Smoking Problem")+
  labs(x="Heavy Smoker", y="Percentage")

## plot of general health distribution

ggplot(diabetes_data, aes(x=GenHlth, fill=GenHlth)) +
  geom_bar(fill="red", color="black") +
  theme_minimal()+
  ggtitle("Distribution of general health from Survey Respondents")

## plot of mental health distribution

ggplot(diabetes_data, aes(x=MentHlth, fill=MentHlth)) +
  geom_histogram(fill="pink", color="black") +
  theme_minimal()+
  ggtitle("Distribution of mental health from Survey Respondents")

## plot of physical health distribution

ggplot(diabetes_data, aes(x=PhysHlth, fill=PhysHlth)) +
  geom_histogram(fill="orange", color="black") +
  theme_minimal()+
  ggtitle("Distribution of physical health from Survey Respondents")

## Diabetes diagnosis status distribution
diabetes_df <- diabetes_data %>%
  filter(Diabetes_binary == "1")
diabetes_counts = table(diabetes_data$Diabetes_binary)
diabetes_counts <- data.frame(Diabetes = factor(names(diabetes_counts)), Count = as.numeric(diabetes_counts))

# Calculate percentages
diabetes_counts$Percent <- round(100 * diabetes_counts$Count / sum(diabetes_counts$Count), 1)

# Create a pie chart
ggplot(diabetes_counts, aes(x = "", y = Count, fill = Diabetes)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percent, "%")), position = position_stack(vjust = 0.5), color = "black", size = 4) + 
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("skyblue", "salmon"), labels = c("No Diabetes", "Diabetes")) +
  ggtitle("Diabetes Status Distribution")


# High BP among diabetics
diabetes_highbp <- table(diabetes_df$HighBP)
# Convert to dataframe
diabetes_highbp <- as.data.frame(diabetes_highbp)
colnames(diabetes_highbp) <- c("HighBP", "Count")
# Calculate percentages
diabetes_highbp$Percent <- round(100 * diabetes_highbp$Count / sum(diabetes_highbp$Count), 1)

diabetes_highbp$HighBP <- ifelse(diabetes_highbp$HighBP == 0, "No", "Yes")

# Create pie chart
ggplot(diabetes_highbp, aes(x = "", y = Count, fill = HighBP)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percent, "%")), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme_void() +
  scale_fill_manual(values = c("beige", "turquoise"), labels = c("No", "Yes")) +
  ggtitle("High BP percentages among diabetes diagnosed") +
  theme(plot.title = element_text(hjust = 0.5))

# High cholesterol among diabetics
diabetes_highchol <- as.data.frame(table(diabetes_df$HighChol))
colnames(diabetes_highchol) <- c("HighChol", "Count")
diabetes_highchol$HighChol <- ifelse(diabetes_highchol$HighChol == 0, "No", "Yes")
# Calculate percentages
diabetes_highchol$Percent <- round(100 * diabetes_highchol$Count / sum(diabetes_highchol$Count), 1)
# Create pie chart
ggplot(diabetes_highchol, aes(x = "", y = Count, fill = HighChol)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percent, "%")), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme_void() +
  scale_fill_manual(values = c("orange", "maroon"), labels = c("No", "Yes")) +
  ggtitle("High Cholesterol distribution among people with diabetes") +
  theme(plot.title = element_text(hjust = 0.5))


diabetes_physactivity <- table(diabetes_df$PhysActivity); diabetes_physactivity

# Gender distribution among diabetics
diabetes_sex <- table(diabetes_df$Sex)
# 0 is female, 1 is male
# Convert to dataframe
diabetes_sex <- as.data.frame(diabetes_sex)
colnames(diabetes_sex) <- c("Gender", "Count")
diabetes_sex$Gender <- ifelse(diabetes_sex$Gender == 0, "Female", "Male")
# Calculate percentages
diabetes_sex$Percent <- round(100 * diabetes_sex$Count / sum(diabetes_sex$Count), 1)
# Create pie chart
ggplot(diabetes_sex, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percent, "%")), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme_void() +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  ggtitle("Gender distribution among people with diabetes") +
  theme(plot.title = element_text(hjust = 0.5))

# Cholesterol check among diabetics 
diabetes_CholCheck = table(diabetes_df$CholCheck)
# 0 is no cholesterol check, 1 is yes cholesterol check in 5 years
# Convert to dataframe
diabetes_cholcheck_df <- as.data.frame(diabetes_CholCheck)
colnames(diabetes_cholcheck_df) <- c("CholCheck", "Count")
diabetes_cholcheck_df$CholCheck <- ifelse(diabetes_cholcheck_df$CholCheck == 0, "No", "Yes")
# Create pie chart
ggplot(diabetes_cholcheck_df, aes(x = "", y = Count, fill = CholCheck)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  ggtitle("Correlation between checking cholesterol and diabetes diagnosis") +
  theme(plot.title = element_text(hjust = 0.5))

# Smoking among diabetics
table(diabetes_df$Smoker)

# Alcohol consumption among diabetics
diabetes_alcohol = as.data.frame(table(diabetes_df$HvyAlcoholConsump))
colnames(diabetes_alcohol) <- c("Alcohol", "Count")
diabetes_alcohol$Alcohol <- ifelse(diabetes_alcohol$Alcohol == 0, "No", "Yes")
# Calculate percentages
diabetes_alcohol$Percent <- round(100 * diabetes_alcohol$Count / sum(diabetes_alcohol$Count), 1)
# Create pie chart
ggplot(diabetes_alcohol, aes(x = "", y = Count, fill = Alcohol)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percent, "%")), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  theme_void() +
  scale_fill_manual(values = c("lightgreen", "purple")) +
  ggtitle("Heavy alcohol consumption among people with diabetes") +
  theme(plot.title = element_text(hjust = 0.5))

no_diabetes_df <- diabetes %>%
  filter(Diabetes_binary == "0")
table(no_diabetes_df$Stroke)

library(dplyr)
# Stroke occurrences among diabetics
# Group by Diabetes_binary and Stroke, count occurrences
stroke_counts <- diabetes_data %>%
  group_by(Diabetes_binary, Stroke) %>%
  summarise(Count = n())

# Print the counts of 1
stroke_counts <- as.data.frame(stroke_counts)
colnames(stroke_counts) <- c("Diabetes", "Stroke", "Count")
stroke_counts <- stroke_counts %>%
  filter(Stroke == 1)

# Plot
ggplot(stroke_counts, aes(x = Diabetes, y = Count, fill = Diabetes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Stroke occurrence", fill = "Diabetes") +
  scale_x_discrete(labels = c("0" = "Non-Diabetic", "1" = "Diabetic")) +
  scale_fill_manual(values = c("azure2", "aquamarine"), labels = c("Non-Diabetic", "Diabetic")) + 
  ggtitle("Comparison of Stroke Occurrence in Diabetic and Non-Diabetic Individuals") +
  theme_minimal()

# Heart Disease among respondents, categorized by diabetes status
heart_disease_counts <- diabetes_data %>%
  group_by(Diabetes_binary, HeartDiseaseorAttack) %>%
  summarise(Count = n())

# Print the counts of 1
heart_disease_counts <- as.data.frame(heart_disease_counts)
heart_disease_counts <- subset(heart_disease_counts, HeartDiseaseorAttack == 1)

# Plot

ggplot(heart_disease_counts, aes(x = Count, y = factor(Diabetes_binary), fill = factor(Diabetes_binary))) +
  geom_bar(stat = "identity") +
  labs(x = "Heart Disease/Attack Occurrence", y = "", fill = "Diabetes Status") +
  ggtitle("Comparison of Heart Diseases/Attack Occurrence in Diabetic and Non-Diabetic Individuals") +
  theme_minimal() +
  scale_fill_manual(values = c("cadetblue","brown"), labels = c("Non-Diabetic", "Diabetic")) + 
  scale_y_discrete(labels = c("0" = "Non-Diabetic", "1" = "Diabetic"))

# Daily vegetable consumption among respondents, categorized by diabetes status
# Group by Diabetes_binary and Veggies, count occurrences
veggies_counts <- diabetes_data %>%
  group_by(Diabetes_binary, Veggies) %>%
  summarise(Count = n())

# Print the counts of 1
veggies_counts <- as.data.frame(veggies_counts)
colnames(veggies_counts) <- c("Diabetes", "Veggies", "Count")
veggies_counts <- veggies_counts %>%
  filter(Veggies == 1)

# Plot
ggplot(veggies_counts, aes(x = Diabetes, y = Count, fill = Diabetes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Daily Vegetable Consumption", fill = "Diabetes") +
  scale_x_discrete(labels = c("0" = "Non-Diabetic", "1" = "Diabetic")) +
  scale_fill_manual(values = c("cornflowerblue", "darkgoldenrod1"), labels = c("Non-Diabetic", "Diabetic")) + 
  ggtitle("Comparison of Vegetable Consumption in Diabetic and Non-Diabetic Individuals") +
  theme_minimal()

# Physical Activity among respondents, categorized by diabetes status
exercise_counts <- diabetes_data %>%
  group_by(Diabetes_binary, PhysActivity) %>%
  summarise(Count = n())

# Print the counts of 1
exercise_counts <- as.data.frame(exercise_counts)
colnames(exercise_counts) <- c("Diabetes", "Exercise", "Count")
exercise_counts <- exercise_counts %>%
  filter(Exercise == 1)

# Plot
ggplot(exercise_counts, aes(x = Diabetes, y = Count, fill = Diabetes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Physical Active Count", fill = "Diabetes") +
  scale_x_discrete(labels = c("0" = "Non-Diabetic", "1" = "Diabetic")) +
  scale_fill_manual(values = c("chocolate", "cyan2"), labels = c("Non-Diabetic", "Diabetic")) + 
  ggtitle("Physical Active People in Diabetic and Non-Diabetic Groups") +
  theme_minimal()

# Difficulties walking among respondents, categorized by diabetes status
diffwalk_counts <- diabetes_data %>%
  group_by(Diabetes_binary, DiffWalk) %>%
  summarise(Count = n())

# Print the counts of 1
diffwalk_counts <- as.data.frame(diffwalk_counts)
colnames(diffwalk_counts) <- c("Diabetes", "DiffWalk", "Count")
diffwalk_counts <- diffwalk_counts %>%
  filter(DiffWalk == 1)

# Plot
ggplot(diffwalk_counts, aes(x = Count, y = Diabetes, fill = Diabetes)) +
  geom_bar(stat = "identity") +
  labs(x = "People with walking difficulty", y = "", fill = "Diabetes Status") +
  ggtitle("Walking struggles in Diabetic and Non-Diabetic Groups") +
  theme_minimal() +
  scale_fill_manual(values = c("darkseagreen","pink"), labels = c("Non-Diabetic", "Diabetic")) + 
  scale_y_discrete(labels = c("0" = "Non-Diabetic", "1" = "Diabetic"))

# No doctor visits among diabetics
diabetes_nodocs = table(diabetes_df$NoDocbcCost)
# Convert to dataframe
diabetes_nodocs <- as.data.frame(diabetes_nodocs)
colnames(diabetes_nodocs) <- c("Doctor_visit", "Count")
diabetes_nodocs$Doctor_visit <- ifelse(diabetes_nodocs$Doctor_visit == 0, "No", "Yes")
# Calculate percentages
diabetes_nodocs$Percent <- round(100 * diabetes_nodocs$Count / sum(diabetes_nodocs$Count), 1)
# Create pie chart
ggplot(diabetes_nodocs, aes(x = "", y = Count, fill = Doctor_visit)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("lightgreen", "lavender")) +
  ggtitle("No doctor visits because of costs among people with diabetes") +
  theme(plot.title = element_text(hjust = 0.5))

table(diabetes_df$GenHlth)
table(diabetes_df$Education)

# Healthcare and diabetes diagnosis
diabetes_healthcare = table(diabetes_df$AnyHealthcare)
# Convert to dataframe
diabetes_healthcare_df <- as.data.frame(diabetes_healthcare)
colnames(diabetes_healthcare_df) <- c("Healthcare", "Count")
diabetes_healthcare_df$Healthcare <- ifelse(diabetes_healthcare_df$Healthcare == 0, "No", "Yes")
# Create pie chart
ggplot(diabetes_healthcare_df, aes(x = "", y = Count, fill = Healthcare)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("turquoise", "brown")) +
  ggtitle("Correlation between healthcare coverage and diabetes diagnosis") +
  theme(plot.title = element_text(hjust = 0.5))

#### logistic regression ####


#converting all variables to factors with the exception of BMI, MentHlth, PHysHlth, and Diabetes_binary

excluded_cols = c(4,15,16)
diabetes_data[,-excluded_cols] = lapply(diabetes_data[,-excluded_cols], factor)

#just converting Diabetes_binary to factor
diabetes_data$Diabetes_binary = as.factor(diabetes_data$Diabetes_binary)

str(diabetes_data)


#splitting the dataset

set.seed(421)
split = createDataPartition(diabetes_data$Diabetes_binary, p=.8, list=FALSE)
train.df = diabetes_data[split,]
test.df = diabetes_data[-split,]


#training the full model, glm
options(scipen = 999)


glm_model_full = glm(Diabetes_binary ~., data = diabetes_data, family = binomial(link = "logit"))
summary(glm_model_full)

summary(glm_model_full$finalModel)

pscl::pR2(glm_model_full)["McFadden"]
PseudoR2(glm_model_full, which = "McFadden")
varImp(glm_model_full, scale=FALSE)
car::vif(glm_model_full)


# training full model using carets train
trControl = caret::trainControl(method="cv", number=5, allowParallel=TRUE)
train_model_full <- caret::train(Diabetes_binary ~ ., data=diabetes_data, trControl=trControl,
                          method="glm", family="binomial")

summary(train_model_full$finalModel)
varImp(train_model_full, scale=FALSE)

#stepwise
step_both = stepAIC(model_full, direction="both", trace=FALSE)
step_both$anova
step_both$finalModel

#regsubsets 
regfit.full = regsubsets(Diabetes_binary~., data=train.df, nbest =1, nvmax = NULL,
                         force.in = NULL, force.out = NULL, method = "exhaustive")
summary(regfit.full)
regfit.full

#### best glm approach ####
excluded_vars = c(8,9,12,13,15,20,21)
glm_vars = train.df[,-excluded_vars]

best_subset = bestglm(glm_vars,
                      family = binomial,
                      IC = "BIC",
                      method = "exhaustive")

summary(best_subset$BestModel)
best_subset$BestModels
bestglm_model = best_subset$BestModel
summary(bestglm_model)

formula(best_subset$BestModel)

## below is the model provided from bestglm to avoid running that function again as it took over an hour

bestglm_model = glm(Diabetes_binary ~ HighBP + HighChol + CholCheck + BMI + Stroke + HeartDiseaseorAttack + 
                   Veggies + HvyAlcoholConsump + GenHlth + PhysHlth + DiffWalk + 
                   Sex + Age, data = train.df, family = binomial(link = "logit"))
summary(bestglm_model)

# Calculate the predicted log odds
predicted_log_odds <- predict(bestglm_model, type = "link")

# Extract the coefficients
coefficients <- coef(bestglm_model)

# Create a data frame with feature names and coefficients
coefficients_df <- data.frame(
  feature = names(coefficients),
  coefficient = coefficients,
  coefficient_abs = abs(coefficients)
)

# Sort coefficients in descending order by their absolute values
coefficients_sorted <- coefficients_df[order(-coefficients_df$coefficient_abs), ]; coefficients_sorted

# Deviance Test for bestglm_model fit
# Ho: The model is a good fitting model
# H1: The model is not a good fitting model
# Set the significance level
alpha <- 0.05
null_deviance <- 78400
residual_deviance <- 57575
# degree of freedom = total number of observations − number of predictors − 1
degree_of_freedom <- 56554 - 13 -1

# We use the Chi-square Deviance Test to check the overall fit of the model

# Calculate the deviance test statistic
deviance_test_statistic <- null_deviance - residual_deviance
print(deviance_test_statistic)

# Find the critical value from the chi-squared distribution
critical_value <- qchisq(1 - alpha, degree_of_freedom)
print(critical_value)

# deviance_test_statistic < critical value
# Do not reject Ho
print("At 0.05 level of significance, there is no evidence against the null hypothesis, thus, enough to conclude that this model is a good fit")


#predicting train and test datasets with bestglm model


# predict training set

glm_model_train = predict(bestglm_model, train.df[,-22], type = "response")
predicted_train = factor(ifelse(glm_model_train > 0.5, 1, 0))


# accuracy train
confusionMatrix(predicted_train, train.df$Diabetes_binary, positive="1")

# predict holdout 
glm_model_test_pred <- predict(bestglm_model, test.df[, -22], type = "response")
predicted_test <- factor(ifelse(glm_model_test_pred > 0.5, 1, 0))

# accuracy holdout
cf = confusionMatrix(predicted_test, test.df$Diabetes_binary, positive="1")
cf

precision_value = 5429/(5429+1987)
precision_value

recall_score = 5429/(5429+1640)
recall_score

f1_score =  (2*precision_value*recall_score)/(precision_value + recall_score)
f1_score

anova(bestglm_model, test="Chisq")


#### plots ####


#confusion matrix plot

fourfoldplot(as.table(cf), color = c("lightblue","red"), main = "Confusion Matrix")


# Create residual plots

residuals = residuals(bestglm_model)
exp_fitted = exp(bestglm_model$fitted.values)

plot(exp_fitted, residuals, ylab = "Residuals", xlab = "Fitted Values", main = "Logistic Regression Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at y = 0 for reference

residualPlot(bestglm_model)
influencePlot(bestglm_model, col="red", id.n=3)
crPlots(bestglm_model, layout = c(4,4))

plot(test.df$BMI,glm_model_test_pred)
logit = log(glm_model_train/(1-glm_model_train))
plot(train.df$PhysHlth, logit)


#standardized and pearson residuals
stand_resid = rstandard(bestglm_model)
pearson_residuals =residuals(bestglm_model, "pearson")

plot(train.df$BMI, pearson_residuals)
plot(train.df$Age, pearson_residuals)

## plots partial residuals for each independent variable

for (j in names(coef(bestglm_model))[-1]){
  plot(
  x= glm_vars[,j],
  y= residuals(bestglm_model, "partial")[,j],
  col= as.numeric(glm_vars$Diabetes_binary) + 1,
  pch = as.numeric(glm_vars$Diabetes_binary) + 1,
  main= paste0("Partial residuals by ",j),
  xlab= j,
  ylab = "partial residual"
)
  # Add horizontal line at 0
abline(h = 0, col = "gray")
}  

qqnorm(residuals)
mean(residuals)

# Cross validation
summary(bestglm_model)

#install.packages("boot")
library(boot)
set.seed(4)
cv_results=cv.glm(train.df, bestglm_model, K=5)
cv_results$delta
# Remove age variable
model2 = glm(Diabetes_binary ~ HighBP + HighChol + CholCheck + BMI + Stroke + HeartDiseaseorAttack + 
               Veggies + HvyAlcoholConsump + GenHlth + DiffWalk + 
               Sex, data = train.df, family = binomial(link = "logit"))
summary(model2)
cv_results2=cv.glm(train.df, model2, K=5)
cv_results2$delta
# Full data
cv_results3=cv.glm(train.df, glm_model_full, K=5)
cv_results3$delta
## bestglm_model has the best delta

# Plot ROC curve

roc_curve = roc(test.df$Diabetes_binary,glm_model_test_pred)

plot(roc_curve,main = "ROC Curve", col = "blue", legacy.axes=TRUE)
abline(a = 1, b = -1, lty = 2, col = "red")
legend("bottomright", legend = c("ROC Curve", "Random Classifier"), col = c("blue", "red"), lty = c(1, 2))


## evaluating other models based on varIMP top variables

model1 = glm(Diabetes_binary ~ BMI + GenHlth + HighBP + HighChol + CholCheck + Age + HvyAlcoholConsump + Sex + HeartDiseaseorAttack,
             data = train.df, family = binomial(link = "logit"))
summary(model1)

model1_train = predict(model1, train.df[,-22], type = "response")
predicted_train1 = factor(ifelse(model1_train > 0.5, 1, 0))


# accuracy train
confusionMatrix(predicted_train1, train.df$Diabetes_binary, positive="1")


#### K nearest neighbor ####

## k nearest neighbor with all variables

k_diabetes_data = diabetes_data
k_diabetes_data$Diabetes_binary = as.factor(k_diabetes_data$Diabetes_binary)
str(k_diabetes_data)

set.seed(33)
k_split = createDataPartition(k_diabetes_data$Diabetes_binary, p=.8, list=FALSE)
k_train.df = k_diabetes_data[split,]
k_test.df = k_diabetes_data[-split,]


train_scaled = scale(k_train.df[-22])
test_scaled = scale(k_test.df[-22])

k_test_pred = knn(train = train_scaled,
                    test = test_scaled,
                    cl= k_train.df$Diabetes_binary,
                    k=10)

k_actual = k_test.df$Diabetes_binary
k_cm = table(k_actual, k_test_pred)
k_cm
k_accuracy = sum(diag(k_cm))/length(k_actual)
k_accuracy


True_Class = factor(c(0, 0, 1, 1))
Predicted_Class = factor(c(0, 1, 0, 1))
Y = c(4920, 2149, 1673, 5396)
df = data.frame(True_Class, Predicted_Class, Y)


ggplot(data =  df, mapping = aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1, size=6) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw(base_size = 12) + theme(legend.position = "none")



## k nearest neighbor with previously identified best variables from glm

excluded_vars = c(8,9,12,13,15,20,21)

k_vars_train = k_train.df[,-excluded_vars]
k_vars_test = k_test.df[,-excluded_vars]
k_vars_train_scaled = scale(k_vars_train[-15])
k_vars_test_scaled = scale(k_vars_test[-15])

k_test_pred2 = knn(train = k_vars_train_scaled,
                  test = k_vars_test_scaled,
                  cl= k_vars$Diabetes_binary,
                  k=10)

k_actual = k_test.df$Diabetes_binary
k_cm2 = table(k_actual, k_test_pred2);k_cm2
k_accuracy2 = sum(diag(k_cm2))/length(k_actual);k_accuracy2


## k nearest neighbor using caret package

set.seed(2)
trainIndex = createDataPartition(k_diabetes_data$Diabetes_binary,
                                  times=1,
                                  p=.8,
                                  list=FALSE)
caret_k_train = k_diabetes_data[trainIndex, ]
caret_k_test = k_diabetes_data[-trainIndex, ]

preProcValues = preProcess(caret_k_train, method=c("center", "scale"))
trainTransformed = predict(preProcValues, caret_k_train)
testTransformed = predict(preProcValues, caret_k_test)

k_values = c(3,5,7)
tuneGrid = expand.grid(k=k_values)

knnModel = train(Diabetes_binary~.,
                 data=trainTransformed,
                 method="knn",
                 trControl = trainControl(method= "cv"),
                 tuneGrid= tuneGrid) 

best_k_model = knn3(Diabetes_binary~.,
                    data= trainTransformed,
                    k=knnModel$bestTune$k)


k_predictions = predict(best_k_model, testTransformed, type = "class")

k_cm3 = confusionMatrix(k_predictions, testTransformed$Diabetes_binary); k_cm3

### Random Forest
# Installing package 
install.packages("caTools")	  
install.packages("randomForest") 

# Loading package 
library(caTools) 
library(randomForest) 

# Splitting data in train and test data 
split <- sample.split(diabetes, SplitRatio = 0.8) 

train <- subset(diabetes, split == "TRUE")
test <- subset(diabetes, split == "FALSE") 

# Fitting Random Forest to the train dataset 
set.seed(120) # Setting seed 
classifier_RF = randomForest(x = train[-22], 
                             y = train$Diabetes_binary, 
                             ntree = 500) 

classifier_RF 

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = test[-22]) 

# Confusion Matrix 
confusion_mtx = table(test[, 22], y_pred) 
confusion_mtx 

# Plotting model 
plot(classifier_RF) 

# Importance plot 
var_importance <- importance(classifier_RF)
var_importance <- var_importance[order(-var_importance)]

# Variable importance plot 
varImpPlot(classifier_RF)

#### lasso regression ####

xfactors = model.matrix(Diabetes_binary ~ ., train.df)[,-1]
y = train.df$Diabetes_binary

set.seed(123)

cv.lasso = cv.glmnet(xfactors, y, family = "binomial", alpha = 1, lambda = NULL)

cv.lasso$lambda.min
coef(cv.lasso, cv.lasso$lambda.min)

cv.lasso$lambda.1se
coef(cv.lasso, cv.lasso$lambda.1se)

lasso_model = glmnet(xfactors, y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.1se)

lasso_train = model.matrix(Diabetes_binary ~., train.df)[,-1]
lasso_train_probs = lasso_model %>% predict(newx = lasso_train)
lasso_train_predict = ifelse(lasso_train_probs > 0.5, 1, 0)

train_observed = train.df$Diabetes_binary
mean(lasso_train_predict == train_observed)