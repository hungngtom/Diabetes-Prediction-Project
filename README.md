# Diabetes-Prediction-Project
### Authors: Hung Nguyen, Paul Draper, Ramya Meduri

## Project Introduction and Overview
### a. About the project
Diabetes is one of the most common chronic diseases in the US, impacting millions. It results in severe complications such as heart disease, vision loss, lower-limb amputation, and kidney disease. The CDC estimates that 1 in 5 diabetics, and roughly 8 in 10 pre-diabetics, are unaware of their risk. Diabetes also places a massive burden on the economy, with undiagnosed diabetes approaching $400 billion annually.
This project aims at identifying individuals at risk of diabetes based on a variety of health indicators, a step towards patient risk assessment. We believe the results or models of this project can be of great use to improve insurance underwriting, health policies for diabetes prevention and management.
### b. Overview
The dataset used for our analysis, found here,  is from Kaggle and was originally sourced by the Centers for Disease Control and Prevention (CDC). It is generated via the Behavioral Risk Factor Surveillance System which surveys respondents over the phone and is collected annually. The data, collected in 2015, contains 70,692 survey responses with 21 attributes and is a 50/50 split between those with and without a diabetes diagnosis. 
By analyzing a dataset with features such as BMI, age, smoking status, physical activity, and more, we develop and train a supervised model to predict the likelihood of diabetes. Here is the full look of the indicators:
1. Diabetes_binary: 0 = no diabetes, 1 = prediabetes or diabetes
2. HighBP: 0 =  no high cholesterol, 1 = high cholesterol
3. Cholcheck: 0 = no cholesterol check in 5 years, 1 = yes cholesterol check in 5 years
4. BMI
5. Smoker: Have you smoked at least 100 cigarettes in your entire life? 0 = no, 1 = yes
6. Stroke: Have you ever had a stroke? 0 = no, 1 = yes
7. HeartDiseaseorAttack: 0 = no, 1 = yes
8. PhysActivity: Have you done any physical activity in the last 30 days? 0 = no, 1 = yes
9. Fruits: Consume fruits 1 or more per day? 0 = no, 1 = yes
10. Veggies: Consume vegetables 1 or more per day? 0 = no, 1 = yes
11. HvyAlcoholConsump: Heavy drinkers (adult men having > 14 drinks/week and adult women having > 7 drinks per week? 0 = no, 1 = yes
12. AnyHealthcare: Have any kind of health care coverage, including health insurance, prepaid plans such as HMO, etc. 0 = no, 1 = yes
13. NoDocbcCost: Was there a time in the past 12 months when you needed to see a doctor but could not because of cost? 0 = no, 1 = yes
14. GenHlth: Would you say that in general your health is: scale 1-5: 1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor
15. MenHlth: Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good? It is in days, scale will be between 0-30
16. PhysHlth: Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good? It is in days, scale will be between 0-30
17. DiffWalk: Do you have serious difficulty walking or climbing stairs? 0 = no, 1 = yes
18. Sex: 0 = female, 1 = male
19. Age: 13-level age category (scale 1-13): 1 = 18-24, 8 = 55-59, 13 = 80 or older
20. Education: Education level (scale 1-6): 1 = Never attended school or only kindergarten 2 = Grades 1 through 8
21. Income: Income scale (scale 1-8): 1 = less than $10,000 5 = less than $35,000 11 = $200,000 or more

The following project demonstrates:

* Data cleaning and preprocessing techniques.
* Exploratory data analysis (EDA) to understand the relationships within the data.
* Feature selection and engineering to prepare the data for modeling.
* Evaluation and comparison of different machine learning models, including Logistic Regression and Random Forest, to find the most effective predictor of diabetes risk.

This work is part of an ongoing exploration into how data science can be applied to healthcare data to predict disease risk and support early intervention strategies.
