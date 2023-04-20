# loading packages
library(readxl)
library(lavaan)

# reading in dataset
data <- read.csv(file = "Data2.csv", sep = ";")
dataXL <- read_excel('dataset_study2_OSF.xlsx')

# 3 modellen testen: 
# 1) Social comparison -> Contingent self-esteem -> Depression, Anxiety, Stress
# 2) Social comparison -> Global self-esteem    -> Depression, Anxiety, Stress
# 3) Social comparison -> Contingent self-esteem -> General self-esteem -> Depression, Anxiety, Stress 

#########################
# Mediation Model 1 & 2 #
#########################
###################################################################################################
# Model 1 ----
# Social comparison = independent variable (X) = COMF
# Contingent self-esteem = mediator (M) = CSS
# Depression, Anxiety, Stress = dependent variables (Y) = Stress, Anxiety, Depression
# Making the model for Stress ----
model_Stress <- ' # direct effect
                   Stress ~ c*COMF
                  # mediator
                  CSS ~ a*COMF
                  Stress ~ b*CSS
                  # indirect effect (a*b)
                  ab := a*b
                  # total effect
                  total := c + (a*b)
                '
fit_Stress <- sem(model_Stress, data = data)
summary(fit_Stress)

# Making the model for Anxiety ----
model_Anxiety <- ' # direct effect
                    Anxiety ~ c*COMF
                  # mediator
                    CSS ~ a*COMF
                    Anxiety ~ b*CSS
                  # indirect effect (a*b)
                    ab := a*b
                  # total effect
                    total := c + (a*b)
                '
fit_Anxiety <- sem(model_Anxiety, data = data)
summary(fit_Anxiety)

# Making the model for Depression ----
model_Depression <- ' # direct effect
                        Depression ~ c*COMF
                      # mediator
                        CSS ~ a*COMF
                        Depression ~ b*CSS
                      # indirect effect (a*b)
                        ab := a*b
                      # total effect
                        total := c + (a*b)
                    '
fit_Depression <- sem(model_Depression, data = data)
summary(fit_Depression)


###################################################################################################
# Model 2 ---- 
# Social comparison = independent variable (X) = COMF
# Global self-esteem = mediator (M) = RSES
# Depression, Anxiety, Stress = dependent variables (Y) = Stress, Anxiety, Depression
# Making the model for Stress ----
model_Stress <- ' # direct effect
                   Stress ~ c*COMF
                  # mediator
                  RSES ~ a*COMF
                  Stress ~ b*RSES
                  # indirect effect (a*b)
                  ab := a*b
                  # total effect
                  total := c + (a*b)
                '
fit_Stress <- sem(model_Stress, data = data)
summary(fit_Stress)

# Making the model for Anxiety ----
model_Anxiety <- ' # direct effect
                    Anxiety ~ c*COMF
                  # mediator
                    RSES ~ a*COMF
                    Anxiety ~ b*RSES
                  # indirect effect (a*b)
                    ab := a*b
                  # total effect
                    total := c + (a*b)
                '
fit_Anxiety <- sem(model_Anxiety, data = data)
summary(fit_Anxiety)

# Making the model for Depression ----
model_Depression <- ' # direct effect
                        Depression ~ c*COMF
                      # mediator
                        RSES ~ a*COMF
                        Depression ~ b*RSES
                      # indirect effect (a*b)
                        ab := a*b
                      # total effect
                        total := c + (a*b)
                    '
fit_Depression <- sem(model_Depression, data = data)
summary(fit_Depression)

###################################################################################################
######################
# SEM Model 1, 2 & 3 #
######################
# Model 1: COMF -> CSS -> Stress ----
# Fitting the model 
# model1_SEM_Stress <- ' # measurement model 
#                        COMF   =~ COMF1 + COMF2 + COMF3 + COMF4 + COMF5 + 
#                                  COMF6 + COMF7 + COMF8 + COMF9 + COMF10 + COMF11
                    #     CSS    =~ CSS1 + CSS2 + CSS3 + CSS4 + CSS5 + CSS6 + CSS7 + 
                    #               CSS8 + CSS9 + CSS10 + CSS11 + CSS12 + CSS13 + CSS14 + CSS15
                    #     Stress =~ DASS15 + DASS16 + DASS17 + DASS18 + DASS19 + DASS20 + DASS21
                    #   # regressions 
                    #     CSS ~ COMF
                    #     Stress ~ CSS 
                    # '
model1_SEM_Stress <- ' # regressions 
                        CSS ~ COMF
                        DASS_stress ~ CSS 
                    '
fit <- sem(model1_SEM_Stress, data = dataXL)
summary(fit, standardized = TRUE)

# Model 1: COMF -> CSS -> Anxiety ----
# Fitting the model 
model1_SEM_Anxiety <- ' # regressions 
                        CSS ~ COMF
                        DASS_anxiety ~ CSS 
                    '
fit <- sem(model1_SEM_Anxiety, data = dataXL)
summary(fit, standardized = TRUE)

# Model 1: COMF -> CSS -> Depression ----
# Fitting the model 
model1_SEM_Depression <- ' # regressions 
                        CSS ~ COMF
                        DASS_depression ~ CSS 
                    '
fit <- sem(model1_SEM_Depression, data = dataXL)
summary(fit, standardized = TRUE)

###################################################################################################
# Model 2: COMF -> RSES -> Stress ----
# Fitting the model 
# model2_SEM_Stress <- ' # measurement model 
#                        COMF   =~ COMF1 + COMF2 + COMF3 + COMF4 + COMF5 + 
#                                  COMF6 + COMF7 + COMF8 + COMF9 + COMF10 + COMF11
#     CSS    =~ CSS1 + CSS2 + CSS3 + CSS4 + CSS5 + CSS6 + CSS7 + 
#               CSS8 + CSS9 + CSS10 + CSS11 + CSS12 + CSS13 + CSS14 + CSS15
#     Stress =~ DASS15 + DASS16 + DASS17 + DASS18 + DASS19 + DASS20 + DASS21
#   # regressions 
#     CSS ~ COMF
#     Stress ~ CSS 
# '
model2_SEM_Stress <- ' # regressions 
                        RSES ~ COMF
                        DASS_stress ~ RSES 
                    '
fit <- sem(model2_SEM_Stress, data = dataXL)
summary(fit, standardized = TRUE)

# Model 2: COMF -> RSES -> Anxiety ----
model2_SEM_Anxiety <- ' # regressions 
                            RSES ~ COMF
                            DASS_anxiety ~ RSES 
                    '
fit <- sem(model2_SEM_Anxiety, data = dataXL)
summary(fit, standardized = TRUE)

# Model 2: COMF -> RSES -> Depression ----
# Fitting the model 
model2_SEM_Depression <- ' # regressions 
                            RSES ~ COMF
                            DASS_depression ~ RSES 
                    '
fit <- sem(model2_SEM_Depression, data = dataXL)
summary(fit, standardized = TRUE)

###################################################################################################
# Model 3: COMF -> CSS -> RSES -> Stress ----
# Fitting the model 
# model3_SEM_Stress <- ' # measurement model 
#                        COMF   =~ COMF1 + COMF2 + COMF3 + COMF4 + COMF5 + 
#                                  COMF6 + COMF7 + COMF8 + COMF9 + COMF10 + COMF11
#     CSS    =~ CSS1 + CSS2 + CSS3 + CSS4 + CSS5 + CSS6 + CSS7 + 
#               CSS8 + CSS9 + CSS10 + CSS11 + CSS12 + CSS13 + CSS14 + CSS15
#     Stress =~ DASS15 + DASS16 + DASS17 + DASS18 + DASS19 + DASS20 + DASS21
#   # regressions 
#     CSS ~ COMF
#     Stress ~ CSS 
# '
model3_SEM_Stress <- ' # regressions 
                        CSS ~ COMF
                        RSES ~ CSS
                        DASS_stress ~ RSES 
                        
                    '
fit <- sem(model3_SEM_Stress, data = dataXL)
summary(fit, standardized = TRUE)

# Model 3: COMF -> CSS -> RSES -> Anxiety ----
# Fitting the model 
model3_SEM_Anxiety <- ' # regressions 
                            CSS ~ COMF
                            RSES ~ CSS
                            DASS_anxiety ~ RSES 
                        
                    '
fit <- sem(model3_SEM_Anxiety, data = dataXL)
summary(fit, standardized = TRUE)

# Model 3: COMF -> CSS -> RSES -> Depression ----
# Fitting the model 
model3_SEM_Depression <- ' # regressions 
                              CSS ~ COMF
                              RSES ~ CSS
                              DASS_depression ~ RSES 
                        
                    '
fit <- sem(model3_SEM_Depression, data = dataXL)
summary(fit, standardized = TRUE)

###################################################################################################
###################################
# Multivariate SEM Model 1, 2 & 3 # 
###################################
# Model 1: COMF -> CSS -> Stress + Anxiety + Depression ----
model1_SEM <- ' # regressions 
                  CSS ~ COMF
                  DASS_stress+DASS_anxiety+DASS_depression ~ CSS 
              '
fit <- sem(model1_SEM, data = dataXL)
summary(fit, standardized = TRUE)

# Model 2: COMF -> RSES -> Stress + Anxiety + Depression ----
model2_SEM <- ' # regressions 
                  RSES ~ COMF
                  DASS_stress+DASS_anxiety+DASS_depression ~ RSES 
              '
fit <- sem(model2_SEM, data = dataXL)
summary(fit, standardized = TRUE)

# Model 3: COMF -> CSS -> RSES -> Stress + Anxiety + Depression ----
# Fitting the model 
model3_SEM <- ' # regressions 
                    CSS ~ COMF
                    RSES ~ CSS
                    DASS_stress+DASS_anxiety+DASS_depression ~ RSES 
              '
fit <- sem(model3_SEM, data = dataXL)
summary(fit, standardized = TRUE)

###################################################################################################
#########
# Plots #
#########
# relation COMF and CSS
plot(x = data$COMF, y = data$CSS)
cor(x = data$COMF, y = data$CSS)
# relation COMF and RSES 
plot(x = data$COMF, y = data$RSES)
cor(x = data$COMF, y = data$RSES)
# relation CSS and RSES 
plot(x = data$CSS, y = data$RSES)
cor(x = data$CSS, y = data$RSES) #-0.38
# relation CSS and Stress/Anxiety/Depression 
plot(x = data$CSS, y = data$Stress)
cor(x = data$CSS, y = data$Stress) #0.34

plot(x = data$CSS, y = data$Anxiety)
cor(x = data$CSS, y = data$Anxiety) #0.17

plot(x = data$CSS, y = data$Depression)
cor(x = data$CSS, y = data$Depression) #0.29

# relation RSES and Stress/Anxiety/Depression 
plot(x = data$RSES, y = data$Stress)
cor(x = data$RSES, y = data$Stress) #-0.53

plot(x = data$RSES, y = data$Anxiety)
cor(x = data$RSES, y = data$Anxiety) #-0.45

plot(x = data$RSES, y = data$Depression)
cor(x = data$RSES, y = data$Depression) #-0.68
