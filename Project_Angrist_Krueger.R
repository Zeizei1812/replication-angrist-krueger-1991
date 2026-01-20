library(tidyverse)
library(ggplot2)                 
library(readr)
library(here)
library(lubridate)
library(dplyr)
library(gridExtra)
library(stargazer)
library(foreign)    ## for read.dta
library(AER)        ## for ivreg
library(ggplot2)
#preliminaries 

pums <- read.table(paste("QOB.raw",sep=""),
                   header           = FALSE,
                   stringsAsFactors = FALSE)
colnames(pums)[c(1,2,4,5,6,9:13,16,18,19:21,24,25,27)] <- c("AGE", "AGEQ", "EDUC",
                                                            "ENOCENT","ESOCENT", "LWKLYWGE", 
                                                            "MARRIED", "MIDATL", "MT", "NEWENG", "CENSUS", "QOB", "RACE",
                                                            "SMSA", "SOATL", "WNOCENT", "WSOCENT", "YOB")
pums <- as_tibble(pums)
pums

pums %>%
  mutate(cohort = factor(1*(YOB<=39 & YOB >=30) +
                           2*(YOB<=49 & YOB >=40),
                         levels=c(1,2), labels=c("30-39","40-49")) ) -> pums
pums

#
ymd(paste("19",30,4 * 3, sep=""),truncated = 2)

pums <- as_tibble(pums)

#Cohort creation 
pums %>%
  mutate(cohort = factor(1*(YOB<=39 & YOB >=30) +
                           2*(YOB<=49 & YOB >=40),
                         levels=c(1,2), labels=c("30-39","40-49")) ) -> pums 
#Changing the date format 
pums <- pums %>%
  mutate(YOB = as.Date(paste("19", YOB, "01-02", sep = ""), format = "%Y%d-%m"))

#Calculating the average of EDUC and LWKLYWGE
educ_mean <- pums %>%
  group_by(YOB, QOB) %>%
  summarise(mean_EDUC = mean(EDUC),
            mean_LWKLYWGE = mean(LWKLYWGE),
            .groups = 'keep')

#Sort by year of birth 
educ_mean <- educ_mean[order(educ_mean$YOB), ]

#Creation of a variable associating year and quarter of birth
educ_mean <- educ_mean %>%
  mutate(YQOB = paste(YOB, QOB, sep = "-"))


#Creation of 2 graphics 
p1 <-ggplot(educ_mean, aes(x = YQOB, y = mean_EDUC, label = as.character(QOB), color = as.character(QOB))) +
  geom_point(size = 3, shape = 16) +
  geom_line(aes(group = 1), color = "black") +
  geom_text(aes(label = QOB), color = "white", size = 2) +
  scale_color_manual(values = c("1" = "red", "2" = "black", "3" = "black", "4" = "black"), guide = "none") +
  labs(x = "Year of birth", y = "Years of completed education", title = "Figure 1 : Average education by quarter of birth") +
  scale_x_discrete(breaks = c("1930-02-01-1","1935-02-01-1", "1940-02-01-1",
                              "1945-02-01-1", "1949-02-01-4"),
                   labels = c("1930-02-01-1" = "1930","1935-02-01-1" ="1935", 
                              "1940-02-01-1" = "1940","1945-02-01-1" = "1945", 
                              "1949-02-01-4" = "1950"))


p2 <-ggplot(educ_mean, aes(x = YQOB, y = mean_LWKLYWGE, label = as.character(QOB), color = as.character(QOB))) +
  geom_point(size = 3, shape = 16) +
  geom_line(aes(group = 1), color = "black") +
  geom_text(aes(label = QOB), color = "white", size = 2) +
  scale_color_manual(values = c("1" = "red", "2" = "black", "3" = "black", "4" = "black"), guide = "none") +
  labs(x = "Year of birth", y = "Log weekly earnings", title = "Figure 2 : Average weekly wage by quarter of birth") +
  scale_x_discrete(breaks = c("1930-02-01-1","1935-02-01-1",
                              "1940-02-01-1","1945-02-01-1", "1949-02-01-4"),
                   labels = c("1930-02-01-1" = "1930","1935-02-01-1" ="1935",
                              "1940-02-01-1" = "1940","1945-02-01-1" = "1945",
                              "1949-02-01-4" = "1950"))

p1
p2



#Table 5 : 
  # Step 1: Filtering the dataset
  pums %>%
  filter(cohort == "30-39") -> pums.tab5

# Step 2: Defining formulas for regressions
# We no longer need the age variables like AGEQ and I(AGEQ^2)
exo3 = "RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT +
        WNOCENT + SOATL + ESOCENT + WSOCENT + MT"

# Step 3: Create region variable by combining the regional variables
pums.tab5 %>%
  mutate(region = as.factor(paste(NEWENG, MIDATL, ENOCENT, WNOCENT, SOATL, ESOCENT, WSOCENT, MT, sep = "_"))) -> pums.tab6

# Step 4: Creating YOB_factor for Year of Birth fixed effects
pums.tab6 %>%
  mutate(YOB_factor = as.factor(YOB)) -> pums.tab6

# Step 5: OLS Regressions
reg1 = lm(LWKLYWGE ~ EDUC + YOB_factor, data = pums.tab6)
reg3 = lm(LWKLYWGE~EDUC + AGEQ + I(AGEQ^2) + YOB_factor ,pums.tab6)
reg5 = lm(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region, data = pums.tab6)
reg7 = lm(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + AGEQ + I(AGEQ^2) + YOB_factor + region, data = pums.tab6)

# Step 6: Creating the OLS Summary Table
stargazer(
  reg1, reg3,reg5, reg7,
  dep.var.caption = "",
  dep.var.labels = "",
  omit = c("YOB_factor", "region", "Constant"), # omit the fixed effects for year of birth and region
  add.lines = list(
    c("9 Year-of-birth dummies", "Yes", "Yes", "Yes"),
    c("8 Region-of-residence dummies", "No", "Yes", "Yes")
  ),
  star.cutoffs = NA,
  keep.stat = c("n", "rsq"),
  no.space = TRUE,
  header = FALSE,
  column.labels = c("OLS", "OLS", "OLS","OLS"),
  title = "OLS Estimation of the Return to Education for Men Born 1930-1939: 1980 Census",
  type = "text"
)

# Step 7: Creating QOB_Year Interaction
pums.tab6 <- pums.tab6 %>%
  mutate(QOB_Year = paste(QOB, YOB, sep = "_"))

# Step 8: TSLS Regressions (Instrumental Variable Regressions)
reg_IV2 <- ivreg(LWKLYWGE ~ EDUC + YOB_factor | QOB_Year + YOB_factor, data = pums.tab6)
reg_IV4 <- ivreg(LWKLYWGE ~ EDUC + AGEQ +  I(AGEQ^2) + YOB_factor  |
                   QOB_Year + AGEQ +  I(AGEQ^2) + YOB_factor, data = pums.tab6)
reg_IV6 <- ivreg(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region | QOB_Year + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region, data = pums.tab6)
reg_IV8 <- ivreg(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + AGEQ +  I(AGEQ^2) + region | QOB_Year + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region, data = pums.tab6)

# Step 9: Creating the TSLS Summary Table
stargazer(
  reg_IV2,reg_IV4, reg_IV6, reg_IV8,
  dep.var.caption = "",
  dep.var.labels = "",
  omit = c("YOB_factor", "region", "Constant"), # omit the fixed effects for year of birth and region
  add.lines = list(
    c("9 Year-of-birth dummies", "Yes", "Yes", "Yes"),
    c("8 Region-of-residence dummies", "No", "Yes", "Yes")
  ),
  star.cutoffs = NA,
  keep.stat = c("n", "rsq"),
  no.space = TRUE,
  header = FALSE,
  column.labels = c("TSLS", "TSLS", "TSLS","TSLS"),
  title = "TSLS Estimation of the Return to Education for Men Born 1930-1939: 1980 Census",
  type = "text"
)

# Step 10: Combining OLS and TSLS Results into One Table
stargazer(
  reg1, reg_IV2, reg3, reg_IV4, reg5, reg_IV6, reg7, reg_IV8,
  dep.var.caption = "",
  dep.var.labels = "",
  omit = c("YOB_factor", "region", "Constant","Observations","R2"),
  add.lines = list(
    c("9 Year-of-birth dummies", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    c("8 Region-of-residence dummies", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")
  ),
  star.cutoffs = NA,
  keep.stat = c("n", "rsq"),
  no.space = TRUE,
  header = FALSE,
  column.labels = c("OLS", "TSLS", "OLS", "TSLS", "OLS", "TSLS", "OLS", "TSLS"),
  title = "OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1930-1939: 1980 CENSUS",
  type = "text",
  covariate.labels = c(
    "Years of education",
    "Age",
    "Age-squared",
    "Race (1 = Black)",
    "SMSA (1 = center city)",
    "Married (1 = married)"
  ),digits = 4
)

########





# # 1.3/WALD ESTIMATOR 


###############

pums.tab5 <- pums.tab5 %>%
  mutate(wald_dum = as.numeric(QOB == 2 | QOB == 3 | QOB == 4))


mean_ln_wage_q1 <- mean(pums.tab5$LWKLYWGE[pums.tab5$QOB == 1], na.rm = TRUE)
mean_ln_wage_q2_4 <- mean(pums.tab5$LWKLYWGE[pums.tab5$wald_dum == 1], na.rm = TRUE)
mean_education_q1 <- mean(pums.tab5$EDUC[pums.tab5$QOB == 1], na.rm = TRUE)
mean_education_q2_4 <- mean(pums.tab5$EDUC[pums.tab5$wald_dum == 1], na.rm = TRUE)

# Calculating Wald estimator
wald_estimator <- (mean_ln_wage_q1 - mean_ln_wage_q2_4) / (mean_education_q1 - mean_education_q2_4)

# Régression OLS pour le retour de l'éducation
reg_ols <- lm(LWKLYWGE ~ EDUC, data = pums.tab5)

# Créer le tableau de résultats
data3 <- data.frame(
  ln_wkly_wage = c(mean_ln_wage_q1, mean_ln_wage_q2_4, mean_ln_wage_q1 - mean_ln_wage_q2_4),
  Education = c(mean_education_q1, mean_education_q2_4, mean_education_q1 - mean_education_q2_4),
  Wald_estimator = c(NA, NA, wald_estimator),
  OLS_return_of_education = c(NA, NA, coef(reg_ols)["EDUC"]),
  row.names = c("Born in 1st quarter of year", 
                "Born in 2nd, 3rd or 4th quarter of year", 
                "Difference (std. error) (1)-(2)")
)

# Displaying the final table with stargazer
stargazer(data3,
          title = "Panel B: Wald Estimates for 1980 Census-Men Born 1930-1939",
          type = "text",
          summary = FALSE,
          header = TRUE,
          rownames = TRUE,
          flip = TRUE,digits=4)



####################
# Test 1
library(dplyr)
library(AER)
library(stargazer)

# Step 1 : Filtering the database
pums %>%
  filter(cohort == "30-39") -> pums.tab5

# Step 2 : Creating a region and YOB_factor variable
pums.tab5 %>%
  mutate(
    region = as.factor(paste(NEWENG, MIDATL, ENOCENT, WNOCENT, SOATL, ESOCENT, WSOCENT, MT, sep = "_")),
    YOB_factor = as.factor(YOB)
  ) -> pums.tab6

# Step 3 : Creating an interaction QOB_Year
pums.tab6 <- pums.tab6 %>%
  mutate(QOB_Year = paste(QOB, YOB, sep = "_"))

# Step 4 : Regressions OLS
reg1 <- lm(LWKLYWGE ~ EDUC + YOB_factor, data = pums.tab6)
reg3 <- lm(LWKLYWGE ~ EDUC + AGEQ + I(AGEQ^2) + YOB_factor, data = pums.tab6)
reg5 <- lm(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region, data = pums.tab6)
reg7 <- lm(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + AGEQ + I(AGEQ^2) + YOB_factor + region, data = pums.tab6)

# Step 5 : Regressions TSLS
reg_IV2 <- ivreg(LWKLYWGE ~ EDUC + YOB_factor | QOB_Year + YOB_factor, data = pums.tab6)
reg_IV4 <- ivreg(LWKLYWGE ~ EDUC + AGEQ + I(AGEQ^2) + YOB_factor |
                   QOB_Year + AGEQ + I(AGEQ^2) + YOB_factor, data = pums.tab6)
reg_IV6 <- ivreg(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region |
                   QOB_Year + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region, data = pums.tab6)
reg_IV8 <- ivreg(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + AGEQ + I(AGEQ^2) + YOB_factor + region |
                   QOB_Year + factor(RACE) + factor(SMSA) + factor(MARRIED) + AGEQ + I(AGEQ^2) + YOB_factor + region, data = pums.tab6)

# Step 6 : Combining OLS and TSLS 
stargazer(
  reg1, reg_IV2, reg3, reg_IV4, reg5, reg_IV6, reg7, reg_IV8,
  dep.var.caption = "",
  dep.var.labels = "Log Weekly Wage", # Titre de la variable dépendante
  column.labels = c("OLS", "TSLS", "OLS", "TSLS", "OLS", "TSLS", "OLS", "TSLS"), # Étiquettes des colonnes
  covariate.labels = c(
    "Years of education",
    "Age",
    "Age-squared",
    "Race (1 = Black)",
    "SMSA (1 = center city)",
    "Married (1 = married)"
  ), # Étiquettes des variables
  omit = c("YOB_factor", "region", "Constant"), # Supprimer les effets fixes et la constante
  add.lines = list(
    c("9 Year-of-birth dummies", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    c("8 Region-of-residence dummies", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")
  ), # Ajouter des notes sur les dummies
  keep.stat = c("n", "rsq"), # Statistiques incluses
  star.cutoffs = NA, # Pas de seuils pour les étoiles de significativité
  no.space = TRUE, # Éliminer les espaces inutiles
  header = FALSE, # Supprimer l'en-tête
  title = "OLS and TSLS Estimates of the Return to Education for Men Born 1930-1939: 1980 Census",
  type = "text", # Format de sortie : texte
  digits = 4 # Précision des chiffres
)
###############################
# Test 2 
library(dplyr)
library(AER)
library(stargazer)


pums %>%
  filter(cohort == "30-39") -> pums.tab5

pums.tab5 %>%
  mutate(
    region = as.factor(paste(NEWENG, MIDATL, ENOCENT, WNOCENT, SOATL, ESOCENT, WSOCENT, MT, sep = "_")),
    YOB_factor = as.factor(YOB)
  ) -> pums.tab6


pums.tab6 <- pums.tab6 %>%
  mutate(QOB_Year = paste(QOB, YOB, sep = "_"))

reg1 <- lm(LWKLYWGE ~ EDUC + YOB_factor, data = pums.tab6)
reg3 <- lm(LWKLYWGE ~ EDUC + AGEQ + I(AGEQ^2) + YOB_factor, data = pums.tab6)
reg5 <- lm(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region, data = pums.tab6)
reg7 <- lm(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + AGEQ + I(AGEQ^2) + YOB_factor + region, data = pums.tab6)


reg_IV2 <- ivreg(LWKLYWGE ~ EDUC + YOB_factor | QOB_Year + YOB_factor, data = pums.tab6)
reg_IV4 <- ivreg(LWKLYWGE ~ EDUC + AGEQ + I(AGEQ^2) + YOB_factor |
                   QOB_Year + AGEQ + I(AGEQ^2) + YOB_factor, data = pums.tab6)
reg_IV6 <- ivreg(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region |
                   QOB_Year + factor(RACE) + factor(SMSA) + factor(MARRIED) + YOB_factor + region, data = pums.tab6)
reg_IV8 <- ivreg(LWKLYWGE ~ EDUC + factor(RACE) + factor(SMSA) + factor(MARRIED) + AGEQ + I(AGEQ^2) + YOB_factor + region |
                   QOB_Year + factor(RACE) + factor(SMSA) + factor(MARRIED) + AGEQ + I(AGEQ^2) + YOB_factor + region, data = pums.tab6)

#  Chi² values for TSLS regressions
chi2_IV2 <- summary(reg_IV2)$waldtest["chi-squared"] # Chi² pour reg_IV2
chi2_IV4 <- summary(reg_IV4)$waldtest["chi-squared"] # Chi² pour reg_IV4
chi2_IV6 <- summary(reg_IV6)$waldtest["chi-squared"] # Chi² pour reg_IV6
chi2_IV8 <- summary(reg_IV8)$waldtest["chi-squared"] # Chi² pour reg_IV8


stargazer(
  reg1, reg_IV2, reg3, reg_IV4, reg5, reg_IV6, reg7, reg_IV8,
  dep.var.caption = "Log Weekly Wage",
  dep.var.labels.include = FALSE,
  column.labels = c("OLS", "TSLS", "OLS", "TSLS", "OLS", "TSLS", "OLS", "TSLS"),
  covariate.labels = c(
    "Years of education",
    "Age",
    "Age-squared",
    "Race (1 = Black)",
    "SMSA (1 = center city)",
    "Married (1 = married)"
  ),
  omit = c("YOB_factor", "region", "Constant","djusted R2","Residual Std. Error"),
  add.lines = list(
    c("9 Year-of-birth dummies", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    c("8 Region-of-residence dummies", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes"),
    c("χ² [dof]", "", round(chi2_IV2, 1), "", round(chi2_IV4, 1), "", round(chi2_IV6, 1), "", round(chi2_IV8, 1))
    
  ),
  star.cutoffs = NA,
  no.space = TRUE,
  header = FALSE,
  title = "OLS and TSLS Estimates of the Return to Education for Men Born 1930-1939: 1980 Census",
  type = "text",
  digits = 4
)
# rm(list = ls())


##################
#Question 3
library(tidyverse)
library(ipumsr)

# get API-key: https://account.ipums.org/api_keys
# Save key in .Renviron for use across sessions
mykey <- "59cba10d8a5da536fc06b59da94951ad25164db68d140378b99e004b"
set_ipums_api_key(mykey, save = TRUE)

# the ACS 2000 (1-in-750 sample) has id: us2000d
usa_ext_def <- define_extract_usa(
  description = "Extract for individuals born between 1930 and 1939",
  samples = c("us1980a"),
  variables = c("BIRTHYR", "EDUC", "AGE", "RACE", "BIRTHQTR", "INCWAGE")
)

usa_ext_submitted <- submit_extract(usa_ext_def)

usa_ext_complete <- wait_for_extract(usa_ext_submitted)

filepath <- download_extract(usa_ext_submitted,
                             download_dir = "~")

ddi <- read_ipums_ddi(filepath)
dat2000 <- read_ipums_micro(ddi)

# Convert column names to lowercase
colnames(dat2000) <- str_to_lower(colnames(dat2000))

# Filter out individuals born between 1930 and 1939
# We ensure 1930 < birthyr < 1939
dat2000_clean <- dat2000 %>%
  select(birthyr, educ, age, race, birthqtr, incwage)%>%
  filter(birthyr > 1930 & birthyr < 1950)

# Glimpse the filtered data
glimpse(dat2000_clean)




educ_mean <- dat2000_clean %>%
  group_by(birthyr, birthqtr) %>%
  summarise(mean_EDUC = mean(educ),
            mean_LWKLYWGE = mean(incwage),
            .groups = 'keep')

#Sort by year of birth 
educ_mean <- educ_mean[order(educ_mean$birthyr), ]

#Creation of a variable associating year and quarter of birth
# Updated to include proper YQOB formatting
educ_mean <- educ_mean %>%
  mutate(YQOB = paste(birthyr, birthqtr, sep = "-Q"))  # Update format to YYYY-Q

# Updated graphics with corrected x-axis values
p3 <- ggplot(educ_mean, aes(x = YQOB, y = mean_EDUC, label = as.character(birthqtr), color = as.character(birthqtr))) +
  geom_point(size = 3, shape = 16) +
  geom_line(aes(group = 1), color = "black") +
  geom_text(aes(label = birthqtr), color = "white", size = 2) +
  scale_color_manual(values = c("1" = "red", "2" = "black", "3" = "black", "4" = "black"), guide = "none") +
  labs(x = "Year of birth", y = "Years of completed education", title = "Figure 1 : Average education by quarter of birth") +
  scale_x_discrete(breaks = c("1930-Q1", "1935-Q1", "1940-Q1", "1945-Q1", "1949-Q4"),
                   labels = c("1930-Q1" = "1930", "1935-Q1" = "1935", "1940-Q1" = "1940", "1945-Q1" = "1945", "1949-Q4" = "1950"))

p4 <- ggplot(educ_mean, aes(x = YQOB, y = mean_LWKLYWGE, label = as.character(birthqtr), color = as.character(birthqtr))) +
  geom_point(size = 3, shape = 16) +
  geom_line(aes(group = 1), color = "black") +
  geom_text(aes(label = birthqtr), color = "white", size = 2) +
  scale_color_manual(values = c("1" = "red", "2" = "black", "3" = "black", "4" = "black"), guide = "none") +
  labs(x = "Year of birth", y = "Weekly earnings", title = "Figure 2 : Average weekly wage by quarter of birth") +
  scale_x_discrete(breaks = c("1930-Q1", "1935-Q1", "1940-Q1", "1945-Q1", "1949-Q4"),
                   labels = c("1930-Q1" = "1930", "1935-Q1" = "1935", "1940-Q1" = "1940", "1945-Q1" = "1945", "1949-Q4" = "1950"))
p3
p4
grid.arrange(p3, p4, nrow = 2)


