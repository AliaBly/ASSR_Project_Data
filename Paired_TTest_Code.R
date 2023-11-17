#I will be running a paired t-test on the completed pre- and post-examinations. 
#3 students have been dropped due to completing only one exam (i.e. pre OR post), so the sample size is 38 individuals. 

#Before computing each t-test, I will validate the normality of the difference between pairs. 

library(readxl)
library(tidyr)
library(dplyr)
library(PairedData)

df <- read_excel(path = "/Users/aliably/Desktop/ASSR_Project/Student_Scoring_Data_ASSR.xlsx")

macro1 <- df$Macroscopic_Pre
macro2 <- df$Macroscopic_Post

summary(macro1)
sd(macro1)
summary(macro2)
sd(macro2)

normality_data_1 <- data.frame( 
  group = rep(c("macro1", "macro2"), each = 38),
  weight = c(macro1,  macro2)
)

print(normality_data_1) #confirming this populated correctly 

#The null hypothesis is that the mean differences are normally distributed. 
d1 <- with(normality_data_1, 
           weight[group == "macro1"] - weight[group == "macro2"])
shapiro.test(d1)

t.test(macro1, macro2, paired = TRUE)

#t= -5.8033, p = 1.557 e-07

micro1 <- df$Microscopic_Pre
micro2 <- df$Microscopic_Post

summary(micro1)
sd(micro1)
summary(micro2)
sd(micro2)

normality_data_2 <- data.frame( 
  group = rep(c("micro1", "micro2"), each = 38),
  weight = c(micro1,  micro2)
)

print(normality_data_2) #confirming this populated correctly 

#The null hypothesis is that the mean differences are normally distributed. 
d2 <- with(normality_data_2, 
           weight[group == "micro1"] - weight[group == "micro2"])
shapiro.test(d2)

#This p-value is marginally larger than 0.05 (aligns with my SAS findings), so we could use a paired t-test here. 

t.test(micro1, micro2, paired = TRUE)
#t = -6.5516, p = 6.758 e-09

symbolic1 <- df$Symbolic_Pre
symbolic2 <- df$Symbolic_Post

summary(symbolic1)
sd(symbolic1)
summary(symbolic2)
sd(symbolic2)

normality_data_3 <- data.frame( 
  group = rep(c("symbolic1", "symbolic2"), each = 38),
  weight = c(symbolic1,  symbolic2)
)

print(normality_data_3) #confirming this populated correctly 

#The null hypothesis is that the mean differences are normally distributed. 
d3 <- with(normality_data_3, 
           weight[group == "symbolic1"] - weight[group == "symbolic2"])
shapiro.test(d3) #favors the alternative hypothesis

t.test(symbolic1, symbolic2, paired = TRUE)
#t = -3.5638, p = 0.0006446

synthesis1 <- df$Synthesis_Pre
synthesis2 <- df$Synthesis_Post

summary(synthesis1)
sd(synthesis1)
summary(synthesis2)
sd(synthesis2)

normality_data_4 <- data.frame( 
  group = rep(c("synthesis1", "synthesis2"), each = 38),
  weight = c(synthesis1,  synthesis2)
)

print(normality_data_4) #confirming this populated correctly 

#The null hypothesis is that the mean differences are normally distributed. 
d4 <- with(normality_data_4, 
           weight[group == "synthesis1"] - weight[group == "synthesis2"])
shapiro.test(d4) #favors the alternative hypothesis

t.test(synthesis1, synthesis2, paired = TRUE)
#t = -5.3653, p = 8.946 e-07

total1 <- df$Total_Pre
total2 <- df$Total_Post

summary(total1)
sd(total1)
summary(total2)
sd(total2)

normality_data_5 <- data.frame( 
  group = rep(c("total1", "total2"), each = 38),
  weight = c(total1,  total2)
)

print(normality_data_5) #confirming this populated correctly 

#The null hypothesis is that the mean differences are normally distributed. 
d5 <- with(normality_data_5, 
           weight[group == "total1"] - weight[group == "total2"])
shapiro.test(d5) #We accept the null in this case. 

t.test(total1, total2, paired = TRUE)
#t = -8.1241, p = 9.542 e-10