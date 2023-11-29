#I will be running a paired t-test on the completed pre- and post-examinations. 
#3 students have been dropped due to completing only one exam (i.e. pre OR post), so the sample size is 38 individuals. 

#Before computing each t-test, I will validate the normality of the difference between pairs. 

library(readxl) #To read in our data
library(tidyr)
library(dplyr)
library(PairedData) #My packages for the analysis

library(ggplot2) #My packages for the figures (density plots and histograms)
library(ggpubr)
library(RColorBrewer) #This package allows for easy color customization
library(rcompanion) #This package provides easy options for making histograms with a curve imposed on top

df <- read_excel(path = "/Users/aliably/Desktop/ASSR_Project/Student_Scoring_Data_ASSR.xlsx")

macro1 <- df$Macroscopic_Pre
macro2 <- df$Macroscopic_Post

#First, I will generate summary statistics for these variables. 
summary(macro1)
sd(macro1)
summary(macro2)
sd(macro2)

#I will now check the normality of the paired differences, as this is an assumption for the paired t-test. 
normality_data_1 <- data.frame( 
  group = rep(c("macro2", "macro1"), each = 38),
  weight = c(macro1,  macro2)
)

print(normality_data_1) #confirming this populated correctly 

#The null hypothesis is that the mean differences are normally distributed. If the p-value is less than 0.05, we cannot assume normality.
d1 <- with(normality_data_1, 
           weight[group == "macro1"] - weight[group == "macro2"])
shapiro.test(d1) #Running the Shapiro-Wilk test of normality. 

t.test(macro1, macro2, paired = TRUE) #Running the paired t-test. 

######

#The procedure above will be replicated for each dimension. 

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

print(normality_data_2) 

d2 <- with(normality_data_2, 
           weight[group == "micro1"] - weight[group == "micro2"])
shapiro.test(d2)

t.test(micro1, micro2, paired = TRUE)

######

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

print(normality_data_3) 

d3 <- with(normality_data_3, 
           weight[group == "symbolic1"] - weight[group == "symbolic2"])
shapiro.test(d3) #favors the alternative hypothesis

t.test(symbolic1, symbolic2, paired = TRUE)

######

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

print(normality_data_4)

d4 <- with(normality_data_4, 
           weight[group == "synthesis1"] - weight[group == "synthesis2"])
shapiro.test(d4) #favors the alternative hypothesis

t.test(synthesis1, synthesis2, paired = TRUE)

######

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

print(normality_data_5) 

d5 <- with(normality_data_5, 
           weight[group == "total1"] - weight[group == "total2"])
shapiro.test(d5) #We accept the null in this case. 

t.test(total1, total2, paired = TRUE)

######

#To examine the normality of the paired differences, both density plots and histograms can be drawn. 
#This is an example of building density plots. 
difference_macro <- macro2 - macro1
difference_micro <- micro2 - micro1
difference_symbolic <- symbolic2 - symbolic1
difference_synthesis <- synthesis2 - synthesis1

brewer.pal(n = 8, name = "Blues") #printing the hex codes of the color palette I want to use 

plot1 <- ggplot(df, aes(x = difference_macro)) +
  geom_density(color="black", fill="#084594") +
  labs(x = "Difference Between Post- and Pre-Assessment Score (Macroscopic)", y = "Probability Density")

plot2 <-  ggplot(df, aes(x = difference_micro)) +
  geom_density(color="black", fill="#6BAED6" ) +
  labs(x = "Difference Between Post- and Pre-Assessment Score (Microscopic)", y = "Probability Density")


plot3 <-  ggplot(df, aes(x = difference_symbolic)) +
  geom_density(color="black", fill="#9ECAE1") + 
  labs(x = "Difference Between Post- and Pre-Assessment Score (Symbolic)", y = "Probability Density")

plot4 <-  ggplot(df, aes(x = difference_synthesis))+
  geom_density(color="black", fill= "#DEEBF7") +
  labs(x = "Difference Between Post- and Pre-Assessment Score (Synthesis)", y = "Probability Density")

compiled_figure <- ggarrange(
  plot1, plot2, plot3, plot4, labels = c("A", "B", "C", "D"),
  common.legend = TRUE, legend = "bottom")

annotate_figure(compiled_figure, top = text_grob("Distributions of the Paired Differences of Post- and Pre-Assessment Scores Across Dimensions", 
                                      color = "black", face = "bold", size = 14))


######

#This is an example of drawing histograms with a normal curve layered on top use base R and the plotNormalHistogram package. 

par(mfrow = c(2,2)) #This provides a base R alternative to ggarrange() in which you can specify 
#the rows and columns you want your figures to print in. 

hist1 <- plotNormalHistogram( difference_macro, prob = FALSE, col="#084594", border="black", 
                     xlab="Paired Differences of Post- vs. Pre-Assessment (Macroscopic)", "A",
                     xlim=c(-2,3),
                     breaks=seq(-2,3,1), #This sets the intervals for the x-axis. 
                     length = 10000, linecol="black", lwd=3 ) 


hist2 <- plotNormalHistogram( difference_micro, prob = FALSE, col="#6BAED6", border="black", 
                     xlab="Paired Differences of Post- vs. Pre-Assessment (Microscopic)", "B",
                     xlim=c(-2,3),
                     breaks=seq(-2,3,1),
                     length = 10000, linecol="black", lwd=3) 


hist3 <- plotNormalHistogram( difference_symbolic, prob = FALSE, col="#9ECAE1", border="black", 
                     xlab="Paired Differences of Post- vs. Pre-Assessment (Symbolic)", "C",
                     xlim=c(-2,3), 
                     breaks=seq(-2,3,1),
                     length = 10000, linecol="black", lwd=3 ) 


hist4 <- plotNormalHistogram( difference_synthesis, prob = FALSE, col="#DEEBF7", border="black", 
                     xlab="Paired Differences of Post- vs. Pre-Assessment (Synthesis)", "D",
                     xlim=c(-2,3), 
                     breaks=seq(-2,3,1),
                     length = 10000, linecol="black", lwd=3 ) 

mtext("Distributions of the Differences of Post- vs. Pre-Assessment Score Across Dimensions", side = 3, line = - 2, outer = TRUE)
#This will allow us to print a main title. 
