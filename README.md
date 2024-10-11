## Hi there 👋

<!--
**Febriansyah45/Febriansyah45** is a ✨ _special_ ✨ repository because its `README.md` (this file) appears on your GitHub profile.

Here are some ideas to get you started:

- 🔭 I’m currently working on Conservation and Wildlife Science
- 🌱 I’m currently learning Remote Sensing and python
- 👯 I’m looking to collaborate on everyon
- 🤔 I’m looking for help with ...
- 💬 Ask me about ...
- 📫 How to reach me: ...
- 😄 Pronouns: ...
- ⚡ Fun fact: ...
-->
library(dplyr)
library(ggplot2)
library(tidyverse)

library(data.table)
library(readxl)
library(compare)

# Read the CSV file
CCB <- fread("Point.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)

# View the data
View(CCB)

# Read the Excel file (assuming it has column names)
env <- read_excel("Data_Env.xlsx", col_names = TRUE)


# Full join CCB and env based on a common column (e.g., "ID") dan menggabungkan dengan kolom bio
ccb1 <- merge(CCB, env, by = "Plot_ID", all = TRUE)  # Replace "ID" with the actual common column name
view(ccb1)

#data kehadiran biodiversity pada setiap plot

bio<- fread("bio.csv",sep=";",dec=",",stringsAsFactors = FALSE)
view(bio)

bio1<-bio%>%
  select(1:4)

view(bio1)
  
#Clean the data and correction(Manipulating)

ccb2<-merge(ccb1, bio1, by = "Plot_ID", all = TRUE)
view(ccb2)

#explorasi data
cor1<-cor(ccb2[,5:21])
print(cor1)
plot(cor1)

install.packages("corrplot")
library(corrplot)
corrplot(cor(cor1))

#
summary(ccb2)

plot(ccb2$Water_Temperature,ccb2$n_zooplankton)
plot(ccb2$DO,ccb2$n_zooplankton)



# Visualize data with scatter plot
# Using the heightweight data set, create a new column that indicates if the
# child weighs < 100 or >= 100 pounds. We'll save this modified dataset as 'hw'.
#hw <- heightweight %>%
 # mutate(weightgroup = ifelse(weightLb < 100, "< 100", ">= 100"))

# Specify shapes with fill and color, and specify fill colors that includes an empty (NA) color
ggplot(ccb2, aes(x =n_zooplankton, y = Water_Temperature , shape = Note)) +
  geom_point(size = 2.5) 

ggplot(ccb2, aes(x = Water_Temperature, y = n_fitoplankton , shape = Note)) +
  geom_point(size = 2.5)

#boxplot

ggplot(ccb2, aes(x = Water_Temperature, y = n_zooplankton, color = Note)) +
  geom_boxplot()

ggplot(ccb2, aes(x = Water_Temperature, y = n_fitoplankton, color = Note)) +
  geom_boxplot()

a<-ggplot(ccb1, aes(x = DO , y = Water_Temperature)) +
  geom_point() +
  labs(title = "Plot of Water Temperature vs. DO")


# Add linear regression line and correlation coefficient
a1 <- a + geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  annotate("text", x = min(ccb2$DO), y = max(ccb1$Water_Temperature),
           label = paste("Correlation Coefficient (r) =", round(cor(ccb1$DO, ccb1$Water_Temperature), 3)),
           hjust = 0, vjust = 1)

a1
