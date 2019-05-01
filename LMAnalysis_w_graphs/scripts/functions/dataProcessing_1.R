#Labor Market Study 1
#Author: Terrence Pope
#Purpose: Analyze Labor Market Data and produce graphs

library(plyr)
library(tidyverse)
library(psych)
library(Hmisc)

getwd()
Data <- read.csv("~/Desktop/LMData/LMAnalysis_w_graphs/data/HiringPerceptionsData.csv")

#Data <- Data[1:437,1:65] #May be needed to clean up excess cells in the data set. Adjust range accordingly

#Greenwald Lab Room
Data$Room <- Data$Room %>%
  recode_factor("T" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6)

Data$Room <- factor(Data$Room,
                         levels = c(2,3,4,5,6),
                         labels = c("T", "C", "D", "E", "F"))

label(Data$Room) <- "Greenwald Lab Room"

#Condition
Data$Condition <- Data$Condition %>%
  recode_factor("C" = 1, "A" = 2, "S" = 3)

Data$Condition <- factor(Data$Condition,
                         levels = c(1,2,3),
                         labels = c("Control", "American", "Status"))

label(Data$Condition) <- "Condition"

#Applicant 1 Race
Data$res1_race <- Data$res1_race %>% 
  recode_factor("AF" = 1, "AA" = 2)

Data$res1_race <- factor(Data$res1_race,
                         levels = c(1,2),
                         labels = c("AF", "AA"))

label(Data$res1_race) <- "Applicant 1 Race"

#Applican 2 Race
Data$res2_race <- Data$res2_race %>% 
  recode_factor("AF" = 1, "AA" = 2)

Data$res2_race <- factor(Data$res2_race,
                         levels = c(1,2),
                         labels = c("AF", "AA"))

label(Data$res2_race) <- "Applicant 2 Race"

#Applicant Names for Resume 1
Data$res1_name <- as.factor(Data$res1_name)
Data$res1_mancheck <- as.factor(Data$res1_mancheck)

Data$res1_name <- ifelse(Data$res1_race=="AA", dplyr::recode(Data$res1_name, `1` = 5, `2` = 6, `3` = 7, `4` = 8),  Data$res1_name)

Data$res1_name <- factor(Data$res1_name,
                         levels = c(1,2,3,4,5,6,7,8),
                         labels = c("Deshawn", "Terell", "Tyrone", "Lamar", "Zhang Wei", "Chen", "Dong", "Wang Xiu"))

label(Data$res1_name) <- "Applicant 1 Name"

#Data$res1_name[Data$res1_race=="AA"] <- recode(Data$res1_name[Data$res1_race=="AA"], `1` = 5, `2` = 6, `3` = 7, `4` = 8) 

#Applicant Names for Resume 2
Data$res2_name <- as.factor(Data$res2_name)
Data$res2_mancheck <- as.factor(Data$res2_mancheck)

Data$res2_name <- ifelse(Data$res2_race=="AA", dplyr::recode(Data$res2_name, `1` = 5, `2` = 6, `3` = 7, `4` = 8),  Data$res2_name)

Data$res2_name <- factor(Data$res2_name,
                         levels = c(1,2,3,4,5,6,7,8),
                         labels = c("Deshawn", "Terell", "Tyrone", "Lamar", "Zhang Wei", "Chen", "Dong", "Wang Xiu"))

label(Data$res2_name) <- "Applicant 2 Name"

#Resume 1 Version
Data$res1_ver <- Data$res1_ver %>%
  recode_factor("A" = 1, "B" = 2)

Data$res1_ver <- factor(Data$res1_ver,
                        levels = c(1,2),
                        labels = c("A", "B"))

label(Data$res1_ver) <- "Resume 1 Version"

#Resume 2 Version
Data$res2_ver <- Data$res2_ver %>%
  recode_factor("A" = 1, "B" = 2)

Data$res2_ver <- factor(Data$res2_ver,
                        levels = c(1,2),
                        labels = c("A", "B"))

label(Data$res2_ver) <- "Resume 2 Version"

#Hired Race Information
Data$hired_race <- Data$hired_race %>% 
  recode_factor("AF" = 1, "AA" = 2)

Data$hired_race <- factor(Data$hired_race,
                          levels = c(0,1,2,3,4),
                          labels = c("No Response","AF", "AA","Neither Selected","Both Selected"))

label(Data$hired_race) <- "Race of Hired Applicant"

#Recode Questions
Data$res1_Q9 <- Data$res1_Q9 %>%
  dplyr::recode("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7)

Data$res2_Q1 <- Data$res2_Q1 %>%
  dplyr::recode("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7)


#Order of Appearance
#This is kind of nonsense. I can't remember what I wanted to look into by creating this variable, but either way... here it is!
Data$AF_hire <- ifelse(Data$res1_race=="AF", dplyr::recode(Data$hired_race, "AF" = 1, "AA" = 2),  0)

Data$AF_hire <- factor(Data$AF_hire,
                          levels = c(0,1,2),
                          labels = c("Not First","Appeared 1st","AF 1st/AA hired"))

Data$AA_hire <- ifelse(Data$res1_race=="AA", dplyr::recode(Data$hired_race, "AA" = 1, "AF" = 2),  0)

Data$AA_hire <- factor(Data$AA_hire,
                       levels = c(0,1,2),
                       labels = c("Not First","Appeared 1st","AA 1st/AF hired"))

label(Data$AF_hire) <- "Order of Appearance (African American)"
label(Data$AA_hire) <- "Order of Appearance (Asian American)"

#Manipulation Check
Data$res1_mc <- ifelse(Data$res1_race=="AF", dplyr::recode(Data$res1racecheck, "1" = 1, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0, "7" = 0, "8" = 0),  0)

Data$res1_mc <- ifelse(Data$res1_race=="AA", dplyr::recode(Data$res1racecheck, "1" = 0, "2" = 1, "3" = 0, "4" = 0, "5" = 0, "6" = 0, "7" = 0, "8" = 0), Data$res1_mc)

label(Data$res1_mc) <- "Resume 1 Race Manipulation Check"

Data$res2_mc <- ifelse(Data$res2_race=="AF", dplyr::recode(Data$res2racecheck, "1" = 1, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0, "7" = 0, "8" = 0),  0)

Data$res2_mc <- ifelse(Data$res2_race=="AA", dplyr::recode(Data$res2racecheck, "1" = 0, "2" = 1, "3" = 0, "4" = 0, "5" = 0, "6" = 0, "7" = 0, "8" = 0), Data$res2_mc)


label(Data$res2_mc) <- "Resume 2 Race Manipulation Check"

#Adjust class of question variables
Data$res1_Q1 <- as.numeric(Data$res1_Q1)
Data$res1_Q2 <- as.numeric(Data$res1_Q2)
Data$res1_Q3 <- as.numeric(Data$res1_Q3)
Data$res1_Q4 <- as.numeric(Data$res1_Q4)
Data$res1_Q5 <- as.numeric(Data$res1_Q5)
Data$res1_Q6 <- as.numeric(Data$res1_Q6)
Data$res1_Q7 <- as.numeric(Data$res1_Q7)
Data$res1_Q8 <- as.numeric(Data$res1_Q8)
Data$res1_Q10 <- as.numeric(Data$res1_Q10)
Data$res1_Q11 <- as.numeric(Data$res1_Q11)
Data$res1_Q12 <- as.numeric(Data$res1_Q12)
Data$res1_Q13 <- as.numeric(Data$res1_Q13)

Data$res2_Q2 <- as.numeric(Data$res2_Q2)
Data$res2_Q3 <- as.numeric(Data$res2_Q3)
Data$res2_Q4 <- as.numeric(Data$res2_Q4)
Data$res2_Q5 <- as.numeric(Data$res2_Q5)
Data$res2_Q6 <- as.numeric(Data$res2_Q6)
Data$res2_Q7 <- as.numeric(Data$res2_Q7)
Data$res2_Q8 <- as.numeric(Data$res2_Q8)
Data$res2_Q9 <- as.numeric(Data$res2_Q9)
Data$res2_Q10 <- as.numeric(Data$res2_Q10)
Data$res2_Q11 <- as.numeric(Data$res2_Q11)
Data$res2_Q12 <- as.numeric(Data$res2_Q12)
Data$res2_Q13 <- as.numeric(Data$res2_Q13)

#Participant Race
Data$PRACE <- ifelse(Data$p_race_af == 1 & Data$p_race_w + Data$p_race_aa + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_af, "1" = 1), 9)

Data$PRACE <- ifelse(Data$p_race_aa == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_aa, "1" = 2), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_ha == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_aa + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_ha, "1" = 3), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_an == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_aa + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_an, "1" = 4), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_pi == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_aa + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_pi, "1" = 5), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_mea == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_aa + Data$p_race_o == 0, dplyr::recode(Data$p_race_mea, "1" = 6), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_w == 1 & Data$p_race_aa + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_w, "1" = 7), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_o == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_aa == 0, dplyr::recode(Data$p_race_o, "1" = 8), Data$PRACE)

Data$PRACE <- factor(Data$PRACE,
                       levels = c(1,2,3,4,5,6,7,8,9),
                       labels = c("African American","Asian American","Hispanic American","Alaskan Native/Native American","Pacific Islander","Middle Eastern American","White American","Other",NA))

label(Data$PRACE) <- "Participant Race"

#Qualification Index
Data$AF_Qual <- if_else(Data$res1_race == "AF", rowMeans(Data[,11:13], na.rm = T), rowMeans(Data[,28:30], na.rm = T))

label(Data$AF_Qual) <- "Qualification (AF)"

Data$AA_Qual <- if_else(Data$res1_race == "AA", rowMeans(Data[,11:13], na.rm = T), rowMeans(Data[,28:30], na.rm = T))

label(Data$AA_Qual) <- "Qualification (AA)"

#Status Index
Data$AF_Stat <- if_else(Data$res1_race == "AF", rowMeans(Data[,14:16], na.rm = T), rowMeans(Data[,31:33], na.rm = T))

label(Data$AF_Stat) <- "Status (AF)"

Data$AA_Stat <- if_else(Data$res1_race == "AA", rowMeans(Data[,14:16], na.rm = T), rowMeans(Data[,31:33], na.rm = T))

label(Data$AA_Stat) <- "Status (AA)"

#American Index
Data$AF_Amer <- if_else(Data$res1_race == "AF", rowMeans(Data[,17:19], na.rm = T), rowMeans(Data[,34:36], na.rm = T))

label(Data$AF_Amer) <- "Americanness (AF)"

Data$AA_Amer <- if_else(Data$res1_race == "AA", rowMeans(Data[,17:19], na.rm = T), rowMeans(Data[,34:36], na.rm = T))

label(Data$AA_Amer) <- "Americanness (AA)"

#recode conservative measure
Data$res1_Q13 <- dplyr::recode(Data$res1_Q13, "7" = 1, "6" = 2, "5" = 3, "4" = 4, "3" = 5, "2" = 6, "1" = 7)
Data$res2_Q13 <- dplyr::recode(Data$res2_Q13, "7" = 1, "6" = 2, "5" = 3, "4" = 4, "3" = 5, "2" = 6, "1" = 7)

#Recode Questions
Data$AF_Q1 <- if_else(Data$res1_race == "AF", Data$res1_Q1, Data$res2_Q1)
Data$AF_Q2 <- if_else(Data$res1_race == "AF", Data$res1_Q2, Data$res2_Q2)
Data$AF_Q3 <- if_else(Data$res1_race == "AF", Data$res1_Q3, Data$res2_Q3)
Data$AF_Q4 <- if_else(Data$res1_race == "AF", Data$res1_Q4, Data$res2_Q4)
Data$AF_Q5 <- if_else(Data$res1_race == "AF", Data$res1_Q5, Data$res2_Q5)
Data$AF_Q6 <- if_else(Data$res1_race == "AF", Data$res1_Q6, Data$res2_Q6)
Data$AF_Q7 <- if_else(Data$res1_race == "AF", Data$res1_Q7, Data$res2_Q7)
Data$AF_Q8 <- if_else(Data$res1_race == "AF", Data$res1_Q8, Data$res2_Q8)
Data$AF_Q9 <- if_else(Data$res1_race == "AF", Data$res1_Q9, Data$res2_Q9)
Data$AF_Q10 <- if_else(Data$res1_race == "AF", Data$res1_Q10, Data$res2_Q10)
Data$AF_Q11 <- if_else(Data$res1_race == "AF", Data$res1_Q11, Data$res2_Q11)
Data$AF_Q12 <- if_else(Data$res1_race == "AF", Data$res1_Q12, Data$res2_Q12)
Data$AF_Q13 <- if_else(Data$res1_race == "AF", Data$res1_Q13, Data$res2_Q13)

Data$AA_Q1 <- if_else(Data$res1_race == "AA", Data$res1_Q1, Data$res2_Q1)
Data$AA_Q2 <- if_else(Data$res1_race == "AA", Data$res1_Q2, Data$res2_Q2)
Data$AA_Q3 <- if_else(Data$res1_race == "AA", Data$res1_Q3, Data$res2_Q3)
Data$AA_Q4 <- if_else(Data$res1_race == "AA", Data$res1_Q4, Data$res2_Q4)
Data$AA_Q5 <- if_else(Data$res1_race == "AA", Data$res1_Q5, Data$res2_Q5)
Data$AA_Q6 <- if_else(Data$res1_race == "AA", Data$res1_Q6, Data$res2_Q6)
Data$AA_Q7 <- if_else(Data$res1_race == "AA", Data$res1_Q7, Data$res2_Q7)
Data$AA_Q8 <- if_else(Data$res1_race == "AA", Data$res1_Q8, Data$res2_Q8)
Data$AA_Q9 <- if_else(Data$res1_race == "AA", Data$res1_Q9, Data$res2_Q9)
Data$AA_Q10 <- if_else(Data$res1_race == "AA", Data$res1_Q10, Data$res2_Q10)
Data$AA_Q11 <- if_else(Data$res1_race == "AA", Data$res1_Q11, Data$res2_Q11)
Data$AA_Q12 <- if_else(Data$res1_race == "AA", Data$res1_Q12, Data$res2_Q12)
Data$AA_Q13 <- if_else(Data$res1_race == "AA", Data$res1_Q13, Data$res2_Q13)

#write.csv(Data, file = "Hiring Perceptions [Cleaned].csv")

#Data <- Data %>% filter(PRACE == "White American")

###Analysis

#Freqs
Descriptives <- apply(Data, 2, table) 
summary(Descriptives)
#running a table on all columns (2)in the raw data

#Warmth Index
Data$AF_Warm <- rowMeans(Data[,86:87], na.rm = T)

Data$AF_Warm <- round(Data$AF_Warm, 2)

label(Data$AF_Warm) <- "Warmth (AF)"

Data$AA_Warm <- rowMeans(Data[,99:100], na.rm = T)

Data$AA_Warm <- round(Data$AA_Warm, 2)

label(Data$AA_Warm) <- "Warmth (AA)"

#Lib/Prog Index
Data$AF_Lib <- rowMeans(Data[,88:89], na.rm = T)

Data$AF_Lib <- round(Data$AF_Lib, 2)

label(Data$AF_Lib) <- "Lib/Prog (AF)"

Data$AA_Lib <- rowMeans(Data[,101:102], na.rm = T)

Data$AA_Lib <- round(Data$AA_Lib, 2)

label(Data$AA_Lib) <- "Lib/Prog (AA)"

write.csv(Data, "hiringPerceptions_cleaned.csv")

