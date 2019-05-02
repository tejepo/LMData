#Labor Market Study 1
#Author: Terrence Pope
#Purpose: Analyze Labor Market Data and produce graphs

library(plyr)
library(tidyverse)
library(psych)
library(Hmisc)

getwd()
Data <- read.csv("~/Desktop/LMData/LMAnalysis_w_graphs/cleaned_data/hiringPerceptions_cleaned.csv")

##Reliability
#AF_Qual
psych::alpha(Data[,77:79], na.rm = T)
#AF_Stat
psych::alpha(Data[,80:82], na.rm = T)
#AF_Amer
psych::alpha(Data[,83:85], na.rm = T)
#AF_Warmth
psych::alpha(Data[,86:87], na.rm = T)
#AF_Lib/Prog
psych::alpha(Data[,88:89], na.rm = T)

#AA_Qual
psych::alpha(Data[,90:92], na.rm = T)
#AA_Stat
psych::alpha(Data[,93:95], na.rm = T)
#AA_Amer
psych::alpha(Data[,96:98], na.rm = T)
#AA_Warmth
psych::alpha(Data[,99:100], na.rm = T)
#AA_Lib/Prog
psych::alpha(Data[,101:102], na.rm = T)


#Convert Data to long form
Data.Anv <- Data %>%
dplyr::select(Subj, Condition, AF_Qual, AA_Qual, AF_Amer, AA_Amer, AF_Stat, AA_Stat)
 
library(reshape2)
 
Data.lng <- melt(Data.Anv, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

## code separate factors for Race and the Type of Measure
Data.lng$Race <- factor(substr(Data.lng$Index, 1, 2))
Data.lng$Measure <- factor(substr(Data.lng$Index, 4, 7))

## Make Subj into a factor
Data.lng$Subj <- factor(Data.lng$Subj)

## Since group is already a factor, does not need to be converted to factor

# ## load library and set options
# library(devtools)
# dev_mode()
# #install_github('mike-lawrence/ez')
# library(ez)
# dev_mode()
# options(contrasts = c("contr.sum","contr.poly"))
# 
# ## run the ANOVA
# ezANOVA(Data.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# ###Leaving the above code in because I have a sense of what's wrong but I'm not entirely sure whether or not there's anything useful in the code
# 
# #So far, it seems like by best bet is to put each within subject measure into it's own datafram and analyze them all separately.

require(lme4)
## load library and set options
library(devtools)
dev_mode()
install_github('mike-lawrence/ez')
library(ez)
dev_mode()
options(contrasts = c("contr.sum","contr.poly"))
library(MASS)
library(Hmisc)

#Chi Squared Analysis
library(ggplot2)

# 2-Way Frequency Table 
# 0 = AF and 1 = AA

#attach(Data)
mytable <- table(Data$hired_race,Data$Condition) # A will be rows, B will be columns 
mytable
mytable <- mytable[2:3,] #since the other rows are empty, we'll just select for the rows with data in them manually.
mytable

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

chi.props <- prop.table(mytable, 2)
chi.props <- as.data.frame(chi.props)
chi.names <- c("Race", "Condition", "Percentage")
names(chi.props) <- chi.names

mytable2 <- table(Data$hired_race)
mytable2 <- mytable2[-1]
mytable2 <- mytable2[-3:-4]
#prop.test(mytable2,428)


# 1-sample proportions test with continuity correction
# 
# data:  mytable2, null probability 0.5
# X-squared = 19.57, df = 1, p-value = 9.696e-06
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#   0.6026344 0.7568045
# sample estimates:
#   p 
# 0.6845638 

# smokers  <- 42
# apatients <- 10
# things <- as.table(c(smokers, patients))
# prop.test(things)
# things

# 1-sample proportions test with continuity correction
# 
# data:  things, null probability 0.5
# X-squared = 18.481, df = 1, p-value = 1.716e-05
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#   0.6703039 0.8991531
# sample estimates:
#   p 
# 0.8076923 

# 2-Way Frequency Table
mytable2 <- xtabs(~hired_race+Condition, data=Data)
mytable2 <- mytable2[2:3,]
ftable(mytable2) # print table 
summary(mytable2) # chi-square test of indepedence

# 2-Way Cross Tabulation (closest to spss Crosstabs)
install.packages("gmodels")
library(gmodels)
CrossTable(Data$hired_race, Data$Condition)

#Chi-Square Test
chisq.test(mytable)
#test independence of the row and column variable. By default, the p-value is calculated from the asymptotic chi-squared distribution of the test statistic. Optionally, the p-value can be derived via Monte Carlo simultation.

dev_mode()
devtools::install_github("cran/fifer")
library(fifer)
dev_mode()

mytable3 <- table(Data$Condition, Data$hired_race)
#mytable3 <- mytable3[,-1]
#mytable3 <- mytable3[,-3:-4]
mytable3
chisq.test(mytable3)

chisq.post.hoc(mytable3, test='chisq.test', popsInRows = F, control = "bonferroni")

install.packages("RVAideMemoire")
library(RVAideMemoire)
mytable
chisq.multcomp(mytable, p.method = "bonferroni")

chisq.bintest(hired_race~Condition, Data, alpha = 0.05, p.method = "bonferroni")

multinomial.multcomp(mytable, p.method = "bonferroni")

#Fisher Exact Test
mytable.m <- as.matrix(mytable)
fisher.test(mytable.m)
#exact test of independence. x is a two dimensional contingency table in matrix form.
# 
# my.array<-array(0,dim=c(10,5,6,8))
# 
# #Mantel-Haenszel test
# mantelhaen.test(mytable.m)
#a Cochran-Mantel-Haenszel chi-squared test of the null hypothesis that two nominal variables are conditionally independent in each stratum, assuming that there is no three-way interaction. x is a 3 dimensional contingency table, where the last dimension refers to the strata.

# Grouped Bar Plot
# counts <- table(Data$hired_race, Data$Condition)
# counts <- counts[2:3,]
# barplot(chi.props, main="Which Race is Hired by Condition",
#         xlab="Condition", col=c("turquoise","blue"),
#         legend = rownames(counts), beside=TRUE)
####

ggplot(chi.props, aes(x=Condition, y=Percentage, fill=Race)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  #geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Rating_norm-ci, ymax=Rating_norm+ci)) +
  coord_cartesian(ylim=c(0,1)) +
  #scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  #scale_y_continuous(breaks=seq(1:100)) +
  #facet_wrap(~Measure) +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="Applicant Race",
                    breaks=c("AF","AA"),
                    labels=c("African American", "Asian American"),
                    values=c("#4b2e83", "#b7a57a")) +
  #ggtitle("Ratings of Applicants on Measures of Qualification") +
  theme(text = element_text(size=40), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = c(0.85, .90))+
  geom_hline(yintercept=0) 

#######
# 
# # Association Plot Example
# library(vcd)
# assoc(mytable, shade=TRUE)
# 
# # Mosaic Plot Example
# mosaic(mytable, shade=TRUE, legend=TRUE)
# 
# #Within subjects mediation, are the differences in the american condition driven by the american stereotypes
# 
# #Correspondence Analysis method 1
# #install.packages("ca")
# library(ca)
# 
# mytable.cor <- table(Data$hired_race,Data$Condition)
# mytable.cor <- mytable.cor[-1,]
# mytable.cor[3,1] <- 1
# mytable.cor[3,2] <- 2
# mytable.cor[3,3] <- 3
# mytable.cor <- mytable.cor[-4,]
# rownames(mytable.cor) <- c("AF","AA","Condition")
# 
# fit <- ca(mytable.cor, nd=3)
# print(fit) # basic results 
# summary(fit) # extended results 
# plot(fit) # symmetric map
# plot(fit, mass = TRUE, contrib = "absolute", map =
#        "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map
# 
# 
# #Correspondence Analysis method 2
# #install.packages(c("FactoMineR", "factoextra"))
# library("FactoMineR")
# library("factoextra")
# 
# #head(housetasks)
# 
# mytable.cor <- table(Data$Condition, Data$hired_race)
# mytable.cor[1,4] <- 5
# mytable.cor[2,4] <- 35
# mytable.cor[3,4] <- 11
# colnames(mytable.cor) <- c("","AF","AA","Diff","")
# mytable.cor <- mytable.cor[,-1]
# mytable.cor <- mytable.cor[,-4]
# #mytable.cor <- mytable.cor[,-3]
# 
# 
# new.table <- as.matrix(table(Data$Condition, Data$res1_name, Data$res2_name, Data$hired_race))
# 
# 
# mytable.cor
# 
# library("gplots")
# 
# #balloon plot with the row totals hidden using the show.margins argument
# balloonplot(t(new.table), main ="Condition", xlab ="", ylab="",
#             label = FALSE, show.margins = FALSE)
# 
# balloonplot(mytable)
# 
# #is there a significant dependence between row and column categories?
# chisq <- chisq.test(mytable.cor)
# chisq
# #the row and the column variables are statistically significantly associated
# 
# mytable.cor2 <- mytable.cor[-1,]
# mytable.cor2 <- mytable.cor2[,-3]
# mytable.cor2


#Other Dependent measures
summary(Data)

Data <- na

#Qual (Hireability)
Data.Qual <- Data %>%
  dplyr::select(Subj, Condition, AF_Qual, AA_Qual)

Data.Qual.lng <- melt(Data.Qual, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.Qual.lng$Race <- factor(substr(Data.Qual.lng$Index, 1, 2))
Data.Qual.lng$Measure <- factor(substr(Data.Qual.lng$Index, 4, 7))
Data.Qual.lng$Subj <- factor(Data.Qual.lng$Subj)

ezANOVA(na.omit(Data.Qual.lng), dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

ezStats(na.omit(Data.Qual.lng), dv=Rating, wid=Subj, within=.(Race), between=Condition)

check <- pairwise.t.test(Data.Qual.lng$Rating,Data.Qual.lng$Race,paired=TRUE,p.adjust.method="bonferroni")

install.packages("phia")
library(phia)

aov.Race.Condition <- aov(Rating ~ Condition*Race + Error(Subj/Race), data=Data.Qual.lng)
summary(aov.Race.Condition)

model.tables(aov.Race.Condition, "means")

# mm.mod <- summary(aov.Race.Condition$`Subj:Race`)
# str(mm.mod)
# str(aov.Race.Condition)
# 
# str(aov.Race.Condition$`Subj:Race`[5])
# 
# testInteractions(aov.Race.Condition, pairwise = Race, fixed = Condition, adjustment="bonferroni") #doesn't work for this analysis but may be a good method for a "regular" linear model

#simple effects analysis
lsmeans::lsmeans(aov.Race.Condition, pairwise~Condition*Race)

#d
#5.12 - 5.42 / mean(1.1396, 1.0405)


####
# #Control subset
# data.Q.Control <- subset(Data.Qual.lng, Condition == "Control")
# #American subset
# data.Q.American <- subset(Data.Qual.lng, Condition == "American")
# #Status subset
# data.Q.Status <- subset(Data.Qual.lng, Condition == "Status")
# 
# anova(lm(Rating ~ Race, data.Q.Control))
# 
# anova(lm(Rating ~ Race, data.Q.American))

#anova(lm(Rating ~ Race, data.Q.Status))

ezPlot(na.omit(Data.Qual.lng),
       dv=.(Rating),
       wid=.(Subj),
       within=.(Race),
       between=.(Condition),
       x=.(Condition),
       split=.(Race),
       x_lab='Condition',
       y_lab='Qualification',
       split_lab='Race'#,
       #print_code=T)
)

#Amer
Data.Amer <- Data %>%
  dplyr::select(Subj, Condition, AF_Amer, AA_Amer)

Data.Amer.lng <- melt(Data.Amer, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.Amer.lng$Race <- factor(substr(Data.Amer.lng$Index, 1, 2))
Data.Amer.lng$Measure <- factor(substr(Data.Amer.lng$Index, 4, 7))
Data.Amer.lng$Subj <- factor(Data.Amer.lng$Subj)

ezANOVA(na.omit(Data.Amer.lng), dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

ezStats(na.omit(Data.Amer.lng), dv=Rating, wid=Subj, within=.(Race), between=Condition)

aov.Race.Condition <- aov(Rating ~ Condition*Race + Error(Subj/Race), data=Data.Amer.lng)
summary(aov.Race.Condition)

model.tables(aov.Race.Condition, "means")

#4.4 - 5.82 / mean(1.174, .969)

####
# #AF subset
# data.A.AF <- subset(Data.Amer.lng, Race == "AF")
# #AA subset
# data.A.AA <- subset(Data.Amer.lng, Race == "AA")
# 
# anova(lm(Rating ~ Condition, data.A.AF))
# 
# anova(lm(Rating ~ Condition, data.A.AA))
# 
# 
# ####
# #Control subset
# data.A.Control <- subset(Data.Amer.lng, Condition == "Control")
# #American subset
# data.A.American <- subset(Data.Amer.lng, Condition == "American")
# #Status subset
# data.A.Status <- subset(Data.Amer.lng, Condition == "Status")
# 
# anova(lm(Rating ~ Race, data.A.Control))
# 
# anova(lm(Rating ~ Race, data.A.American))
# 
# anova(lm(Rating ~ Race, data.A.Status))

# quickobject <- ezStats(Data.Amer.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition)
# quickobject <- quickobject %>% dplyr::select(Race, Mean, SD)
# aggregate(quickobject$SD~quickobject$Race, FUN=mean)

####

ezPlot(na.omit(Data.Amer.lng),
       dv=.(Rating),
       wid=.(Subj),
       within=.(Race),
       between=.(Condition),
       x=.(Condition),
       split=.(Race),
       x_lab='Condition',
       y_lab='Americanness',
       split_lab='Race'
)

#Stat
Data.Stat <- Data %>%
  dplyr::select(Subj, Condition, AF_Stat, AA_Stat)

#Data.Stat <- na.omit(Data.Stat)

Data.Stat.lng <- melt(Data.Stat, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.Stat.lng$Race <- factor(substr(Data.Stat.lng$Index, 1, 2))
Data.Stat.lng$Measure <- factor(substr(Data.Stat.lng$Index, 4, 7))
Data.Stat.lng$Subj <- factor(Data.Stat.lng$Subj)

ezANOVA(na.omit(Data.Stat.lng), dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

# lm.x <- lm(AF_Qual ~ AF_Q4 + AF_Q5 + AF_Q6, Data[Data[,6] == "Status",])
# summary(lm.x)
# 
# lm.y <- lm(AA_Qual ~ AA_Q4 + AA_Q5 + AA_Q6, Data[Data[,6] == "Status",])
# summary(lm.y)
# 
# Data.lng.s <- Data.lng %>%
#   dplyr::filter(Condition == "Status")
# 
# Data.lng.s <- Data.lng.s[,-3]
# Data.lng.s <- Data.lng.s[,-5]
# 
# names(Data.lng.s) <- c("Subj", "Condition", "Stat", "Race")
# 
# Data.lng.q <- Data.lng %>%
#   dplyr::filter(Measure == "Qual")
# 
# Data.lng.q <- Data.lng.q[,-3]
# Data.lng.q <- Data.lng.q[,-5]
# 
# names(Data.lng.q) <- c("Subj", "Condition", "Qual", "Race")
# 
# Data.lng.t <- left_join(Data.lng.s, Data.lng.q, by = c("Subj", "Condition", "Race"))
# 
# Data.lng.t <- Data.lng.t %>%
#   dplyr::select(Subj, Condition, Race, Qual, Stat)

# aov.Race.Condition <- aov(Rating ~ Race + Measure + Measure*Race + Error(Subj/Race), data=Data.lng.s)
# summary(aov.Race.Condition)


aov.Race.Condition <- aov(Rating ~ Condition*Race + Error(Subj/Race), data=Data.Stat.lng)
summary(aov.Race.Condition)

lsmeans::lsmeans(aov.Race.Condition, pairwise~Race*Measure)

ezPlot(na.omit(Data.Stat.lng),
       dv=.(Rating),
       wid=.(Subj),
       within=.(Race),
       between=.(Condition),
       x=.(Condition),
       split=.(Race),
       x_lab='Condition',
       y_lab='Status',
       split_lab='Race'
)

####
# #Control subset
# data.S.Control <- subset(Data.Stat.lng, Condition == "Control")
# #American subset
# data.S.American <- subset(Data.Stat.lng, Condition == "American")
# #Status subset
# data.S.Status <- subset(Data.Stat.lng, Condition == "Status")
# 
# anova(lm(Rating ~ Race, data.S.Control))
# 
# anova(lm(Rating ~ Race, data.S.American))
# 
# anova(lm(Rating ~ Race, data.S.Status))

#Warmth
Data.Warm <- Data %>%
  dplyr::select(Subj, Condition, AF_Warm, AA_Warm)

Data.Warm <- na.omit(Data.Warm)

Data.Warm.lng <- melt(Data.Warm, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.Warm.lng$Race <- factor(substr(Data.Warm.lng$Index, 1, 2))
Data.Warm.lng$Measure <- factor(substr(Data.Warm.lng$Index, 4, 7))
Data.Warm.lng$Subj <- factor(Data.Warm.lng$Subj)

ezANOVA(Data.Warm.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

ezStats(Data.Warm.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition)

aov.Race.Condition <- aov(Rating ~ Condition*Race + Error(Subj/Race), data=Data.Warm.lng)

summary(aov.Race.Condition)

model.tables(aov.Race.Condition, "means")

#5.02 - 5.07 / mean(.98, 1.04)

ezPlot(na.omit(Data.Warm.lng),
       dv=.(Rating),
       wid=.(Subj),
       within=.(Race),
       between=.(Condition),
       x=.(Condition),
       split=.(Race),
       x_lab='Condition',
       y_lab='Warmth',
       split_lab='Race'
)

#Liberal
Data.Lib <- Data %>%
  dplyr::select(Subj, Condition, AF_Lib, AA_Lib)

Data.Lib.lng <- melt(na.omit(Data.Lib), id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.Lib.lng$Race <- factor(substr(Data.Lib.lng$Index, 1, 2))
Data.Lib.lng$Measure <- factor(substr(Data.Lib.lng$Index, 4, 7))
Data.Lib.lng$Subj <- factor(Data.Lib.lng$Subj)

ezANOVA(Data.Lib.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

ezStats(Data.Lib.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition)

aov.Race.Condition <- aov(Rating ~ Condition*Race + Error(Subj/Race), data=Data.Lib.lng)

summary(aov.Race.Condition)

model.tables(aov.Race.Condition, "means")

#4.402 - 4.557 / mean(.77, .89)

ezPlot(Data.Lib.lng,
       dv=.(Rating),
       wid=.(Subj),
       within=.(Race),
       between=.(Condition),
       x=.(Condition),
       split=.(Race),
       x_lab='Condition',
       y_lab='Liberal',
       split_lab='Race'
)

#Conservative
Data.Con <- Data %>%
  dplyr::select(Subj, Condition, AF_Q13, AA_Q13)

Data.Con <- na.omit(Data.Con)

Data.Con.lng <- melt(Data.Con, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.Con.lng$Race <- factor(substr(Data.Con.lng$Index, 1, 2))
Data.Con.lng$Measure <- factor(substr(Data.Con.lng$Index, 4, 7))
Data.Con.lng$Subj <- factor(Data.Con.lng$Subj)

ezANOVA(na.omit(Data.Con.lng), dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

ezPlot(na.omit(Data.Con.lng),
       dv=.(Rating),
       wid=.(Subj),
       within=.(Race),
       between=.(Condition),
       x=.(Condition),
       split=.(Race),
       x_lab='Condition',
       y_lab='Conservative (Reverse Coded)',
       split_lab='Race'
)

#########################
#I have a hunch that this personality measure might predict differences in the groups because there seems to be a main effect of race (African Americans are seen as more friendly, moral, and liberal) so lets look at them
#########################
# 
# 
# #Qual + Warmth
# ezANOVA(Data.Qual.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# Data.Qual.W <- Data %>%
#   dplyr::select(Subj, Condition, AF_Qual, AA_Qual, AF_Warm, AA_Warm)
# 
# Data.Qual.W.lng <- melt(Data.Qual.W, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")
# 
# Data.Qual.W.lng$Race <- factor(substr(Data.Qual.W.lng$Index, 1, 2))
# Data.Qual.W.lng$Measure <- factor(substr(Data.Qual.W.lng$Index, 4, 7))
# Data.Qual.W.lng$Subj <- factor(Data.Qual.W.lng$Subj)
# 
# #Data.Qual.W.lng <- na.omit(Data.Qual.W.lng)
# 
# ezANOVA(Data.Qual.W.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# ezStats(Data.Qual.W.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition)
# 
# #Plot Main Effect of Condition
# ezPlot(na.omit(Data.Qual.W.lng), 
#        dv=Rating, 
#        wid=Subj, 
#        between=Condition,
#        x=Condition,
#        do_lines=F,
#        x_lab='Condition',
#        y_lab='Qualification')
# 
# #Plot Main Effect of Race
# ezPlot(na.omit(Data.Qual.W.lng), 
#        dv=Rating, 
#        wid=Subj, 
#        between=Race,
#        x=Race,
#        do_lines=F,
#        x_lab='Race',
#        y_lab='Qualification')
# 
# #Plot Race*Condition Interaction
# ezPlot(Data.Qual.W.lng,
#        dv=.(Rating),
#        wid=.(Subj),
#        within=.(Race),
#        between=.(Condition),
#        x=.(Condition),
#        split=.(Race),
#        x_lab='Condition',
#        y_lab='Qualification',
#        split_lab='Race',
#        print_code = T)
# 
# Data2 <- c(Data[,11:13],Data[,28:30],Data[,14:16],Data[,28:30],Data[,17:19],Data[,31:33],Data[,20:21],Data[,34:35])
# 
# Data2 <- as.data.frame(Data2)
# Data2$Qual <- rowMeans(Data2[,1:6], na.rm=T)
# Data2$Amer <- rowMeans(Data2[,7:12], na.rm=T)
# Data2$Stat <- rowMeans(Data2[,13:18], na.rm=T)
# Data2$Warm <- rowMeans(Data2[,19:22], na.rm=T)
# Data2$hired <- Data$hired_race
# 
# lm20 <- glm(as.numeric(Data$hired_race) ~ Warm*Qual + Warm*Amer + Warm*Stat + Warm*Qual*Amer*Stat, data = Data2)
# summary(lm20)
# 
# lm21 <- lm(as.numeric(Data$hired_race) ~ Qual*Amer*Stat, data = Data2)
# summary(lm21)
# 
# #Amer + Warmth
# ezANOVA(Data.Amer.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# Data.Amer.W <- Data %>%
#   dplyr::select(Subj, Condition, AF_Amer, AA_Amer, AF_Warm, AA_Warm)
# 
# Data.Amer.W.lng <- melt(Data.Amer.W, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")
# 
# Data.Amer.W.lng$Race <- factor(substr(Data.Amer.W.lng$Index, 1, 2))
# Data.Amer.W.lng$Measure <- factor(substr(Data.Amer.W.lng$Index, 4, 7))
# Data.Amer.W.lng$Subj <- factor(Data.Amer.W.lng$Subj)
# 
# Data.Amer.W.lng <- na.omit(Data.Amer.W.lng)
# 
# ezANOVA(Data.Amer.W.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# ezStats(Data.Amer.W.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition)
# 
# ezPlot(Data.Amer.W.lng,
#        dv=.(Rating),
#        wid=.(Subj),
#        within=.(Race),
#        between=.(Condition),
#        x=.(Condition),
#        split=.(Race),
#        x_lab='Condition',
#        y_lab='Americanness',
#        split_lab='Race',
#        print_code = F)
# 
# #Stat + Warmth
# ezANOVA(Data.Stat.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# Data.Stat.W <- Data %>%
#   dplyr::select(Subj, Condition, AF_Stat, AA_Stat, AF_Warm, AA_Warm)
# 
# Data.Stat.W.lng <- melt(Data.Stat.W, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")
# 
# Data.Stat.W.lng$Race <- factor(substr(Data.Stat.W.lng$Index, 1, 2))
# Data.Stat.W.lng$Measure <- factor(substr(Data.Stat.W.lng$Index, 4, 7))
# Data.Stat.W.lng$Subj <- factor(Data.Stat.W.lng$Subj)
# 
# Data.Stat.W.lng <- na.omit(Data.Stat.W.lng)
# 
# ezANOVA(Data.Stat.W.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# ezStats(Data.Stat.W.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition)
# 
# ezPlot(Data.Stat.W.lng,
#        dv=.(Rating),
#        wid=.(Subj),
#        within=.(Race),
#        between=.(Condition),
#        x=.(Condition),
#        split=.(Race),
#        x_lab='Condition',
#        y_lab='Status',
#        split_lab='Race',
#        print_code = F)
# 
# #Stat + Lib
# ezANOVA(Data.Stat.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# Data.Stat.L <- Data %>%
#   dplyr::select(Subj, Condition, AF_Stat, AA_Stat, AF_Q12, AA_Q12)
# 
# Data.Stat.L.lng <- melt(Data.Stat.L, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")
# 
# Data.Stat.L.lng$Race <- factor(substr(Data.Stat.L.lng$Index, 1, 2))
# Data.Stat.L.lng$Measure <- factor(substr(Data.Stat.L.lng$Index, 4, 7))
# Data.Stat.L.lng$Subj <- factor(Data.Stat.L.lng$Subj)
# 
# ezANOVA(Data.Stat.L.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# ezStats(Data.Stat.L.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition)
# 
# ezPlot(Data.Stat.L.lng,
#        dv=.(Rating),
#        wid=.(Subj),
#        within=.(Race),
#        between=.(Condition),
#        x=.(Condition),
#        split=.(Race),
#        x_lab='Condition',
#        y_lab='Status',
#        split_lab='Race',
#        print_code = F)
# 
# #Stat + Con
# ezANOVA(Data.Stat.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# Data.Stat.C <- Data %>%
#   dplyr::select(Subj, Condition, AF_Stat, AA_Stat, AF_Q13, AA_Q13)
# 
# Data.Stat.C.lng <- melt(Data.Stat.C, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")
# 
# Data.Stat.C.lng$Race <- factor(substr(Data.Stat.C.lng$Index, 1, 2))
# Data.Stat.C.lng$Measure <- factor(substr(Data.Stat.C.lng$Index, 4, 7))
# Data.Stat.C.lng$Subj <- factor(Data.Stat.C.lng$Subj)
# 
# ezANOVA(Data.Stat.C.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)
# 
# ezStats(Data.Stat.C.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition)
# 
# ezPlot(Data.Stat.C.lng,
#        dv=.(Rating),
#        wid=.(Subj),
#        within=.(Race),
#        between=.(Condition),
#        x=.(Condition),
#        split=.(Race),
#        x_lab='Condition',
#        y_lab='Status',
#        split_lab='Race',
#        print_code = F)

#Plots
library(ggthemes)

# #simple ggplot
# ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating)) +
#   geom_point()
# 
# #base plot
# plot(Rating ~ Condition, data = Data.Qual.lng, xlab = "Participant Condition", ylab = "Rating of Qualification", main = "How Qualified is the Applicant?", col = "red", cex = 1, pch = 16)
# 
# #more complex ggplot
# ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating)) + #this is where you set the x and y
#   geom_point(color = "red", size = 3) + #this is changing the color and size of the points on your plot. This draws points whereas other functions might draw lines
#   scale_y_continuous(limits = c(0, NA)) + #tells ggplot to set the bottom boundary of the y axis at 0
#   xlab("Condition") + ylab("Rating of Qualification") + #this provides labels
#   ggtitle("How Qualified is the Applicant?") + #this gives your plot a title
#   theme_excel()
# 
# #color the ggplot by race:
# ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating, color = Race)) + #this is coloring the dots based on your factor variable
#   geom_point() +
#   scale_y_continuous(limits = c(0, NA)) +
#   xlab("Condition") + ylab("Rating of Qualification") +
#   ggtitle("Qualification for Each Job by Race") +
#   theme_stata()
# 
# #Here's one with a few more arguments. the order doesn't really matter when you name them. However you don't have to name them as they have a default order that can be seen by asking ?ggplot or ?"argument." You might notice how there aren't many african american dots relative to the asian american dots...
# 
# #When dealing with categorical data you might want to "jitter" your data. This displays your data but takes each point and moves it randomly around it's set point. For example, you turn this:
# ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating, color = Race)) +
#   geom_point()

#which is pretty useless. 
#Into this:
ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating, color = Race)) +
  geom_point(position = position_jitter(width = 0.25, height = 0)) #for categorical stuff jitter .5 should be your maximum as the distance between one point and the next is 1.

#Creating a function that helps summarize data
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Data.lng.c <- summarySE(data=Data.lng, measurevar="Rating", groupvars=c("Condition","Measure", "Race"))
# Data.lng.c
# 
# Data.Qual.lng.c <- summarySE(Data.Qual.lng, measurevar="Rating", groupvars=c("Condition","Measure", "Race"))
# Data.Amer.lng.c <- summarySE(Data.Amer.lng, measurevar="Rating", groupvars=c("Condition","Measure", "Race"))
# Data.Stat.lng.c <- summarySE(Data.Stat.lng, measurevar="Rating", groupvars=c("Condition","Measure", "Race"))
# #Data.Warm.lng.c <- summarySE(Data.Warm.lng, measurevar="Rating", groupvars=c("Condition","Measure", "Race"))

##Line Graphs

#########
#########Experiment
#########

qual.int <- ezStats(Data.Qual.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition)


ggplot(qual.int, aes(y=Mean, x=Condition))+
  geom_point(aes(colour=Race, shape=Race), alpha = .8)+
  geom_line(aes(colour=Race, linetype=Race, x = I(as.numeric(Condition))), alpha = .8)+
  geom_errorbar(aes(colour = Race, 
                    ymin = Mean-FLSD, 
                    ymax = Mean+FLSD), 
                linetype = 1, 
                show.legend = FALSE, 
                width = 0.25, 
                alpha = .5)+
  labs(x = 'Condition', 
       y = 'Qualification', 
       colour = 'Race', 
       shape = 'Race', 
       linetype = 'Race'
  )+
  scale_y_continuous(limits=c(1,7))


#All measures into the same anova
Data.all <- Data %>%
  dplyr::select(Subj, Condition, AF_Qual, AA_Qual, AF_Amer, AA_Amer, AF_Stat, AA_Stat)

Data.all.lng <- melt(na.omit(Data.all), id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.all.lng$Race <- factor(substr(Data.all.lng$Index, 1, 2))
Data.all.lng$Measure <- factor(substr(Data.all.lng$Index, 4, 7))
Data.all.lng$Subj <- factor(Data.all.lng$Subj)

ezANOVA(Data.all.lng, dv=Rating, wid=Subj, within=.(Race, Measure), between=Condition, detailed=T)

all.int <- ezStats(Data.all.lng, dv=Rating, wid=Subj, within=.(Race,Measure), between=Condition)


ggplot(all.int, aes(y=Mean, x=Condition))+
  geom_point(aes(colour=Race, shape=Race), alpha = .8)+
  geom_line(aes(colour=Race, linetype=Race, x = I(as.numeric(Condition))), alpha = .8)+
  geom_errorbar(aes(colour = Race, 
                    ymin = Mean-FLSD, 
                    ymax = Mean+FLSD), 
                linetype = 1, 
                show.legend = FALSE, 
                width = 0.25, 
                alpha = .5)+
  labs(x = 'Condition', 
       y = 'Qualification', 
       colour = 'Race', 
       shape = 'Race', 
       linetype = 'Race'
  )+
  #scale_y_continuous(limits=c(1,7)) +
  facet_wrap(~Measure)

###########
###########

# # Standard error of the mean
# ggplot(Data.lng.c, aes(x=Condition, y=Rating, colour=Race)) + 
#   geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), width=.1) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~Measure)
# 
# 
# # The errorbars overlapped, so use position_dodge to move them horizontally
# pd <- position_dodge(0.25) # move them .05 to the left and right
# 
# ggplot(Data.lng.c, aes(x=Condition, y=Rating, colour=Race)) + 
#   geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), width=.1, position=pd) +
#   geom_line(position=pd) +
#   geom_point(position=pd) +
#   facet_wrap(~Measure)
# 
# 
# # Use 95% confidence interval instead of SEM
# ggplot(Data.lng.c, aes(x=Condition, y=Rating, colour=Race)) + 
#   geom_errorbar(aes(ymin=Rating-ci, ymax=Rating+ci), width=.1, position=pd) +
#   geom_line(position=pd) +
#   geom_point(position=pd) +
#   facet_wrap(~Measure)
# 
# # Black error bars - notice the mapping of 'group=Race' -- without it, the error
# # bars won't be dodged!
# ggplot(Data.Qual.lng.c, aes(x=Condition, y=Rating, colour=Race, group=Race)) + 
#   #geom_errorbar(aes(ymin=Rating-ci, ymax=Rating+ci), colour="black", width=.1, position=pd) +
#   geom_line() +
#   geom_point(size=3) +
#   scale_y_continuous(limits = c(0,7)) +
#   facet_wrap(~Measure)
# 
# #A finished graph with error bars representing the standard error of the mean might look like this. The points are drawn last so that the white fill goes on top of the lines and error bars.
# 
# ggplot(Data.lng.c, aes(x=Condition, y=Rating, colour=Race, group=Race)) + 
#   geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), colour="black", width=.1, position=pd) +
#   geom_line(position=pd) +
#   geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
#   xlab("Condition") +
#   ylab("Participant Ratings (1-7)") +
#   scale_colour_hue(name="Race of Applicant",    # Legend label, use darker colors
#                    breaks=c("AF", "AA"),
#                    labels=c("African American", "Asian American"),
#                    l=40) +                    # Use darker colors, lightness=40
#   ggtitle("Ratings of Applicants on Measures of Americanness, Qualification, and Status") +
#   expand_limits(y=0) +                        # Expand y range
#   scale_y_continuous(limits=c(0,7)) +         # can set # of ticks with break= argument (not used here)
#   facet_wrap(~Measure) +
#   theme_bw() +
#   theme(legend.justification=c(1,0),
#         legend.position=c(1,0))               # Position legend in bottom right
# 
#  
# ###Bar Graphs
# 
# #Bar plot with error bars
# ggplot(Data.lng.c, aes(x=Condition, y=Rating, fill=Race, group=Race)) +
#   geom_bar(stat="identity", position=position_dodge())  +
#   geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), width=.2,
#                 position=position_dodge(.9)) +
#                 ylab("Participant Ratings (1-7)") +
#                 ggtitle("Ratings of Applicants on Measures of Americanness,       Qualification, and Status") +
#                 facet_wrap(~Measure) +
#                 scale_fill_brewer(palette="Paired") + 
#   theme_minimal()
# 
# #If you wnat to save all three plots separately to a list you can do that by splitting the data then applying the plot as a function
# plist <- lapply(split(Data.lng.c, Data.lng.c$Measure), function(d) {
#   ggplot(d, aes(Condition, Rating, fill=Race, group=Race)) + 
#     geom_bar(stat="identity", position=position_dodge())  +
#     geom_errorbar(aes(ymin=Rating-sd, ymax=Rating+sd), width=.2,
#                   position=position_dodge(.9)) +
#     facet_wrap(~ Measure) +
#     coord_cartesian(ylim = c(1,7)) 
#     #theme_bw() +
#     #theme(plot.margin=unit(rep(0.4,4),"lines"),
#           #axis.title=element_blank())
# })
# 
# #Example of a single bar plot
# ggplot(Data.Qual.lng.c, aes(Condition, Rating)) + 
#   geom_bar(aes(fill = Race), stat = "identity", position = "dodge") +
#   #geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), width=.2, position= position_dodge(width = .9)) +
#   scale_fill_brewer(palette="Paired") +
#   coord_cartesian(ylim = c(1,7)) +
# theme_minimal()
#   #At some point I have to figure out how to center error bars without a bunch of extra steps
# 
# 
# #A few other bar plots
# # Check to make sure that Condition is a factor
# Data.lng.c2 <- Data.lng.c
# class(Data.lng.c2$Condition)
# 
# #selecting only the qualification measure
# Data.lng.c3 <- Data.lng.c2 %>%
#   dplyr::filter(Measure == "Qual")
# 
# # Error bars represent standard error of the mean
# ggplot(Data.lng.c3, aes(x=Condition, y=Rating, fill=Race)) + 
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   #facet_wrap(~Measure)
# 
# 
# # Use 95% confidence intervals instead of SEM
# ggplot(Data.lng.c3, aes(x=Condition, y=Rating, fill=Race)) + 
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=Rating-ci, ymax=Rating+ci),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   facet_wrap(~Measure)
# 
# #Note that in both cases there are multiple error bars. This is an easy fix but we'll address it later
# 
# #An example of one closer to being finished
# ggplot(Data.lng.c3, aes(x=Condition, y=Rating, fill=Race)) + 
#   geom_bar(position=position_dodge(), stat="identity",
#            colour="black", # Use black outlines,
#            size=.3) +      # Thinner lines
#   geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se),
#                 size=.3,    # Thinner lines
#                 width=.2,
#                 position=position_dodge(.9)) +
#   xlab("Participant Condition") +
#   ylab("Participant Ratings (1-7)") +
#   scale_fill_hue(name="Applicant Race", # Legend label, use darker colors
#                  breaks=c("AF", "AA"),
#                  labels=c("African American", "Asian American")) +
#   ggtitle("Ratings of Applicants on Measures of Qualification") +
#   scale_y_continuous(limits = c(0,7)) +
#   #facet_wrap(~Measure) +
#   theme_bw()
# 
# ##Scatter
# 
# #Example of a jittered scatterplot
# ggplot(Data.lng, aes(x= Condition, y = Rating, color = Race)) +
#   geom_point(position = position_jitter(width = 0.25)) +
#   facet_wrap(~ Measure) +
#   scale_y_continuous(limits = c(0, NA))
# 
# ##If you want to export these graphs into their own PDF files
# library(gridExtra)
# 
# # 3 Single-page PDF file, each with 1 plot
# for (i in seq(1, length(plist))) {
#   pdf(paste0("Graph of Indeces",i,".pdf"), 7, 5)
#   grid.arrange(grobs=plist[i:(i)], 
#                ncol=1, left="Participant Rating", bottom="Condition")
#   dev.off()
# }

#describe(Data)
#summary(Data)

#... A few more things. First are two functions to help calculate within subject error bars:

## Norms the data within specified groups in a data frame; it normalizes each subject (identified by idvar) so that they have the same mean, within each group specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- plyr::ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}


## Summarizes data, handling within-subjects variables by removing inter-subject variability. It will still work if there are no within-S variables. Gives count, un-normed mean, normed mean (with same between-group mean), standard deviation, standard error of the mean, and confidence interval.If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

#Now to collapse the data
Data.lng <- na.omit(Data.lng)
Data.lng.wc <- summarySEwithin(Data.lng, measurevar="Rating", withinvars=c("Race","Measure"),
                        idvar=c("Subj"), na.rm=FALSE, conf.interval=.95)

Data.lng.wc

# Make the graph with the 95% confidence interval
ggplot(Data.lng.wc, aes(x=Measure, y=Rating_norm, group=Race, colour=Race)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=Rating-ci, ymax=Rating+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0,7)

#Why is it important to calculate these differently? Take a looksee:
# Instead of summarySEwithin, use summarySE, which treats condition as though it were a between-subjects variable
Data.lng.wc_between <- summarySE(data=Data.lng, measurevar="Rating", groupvars=c("Measure","Race"), na.rm=FALSE, conf.interval=.95)
Data.lng.wc_between
#>   condition  N value       sd       se       ci
#> 1   pretest 10 47.74 8.598992 2.719240 6.151348
#> 2  posttest 10 51.43 7.253972 2.293907 5.189179

# Show the between-S CI's in black, and the within-S CI's in group color
ggplot(Data.lng.wc_between, aes(x=Measure, y=Rating, group=Race, color=Race)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=Rating-ci, ymax=Rating+ci), colour="black") +
  geom_errorbar(width=.1, aes(ymin=Rating-ci, ymax=Rating+ci), data=Data.lng.wc) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(1,7)

#And one more graph for fun
#This time we'll add the between measure back in
Data.lng.wbc <- summarySEwithin(Data.lng, measurevar="Rating", betweenvars="Condition", withinvars=c("Race","Measure"),
                               idvar=c("Subj"), na.rm=FALSE, conf.interval=.95)


Data.lng.wbc2 <- Data.lng.wbc %>%
  filter(Measure == "Qual")

Data.lng.wbc2$Race <- Data.lng.wbc2$Race %>%
  dplyr::recode("AF" = 1, "AA" = 2)

Data.lng.wbc2$Race <- factor(Data.lng.wbc2$Race,
                    levels = c(1,2),
                    labels = c("AF", "AA"))

ggplot(Data.lng.wbc2, aes(x=Condition, y=Rating_norm, fill=Race)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Rating_norm-ci, ymax=Rating_norm+ci)) +
  coord_cartesian(ylim=c(1,7)) +
  #scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(1:100)) +
  #facet_wrap(~Measure) +
  xlab("") +
  ylab("") +
  #scale_fill_hue(name="Applicant Race", # Legend label, use darker colors
                 #breaks=c("AA", "AF"),
                 #labels=c("Asian American", "African American")) +
  scale_fill_manual(name="Applicant Race",
                    breaks=c("AF","AA"),
                    labels=c("African American", "Asian American"),
                    values=c("#4b2e83", "#b7a57a")) +
  #ggtitle("Ratings of Applicants on Measures of Qualification") +
  theme(text = element_text(size=40), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = c(0.85, .92))+
  geom_hline(yintercept=0) 

#More Chi-squared related code

# #computing the correspondence analysis
# testdf.mytable <- Data %>%
#   dplyr::select(Condition, hired_race)
# 
# testdf.mytable$Condition <- as.numeric(testdf.mytable$Condition)
# testdf.mytable$hired_race <- as.numeric(testdf.mytable$hired_race)
# testdf.mytable <- na.omit(testdf.mytable)
# 
# str(testdf.mytable)
# 
# res.ca <- CA(testdf.mytable, graph = TRUE)
# 
# 
# 
# print(res.ca)

# To interpret correspondence analysis, the first step is to evaluate whether there is a significant dependency between the rows and columns.

#Here, the association is significant (chi-square: 6.642814.456, p = 0.036102).
# 
# #To calculate the p value the long way:
# # Chi-square statistics
# chi2 <- 26.14606
# # Degree of freedom
# df <- (nrow(mytable.cor) - 1) * (ncol(mytable.cor) - 1)
# # P-value
# pval <- pchisq(chi2, df = df, lower.tail = FALSE)
# pval
# 
# #we examine the eigenvalues to determine the number of axis to be considered. The eigenvalues and the proportion of variances retained by the different axes can be extracted using the function get_eigenvalue() [factoextra package]. 
# 
# eig.val <- get_eigenvalue(res.ca)
# eig.val
# #The eigen value is pretty small
# 
# #Eigenvalues correspond to the amount of information retained by each axis. Dimensions are ordered decreasingly and listed according to the amount of variance explained in the solution. Dimension 1 explains the most variance in the solution, followed by dimension 2 and so on.
# 
# #Eigenvalues can be used to determine the number of axes to retain. There is no “rule of thumb” to choose the number of dimensions to keep for the data interpretation. It depends on the research question and the researcher’s need. For example, if you are satisfied with 80% of the total variances explained then use the number of dimensions necessary to achieve that.
# 
# 
# #An alternative method to determine the number of dimensions is to look at a Scree Plot, which is the plot of eigenvalues/variances ordered from largest to the smallest. The number of component is determined at the point, beyond which the remaining eigenvalues are all relatively small and of comparable size.
# fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 100))
# 
# #Our data contains 3 rows and 2 columns.
# 
# #If the data were random, the expected value of the eigenvalue for each axis would be 1/(nrow(mytable.cor)-1) = 1/2 = 50% in terms of rows.
# 
# #Likewise, the average axis should account for 1/(ncol(mytable.cor)-1) = 1/1 = 100% in terms of the 2 columns
# 
# #this is a scree plot with that average eigenvalue drawn
# fviz_screeplot(res.ca) +
#   geom_hline(yintercept=100, linetype=2, color="red")
# 
# #draw the biplot of rows and columns variables
# fviz_ca_biplot(res.ca, repel = TRUE)
# #it cant do it here with so few dimensions
# 
# #The graph above is called symetric plot and shows a global pattern within the data. Rows are represented by blue points and columns by red triangles.
# 
# #The distance between any row points or column points gives a measure of their similarity (or dissimilarity). Row points with similar profile are closed on the factor map. The same holds true for column points.
# 
# #extract results for row variables
# row <- get_ca_row(res.ca)
# row
# 
# #The components of the get_ca_row() function can be used in the plot of rows as follow:
#   
# ##row$coord: coordinates of each row point in each dimension (1, 2 and 3). Used to create the scatter plot.
# ##row$cos2: quality of representation of rows.
# ##var$contrib: contribution of rows (in %) to the definition of the dimensions.
# 
# #The different components can be accessed as follow:
#   
# # Coordinates
# head(row$coord)
# # Cos2: quality on the factore map
# head(row$cos2)
# # Contributions to the principal components
# head(row$contrib)
# 
# fviz_ca_row(res.ca, ndim = 1, repel = TRUE)
# 
# 
# library("corrplot")
# corrplot(row$cos2, is.corr=FALSE)
# 
# 
# fviz_cos2(res.ca, choice = "row", axes = 1:2)
# 
# fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# 
# col <- get_ca_col(res.ca)
# col
# 
# fviz_ca_biplot(res.ca, 
#                map ="rowprincipal", arrow = c(TRUE, TRUE),
#                repel = TRUE)
# 
# fviz_ca_biplot(res.ca, map ="colgreen", arrow = c(TRUE, FALSE),
#                repel = TRUE)

##Attempting to do anovas before remembering that the models are mixed
# Data.Qual.lng
# anova(lm(Rating ~ Race*Condition, Data.Qual.lng))
# pairwise.t.test(Data.Qual.lng$Rating, Data.Qual.lng$Condition, p.adj = "bonf")
# 
# Data.Amer.lng
# anova(lm(Rating ~ Race*Condition, Data.Amer.lng))
# pairwise.t.test(Data.Amer.lng$Rating, Data.Amer.lng$Condition, p.adj = "bonf")
# 
# Data.Stat.lng
# anova(lm(Rating ~ Race*Condition, Data.Stat.lng))
# 
# Data.Warm.lng
# anova(lm(Rating ~ Race*Condition, Data.Warm.lng))


###doing a binomial regression
# Data$hired_race <- Data$hired_race %>%
#   dplyr::recode("AF" = 0, "AA" = 1)
# 
# Qual <- Data.Index %>% dplyr::select(AF_Qual, AA_Qual) %>% rowMeans
# 
# modeltalk <- glm(Data$hired_race ~ Data$AF_Qual + Data$AA_Qual, family="binomial")
# summary(modeltalk)
# modeltalk2 <- lm(hired_race ~ AF_Qual + AA_Qual, Data)
# summary(modeltalk2)
# modeltalk3 <- lm(hired_race ~ AF_Qual + AA_Qual + AF_Amer + AA_Amer + AF_Stat + AA_Stat + AF_Warm + AA_Warm, Data)
# summary(modeltalk3)
# modeltalk4 <- lm(hired_race ~ AF_Amer + AA_Amer, Data)
# summary(modeltalk4)
# 
# modeltalk5 <- lm(hired ~ Qual, Data2)
# summary(modeltalk5)
# modeltalk6 <- lm(hired ~ Qual*Amer*Stat*Warm, Data2)
# summary(modeltalk6)

##mediation
install.packages("mediation")
library(mediation)

#Mediation of American stereotype
Data.step1 <- Data %>%
  dplyr::select(Subj, Condition, AF_Qual, AA_Qual)

Data.step2 <- Data %>%
  dplyr::select(Subj, Condition, AF_Amer, AA_Amer)

Data.lng.1 <- melt(Data.step1, id=c("Subj", "Condition"), variable.name = "Index", value.name="Qual")

Data.lng.1$Race <- factor(substr(Data.lng.1$Index, 1, 2))
Data.lng.1$Subj <- factor(Data.lng.1$Subj)


Data.lng.2 <- melt(Data.step2, id=c("Subj", "Condition"), variable.name = "Index", value.name="Amer")

Data.lng.2$Race <- factor(substr(Data.lng.2$Index, 1, 2))
Data.lng.2$Subj <- factor(Data.lng.2$Subj)

Data.lng.1 <- Data.lng.1[,-3]
Data.lng.2 <- Data.lng.2[,-3]

Data.lng.Amer <- merge(x = Data.lng.2, y = Data.lng.1, by = c("Subj", "Condition", "Race"), all.x = TRUE)

# med.fit <- lm(Amer ~ Race + Race*Condition, data = Data.lng.Amer)
# 
# out.fit <- glm(Qual ~ Race + Amer + Race*Condition, data = Data.lng.Amer)
# 
# med.out <- mediation::mediate(med.fit, out.fit, treat = "Race", mediator = "Amer", robustSE = TRUE, sims = 1000, dropobs = TRUE)
# 
# summary(med.out)
# plot(med.out)

#Mediation of Stats stereotype
Data.steps.1 <- Data %>%
  dplyr::select(Subj, Condition, AF_Qual, AA_Qual)

Data.steps.2 <- Data %>%
  dplyr::select(Subj, Condition, AF_Stat, AA_Stat)

Data.lngs.1 <- melt(Data.steps.1, id=c("Subj", "Condition"), variable.name = "Index", value.name="Qual")

Data.lngs.1$Race <- factor(substr(Data.lngs.1$Index, 1, 2))
Data.lngs.1$Subj <- factor(Data.lngs.1$Subj)


Data.lngs.2 <- melt(Data.steps.2, id=c("Subj", "Condition"), variable.name = "Index", value.name="Stat")

Data.lngs.2$Race <- factor(substr(Data.lngs.2$Index, 1, 2))
Data.lngs.2$Subj <- factor(Data.lngs.2$Subj)

Data.lngs.1 <- Data.lngs.1[,-3]
Data.lngs.2 <- Data.lngs.2[,-3]

Data.lng.Stat <- merge(x = Data.lngs.2, y = Data.lngs.1, by = c("Subj", "Condition", "Race"))

# View(Data.lng.Stat)
# 
# med.fit <- lm(Stat ~ Race + Race*Condition, data = Data.lng.Stat)
# 
# out.fit <- glm(Qual ~ Race + Stat + Race*Condition, data = Data.lng.Stat)
# 
# med.out <- mediation::mediate(med.fit, out.fit, treat = "Race", mediator = "Stat", robustSE = TRUE, sims = 1000, dropobs = TRUE)
# 
# summary(med.out)
# plot(med.out)

#moderated mediation
devtools::install_github("markhwhiteii/processr")
install.packages("params")
library(processr)
library(params)

Data.lng.Stat <- Data.lng.Stat[,-5]

Data.lng.mm <- base::merge(x = Data.lng.Amer, y = Data.lng.Stat, by = c("Subj", "Condition", "Race"))

# View(Data.lng.mm)
# 
# mod.med.Data <- Data.lng.mm %>%
#   dplyr::filter(Condition == "Control" | Condition == "American")
# 
# mod.med.Data <- processr::make_numeric("Race", "AA", "Race", mod.med.Data)
# mod.med.Data <- processr::make_numeric("Condition", "Status", "Condition", mod.med.Data)
# 
# View(mod.med.Data)
# 
# #Interaction (model 1)
# mod1result <- model1(iv = "Amer", dv = "Qual", mod = "Condition", data = mod.med.Data)
# #kable(mod1result)
# mod1result
# 
# mod1result2 <- model1(iv = "Condition", dv = "Qual", mod = "Amer", data = mod.med.Data)
# mod1result2
# 
# #interaction (only qual)
# 
# mod1result3 <- model1(iv = "Condition", dv = "Qual", mod = "Race", data = Data.lng.mm2)
# #kable(mod1result)
# mod1result3

#mediation (model4) Foreignness
set.seed(1839)

mod.med.Data2 <- Data.lng.mm %>%
  dplyr::filter(Condition == "American")

#This will take a while to run so give it time. Don't be surprised if it's a whole 2-5 minutes or more.
mod4result <- model4(iv = "Race", dv = "Qual", med = "Amer", data = mod.med.Data2)
mod4result
# 
# #second-stage moderated mediation (model14) Foreignness
# set.seed(1839)
# mod14result <- model14(iv = "Race", dv = "Qual", med = "Amer", mod = "Condition", mod.med.Data)
# mod14result

# ##Status##
# 
# #mediation (model4) Status
# set.seed(1839)
# mod5result <- model4(iv = "Race", dv = "Qual", med = "Stat", data = mod.med.Data)
# mod5result
# 
# #second-stage moderated mediation (model14) Status
# set.seed(1839)
# mod15result <- model14(iv = "Race", dv = "Qual", med = "Stat", mod = "Condition", mod.med.Data)
# mod15result
# 
# 
# #Trying the mediation package again 
# med.fit <- lm(Amer ~ Race, data = mod.med.Data)
# 
# out.fit <- glm(Qual ~ Race + Amer, data = mod.med.Data)
# 
# med.out <- mediation::mediate(med.fit, out.fit, treat = "Race", mediator = "Amer", robustSE = TRUE, sims = 5000, dropobs = TRUE)
# 
# summary(med.out)
# plot(med.out)
# 
# plot(med.out, treatment = "both", labels = c("ind", "c'", "c"),
#      effect.type = c("indirect","direct","total"), xlab = "", ylab = "Percieved Qualifiation",
#      main = "Moderation Model", lwd = 1.5, cex = .85,
#      col = "black")
# 
# #more plotting
# #install.packages("MBESS")
# library(MBESS)
# mediation.effect.plot(mod.med.Data$Race, mod.med.Data$Amer, mod.med.Data$Qual,
#                       ylab = "Perceived Qualification", 
#                       xlab = "Perceived Americanness",
#                       main = "Mediation Effect Plot", 
#                       pct.from.top.a = 0.05, 
#                       pct.from.left.c = 0.05, 
#                       arrow.length.a = 0.05, 
#                       arrow.length.c = 0.05, 
#                       legend.loc = "bottomright" 
#                       #file = "", 
#                       #pch = 20, 
#                       #xlim = NULL, 
#                       #ylim = NULL, 
#                       #save.pdf = FALSE, 
#                       #save.eps = FALSE, 
#                       #save.jpg = FALSE
#                       )
# 
# mediation.effect.plot

#Create a mediation diagram
#install.packages("diagram")
library(diagram)
data <- c(0, "'1.52***'", 0,
          0, 0, 0, 
          "'-.53***'", "'-.32** (.49***)'", 0)
M<- matrix(nrow=3, ncol=3, byrow = TRUE, data=data)
plot<- plotmat(M, pos=c(1,2), 
               name= c("Cultural Foreignness","Applicant Race \n(AF = 0; AA = 1)", "Hireability"),
                box.type = "rect", box.size = 0.12, box.prop=0.5,  curve=0)

#install.packages("sjPlot")
#install.packages("sjmisc")
# library(sjPlot)
# library(sjmisc)
# 
# 
# # fit model with interaction
# fit <- lm(Qual ~ Amer * Race, data = mod.med.Data)
# 
# plot_model(fit, type = "int")
# 
# mod.med.Data <- processr::make_numeric("Race", "AA", "Race", mod.med.Data)
# 
# ggplot(mod.med.Data, aes(Race, Qual, color = Race)) +
#   stat_summary(fun.data=mean_cl_normal) + 
#   geom_smooth(method='lm',formula=y~x) + 
#   geom_point(position = position_jitter(width = 1, height = 0)) + 
#   xlim(0,1)
# 
# ####I think I figured it out####
# model1 <- lm(Qual ~ Race, data = mod.med.Data)
# mod.med.Data$y <- model1$fitted.values
# 
# model2 <- lm(Qual ~ Race + Amer, data = mod.med.Data)
# mod.med.Data$y.med <- model2$fitted.values
# 
# df1 <- data.frame(x=mod.med.Data$Race, y=mod.med.Data$y)
# df2 <- data.frame(x=mod.med.Data$Race, y=mod.med.Data$y.med)
# 
# df1$model <- "Total"
# df2$model <- "Direct"
# 
# dfc <- rbind(df1, df2)
# 
# 
# ggplot(dfc, aes(x, group=model)) + 
#   geom_bar() +
#   facet_wrap(~model)
# 
# 
# ####
# 
# 
# df1=data.frame(x=rnorm(10),y=rnorm(10))
# df2=data.frame(x=rnorm(10),y=rnorm(10))
# 
# df1$model <- "A"
# df2$model <- "B"
# 
# dfc <- rbind(df1, df2)
# 
# ggplot(dfc, aes(x, y, color = model)) +
#   stat_summary(fun.data=mean_cl_normal) + 
#   geom_smooth(method='lm',formula=y~x) + 
#   geom_point(position = position_jitter(width = .25, height = 0)) + 
#   xlim(0,1)
# 
# ggplot(dfc, aes(x, y, group=model)) + geom_point() + stat_smooth(aes(col=model)) +
#   facet_wrap(~model)
# 
# summary(df1)
# 
# ggplot(df1, aes(x, y)) +
#   geom_point() + 
#   geom_smooth(method='lm', formula=y~x)
# 
# aggregate(dfc$y, list(dfc$x, dfc$model), mean)
