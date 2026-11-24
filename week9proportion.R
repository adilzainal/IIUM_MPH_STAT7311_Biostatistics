#--------------------------------------------------------
# Biostatistic practical using R Software week 9 compare proportion (2025)
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# Bivariate data analysis
#--------------------------------------------------------

# 1. chi square test (association,unpair data, nominal)
# 2. yates and fisher exact (association, unpair, nominal, but violated chi square assumption)
# 3. chi square test for trend (Cochran-Armitage trend test) (association, unpair, ordinal)
# 4. mcnemar test (association, pair data, but 2 pair)
# 5. cochran q test (association pair data, but >2 pair)
# 6. Proportion z test compare 2 proportion with 95% ci (compare proportion instead of association)
# 7. measure 95% ci (method to estimate confidence interval for proportion)

########################################################################################

setwd("~/Desktop/Medical Statistic/R MPH IIUM/stat7311")
library(foreign)  # library to read .sav (SPSS) and .dta (STATA) files
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)  #SPSS

########################################################################################

#1. chi square test for categorical variable
tbl = table(data.sav$sex, data.sav$hpt)
tbl # count only
prop.table(tbl,1) # 1 row % , 2 column % 
chisq.test(tbl) 

tbl2 = table(data.sav$BMICat, data.sav$hpt)
tbl2
prop.table(tbl2,1)
chisq.test(tbl2) 

########################################################################################

#yates correction for correction works when at least one of the cell have less than 5 , more conservative
t3 = table(data.sav$sex, data.sav$smoking)
t3
prop.table(t3, 1)
chisq.test(t3)

#fisher exact test when small sample size (when more than 20% of cells have exp frequencies less than 5)
tbl3 = table(data.sav$sex, data.sav$smoking)
tbl3
prop.table(tbl3,1)
chisq.test(tbl3) 
fisher.test(data.sav$sex,data.sav$smoking)

ifelse(any(suppressWarnings(chisq.test(tbl2))$expected < 5), "Assumption violated: Use Fisher's exact test", "Assumption met: Use Chi-square test")

########################################################################################

#chi square test for trend
#Performs chi-squared test for trend in proportion. 
#This test is also known as Cochran-Armitage trend test.
# Example with default scores
smokers <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)
non_smokers <- patients - smokers  # Results in: 3, 3, 7, 12

# Combine into a 2-row table
smoke_table <- rbind(Smokers = smokers, Non_Smokers = non_smokers)

# Optional: Add names to your 4 ordered columns
colnames(smoke_table) <- c("Group1", "Group2", "Group3", "Group4")

# View your table
smoke_table

prop.trend.test(smokers, patients)
chisq.test(smokers,patients) 

# Install and load the 'coin' package if you don't have it
# install.packages("coin")
install.packages("coin")
library(coin)

# 1. Create the matrix and explicitly convert it to a 'table'
smoke_table <- as.table(rbind(Smokers = smokers, Non_Smokers = non_smokers))
# 2. Name the dimensions so 'scores' knows which axis is ordered
dimnames(smoke_table) <- list(Status = c("Smoker", "Non_Smoker"), 
                              Group = c("1", "2", "3", "4"))
# Run an exact permutation trend test (replace 'your_table' with your 2xK table)
independence_test(smoke_table, 
                  scores = list(Group = 1:4), 
                  distribution = "exact")


# Proportion of renal stone (calculi) across age
install.packages("rstatix")
library(rstatix)
xtab <- as.table(rbind(
  c(384, 536, 335),
  c(951, 869, 438)))
dimnames(xtab) <- list(
  stone = c("yes", "no"),
  age = c("30-39", "40-49", "50-59"))
xtabs$age <- factor(xtabs$age, levels = c("30-39", "40-49", "50-59"), ordered = TRUE)
xtab
# Compare the proportion of survived between groups
prop_trend_test(xtab)

########################################################################################

#mcnemar test for binary paired categorical data
HPTbtw <-matrix(c(30,12,40,18),nrow = 2,
                dimnames = list("HPT before" = c("normal", "hypertension"),
                                "HPT after" = c("normal", "hypertension")))
HPTbtw
mcnemar.test(HPTbtw)

tbl4 = table(data.sav$sbpcat, data.sav$dbpcat)
tbl4 
mcnemar.test(tbl4)

# Check discordant counts from the 'data_matrix' example
discordant_b <- HPTbtw[1, 2]
discordant_c <- HPTbtw[2, 1]
total_discordant <- discordant_b + discordant_c
total_discordant
print(paste("Count in Cell B:", discordant_b))
print(paste("Count in Cell C:", discordant_c))
print(paste("Total discordant pairs:", total_discordant))

#if discordant pair less than 25 use exact mcnemar test

install.packages("exact2x2")
library(exact2x2)
mcnemar.exact(HPTbtw)
########################################################################################

#cochran q test is extension for mcnemar for 3 or more matched groups or binary measurement
#Create a data frame
install.packages("RVAideMemoire")
library(RVAideMemoire)
data <- data.frame(
  Subject = 1:10,
  ConditionA = c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1),
  ConditionB = c(1, 1, 1, 0, 0, 1, 1, 1, 0, 1),
  ConditionC = c(0, 1, 1, 1, 0, 0, 1, 1, 1, 0)
)
install.packages("reshape")
library(reshape)
data_long <- melt(data, id.vars = "Subject", variable.name = "Condition", value.name = "Outcome")


# Perform Cochran's Q test
cochran.qtest(value ~ variable | Subject, data = data_long)

data <- data.frame(
  Subject = 1:10,
  Method_A = c(1,1,0,1,0,1,0,1,0,1),
  Method_B = c(1,1,1,1,1,1,0,1,1,1),
  Method_C = c(0,0,0,1,0,0,0,0,0,1)
)
data
data_long <- melt(data, id.vars = "Subject", variable.name = "Condition", value.name = "Outcome")
cochran.qtest(value ~ variable | Subject, data = data_long)

# Perform Cochran's Q test
cochran.qtest(value ~ variable | Subject, data = data_long, p.method = "bonferroni")

########################################################################################

# z test with 95% ci 
cth1 <- prop.test(x = c(490, 400), n = c(500, 500))
# Printing the results
cth1

cth2 <- prop.test(x = c(493, 490), n = c(500, 500))
cth2

# one sided p value
prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "greater")
prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "less")

########################################################################################

install.packages("DescTools")
library(DescTools)
# Example data
x1 <- 80 # Number of successes in group 1
n1 <- 100 # Number of trials in group 1
x2 <- 65 # Number of successes in group 2
n2 <- 100 # Number of trials in group 2

# Calculate the confidence interval for the difference in proportions
BinomCI(x=c(42, 35, 23, 22), n=c(50, 60, 70, 80), method="clopper-pearson") # most conservative

## other method = c("wilson", "wald", "waldcc", "agresti-coull", "jeffreys",
#           "modified wilson", "wilsoncc","modified jeffreys",
#          "clopper-pearson", "arcsine", "logit", "witting", "pratt", 
#          "midp", "lik", "blaker").

#Clopper-Pearson ("clopper-pearson"): Inverts the exact binomial test. Guaranteed to have at least the nominal coverage (e.g., ≥ 95%), 
#making it the standard for regulatory filings (like FDA trials), though it produces overly wide, conservative intervals.
#for small sample as well

#For larger sample size choices: Wilson ("wilson") or Agresti-Coull ("agresti-coull"). 
#Both remain accurate even if the event is very rare (e.g., 3 out of 500).
########################################################################################

