install.packages("tidyverse")
library(tidyverse)

MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors = F)
head(MechaCar_mpg)

# Rename columns of the dataset
MechaCar_mpg <- MechaCar_mpg %>% 
  rename(
    vlength = "vehicle length", 
    vweight = "vehicle weight", 
    spangle = "spoiler angle", 
    grclearance = "ground clearance")
head(MechaCar_mpg)

# Generate multiple linear regression model
lm(formula = mpg ~ vlength + vweight + spangle + grclearance + AWD, data = MechaCar_mpg)

#Call:
#  lm(formula = mpg ~ vlength + vweight + spangle + grclearance + 
#       AWD, data = MechaCar_mpg)

#Coefficients:
#  (Intercept)      vlength      vweight      spangle  grclearance          AWD  
#-1.040e+02    6.267e+00    1.245e-03    6.877e-02    3.546e+00   -3.411e+00#

# mpg = 6.27*vlength + 0*vweight + 0.07*spangle + 3.55*grclearance - 3.41*AWD - 104
# From this formula we can already see that vehicle length and spoiler angle have
# mpg = 6.27*vlength + 3.55*grclearance - 3.41*AWD - 104

# Generate summary statistics
summary(lm(mpg ~ vlength + vweight + spangle + grclearance + AWD,MechaCar_mpg))
#the r-squared value of the multiple linear regression model is 0.71
#p-value is smaller than our assumed significance level of 0.05. 
# sufficient evidence to reject our null hypothesis.

# Reading the Suspension Coil dataset
susp_coil <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors = F)

# Visualizing PSI distribution using density plot
ggplot(susp_coil,aes(x=PSI)) + geom_density()
# The distribution looks normal

shapiro.test(susp_coil$PSI)
#W = 0.60984, p-value < 2.2e-16
# p-value is a lot less than 0.05 


summary(susp_coil$PSI) 
# mean = 1498.78 and median = 1500
var(susp_coil$PSI) 
# variance = 62.29
sd(susp_coil$PSI) 
# standard deviation = 7.89

# Suspension Coil T-Test
t.test(x=susp_coil$PSI,mu=1500)



#Lot 1
Lot1_PSI <- susp_coil %>%   filter(Manufacturing_Lot=="Lot1")
t.test(log10(Lot1_PSI$PSI), mu=mean(log10(1500)))

# Lot 2
Lot2_PSI <- susp_coil %>% filter(Manufacturing_Lot=="Lot2")
t.test(log10(Lot2_PSI$PSI), mu=mean(log10(1500)))
#data:  log10(Lot2_PSI$PSI)
#t = 0.51117, df = 49, p-value = 0.6115
#alternative hypothesis: true mean is not equal to 3.176091
#95 percent confidence interval:
#  3.175924 3.176373
#sample estimates:
#  mean of x 
#3.176148 

# Lot 3
Lot3_PSI <- susp_coil %>% filter(Manufacturing_Lot=="Lot3") 
t.test(log10(Lot3_PSI$PSI), mu=mean(log10(1500)))
#data:  log10(Lot3_PSI$PSI)
#t = -2.1137, df = 49, p-value = 0.03966
#alternative hypothesis: true mean is not equal to 3.176091
#95 percent confidence interval:
#  3.173877 3.176035
#sample est
