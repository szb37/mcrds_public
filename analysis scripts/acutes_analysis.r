# Analysis of acute and post acute outcomes

# first load libraries
library(lme4)
library(lmerTest)

# get data
pacutes = read.csv("[PATH TO FILE]/pacutes_314.csv")

# The code below reproduces the last 2 columns of Table2 in the man manuscript
# The first model is without, the second is with the guess component
m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past , data=subset(pacutes, test_name=='PANAS'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + guess, data=subset(pacutes, test_name=='PANAS'))

# Next obtain the summary stats from both model: these are the numbers in the table; estimate+-1.96*std error
summary(m1)
summary(m2)

# Repeat the same for all acute / post-acute outcomes
m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past , data=subset(pacutes, test_name=='positive'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + guess, data=subset(pacutes, test_name=='positive'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past , data=subset(pacutes, test_name=='negative'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + guess, data=subset(pacutes, test_name=='negative'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation, data=subset(pacutes, test_name=='intensity'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + guess, data=subset(pacutes, test_name=='intensity'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + c(sex), data=subset(pacutes, test_name=='energy'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + c(sex) + guess, data=subset(pacutes, test_name=='energy'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past, data=subset(pacutes, test_name=='mood'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + guess, data=subset(pacutes, test_name=='mood'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past, data=subset(pacutes, test_name=='creativity'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + guess, data=subset(pacutes, test_name=='creativity'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past, data=subset(pacutes, test_name=='focus'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + guess, data=subset(pacutes, test_name=='focus'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past, data=subset(pacutes, test_name=='temper'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + guess, data=subset(pacutes, test_name=='temper'))

m1  = lmer(formula = value ~ (1|trial_id) + age + condition + c(sex), data=subset(pacutes, test_name=='CPS'))
m2  = lmer(formula = value ~ (1|trial_id) + age + condition + c(sex) + guess, data=subset(pacutes, test_name=='CPS'))

m1  = lmer(formula = value ~ (1|trial_id) + age + condition, data=subset(pacutes, test_name=='ooo'))
m2  = lmer(formula = value ~ (1|trial_id) + age + condition + guess, data=subset(pacutes, test_name=='ooo'))

m1  = lmer(formula = value ~ (1|trial_id) + age + condition, data=subset(pacutes, test_name=='rotations'))
m2  = lmer(formula = value ~ (1|trial_id) + age + condition + guess , data=subset(pacutes, test_name=='rotations'))

m1  = lmer(formula = value ~ (1|trial_id) + age + condition, data=subset(pacutes, test_name=='spatial_planning'))
m2  = lmer(formula = value ~ (1|trial_id) + age + condition + guess , data=subset(pacutes, test_name=='spatial_planning'))

m1  = lmer(formula = value ~ (1|trial_id) + age + condition, data=subset(pacutes, test_name=='spatial_span'))
m2  = lmer(formula = value ~ (1|trial_id) + age + condition + guess , data=subset(pacutes, test_name=='spatial_span'))

m1  = lmer(formula = value ~ (1|trial_id) + age + condition, data=subset(pacutes, test_name=='feature_match'))
m2  = lmer(formula = value ~ (1|trial_id) + age + condition + guess , data=subset(pacutes, test_name=='feature_match'))

m1  = lmer(formula = value ~ (1|trial_id) + age + condition, data=subset(pacutes, test_name=='paired_associates'))
m2  = lmer(formula = value ~ (1|trial_id) + age + condition + guess , data=subset(pacutes, test_name=='paired_associates'))


# Post-acute outcomes
m1  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past, data=subset(pacutes, test_name=='WEMWB'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + expectation + psychiatric_past + guess, data=subset(pacutes, test_name=='WEMWB'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + psychiatric_past + sss, data=subset(pacutes, test_name=='QIDS'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + psychiatric_past + sss + guess, data=subset(pacutes, test_name=='QIDS'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + psychiatric_past + sss, data=subset(pacutes, test_name=='STAIT'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + psychiatric_past + sss + guess, data=subset(pacutes, test_name=='STAIT'))

m1  = lmer(formula = value ~ (1|trial_id) + condition + psychiatric_past, data=subset(pacutes, test_name=='SCS'))
m2  = lmer(formula = value ~ (1|trial_id) + condition + psychiatric_past + guess, data=subset(pacutes, test_name=='SCS'))




