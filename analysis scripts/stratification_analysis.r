# Stratiifaction analysis of acute outcomes

#first load libraries
library(lme4)
library(lmerTest)
library(emmeans)

pacutes = read.csv("[PATH TO FILE]/pacutes.csv")

# to reproduce the stratification analysis, define the same model as before (acute_analysis.r)
# and add the guess + guess*condition terms (the PANAS is shown in example but works the same for all other outcomes)
m = lmer(formula = value ~ (1|trial_id) + condition + guess + guess*condition + expectation + psychiatric_past, data=subset(pacutes, test_name=='PANAS'))

# then, define the estimated marginal means to compare the 4 strata
emm_fix_guess = emmeans(m, specs = pairwise ~ condition|guess) # stratas with fixed guess
emm_fx_cond   = emmeans(m, specs = pairwise ~ guess|condition) # stratas with fixed condition 

emm_fix_guess$contrasts # Comparison of strata with fixed guess (i.e. the two comparisons in the top row of fig5 - PL/PL vs MD/PL and PL/MD vs MD/MD)
emm_fix_cond$contrasts  # Comparison of strata with fixed condition (i.e. the bottom two comparisons on fig 5 - PL/PL vs PL/MD and MD/PL vs MD/MD)


