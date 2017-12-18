### methods to calculate anova and the post hoc analysis for main and interaction effects.

### ANOVA 
## aov
aov.acc <- aov(Acc ~ Experiment*Alignment + Error(Participant/(Experiment*Alignment)), data = data)
summary(aov.acc)
## TukeyHSD(aov.acc)

## anova
lme_velocity = lme(Velocity ~ Material, data=scrd, random = ~1|Subject)
anova(lme_velocity)
summary(glht(lme_velocity, linfct=mcp(Material = "Tukey")), test = adjusted(type = "bonferroni"))

## Anova

## ezANOVA


## linear mixed model
lme.acc <- lme(Acc ~ Experiment*Alignment, random = ~1|Participant/Experiment/Alignment, data = data)
summary(lme.acc)

# for both main and interaction effects
lsmeans(lme.acc, pairwise ~ Experiment, adjust="tukey") 
lsmeans(lme.acc, pairwise ~ Alignment, adjust="tukey") 
lsmeans(lme.acc, pairwise ~ Experiment:Alignment, adjust="tukey") 

# only for interaction effect
data$EA <- interaction(data$Experiment, data$Alignment)
catch.Acc.lme <- lme(Acc ~ EA, random = ~1|Participant/Experiment/Alignment, data = data)
catch.Acc.post <- summary(glht(catch.Acc.lme, linfct = mcp(EA = "Tukey")))
catch.Acc.post
