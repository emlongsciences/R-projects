library(lme4)
library(lmerTest)
library(sjPlot)

m = read.csv('sp2ndround.csv')

#simple model
simplePrime<- glmer(targetPoOrDo ~ primePoOrDo * lexicalBoost + (1|id) + (1|item), data = m, family = binomial)
summary(simplePrime)

#other models maximalizing variables
spPrimersmax <- glmer(targetPoOrDo ~ primePoOrDo * lag + (1 + primePoOrDo|item) + (1 + lag|item) + (1 + primePoOrDo * lag|item), family = binomial)
summary(spPrimersmax)

spPrimers2 <- glmer(targetPoOrDo ~ primePoOrDo * lag + (1 + primePoOrDo|item) + (1 + lag|item), family = binomial)
summary(spPrimers2)

spPrimers3 <- glmer(targetPoOrDo ~ primePoOrDo * lag + (1 + primePoOrDo|item) + (1 + primePoOrDo * lag|item), family = binomial)
summary(spPrimers3)

spPrimers4 <- glmer(targetPoOrDo ~ primePoOrDo * lag + (1 + lag|item) + (1+ primePoOrDo * lag|item), family = binomial)
summary(spPrimers4)

spPrimers5 <- glmer(targetPoOrDo ~ primePoOrDo * lag + (1 + primePoOrDo|item), family = binomial)
summary(spPrimers5)

spPrimers6 <- glmer(targetPoOrDo ~ primePoOrDo * lag + (1 + lag|item), family = binomial)
summary(spPrimers6)

spPrimere <- glmer(targetPoOrDo ~ primePoOrDo * lag + (1|item), family = binomial)
summary(spPrimere)

spPrime3 <- glmer(targetPoOrDo ~ primePoOrDo * lag + (1 + lag|item), family = binomial)
summary(spPrime3)

ezmod <- glm(targetPoOrDo ~ primePoOrDo, data = m, family = "binomial")
summary(ezmod)

spLB <- glmer(targetPoOrDo ~ lexicalBoost + (1|item),family = binomial)
summary(spLB)

#additional languages
spAdl <- glmer(targetPoOrDo ~ additionalLanguages + (1|item),family = binomial)
summary(spAdl)
spPrimeAdlInt <- glmer(targetPoOrDo ~ primePoOrDo * additionalLanguages + (1|item),family = binomial)
summary(spPrimeAdlInt)
sp <- glmer(targetPoOrDo ~ primePoOrDo + lexicalBoost  + (1|item),family = binomial)
summary(sp)
sp2 <- glmer(targetPoOrDo ~ primePoOrDo + lexicalBoost + heritage + (1|item),family = binomial)
summary(sp2)
spl2alltens <- glm(l2tensproduced ~ podo * lb + (1|itm),family = binomial)
summary(spl2alltens)

#plot
spmest <- plot_model(sp)
spmre <- plot_model(sp,type = "re")
spmest
spmre
splr <- glm(targetPoOrDo ~ primePoOrDo + lexicalBoost + additionalLanguages)
summary(splr)
splri <- glm(targetPoOrDo ~ primePoOrDo * lexicalBoost + additionalLanguages)
anova(splr,splri)
