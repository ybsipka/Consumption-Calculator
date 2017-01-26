# 1) OrchardSprays
# 2) lm vs gls
# 3) Contrasts
#   -> treatment(reference level)
#   -> successive difference contrast(A-B, B-C, C-D...)

data("OrchardSprays")
str(OrchardSprays)

plot(decrease~treatment,data=OrchardSprays)

model1=lm(decrease~treatment,data=OrchardSprays)
summary(model1)

library(nlme)
model2=gls(decrease~treatment,data=OrchardSprays)
model3 = update(model2,weights=varIdent(form=~1|treatment))
AIC(model2,model3)

summary(model3)

#successive different contrasts
library(MASS) #VEne=abes & Ripley
#contr.sdif()

options(contrasts=c("contr.sdif","contr.sdif"))

model4=gls(decrease~treatment,data=OrchardSprays)
model5 = update(model2,weights=varIdent(form=~1|treatment))

summary(model5)

