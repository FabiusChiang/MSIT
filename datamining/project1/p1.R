library(car);

purchase=read.csv("party.csv")
head(purchase,20)
attach(purchase)

purchase$postTotal = mile1 + mile2 + mile3 + mile4;
purchase$diffRatio = (purchase$postTotal - basemile)/basemile;

# model1=lm(mile1 ~ as.factor(particpate)+elaboration+prefood+Rfreq, purchase);
# summary(model1);
# plot(model1);

# model2=lm(log(diffRatio) ~ as.factor(particpate)+elaboration+Rfreq+log(basemile), purchase);
# summary(model2);
# plot(model2);
# vif(model2)

# model21=lm(log(diffRatio) ~ as.factor(particpate)+Rfreq+log(basemile), purchase);
# summary(model21);
# plot(model21);
# vif(model21)

# model3=lm(log(diffRatio) ~ as.factor(particpate)+elaboration+log(basemile), purchase);
# summary(model3);
# plot(model3);
# vif(model3)

model4=lm(log(diffRatio) ~ particpate+log(basemile), purchase);
summary(model4);
plot(model4);
vif(model4)

model5=lm(log(diffRatio) ~ particpate+log(basemile)+I(log(basemile)^2)+particpate:log(basemile)+particpate:I(log(basemile)^2), purchase);
summary(model5);
plot(model5);
vif(model5)

model6=lm(log(diffRatio) ~ particpate+log(basemile)+I(log(basemile)^2)+particpate:I(log(basemile)^2), purchase);
summary(model6);
plot(model6);
vif(model6)

purchase$totalRatio = (purchase$postTotal+1)/(basemile+1);

model7=lm(log(totalRatio) ~ particpate+log(basemile)+I(log(basemile)^2)+particpate:log(basemile), purchase);
summary(model7);
plot(model7);
vif(model7)