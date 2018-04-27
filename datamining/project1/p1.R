library(car);

purchase=read.csv("party.csv")
head(purchase,4)
attach(purchase)


# model1=lm(mile1 ~ as.factor(particpate)+elaboration+prefood+Rfreq, purchase);
# summary(model1);
# plot(model1);

purchase$postTotal = mile1 + mile2 + mile3 + mile4;
# model2=lm(postTotal ~ as.factor(particpate)+elaboration+Rfreq+basemile, purchase);
# summary(model2);
# plot(model2);

purchase$diffRatio = (purchase$postTotal - basemile)/basemile;
model3=lm(log(diffRatio) ~ as.factor(particpate)+elaboration+Rfreq+log(basemile), purchase);
summary(model3);
plot(model3);