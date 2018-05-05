library(car);

purchase=read.csv("party.csv")
head(purchase,20)
attach(purchase)

round(cor(purchase), 2);
# plot(purchase)

purchase$postTotal = mile1 + mile2 + mile3 + mile4;
purchase$diffRatio = (purchase$postTotal - basemile)/basemile;

plot(lm(mile1~basemile+particpate))

fit1=lm(log(mile1+1)~log(basemile+1)+particpate);
plot(fit1)

fit2=lm(log(mile2+1)~log(basemile+1)+particpate);
fit3=lm(log(mile3+1)~log(basemile+1)+particpate);
fit4=lm(log(mile4+1)~log(basemile+1)+particpate);

fit21=lm(log(mile1+1)~log(basemile+1)+elaboration)
fit22=lm(log(mile2+1)~log(basemile+1)+elaboration);
fit23=lm(log(mile3+1)~log(basemile+1)+elaboration);
fit24=lm(log(mile4+1)~log(basemile+1)+elaboration);


summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

summary(fit21)
summary(fit22)
summary(fit23)
summary(fit24)