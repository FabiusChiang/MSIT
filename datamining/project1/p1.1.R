library(car);


party=read.csv("party.csv")
head(party,20)
attach(party)

party$postTotal = mile1 + mile2 + mile3 + mile4;
party$totalRatio = (party$postTotal)/(basemile);


fit1=lm(log(mile1+1)~log(particpate+1)+log(Rfreq+1)+log(basemile),party)
fit2=lm(log(mile1+1)~log(elaboration+1)+log(Rfreq+1)+log(basemile),party)
plot(fit1)
plot(fit2)