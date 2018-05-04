library(car);
library(sqldf);
set.seed(22);

purchase=read.csv("party.csv")
head(purchase,20)

purchase=sqldf("SELECT * FROM purchase WHERE basemile < 25000");
attach(purchase)


purchase$postTotal = mile1 + mile2 + mile3 + mile4;
purchase$totalRatio = (purchase$postTotal)/(basemile);
purchase$training = as.integer(runif(nrow(purchase))*5);

purchaseTraining = sqldf("SELECT * FROM purchase WHERE training < 4");
purchaseValidation = sqldf("SELECT * FROM purchase WHERE training = 4");
im1base=lm(log(mile1+1)~1,purchaseTraining)

im11=step(im1base, scope=~particpate+log(prefood+1)+I(log(prefood+1)^2)+particpate:log(prefood+1)+
log(prebank+1)+I(log(prebank+1)^2)+particpate:log(prebank+1)+
log(pregas+1)+I(log(pregas+1)^2)+particpate:log(pregas+1)+
log(preretail+1)+I(log(preretail+1)^2)+particpate:log(preretail+1)+
log(preother+1)+I(log(preother+1)^2)+particpate:log(preother+1)+
Rfreq+log(Rfreq+1)+1/Rfreq
, test="F", data=purchaseTraining)
summary(im11)
plot(im11)

im12=step(im1base, scope=~particpate+log(prefood+1)+particpate:log(prefood+1)+
log(prebank+1)+particpate:log(prebank+1)+
log(pregas+1)+particpate:log(pregas+1)+
log(preretail+1)+particpate:log(preretail+1)+
log(preother+1)+particpate:log(preother+1)+
Rfreq+log(Rfreq+1)+1/Rfreq
, test="F", data=purchaseTraining)
summary(im12)
plot(im12)

yhat=predict(im11, purchaseValidation);
mean((log(purchaseValidation$mile1+1)-yhat)^2);

yhat=predict(im12, purchaseValidation);
mean((log(purchaseValidation$mile1+1)-yhat)^2);

# im11
# lm(formula = log(mile1 + 1) ~ log(prefood + 1) + log(prebank +
#     1) + log(pregas + 1) + log(preother + 1) + log(preretail +
#     1) + I(log(prebank + 1)^2) + I(log(prefood + 1)^2) + I(log(preretail +
#     1)^2) + I(log(pregas + 1)^2) + particpate, data = purchaseTraining)

# im12
# lm(formula = log(mile1 + 1) ~ log(prefood + 1) + log(prebank +
#     1) + log(pregas + 1) + log(preother + 1) + log(preretail +
#     1) + particpate, data = purchaseTraining)