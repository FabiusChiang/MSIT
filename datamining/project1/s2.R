library(car);
library(sqldf);
set.seed(10086);

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
im1=step(im1base, scope=~particpate+log(prefood+1)+I(log(prefood+1)^2)+particpate:log(prefood+1)+
log(prebank+1)+I(log(prebank+1)^2)+particpate:log(prebank+1), test="F", data=purchaseTraining)
summary(im1)
plot(im1)

im11=step(im1base, scope=~particpate+log(prefood+1)+I(log(prefood+1)^2)+particpate:log(prefood+1)+
log(prebank+1)+I(log(prebank+1)^2)+particpate:log(prebank+1)+
log(pregas+1)+I(log(pregas+1)^2)+particpate:log(pregas+1)+
log(preretail+1)+I(log(preretail+1)^2)+particpate:log(preretail+1)+
log(preother+1)+I(log(preother+1)^2)+particpate:log(preother+1)+
Rfreq+log(Rfreq+1)+1/Rfreq
, test="F", data=purchaseTraining)
summary(im11)
plot(im11)

im2=step(im1base, scope=~particpate+log(basemile+1)+particpate*log(basemile+1)
+particpate*basemile+log(basemile+1)*basemile+particpate*log(basemile+1)*basemile, test="F", data=purchaseTraining)
summary(im2)
plot(im2)

yhat=predict(im1, purchaseValidation);
mean((log(purchaseValidation$mile1+1)-yhat)^2);

yhat=predict(im11, purchaseValidation);
mean((log(purchaseValidation$mile1+1)-yhat)^2);
# vif(im11);

yhat=predict(im2, purchaseValidation);
mean((log(purchaseValidation$mile1+1)-yhat)^2);


summary(glm(particpate~log(1+prefood)+log(1+pregas)+log(1+prebank)+log(1+preother)+log(1++preretail)))

