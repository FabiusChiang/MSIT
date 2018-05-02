library(car);


purchase=read.csv("party.csv")
head(purchase,20)
attach(purchase)

purchase$postTotal = mile1 + mile2 + mile3 + mile4;
purchase$totalRatio = (purchase$postTotal)/(basemile);

# model1 = lm(mile1~log(1+particpate)+log(1+elaboration)+prefood+
#      pregas+   prebank+  preretail+preother+ Rfreq + mile2 + mile3 +mile4)

# vif(model1);

# hist(purchase$basemile)
# hist(purchase$prefood)
# hist(purchase$pregas)
# hist(purchase$prebank)
# hist(purchase$preretail)
# hist(purchase$preother)
# hist(purchase$Rfreq)
# hist(purchase$elaboration)

# plot(data.frame(purchase$Rfreq, log(purchase$basemile+1)))
# plot(data.frame(purchase$Rfreq, purchase$basemile))

# cor(purchase)

library(sqldf);

# ownMiles = sqldf("SELECT mile1, mile2, mile3, mile4, prefood, prebank, particpate, elaboration, postTotal, basemile FROM purchase WHERE basemile > 0 AND basemile < 25000");
# plot(ownMiles)
# cor(ownMiles)
# summary(lm(postTotal~basemile+particpate, ownMiles))

# ownMiles = sqldf("SELECT mile1, 1/mile2 FROM purchase WHERE mile1 > 0 AND mile2 >0 ");
# plot(ownMiles)
# cor(ownMiles)

# check = sqldf("SELECT COUNT(*) FROM purchase WHERE postTotal >= 25000 OR basemile >= 25000");
# check

# check = sqldf("SELECT COUNT(*) FROM purchase WHERE particpate = 1 AND (postTotal = 0 OR basemile = 0)");
# check

# check = sqldf("SELECT COUNT(*) FROM purchase WHERE particpate = 1 AND (basemile = 0)");
# check

# check = sqldf("SELECT COUNT(*) FROM purchase");
# check

purchase2 = sqldf("SELECT * FROM purchase WHERE postTotal > 0 AND basemile > 0");
head(purchase2)
model2=lm(log(totalRatio) ~ particpate+log(basemile)+I(log(basemile)^2)+particpate:log(basemile), purchase2);
summary(model2);
plot(model2);

model3=lm(log(postTotal) ~ particpate+log(basemile)+I(log(basemile)^2)+particpate:log(basemile), purchase2);
summary(model3);
plot(model3);