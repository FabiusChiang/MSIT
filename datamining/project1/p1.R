library(car);

purchase=read.csv("party.csv")

names(purchase);
# plot(purchase);
# purchase;
model1=lm(mile1 ~ particpate+elaboration+prefood, purchase);

vif(lm)
# lm()

# plot()