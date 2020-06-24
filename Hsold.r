crime = read.csv(file.choose(),header = T)
str(crime)
head(crime,11)
library(tidyr)
crime_gather = gather(crime,year,value,-borough, -crime)
str(crime_gather)
head(crime_gather)
crime_spread = spread(crime_gather,crime,value)
str(crime_spread)
head(crime_spread)

library(stringr)

crime_spread$year = str_replace(crime_spread$year,"X","")
colnames(crime_spread) = c("borough","year","blgry","assault","grand_larc","grand_larc_motor","murder","rape","robbery","Hsold")
str(crime_spread)
View(crime_spread)
summary(crime_spread)

hist(crime_spread$Hsold, prob =T)
lines(density(crime_spread$Hsold,bw="sj"), lty = 5)

#crime_spread$Hsold = sqrt(crime_spread$Hsold)

Mod1 = glm(Hsold ~ rape + grand_larc_motor,crime_spread, family = poisson)
summary(Mod1)
AIC: AIC: 119551
BIC(Mod1)
#[1] 119556.9

Mod2 = glm(Hsold ~ rape + grand_larc_motor+robbery,crime_spread, family = poisson)
summary(Mod2)
#Bayesian Information Criterion
BIC(Mod2)
#[1] 86138.88




myvars1 = c("Hsold","rape","assault","grand_larc","grand_larc_motor")
myvars2 = c("Hsold","rape","assault","grand_larc","grand_larc_motor","robbery")

plot(crime_spread[myvars1])
plot(crime_spread[myvars2])

cor(crime_spread[myvars2])

windows()
plot(crime_spread[myvars1])
plot(crime_spread[myvars1]$Hsold ~ crime_spread[myvars1]$grand_larc)
#library(xlsx)
#write.xlsx(mydata, "c:/mydata.xlsx")
c1 = cor(crime_spread[myvars1])
#symnum(c1)

#smoothScatter(crime_spread$Hsold ~ crime_spread$year)
model1 = lm(Hsold ~ rape + grand_larc_motor, data = crime_spread)
#model = glm(borough ~, data = crime_spread)
model
summary(model1)
anova(model1)
model2 = lm(Hsold ~ rape + grand_larc_motor+robbery , data = crime_spread)
summary(model2)
anova(model2)
anova(model1,model2)


model3 = lm(Hsold ~ rape + assault + grand_larc +grand_larc_motor , data = crime_spread)
summary(model3)
anova(model3)
model4 = lm(Hsold ~ rape + assault + grand_larc +grand_larc_motor+robbery , data = crime_spread)
summary(model4)
anova(model3, model4)
head(crime_spread)

#glm

model1 = glm(Hsold ~ rape + grand_larc_motor, data = crime_spread,family = "poisson")
summary(model1)


install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(crime_spread$Hsold, fitted(Mod2))



exp(3.152e-04)
