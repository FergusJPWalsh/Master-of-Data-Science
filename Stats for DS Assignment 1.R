install.packages("PPforest")
library(PPforest)
data(fishcatch)
is.data.frame(fishcatch)
dim(fishcatch)
head(fishcatch)
colnames(fishcatch)
any(is.na(fishcatch))
summary(fishcatch)

bream <- fishcatch[1:35,]
whitewish <- fishcatch[36:41,]
roach <- fishcatch[42:61,]
parkki <- fishcatch[62:72,]
smelt <- fishcatch[73:86,]
pike <- fishcatch[87:103,]
perch <- fishcatch[104:142,]

par(mfrow=c(1,7))
boxplot(bream$weight, ylab = "Weight", main = "Bream")
boxplot(whitewish$weight, ylab = "Weight (g.)", main ="Whitewash")
boxplot(roach$weight, ylab = "Weight (g.)", main ="Roach")
boxplot(parkki$weight, ylab = "Weight (g.)", main ="Parkki")
boxplot(smelt$weight, ylab = "Weight (g.)", main="Smelt")
boxplot(pike$weight, ylab = "Weight (g.)", main ="Pike")
boxplot(perch$weight, ylab = "Weight (g.)", main ="Perch")

summary(bream$weight)
summary(whitewish$weight)
summary(roach$weight)
summary(parkki$weight)
summary(smelt$weight)
summary(pike$weight)
summary(perch$weight)

qqnorm(fishcatch$weight, pch = 1, ylab = "Weight", main = "All Species")
qqline(bream$weight, col = 2, lwd =2)

qqnorm(bream$weight, pch = 1, ylab = "Weight", main = "Bream")
qqline(bream$weight, col = 2, lwd=2)

par(mfrow=c(2,3))
qqnorm(whitewish$weight, pch = 1, ylab = "Weight", main = "Whitewish")
qqline(whitewish$weight, col = 2, lwd=2)

qqnorm(roach$weight, pch = 1, ylab = "Weight", main = "Roach")
qqline(roach$weight, col = 2, lwd=2)

qqnorm(parkki$weight, pch = 1, ylab = "Weight", main = "Parkki")
qqline(parkki$weight, col = 2, lwd=2)

qqnorm(smelt$weight, pch = 1, ylab = "Weight", main = "Smelt")
qqline(smelt$weight, col = 2, lwd=2)

qqnorm(pike$weight, pch = 1, ylab = "Weight", main = "Pike")
qqline(pike$weight, col = 2, lwd=2)

qqnorm(perch$weight, pch=1, ylab = "Weight", main = "Perch")
qqline(perch$weight, col = 2, lwd = 2)

shapiro.test(fcweight)
shapiro.test(bream$weight)
shapiro.test(whitewish$weight)
shapiro.test(roach$weight)
shapiro.test(parkki$weight)
shapiro.test(smelt$weight)
shapiro.test(pike$weight)
shapiro.test(perch$weight)

bream_model <- lm(bream$weight ~ length3 + height + width, data = bream[,2:7])
summary(bream_model)

whitewish_model <- lm(whitewish$weight ~ length1 + length2 + height + width, data = whitewish[,2:7])
summary(whitewish_model)

roach_model <- lm(roach$weight ~ length2 + length3 + width, data = roach[,2:7])
summary(roach_model)

parkki_model <- lm(parkki$weight ~  length1 + width, data = parkki[,2:7])
summary(parkki_model)

smelt_model <- lm(smelt$weight ~  length2 + height + width, data = smelt[,2:7])
summary(smelt_model)

pike_model <- lm(pike$weight ~ length1 + length2 + length3 + width, data = pike[,2:7])
summary(pike_model)

perch_model <- lm(perch$weight ~ ., data = perch[,2:7])
summary(perch_model)

par(mfrow=c(2,3))
plot(bream_model,3, main = "Assumptions Check for Bream")
plot(roach_model,3, main = "Assumptions Check for Roach")
plot(parkki_model,3, main = "Assumptions Check for Parkki")
plot(smelt_model,3, main = "Assumptions Check for Smelt")
plot(pike_model,3, main = "Assumptions Check for Pike")
plot(perch_model,3, main = "Assumptions Check for Perch")

predict_bream_weight <- data.frame(length3 = 40, height = 45, width = 15)
predict(bream_model, newdata = predict_bream_weight, interval = "confidence")
