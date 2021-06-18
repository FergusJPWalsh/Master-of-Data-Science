# Install packages and libraries

install.packages("Guerry")
library(Guerry)
library(RColorBrewer)
install.packages("ggrepel")
library(ggrepel)
#install.packages("sf")
#library(sf)
install.packages("ggplot2")
library(ggplot2)
#install.packages("ggraph")
#library(ggraph)
#install.packages("ggfortify")
#library(ggfortify)
install.packages("caret")
library(caret)
install.packages("tidyverse")
library(tidyverse)
library(boot)
install.packages("car")
library(car)


data(gfrance)

# Create départment population density data frame
dfPop_density <- data.frame(dept = Guerry$dept, Pop_density = c(Guerry$Pop1831/Guerry$Area), Legit_births_pd = c(Angeville$Legit_births/Angeville$Pop1831), Illeg_births_pd = c(Angeville$Illeg_births/Angeville$Pop1831), Recruits_pd = c(Angeville$Recruits/Angeville$Pop1831), Farmers_pd = c(Angeville$Farmers/Angeville$Pop1831), Primary_schools_pd = c(Angeville$Primary_schools/Angeville$Pop1831))

# Merge Guerry, Angeville and dfPop_density data frames
Angeville_duplicates <- names(Angeville) %in% c("Department","Pop1831")
Angeville2 <- Angeville[!Angeville_duplicates]
Guerry2 <- merge(Guerry, Angeville2, by = "dept")
Guerry2 <- merge(Guerry2, dfPop_density, by = "dept")

# Remove NA values (i.e. remove Corsica)
Guerry3 <- na.omit(Guerry2)

# Section 1.1, Figure 1, recreate Plate I from Guerry (1833)
essai.colours <- rev(brewer.pal(n = 5, name = "Greys"))
spplot(gfrance, "Crime_pers", xlab = "1 Accusé sur ... habitants", main = "CRIMES CONTRE LES PERSONNES", col.regions = essai.colours, cuts = 4)

# Section 1.4 Preliminary Correlations
cor.test(Guerry$Crime_parents, Guerry$Crime_pers, alternative = "two.sided", method = "pearson")
cor.test(Guerry$Crime_parents, Guerry$Crime_pers, alternative = "two.sided", method = "spearman")

# Figure 2
ggplot(data = Guerry, mapping = aes(x = Prostitutes, y = Crime_pers)) + geom_point() + geom_text_repel(aes(label = Department), size = 4) + labs(x = "Prostitutes in Paris", y = "Population per Crime against Persons") + theme_bw()

ggplot(data = Guerry, mapping = aes(x = Distance, y = Crime_pers)) + geom_point() + geom_text_repel(aes(label = Department), size = 4) + labs(x = "Distance from Paris (km)", y = "Population per Crime against Persons") + theme_bw()

ggplot(data = Guerry, mapping = aes(x = Lottery, y = Crime_pers)) + geom_point() + geom_text_repel(aes(label = Department), size = 4) + labs(x = "Wager on Royal Lottery (rank)", y = "Population per Crime against Persons") + theme_bw()

ggplot(data = Guerry, mapping = aes(x = Desertion, y = Crime_pers)) + geom_point() + geom_text_repel(aes(label = Department), size = 4) + labs(x = "Desertion per Size of Military Unit (rank)", y = "Population per Crime against Persons") + theme_bw()


# Correlation (without, e.g. Seine)
ggplot(data = Guerry[-21,], mapping = aes(x = Lottery, y = Crime_pers)) + geom_point() +   geom_text_repel(aes(label = Department), size = 4)
cor.test(Angeville[-71,]$Marriages, Guerry[-71,]$Crime_pers, alternative = "two.sided", method = "pearson")
cor.test(Guerry[-21,]$Lottery, Guerry[-21,]$Crime_pers, alternative = "two.sided", method = "spearman")

# Correlation (with variable per pop. for Angeville data set)
ggplot(data = Angeville, mapping = aes(x = (Farmers), y = Guerry$Crime_pers)) + geom_point() + geom_text_repel(aes(label = Department), size = 4)
cor.test(dfPop_density$Farmers_pd, Guerry$Crime_pers, alternative = "two.sided", method = "pearson")
cor.test(Angeville$Life_exp, Guerry$Crime_pers, alternative = "two.sided", method = "spearman")

# Testing variable per pop. in Angeville data set.
ggplot(data = Angeville, mapping = aes(x = Primary_schools, y = Pop1831)) + geom_point() +   geom_text_repel(aes(label = Department), size = 4)
cor.test(Angeville$Pop1831, Angeville$Primary_schools, alternative = "two.sided", method = "pearson")

# Outliers
which.max(Guerry2$Pop_density)

# Section 2.2 Modelling a Non-Linear Realationship
model1 <- lm(Guerry$Crime_pers ~ Guerry$Lottery, data = Guerry)
model2 <- lm(Guerry$Crime_pers ~ Guerry$Lottery + I(Guerry$Lottery^2), data = Guerry)

model1predict <- model1 %>% predict(Guerry)
ggplot(Guerry, aes(Lottery, Crime_pers) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) + labs(x = "Wager on Royal Lottery (rank)", y = "Population per Crime against Persons")
model2predict <- model2 %>% predict(Guerry)
ggplot(Guerry, aes(Lottery, Crime_pers) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) + labs(x = "Wager on Royal Lottery (rank)", y = "Population per Crime against Persons")
summary(model1)
summary(model2)
?anova(model1, model2)

# K-fold validation

model1.fit = glm(Crime_pers ~ Lottery, data = Guerry)
cv.err.1 = cv.glm(Guerry, model1.fit, K = 5)
cv.err.1$delta

model2.fit = glm(Crime_pers ~ Lottery + I(Lottery^2), data = Guerry)
cv.err.2 = cv.glm(Guerry, model2.fit, K = 5)
cv.err.2$delta


# Section 2.3 MainCity and Dummy Variables
modeldummy <- lm(Guerry$Crime_pers ~ Guerry$MainCity, data = Guerry)
summary(modeldummy)
attach(Guerry)
contrasts(MainCity)

fix(Guerry)
levels(Guerry$MainCity)
anova(modeldummy)

summary(model1)
coef(model1)
confint(model1)

# Section 2.4 Variable Selection and Cross-Validation
# (Model A)

model3 <- glm(Crime_pers ~ Literacy + Donations + Infants + Suicides + Wealth + Commerce + Clergy + Crime_parents + Infanticide + Donation_clergy + Lottery + I(Lottery^2) + Desertion + Instruction + Prostitutes + Distance + Area + Pop1831 + Mortality + Marriages + Legit_births_pd + Illeg_births_pd + Recruits_pd + Conscripts + Exemptions + Farmers_pd + Recruits_ignorant + Schoolchildren + Windows_doors + Primary_schools_pd + Life_exp, data = Guerry2)
summary(model3)
cv.err.3 = cv.glm(Guerry3, model3, K = 5)
cv.err.3$delta

# (Model B)
model4 <- step(model3, direction="backward", trace = 0)
#model4b <- step(model3, scope = list(upper=model3), direction="both")

summary(model4)
cv.err.4 = cv.glm(Guerry3, model4, K = 5)
cv.err.4$delta

# (Model C)
model5 <- glm(Crime_pers ~ Infanticide + Lottery + I(Lottery^2) + Distance + Area + Marriages + Conscripts + Farmers_pd + Primary_schools_pd, data = Guerry3)
summary(model5)
cv.err.5 = cv.glm(Guerry3, model5, K = 5)
cv.err.5$delta

# (Model D)
model6 <- lm(Crime_pers ~ Infanticide + Lottery + I(Lottery^2) + Distance + Area + Conscripts + Farmers_pd + Primary_schools_pd, data = Guerry2)
summary(model6)
cv.err.6 = cv.glm(Guerry3, model6, K = 5)
cv.err.6$delta

# (Model E)
model7 <- glm(Crime_pers ~ Infanticide + Lottery + I(Lottery^2) + Distance + Area + Conscripts + Marriages + Primary_schools_pd, data = Guerry3)
summary(model7)
cv.err.7 = cv.glm(Guerry3, model7, K = 5)
cv.err.7$delta

# (Model F)
model8 <- glm(Crime_pers ~ Infanticide + Lottery + I(Lottery^2) + Distance + Area + Conscripts + Primary_schools_pd, data = Guerry3)
summary(model8)
cv.err.8 = cv.glm(Guerry3, model8, K = 5)
cv.err.8$delta

# (Model G)
model9 <- glm(Crime_pers ~ Infanticide + Lottery + I(Lottery^2) + Distance + Area + Primary_schools_pd, data = Guerry3)
summary(model9)
cv.err.9 = cv.glm(Guerry3, model9, K = 5)
cv.err.9$delta

# Section 2.5 Diagnostics and Assumptions

plot(model6, which = 5, id.n = 10, labels.id = Guerry$Department, pch = 19, lwd = 2)

residuals(model6)
cor.test(predict(model6), residuals(model6), alternative = "two.sided", method = "kendall")
plot(predict(model6), residuals(model6))
plot(hatvalues(model6), residuals(model6))
a = hatvalues(model6)
b = residuals(model6)
text(a, b, labels = Guerry2$Department, data = Guerry2)
which.max(hatvalues(model6))
which.min(hatvalues(model6))
mean(hatvalues(model6))

student(model6)

vif(model6)

# Region
plot(Guerry$Crime_pers, Guerry$MainCity
     , pch = 19, col = (Guerry$Region))
cor.test(Angeville$Primary_schools, Angeville$Schoolchildren)

