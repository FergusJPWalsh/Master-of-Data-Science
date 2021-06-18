# Machine Learning Assignment 2
# Fergus Walsh

library("tree")
library("randomForest")
library("rsample")
library("recipes")
library("keras")
library("tensorflow")

# Download the data set from archive.ics.uci.edu/ml/datasets/Student+Performance and save it in your local R folder.
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

# Create a new data frame d4 with categorical variable "pass"
pass = ifelse(d2$G3 >= 10, "Pass", "Fail")
d4 = data.frame(d2, pass)
d4$pass <- as.factor(d4$pass)

# Convert all categorical variables to factors.
apply(d4, MARGIN = 2, as.factor)

# Used for creating plots.
pass_plot <- table(d4$pass)
school_plot <- table(d4$school)
Walc_plot <- table(d4$Walc)
Dalc_plot <- table(d4$Dalc)
failures_plot <- table(d4$failures)
address_plot <- table(d4$address)
Fedu_plot <- table(d4$Fedu)
Medu_plot <- table(d4$Medu)

# Figures 1-6
hist(d4$G3, main = "Final Exam Scores", xlab = "Exam Score")
barplot(pass_plot, main = "Final Exam Passes & Failures")
barplot(school_plot, main = "Surveyed Students' School", names.arg = c("Gabriel Pereira", "Mousinho da Silveira"))
barplot(address_plot, main = "Surveyed Students' Address Type", names.arg = c("Rural", "Urban"))
barplot(Walc_plot, main = "Weekend Alcohol Consumption", xlab = "Very Low (1) - Very High (5)")
barplot(Dalc_plot, main = "Work-Day Alcohol Consumption", xlab = "Very Low (1) - Very High (5)")
barplot(failures_plot, main = "Number of Previous Exam Failures")
hist(d4$absences, main = "Recorded Absences from School", xlab = "")
plot(d4$Fjob, main = "Father's Job")
plot(d4$Mjob, main = "Mother's Job")
barplot(Fedu_plot, main = "Father's Education")
barplot(Medu_plot, main = "Mother's Education")

# Splitting data into training and testing folds.
set.seed(999)
train_index = sample(1:nrow(d4), 400)
d4_train = d4[train_index,]
d4_test = d4[-train_index,]

# Creating an unpruned single tree.
tree_d4 = tree(pass ~ . -G1 -G2 -G3, data = d4_train)
# Figure 7
plot(tree_d4); text(tree_d4, pretty = 0)
tree_pred = predict(tree_d4, d4_test, type = "class")
summary(tree_d4)
tree_d4

# Error matrix
table(tree_pred, d4_test$pass)
(table(tree_pred, d4_test$pass)[2] + table(prune_pred, d4_test$pass)[3])/249

# Pruning the tree.

dev_df <- data.frame("1" = c(rep(0,50)), 
                     "2" = c(rep(0,50)), 
                     "3" = c(rep(0,50)), 
                     "4" = c(rep(0,50)), 
                     "5" = c(rep(0,50)), 
                     "6" = c(rep(0,50)), 
                     "7" = c(rep(0,50)))
k_df <- data.frame("1" = c(rep(0,50)), 
                   "2" = c(rep(0,50)), 
                   "3" = c(rep(0,50)), 
                   "4" = c(rep(0,50)), 
                   "5" = c(rep(0,50)), 
                   "6" = c(rep(0,50)), 
                   "7" = c(rep(0,50)))
size_df <- data.frame("1" = c(rep(0,50)), 
                      "2" = c(rep(0,50)), 
                      "3" = c(rep(0,50)), 
                      "4" = c(rep(0,50)), 
                      "5" = c(rep(0,50)), 
                      "6" = c(rep(0,50)), 
                      "7" = c(rep(0,50)))
dev = rep(0, 7)
k = rep(0, 7)
size = rep(0, 7)

for (i in 1:50){
  cv_d4 = cv.tree(tree_d4, FUN = prune.misclass)
  for (j in 1:7){
    dev[j] <- cv_d4$dev[j]
    k[j] <- cv_d4$k[j]
    size[j] <- cv_d4$size[j]
    dev_df[i,j] <- dev[j]
    k_df[i,j] <- k[j]
    size_df[i,j] <- size[j]
  }
}

# Figure 8
plot(cv_d4$size, dev, type = "o", xlab = "Number of Terminal Nodes", ylab = "Cross-Validation Error", main = "Candidate Trees by No. Terminal Nodes", xlim = c(-1,26), pch = 16)
points(cv_d4$size[6], dev[6], col = "red", cex = 2, pch = 8, lwd = 2)
plot(cv_d4$k, dev, type = "o", xlab = "Tuning Parameter Value", ylab = "Cross-Validation Error", main = "Candidate Trees by Tuning Parameter" , xlim = c(-1,8), pch = 16)
points(cv_d4$k[6], dev[6], col = "red", cex = 2, pch = 8, lwd = 2)

prune_d4 <- prune.misclass(tree_d4, k = 3)
# Figure 9
plot(prune_d4)
text(prune_d4, pretty = 0)

# Error matrix and error.
prune_pred = predict(prune_d4, d4_test)
prune_pred
table(prune_pred, d4_test$pass)
(table(tree_pred, d4_test$pass)[2] + table(prune_pred, d4_test$pass)[3])/249

# Mean test error for each tree (Table 3)

k_values <- c(k_df[1,1:7])
k_values[2]
prune_d4 <- prune.misclass(tree_d4, k = k_values[1]) # Note tuning parameter must be manually set.
e <- rep(0,1000)

for (i in 1:1000){
  train_index = sample(1:nrow(d4), 400)
  d4_train = d4[train_index,]
  d4_test = d4[-train_index,]
  prune_pred = predict(prune_d4, d4_test, type = "class")
  e[i] <- (table(prune_pred, d4_test$pass)[2] + table(prune_pred, d4_test$pass)[3])/249
}
mean(e)
range(e)[2]-range(e)[1]
min(e)
max(e)

prune_pred = predict(prune_d4, d4_test, type = "class")
(table(prune_pred, d4_test$pass)[2] + table(prune_pred, d4_test$pass)[3])/249

# Random Forest

gini.df <- data.frame("school" = c(rep(0,10)), 
                      "sex" = c(rep(0,10)), 
                      "age" = c(rep(0,10)), 
                      "address" = c(rep(0,10)), 
                      "famsize" = c(rep(0,10)), 
                      "Pstatus" = c(rep(0,10)), 
                      "Medu" = c(rep(0,10)), 
                      "Fedu" = c(rep(0,10)), 
                      "Mjob" = c(rep(0,10)), 
                      "Fjob" = c(rep(0,10)),
                      "reason" = c(rep(0,10)), 
                      "guardian" = c(rep(0,10)), 
                      "traveltime" = c(rep(0,10)), 
                      "studytime" = c(rep(0,10)), 
                      "failures" = c(rep(0,10)), 
                      "schoolsup" = c(rep(0,10)), 
                      "famsup" = c(rep(0,10)), 
                      "paid" = c(rep(0,10)), 
                      "activities" = c(rep(0,10)),
                      "nursery" = c(rep(0,10)), 
                      "higher" = c(rep(0,10)), 
                      "internet" = c(rep(0,10)), 
                      "romantic" = c(rep(0,10)), 
                      "famrel" = c(rep(0,10)), 
                      "freetime" = c(rep(0,10)), 
                      "goout" = c(rep(0,10)), 
                      "Dalc" = c(rep(0,10)), 
                      "Walc" = c(rep(0,10)),
                      "health" = c(rep(0,10)), 
                      "absences" = c(rep(0,10)))
forest_error <- c(rep(0,10))
for (i in 1:10){
  train_index = sample(1:nrow(d4), 400)
  d4_train = d4[train_index,]
  d4_test = d4[-train_index,]
  d4_for <- randomForest(pass ~ . -G1 -G2 -G3, data = d4, subset = train_index, mtry = 5, ntree = 1000, importance = TRUE)
  gini.df[i,1:30] <- importance(d4_for)[,4]
  forest_error[i] <- (table(d4_for_pred, d4_test$pass)[2] + table(d4_for_pred, d4_test$pass)[3])/249
}

# Mean Gini index reduction value per predictor.
gini.list <- apply(gini.df, MARGIN = 2, mean)
gini.list <- sort(gini.list)
gini.list

# Figure 10
par(mar=c(5,7,5,5))
barplot(gini.list, width = 1, space = 0.4, horiz = TRUE, col = "red", xlim = range(0:12), xlab = "Gini Index Reduction", main = "Mean Variable Importance in Random Forest Model", las =1)

# Neural Network

# k-fold split.
d4_split <- vfold_cv(d4, v = 3)

# Data is prepared and network fitted and tested within for-loop.
# Architecture must be manually set in each instance.
for (i in 1:3){
  if (i == 1){
    d4_train <- d4_split$splits[[1]] %>% analysis()
    d4_validate <- d4_split$splits[[2]] %>% analysis()
    d4_test <- d4_split$splits[[3]] %>% analysis()
    d4_fail <- d4 %>% filter(pass == "Fail")
    cake <- recipe(pass ~ ., data = d4) %>% 
      step_meanimpute(all_numeric())%>%
      step_center(all_numeric()) %>%
      step_scale(all_numeric()) %>%
      step_unknown(all_nominal(), - all_outcomes()) %>%
      step_dummy(all_nominal(), one_hot = TRUE) %>%
      prep(training = d4_train)
    d4_train_final <- bake(cake, new_data = d4_train)
    d4_validate_final <- bake(cake, new_data = d4_validate)
    d4_test_final <- bake(cake, new_data = d4_test)
    d4_train_x <- d4_train_final %>%
      select(-starts_with("pass_")) %>%
      as.matrix()
    d4_train_y <- d4_train_final %>%
      select(pass_Fail) %>%
      as.matrix
    d4_validate_x <- d4_validate_final %>%
      select(-starts_with("pass_")) %>%
      as.matrix()
    d4_validate_y <- d4_validate_final %>%
      select(pass_Fail) %>%
      as.matrix()
    d4_test_x <- d4_test_final %>%
      select(-starts_with("pass_")) %>%
      as.matrix()
    d4_test_y <- d4_test_final %>%
      select(pass_Fail) %>%
      as.matrix
    deep.net <- keras_model_sequential() %>%
      layer_dense(units = 16, activation = "relu",
                  input_shape = c(ncol(d4_train_x))) %>%
      layer_dense(units = 16, activation = "relu") %>%
      layer_dense(units = 1, activation = "sigmoid")
    deep.net %>% compile(
      loss = "binary_crossentropy",
      optimizer = optimizer_rmsprop(),
      metrics = c("accuracy")
    )
    
    fit_hist1 <- deep.net %>% fit(
      d4_train_x, d4_train_y,
      epochs = 50, batch_size = 32,
      validation_data = list(d4_validate_x, d4_validate_y),
      verbose = 1,
    )
    
    fit_hist1 <- as.data.frame(fit_hist1)
    pred_test_prob1 <- deep.net %>% predict_proba(d4_test_x)
    pred_test_res1 <- deep.net %>% predict_classes(d4_test_x)
    table1 <- table(pred_test_res1, d4_test_y)
  } else if (i == 2){
    d4_train <- d4_split$splits[[2]] %>% analysis()
    d4_validate <- d4_split$splits[[3]] %>% analysis()
    d4_test <- d4_split$splits[[1]] %>% analysis()
    d4_fail <- d4 %>% filter(pass == "Fail")
    cake <- recipe(pass ~ ., data = d4) %>% 
      step_meanimpute(all_numeric())%>%
      step_center(all_numeric()) %>%
      step_scale(all_numeric()) %>%
      step_unknown(all_nominal(), - all_outcomes()) %>%
      step_dummy(all_nominal(), one_hot = TRUE) %>%
      prep(training = d4_train)
    d4_train_final <- bake(cake, new_data = d4_train)
    d4_validate_final <- bake(cake, new_data = d4_validate)
    d4_test_final <- bake(cake, new_data = d4_test)
    d4_train_x <- d4_train_final %>%
      select(-starts_with("pass_")) %>%
      as.matrix()
    d4_train_y <- d4_train_final %>%
      select(pass_Fail) %>%
      as.matrix
    d4_validate_x <- d4_validate_final %>%
      select(-starts_with("pass_")) %>%
      as.matrix()
    d4_validate_y <- d4_validate_final %>%
      select(pass_Fail) %>%
      as.matrix()
    d4_test_x <- d4_test_final %>%
      select(-starts_with("pass_")) %>%
      as.matrix()
    d4_test_y <- d4_test_final %>%
      select(pass_Fail) %>%
      as.matrix
    deep.net <- keras_model_sequential() %>%
      layer_dense(units = 16, activation = "relu",
                  input_shape = c(ncol(d4_train_x))) %>%
      layer_dense(units = 16, activation = "relu") %>%
      layer_dense(units = 1, activation = "sigmoid")
    deep.net %>% compile(
      loss = "binary_crossentropy",
      optimizer = optimizer_rmsprop(),
      metrics = c("accuracy")
    )
    
    fit_hist2 <- deep.net %>% fit(
      d4_train_x, d4_train_y,
      epochs = 50, batch_size = 32,
      validation_data = list(d4_validate_x, d4_validate_y),
      verbose = 1,
    )
    
    fit_hist2 <- as.data.frame(fit_hist2)
    pred_test_prob2 <- deep.net %>% predict_proba(d4_test_x)
    pred_test_res2 <- deep.net %>% predict_classes(d4_test_x)
    table2 <- table(pred_test_res2, d4_test_y)
  } else if (i == 3){
    d4_train <- d4_split$splits[[3]] %>% analysis()
    d4_validate <- d4_split$splits[[1]] %>% analysis()
    d4_test <- d4_split$splits[[2]] %>% analysis()
    d4_fail <- d4 %>% filter(pass == "Fail")
    cake <- recipe(pass ~ ., data = d4) %>% 
      step_meanimpute(all_numeric())%>%
      step_center(all_numeric()) %>%
      step_scale(all_numeric()) %>%
      step_unknown(all_nominal(), - all_outcomes()) %>%
      step_dummy(all_nominal(), one_hot = TRUE) %>%
      prep(training = d4_train)
    d4_train_final <- bake(cake, new_data = d4_train)
    d4_validate_final <- bake(cake, new_data = d4_validate)
    d4_test_final <- bake(cake, new_data = d4_test)
    d4_train_x <- d4_train_final %>%
      select(-starts_with("pass_")) %>%
      as.matrix()
    d4_train_y <- d4_train_final %>%
      select(pass_Fail) %>%
      as.matrix
    d4_validate_x <- d4_validate_final %>%
      select(-starts_with("pass_")) %>%
      as.matrix()
    d4_validate_y <- d4_validate_final %>%
      select(pass_Fail) %>%
      as.matrix()
    d4_test_x <- d4_test_final %>%
      select(-starts_with("pass_")) %>%
      as.matrix()
    d4_test_y <- d4_test_final %>%
      select(pass_Fail) %>%
      as.matrix
    deep.net <- keras_model_sequential() %>%
      layer_dense(units = 16, activation = "relu",
                  input_shape = c(ncol(d4_train_x))) %>%
      layer_dense(units = 16, activation = "relu") %>%
      layer_dense(units = 1, activation = "sigmoid")
    deep.net %>% compile(
      loss = "binary_crossentropy",
      optimizer = optimizer_rmsprop(),
      metrics = c("accuracy")
    )
    
    fit_hist3 <- deep.net %>% fit(
      d4_train_x, d4_train_y,
      epochs = 50, batch_size = 32,
      validation_data = list(d4_validate_x, d4_validate_y),
      verbose = 1,
    )
    
    fit_hist3 <- as.data.frame(fit_hist3)
    pred_test_prob3 <- deep.net %>% predict_proba(d4_test_x)
    pred_test_res3 <- deep.net %>% predict_classes(d4_test_x)
    table3 <- table(pred_test_res3, d4_test_y)
  }  
}

# Calculating average perfromance statistics.
avg_train_loss <- c((fit_hist1[1:50,"value"] + fit_hist2[1:50,"value"] + fit_hist3[1:50,"value"])/3)
avg_train_acc <- c((fit_hist1[51:100,"value"] + fit_hist2[51:100,"value"] + fit_hist3[51:100,"value"])/3)
avg_val_loss <- c((fit_hist1[101:150,"value"] + fit_hist2[101:150,"value"] + fit_hist3[101:150,"value"])/3)
avg_val_acc <- c((fit_hist1[151:200,"value"] + fit_hist2[151:200,"value"] + fit_hist3[151:200,"value"])/3)
epochs <- c(1:50)

# Figure 11 and 12
plot(epochs, avg_train_loss, type = "o", pch = 16, main = "Average Loss for Neural Network IV", xlab = "Epochs", ylab = "Loss")
points(epochs, avg_val_loss, type = "o", pch = 16, col = "red")
legend("top", legend = c("Training Data", "Validation Data"), col = c("black", "red"), pch = c(16,16))

plot(epochs, avg_train_acc, type = "o", pch = 16, main = "Average Accuracy for Neural Network IV",xlab = "Epochs", ylab = "Accuracy")
points(epochs, avg_val_acc, type = "o", pch = 16, col = "red")
legend("bottomright", legend = c("Training Data", "Validation Data"), col = c("black", "red"), pch = c(16,16))

# Table 4
error1 <- table1[2] +  table2[2]  + table3[2]
error2 <- table1[3] + table2[3] + table3[3]
success1 <- table1[1] +  table2[1]  + table3[1] 
success2 <- table1[4] + table2[4] + table3[4]
error_rate <- (error1 + error2)/649
error_rate

avg_val_loss[25]
avg_val_loss[50]
avg_train_acc[50]
avg_val_acc[50]