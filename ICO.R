# Clear workspace and set working directory
rm(list = ls())
#-----------------------------------------------------------------------------
# Add packages to Library
#-----------------------------------------------------------------------------
pkgs <- c("dplyr", "visdat", "ggplot2", "patchwork", "VIM",
          "stringr", "missForest", "caret", "corrgram",
          "randomForest", "class", "gmodels", "naivebayes",
          "C50", "kernlab", "tidyverse", "pROC", "irr")
invisible(lapply(pkgs, library, character.only = TRUE))
#-----------------------------------------------------------------------------
# Data understanding 
#-----------------------------------------------------------------------------
data <- read.csv("LUBS5990M_courseworkData_202425.csv",encoding = 'UTF-8')
str(data) #6146 obs. of 25 variables 
summary(data)
data<-as.data.frame(data)

###Visualisation of variables 
#numeric+integer 
num_vars <- data %>% 
  select(where(is.numeric) | where(is.integer)) %>% 
  names()

num_plots <- map(num_vars, function(v){
  x <- data[[v]]
  if (n_distinct(x, na.rm = TRUE) <= 10) {            
    ggplot(data, aes(x = factor(.data[[v]]))) +
      geom_bar(na.rm = TRUE) +
      labs(title = v, x = NULL, y = "count")
  } else {
    ggplot(data, aes(x = .data[[v]])) +               
      geom_histogram(bins = 30, na.rm = TRUE)+
      labs(title = v, x = NULL, y = "count")
  }
})

wrap_plots(num_plots, ncol = 3)
#character (variables with less than 50 levels)
cat_vars <- data %>% select(where(is.character), where(is.factor)) %>% 
  names() %>% keep(~ n_distinct(data[[.x]], na.rm = TRUE) <= 50)

cat_plot <- map(cat_vars, ~
  ggplot(data, aes(x = .data[[.x]])) +
    geom_bar(na.rm = TRUE) +
    labs(title = .x, x = NULL, y = "count") 
)
wrap_plots(cat_plot, ncol = 3)  

sum(complete.cases(data))
sum(!complete.cases(data)) #6146 missing values 
vis <- vis_miss(data)#visualisation of missing values 
#29.7% missing, 70.3% present 

#-----------------------------------------------------------------------------
# Data preparation 
#-----------------------------------------------------------------------------

###Handling missing values 
#--------------------------
#Delete rows,if there is more than 50% missing values
data<- data [rowMeans(is.na(data)) < 0.5,]
#Delete columns,if variables has more than 50% missing values
data<- data [, colMeans(is.na(data)) < 0.5]
# remove "link" variables 
data <- data %>% 
  select(-link_white_paper, -linkedin_link, -github_link, -website)

# find the row that starts with only the missing values in the variables "ERC20" and "rating".

first_row <- max(which(!is.na(data$rating) | !is.na(data$ERC20)))
first_row #[1] 5226
#delete rows from 5226 because there are only missing values 
data <- data[1:5226,]

###Сonvert the format of variables
#---------------------------------

#Change format for success variable to factor
data$success <- case_when(
  data$success =="Y" ~ 1L,
  data$success == "N" ~ 0L,
  TRUE ~ NA_integer_)
data$success  <- as.factor(data$success )
#Change format for whitelist variable to factor
data$whitelist <- case_when(
  data$whitelist =="Yes" ~ 1L,
  data$whitelist == "No" ~ 0L,
  TRUE ~ NA_integer_)
data$whitelist <- as.factor(data$whitelist)
#Change format for ico_start variable to date
data$ico_start[data$ico_start == ""] <- NA 
data$ico_start_1 <- dmy(data$ico_start) #3 failed to parse
#find rows where format isn't date 
wrong_idx <- which(is.na(data$ico_start_1) & !is.na(data$ico_start))
wrong_idx # [1]  1395  1499  5324 
#delete these rows 
data <- data[-wrong_idx,]
data$ico_start <- data$ico_start_1
data<- data %>% 
  select (-ico_start_1)
data$ico_start_num <- as.numeric(data$ico_start)
data <- data %>% select (-ico_start)
#Change format for ico_end variable to date
data$ico_end[data$ico_end == ""] <- NA 
data$ico_end <- dmy(data$ico_end)
data$ico_end_num <- as.numeric(data$ico_end)
data <- data %>% select (-ico_end)
#Change format for country variables 
data$country <- as.factor (data$country)
#Create new variable for accepting currencies
data <- data %>%
  mutate (
    num_currencies = (case_when(
      is.na(data$accepting) ~ NA_integer_, 
      TRUE ~ accepting %>%
        {\(x) ifelse(x == "", 0L, str_count(x, ",") + 1L)}()
    )
    )
  )
data$num_currencies <- as.numeric (data$num_currencies)
data<- data %>% 
  select (-accepting)
#Change format for teamsize variables 
data$teamsize <- as.numeric(data$teamsize)
#Change format for price_usd variables 
data$price_usd <- as.numeric(data$price_usd)
#Change format for distributed_in_ico variables 
data$distributed_in_ico <- as.numeric(data$distributed_in_ico)
#Change format for integer variables
data$kyc <- as.factor (data$kyc)
data$bonus <- as.factor(data$bonus)
data$ERC20 <- as.factor(data$ERC20)

#checking the data before imputation
str(data)
vis_miss(data)
colSums(is.na(data))

#Imputation 
#-----------
data_imi <- data[, !names(data) %in% c("country")] #deleting the "country" variable because the number of levels is more than 32
set.seed(123)
imi <- missForest(data_imi)
data_full <- imi$ximp
colSums(is.na(data_full))

#Creation of dummy variables (except "success")
#--------------------------------------------
d <- dummyVars(~ . - success, data = data_full) 
dummy <- predict(d, newdata = data_full)
newdata <- as.data.frame(dummy) %>%
  mutate(success = data_full$success)

###Correlation 
#-------------
corrgram(newdata)
#importance metrics
rf <- randomForest(success ~ ., data = newdata, importance = TRUE)
imp <- importance(rf)
print(imp)
varImpPlot(rf)

#delete variables with less importance 
newdata <- newdata %>% select (c(-ERC20.0,-ERC20.1,-whitelist.1,-whitelist.0,))
str(newdata)
#-----------------------------------------------------------------------------
# Modelling
#-----------------------------------------------------------------------------
#Conversion of the target variable
newdata$success <- factor(newdata$success, 
                          levels = c("0", "1"),
                          labels = c("failure", "success"))

#-----------------------------------------------------------------------------
#Classification using Nearest Neighbour
#--------------------------------------

#Normalisation of numeric variables 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# Apply normalisation to all numeric features 
newdata_n <- newdata %>%
  select(-success) %>%                      
  mutate(across(where(is.numeric), normalize))
# Set train-test split parameters
# Set parameters for train-test split for KNN model 
TRAIN_RATIO <- 0.8
RANDOM_SEED <- 123

# Create train-test split
set.seed(RANDOM_SEED)
train_size <- floor(TRAIN_RATIO * nrow(newdata_n))
train_indices <- sample(nrow(newdata_n), train_size)
train_data <- newdata_n[train_indices, ]
test_data <- newdata_n[-train_indices, ]
test_size <- nrow(newdata_n) - train_size

# Create corresponding label vectors
labels <- newdata$success
data_train_labels <- labels[train_indices]
data_test_labels <- labels[-train_indices]

#cross-validation control
k_values <- seq(1, 65, by = 2)
ctrl <- trainControl(
  method         = "cv",
  number         = 5,
  classProbs     = TRUE,
  summaryFunction = twoClassSummary
)
#number of k 
knn_tune <- train(
  success ~ .,
  data       = newdata,
  method     = "knn",
  preProcess = c("center","scale"),
  tuneGrid   = data.frame(k = k_values),
  metric     = "ROC",
  trControl  = ctrl
)
best_k <- knn_tune$bestTune$k 
best_k
# Generate predictions
success_test_pred <- knn(train = train_data,
                        test = test_data,
                        cl = data_train_labels,
                        k = best_k, prob=TRUE)
knn_prob <- attr (success_test_pred, "prob")
knn_prob_success <- ifelse (success_test_pred =="success",knn_prob, 1- knn_prob)

knn_results <- data.frame (
  actual = data_test_labels,
  predicted = success_test_pred,
  prob_success = round (knn_prob_success,5),
  prob_failure = round (1-knn_prob_success,5)
)

#-----------------------------------------------------------------------------
# creating training and test datasets for other models (without nirmalisation)
set.seed(RANDOM_SEED)
train_idx <- sample(nrow(newdata), size = TRAIN_RATIO * nrow(newdata))
d_train  <- newdata[train_idx, ]
d_test   <- newdata[-train_idx, ]
labels <- newdata$success
d_train_labels <- labels[train_idx]
d_test_labels <- labels[-train_idx]
#-----------------------------------------------------------------------------
#Classification using Naïve bayes
#--------------------------------
#Training a model on the data
naive_model <- naive_bayes(d_train$success ~ ., data = d_train)
pred_naive <- predict(naive_model, newdata = d_test)
nb_prob <- predict(naive_model, newdata = d_test, type ="prob")
nb_results <- data.frame (
  actual = d_test_labels,
  predicted = pred_naive,
  prob_success = round (nb_prob[, "success"], 5),
  prob_failure = round (nb_prob[, "failure"], 5)
)
#-----------------------------------------------------------------------------
#Classification using Decision Trees
# Basic Model
basic_model <- C5.0(success ~ ., data = d_train)
# display simple facts about the tree
basic_model
# display detailed information about the tree
summary(basic_model)

# make predictions
# create a factor vector of predictions on test data
pred_trees <- predict(basic_model, d_test)
prob_trees <- predict(basic_model, d_test, type = "prob")
trees_results <- data.frame (
  actual = d_test_labels,
  predicted = pred_trees,
  prob_success = round (prob_trees[, "success"], 5),
  prob_failure = round (prob_trees[, "failure"], 5)
)

#optimal number of boost 
boost_grid <- expand.grid(
  trials = seq(1, 30, by = 5),
  model  = "tree",      
  winnow = FALSE        
)
set.seed(123)
boost_tuned <- train(
  success ~ .,
  data       = d_train,
  method     = "C5.0",
  metric     = "ROC",
  tuneGrid   = boost_grid,
  trControl  = ctrl
)
boost_tuned
best_trials <- boost_tuned$bestTune$trials
print (best_trials)

# boosted decision tree
BOOST_TRIALS <- best_trials
boost <- C5.0(success ~ ., data = d_train,
                trials = BOOST_TRIALS)

boost_pred <- predict(boost, d_test)
boost_prob <- predict(boost, d_test, type = "prob")
boost_results <- data.frame (
  actual = d_test_labels,
  predicted = boost_pred,
  prob_success = round (boost_prob[, "success"], 5),
  prob_failure = round (boost_prob[, "failure"], 5)
)

#-----------------------------------------------------------------------------
#Classification using Support Vector Machines
#--------------------------------------------
svm_model <- ksvm (success ~ ., data = d_train, kernel="rbfdot", prob.model=TRUE)
print (svm_model)
svm_pred <- predict (svm_model, select (d_test, -success))
svm_prob <- predict(svm_model, d_test, type = "prob")
svm_results <- data.frame (
  actual = d_test_labels,
  predicted = svm_pred,
  prob_success = round (svm_prob[, "success"], 5),
  prob_failure = round (svm_prob[, "failure"], 5)
)
#-----------------------------------------------------------------------------
# Evaluation 
#-----------------------------------------------------------------------------
POSITIVE_CLASS <- "success"  # Define positive class for metrics
NEGATIVE_CLASS <- 'failure'
# Evaluation of Nearest Neighbour
#-----------------------------------
# Examine predictions
head (nb_results)
head (subset(nb_results, actual !=predicted))
# Create confusion matrix
CrossTable(knn_results$actual,
           knn_results$predicted,
           dnn= c("actual", "predicted"),
           prop.chisq = FALSE)
# Calculate comprehensive metrics
conf_matrix_knn <- confusionMatrix(knn_results$predicted, knn_results$actual, positive =POSITIVE_CLASS)
conf_matrix_knn
# Create ROC curve for Naive Bayes
knn_roc <- roc (knn_results$actual, knn_results$prob_success)
# Plot settings
par(mar = c(4, 4, 4, 4))  # Adjust margins
plot(knn_roc, 
     main = "ROC curve for k-NN model",
     col = "blue", 
     lwd = 2, 
     grid = TRUE, 
     legacy.axes = TRUE)
auc(knn_roc)

#Classification using Naïve bayes
#-----------------------------------
# Examine predictions
head (nb_results)
head (subset(nb_results, actual !=predicted))
print("Base Model Performance:")
CrossTable(nb_results$actual,
           nb_results$predicted,
           dnn= c("actual", "predicted"),
           prop.chisq = FALSE)
# Calculate comprehensive metrics
conf_matrix_nb <- confusionMatrix(nb_results$predicted, nb_results$actual, positive =POSITIVE_CLASS)
conf_matrix_nb
# Create ROC curve for Naive Bayes
nb_roc <- roc (nb_results$actual, nb_results$prob_success)
# Plot settings
par(mar = c(4, 4, 4, 4))  # Adjust margins
plot(nb_roc, 
     main = "ROC curve for Naive Bayes model",
     col = "blue", 
     lwd = 2, 
     grid = TRUE, 
     legacy.axes = TRUE)
auc(nb_roc)
#Decision Trees
#-----------------------------------
# Evaluate base model
head (trees_results)
head (subset(trees_results, actual !=predicted))
print("Decision Trees Model Performance:")
CrossTable(trees_results$actual,
           trees_results$predicted,
           dnn= c("actual", "predicted"),
           prop.chisq = FALSE)
# Calculate comprehensive metrics
conf_matrix_trees <- confusionMatrix(trees_results$predicted, trees_results$actual, positive =POSITIVE_CLASS)
conf_matrix_trees
# Create ROC curve for Naive Bayes
trees_roc <- roc (trees_results$actual, trees_results$prob_success)
# Plot settings
par(mar = c(4, 4, 4, 4))  # Adjust margins
plot(trees_roc, 
     main = "ROC curve for Decision Trees model",
     col = "blue", 
     lwd = 2, 
     grid = TRUE, 
     legacy.axes = TRUE)
auc(trees_roc)
#boosted decision tree
#-----------------------------------
# Evaluate base model
head (boost_results)
head (subset(boost_results, actual !=predicted))
print("Boosted Decision Trees Model Performance:")
CrossTable(boost_results$actual,
           boost_results$predicted,
           dnn= c("actual", "predicted"),
           prop.chisq = FALSE)
# Calculate comprehensive metrics
conf_matrix_boost <- confusionMatrix(boost_results$predicted, boost_results$actual, positive =POSITIVE_CLASS)
conf_matrix_boost
# Create ROC curve for Naive Bayes
boost_roc <- roc (boost_results$actual, boost_results$prob_success)
# Plot settings
par(mar = c(4, 4, 4, 4))  # Adjust margins
plot(boost_roc, 
     main = "ROC curve for Boosted C5.0",
     col = "blue", 
     lwd = 2, 
     grid = TRUE, 
     legacy.axes = TRUE)
auc(boost_roc)

#SVM
#-----------------------------------
# Evaluate base model
head (svm_results)
head (subset(svm_results, actual !=predicted))
print("SVM Model Performance:")
CrossTable(svm_results$actual,
           svm_results$predicted,
           dnn= c("actual", "predicted"),
           prop.chisq = FALSE)
# Calculate comprehensive metrics
conf_matrix_svm<- confusionMatrix(svm_results$predicted, svm_results$actual, positive =POSITIVE_CLASS)
conf_matrix_svm
# Create ROC curve for Naive Bayes
svm_roc <- roc (svm_results$actual, svm_results$prob_success)
# Plot settings
par(mar = c(4, 4, 4, 4))  # Adjust margins
plot(svm_roc, 
     main = "ROC curve for SVM model",
     col = "blue", 
     lwd = 2, 
     grid = TRUE, 
     legacy.axes = TRUE)
auc(svm_roc)

#summary
summary <- bind_rows(
  tibble(
    model       = "k-NN",
    Accuracy    = as.numeric(conf_matrix_knn$overall["Accuracy"]),
    Sensitivity = as.numeric(conf_matrix_knn$byClass["Sensitivity"]),
    Specificity = as.numeric(conf_matrix_knn$byClass["Specificity"]),
    AUC         = as.numeric(auc(knn_roc))
  ),
  tibble(
    model       = "Naive Bayes",
    Accuracy    = as.numeric(conf_matrix_nb$overall["Accuracy"]),
    Sensitivity = as.numeric(conf_matrix_nb$byClass["Sensitivity"]),
    Specificity = as.numeric(conf_matrix_nb$byClass["Specificity"]),
    AUC         = as.numeric(auc(nb_roc))
  ),
  tibble(
    model       = "Decision Tree",
    Accuracy    = as.numeric(conf_matrix_trees$overall["Accuracy"]),
    Sensitivity = as.numeric(conf_matrix_trees$byClass["Sensitivity"]),
    Specificity = as.numeric(conf_matrix_trees$byClass["Specificity"]),
    AUC         = as.numeric(auc(trees_roc))
  ),
  tibble(
    model       = "Boosted C5.0",
    Accuracy    = as.numeric(conf_matrix_boost$overall["Accuracy"]),
    Sensitivity = as.numeric(conf_matrix_boost$byClass["Sensitivity"]),
    Specificity = as.numeric(conf_matrix_boost$byClass["Specificity"]),
    AUC         = as.numeric(auc(boost_roc))
  ),
  tibble(
    model       = "SVM",
    Accuracy    = as.numeric(conf_matrix_svm$overall["Accuracy"]),
    Sensitivity = as.numeric(conf_matrix_svm$byClass["Sensitivity"]),
    Specificity = as.numeric(conf_matrix_svm$byClass["Specificity"]),
    AUC         = as.numeric(auc(svm_roc))
  )
)

print(summary)

# Summary of ROC curves
par(mfrow=c(1,1))
plot(knn_roc,
     col   = "blue",
     lwd   = 2,
     legacy.axes = TRUE,
     grid  = TRUE,
     main  = "ROC curves for all models")

lines(nb_roc,    col = "red", lwd = 2)
lines(trees_roc, col = "green", lwd = 2)
lines(boost_roc, col = "black", lwd = 2)
lines(svm_roc,   col = "pink", lwd = 2)

col <- c("blue", "red", "green", "black", "pink")
legend("bottomright",
       legend = c(
         sprintf("k-NN (AUC=%.3f)",  auc(knn_roc)),
         sprintf("NaiveBayes (AUC=%.3f)", auc(nb_roc)),
         sprintf("DecisionTree (AUC=%.3f)", auc(trees_roc)),
         sprintf("Boosted C5.0 (AUC=%.3f)", auc(boost_roc)),
         sprintf("SVM (AUC=%.3f)", auc(svm_roc))
       ),
       lwd  = 2,
       col = col,
       cex  = 0.8)
