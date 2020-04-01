# Import libraries & dataset ----
library(data.table)
library(tidyverse)
library(scorecard)
library(inspectdf)
library(h2o)
library(highcharter)
library(stats)

data <- fread("C:/Users/dell/Downloads/credit.csv")
data <- data %>% rename('target' = 'creditability')

data %>% glimpse()

data$target <- as.factor(data$target)
data$target <- as.numeric(data$target)
data$target <- ifelse(data$target>1, 1, 0)

# BINNING ----

# IV (important variables) ---
iv <- data %>% 
  iv(y = 'target') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3)) %>%
  arrange( desc(info_value) )

# Exclude not important variables ---
ivars <- iv %>% 
  filter(info_value>0.02) %>% 
  select(variable) %>% 
  .[[1]] 

data_iv <- data %>% 
  select(ivars,target)

data_iv %>% dim()


# breaking dt into train and test ---
dt_list <- split_df(data_iv, "target", ratio = 0.8, seed=123)
train <- dt_list$train 
test <- dt_list$test


# woe binning ---
bins <- data_iv %>% woebin("target")

# converting train and test into woe values
train_woe <- train %>% woebin_ply(bins) 
test_woe <- test %>% woebin_ply(bins)


names <- train_woe %>% names()                     
names <- gsub("_woe","",names)                     
names(train_woe) <- names                          
names(test_woe) <- names
train_woe %>% inspect_na(show_plot = F) 
test_woe %>% inspect_na(show_plot = F) 

# # Check normality 
# num_vars <- train_woe %>% 
#   select(-target) %>% 
#   names()
# num_vars
# 
# norm <- c()
# for (s in 1:length(num_vars)) {
#   val <- round(e1071::skewness(train_woe[[num_vars[s]]]), 2)
#   norm[s] <- val
# }
# 
# par(mfrow=c(5, 10))  # divide graph area in 2columns & 2rows (number of variables)
# 
# for (s in 1:length(num_vars)) {
#   var.name = num_vars[s]
#   plot(density(train_woe[[num_vars[s]]]),
#        main=glue('{enexpr(var.name)}'), 
#        ylab="Frequency", 
#        sub=paste("Skewness:", round(e1071::skewness(train_woe[[num_vars[s]]]), 2)))  
#   polygon(density(train_woe[[num_vars[s]]]), col="red")
# }

# Logistic Linear Regression Diagnostics ----
outcome <- 'target'
features <- train_woe %>% select(-target) %>% names()

f <- as.formula(paste(outcome, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = train_woe)
glm %>% summary()

# Select a formula-based model by AIC
step <- glm %>% step()
step$call # copy paste

glm2 <- glm(formula = target ~ status.of.existing.checking.account + 
              duration.in.month + credit.history + age.in.years + 
              savings.account.and.bonds + 
              purpose + present.employment.since + housing + 
              other.installment.plans + 
              credit.amount + other.debtors.or.guarantors + 
              installment.rate.in.percentage.of.disposable.income, 
            data = train_woe)
glm2 %>% summary()

glm2 %>% 
  coefficients() %>% 
  as.data.frame() %>%
  rownames() %>% 
  .[-1] %>% 
  as.factor() -> all.vars
all.vars %>% length()
all.vars_char <- all.vars %>% as.character()

glm2 %>% vif() %>% arrange(desc(gvif)) %>% 
  pull(variable) -> all_vars
# Multicollinrarity
hchart(cor(train_woe %>% select(data$target,all_vars) %>% round(.,2),label = T)
    

# VIF - glm2
# https://www.statisticshowto.datasciencecentral.com/variance-inflation-factor/
glm2 %>% vif() %>% arrange(desc(gvif)) %>% 
  filter(gvif<10) %>% 
  pull(variable) -> afterVIF

f <- as.formula(paste(outcome, paste(afterVIF, collapse = " + "), sep = " ~ "))
glm3 <- glm(f, data = train_woe)




glm3 %>% summary()
glm3 %>% vif() %>% arrange(desc(gvif)) %>% 
  pull(variable) -> selected

hchart(cor(
  train_woe %>% 
    select(target,selected)) %>%
    round(.,2),label = T)


# Modeling with GLM ----
h2o.init()
train_h2o <- as.h2o(train_woe %>% select(target,selected)) 
test_h2o <- as.h2o(test_woe %>% select(target,selected))

outcome <- "target"
features <- train_woe %>% select(selected) %>% 
  names()

model <- h2o.glm(
  x = features,
  y = outcome,
  training_frame = train_h2o,
  family = "binomial", 
  seed = 123,
  nfolds = 10, #Number of folds for K-fold cross-validation
  remove_collinear_columns = T, #Collinear columns can cause problems during model fitting. This option can only be used with the 'IRLSM' solver
  #balance_classes = T, 
  max_runtime_secs = 180
)

model %>% h2o.auc() %>% round(2)
#model %>% h2o.giniCoef() %>% round(2)

model %>% h2o.performance(newdata = test_h2o) %>% h2o.auc() %>% round(2)
#model %>% h2o.performance(newdata = test_h2o) %>% h2o.giniCoef() %>% round(2)


model %>% h2o.std_coef_plot()

model@model$coefficients %>% as.data.frame() %>% 
  mutate(names = rownames(model@model$coefficients %>% as.data.frame())) %>%
  `colnames<-`(c('coefficients','names')) %>% 
  select(names,coefficients) %>%
  filter(coefficients != 0) %>%
  arrange(desc(coefficients))

h2o.varimp(model) %>% as.data.frame() %>% 
  pull(percentage) %>% sum()

h2o.varimp(model) %>% as.data.frame() %>% .[.$percentage>0,] %>%
  pull(variable) -> imp.vars
imp.vars %>% length()

h2o.varimp(model) %>% as.data.frame() %>% .[.$percentage != 0,] %>%
  select(variable, percentage) %>%
  hchart("pie", hcaes(x = variable, y = percentage)) %>%
  hc_colors(colors = 'orange') %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)

model %>% h2o.performance(newdata = test_h2o) %>% 
  h2o.find_threshold_by_max_metric('f1')

pred <- model %>% h2o.predict(newdata = test_h2o) %>% as.data.frame()
pred %>% select(predict) %>% table()


# scorecard
card <- bins %>% scorecard(model@model)

# credit score, only_total_score = TRUE
train_score <- train %>% scorecard_ply(card)
test_score <- test %>% scorecard_ply(card)

# psi
psi <- perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train$target, test = test$target)
)
psi$psi  
#psi$pic  


# only_total_score = FALSE
train_score2 <- train %>% scorecard_ply(card, only_total_score=FALSE)
test_score2 <- test %>% scorecard_ply(card, only_total_score=FALSE)

# psi
psi2 <- perf_psi(
  score = list(train = train_score2, test = test_score2),
  label = list(train = train$target, test = test$target)
)
psi2$psi  
#psi2$pic

# AUC
perf <- h2o.performance(model, train_h2o)
train_auc<-h2o.auc(perf, xval = TRUE)

perf <- h2o.performance(model, test_h2o)
test_auc<-h2o.auc(perf, xval = TRUE)

tibble(train_auc, test_auc)
