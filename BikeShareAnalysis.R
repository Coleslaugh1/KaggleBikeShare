library(tidyverse)
library(vroom)
library(tidymodels)
library(poissonreg)
rawdata <- vroom("KaggleBikeShare/train.csv")
cleandata <- rawdata %>% 
  filter(weather != 4) %>% 
  select(-casual,-registered,-workingday)
my_recipe <- recipe(count~., data=cleandata) %>% # Set model formula and dataset2
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = factor(datetime_hour))
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet14
featuredata <- bake(prepped_recipe, new_data=cleandata)


my_mod <- linear_reg() %>% #Type of model
  set_engine("lm") # Engine = What R function to use

bike_workflow <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(my_mod) %>%
fit(data = featuredata) # Fit the workflow

myNewData<-vroom("KaggleBikeShare/test.csv")

bike_predictions <- predict(bike_workflow,
                            new_data=myNewData) # Use fit to predict

## poisson

pois_mod <- poisson_reg() %>% #Type of model3
  set_engine("glm") # GLM = generalized linear model

bike_pois_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = featuredata) # Fit the workflow

myNewData<-vroom("KaggleBikeShare/test.csv")

bike_predictions <- predict(bike_pois_workflow,
                            new_data=myNewData)

roundto0 <- function(x){
  if(x < 0)
    x <- 0
  else
    x <- x
}

## penalized regression

library(glmnet)

my_recipe <- recipe(count~., data=cleandata) %>% # Set model formula and dataset2
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = factor(datetime_hour)) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_nominal_predictors()) %>% 
  update_role(datetime, new_role = "id") # how to change between id and predictor
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet14
featuredata <- bake(prepped_recipe, new_data=cleandata)

preg_model <- linear_reg(penalty=1, mixture=0) %>% #Set model and tuning11
  set_engine("glmnet") # Function to fit in R12
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data=featuredata)
bike_predictions <- predict(preg_wf, new_data=myNewData)

## tuning models

preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning6
  set_engine("glmnet") # Function to fit in R7
## Set Workflow9
preg_wf <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(preg_model)

## Grid of values to tune over14
tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 10) ## L^2 total tuning possibilities17

## Split data for CV19
folds <- vfold_cv(featuredata, v = 2, repeats=1)

CV_results <- preg_wf %>%
tune_grid(resamples=folds,
          grid=tuning_grid,
          metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL5

## Plot Results (example)7
collect_metrics(CV_results) %>% # Gathers metrics into DF8
  filter(.metric=="rmse") %>%
ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
geom_line()

## Find Best Tuning Parameters13
bestTune <- CV_results %>%
select_best("rmse")

final_wf <-
  preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=featuredata)

## Predict7
bike_predictions<- final_wf %>%
  predict(new_data = myNewData)

## regression trees





final<-bike_predictions %>% 
  mutate(datetime = as.character(format(myNewData$datetime))) %>% 
  mutate(count = map(.pred,roundto0)) %>% 
  mutate(count = as.numeric(count)) %>% 
  select(datetime,count)

vroom_write(final,"preds.csv",delim = ",")

