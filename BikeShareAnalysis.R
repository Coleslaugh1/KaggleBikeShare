setwd(dir = "C:/Users/camer/Documents/Stat 348/KaggleBikeShare/")

library(tidyverse)
library(vroom)
library(tidymodels)
library(poissonreg)
library(rpart)
rawdata <- vroom("train.csv")
cleandata <- rawdata %>% 
  filter(weather != 4) %>% 
  mutate(count = log(count)) %>% 
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

my_recipe <- recipe(count~., data=cleandata) %>% # Set model formula and dataset2
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = factor(datetime_hour)) %>% 
  update_role(datetime, new_role = "id") # how to change between id and predictor
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet14
featuredata <- bake(prepped_recipe, new_data=cleandata)
featuredata <- featuredata %>% 
  mutate(datetime_hour = as.numeric(datetime_hour))

my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>% #Type of model6
  set_engine("rpart") %>% # Engine = What R function to use7
  set_mode("regression")

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

## Grid of values to tune over14
tuning_grid <- grid_regular(cost_complexity(),
                            tree_depth(),
                            min_n(),
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
  ggplot(data=., aes(x=cost_complexity, y=mean, color=factor(tree_depth))) +
  geom_line()

## Find Best Tuning Parameters13
bestTune <- CV_results %>%
  select_best("rmse")

final_wf <-
  preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=featuredata)

## random forest

library(ranger)

cleandata <- rawdata %>% 
  filter(weather != 4) %>% 
  mutate(count = log(count)) %>% 
  select(-casual,-registered,-workingday)

my_recipe <- recipe(count~., data=cleandata) %>% # Set model formula and dataset2
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = as.factor(datetime_hour)) %>% 
  step_date(datetime, features=c("year")) %>% #create time variable
  step_mutate(datetime_year = as.factor(datetime_year)) %>%
  update_role(datetime, new_role = "id") # how to change between id and predictor
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet14
featuredata <- bake(prepped_recipe, new_data=cleandata)

my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=250) %>% #Type of model6
  set_engine("ranger") %>% # What R function to use7
  set_mode("regression")

randf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

## Grid of values to tune over14
tuning_grid <- grid_regular(mtry(range=c(1,9)),
                            min_n(),
                            levels = 10) ## L^2 total tuning possibilities17

## Split data for CV19
folds <- vfold_cv(featuredata, v = 3, repeats=3)

CV_results <- randf_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
              metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL5

# # Plot Results (example)7
collect_metrics(CV_results) %>% # Gathers metrics into DF8
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=mtry, y=mean, color=factor(min_n))) +
  geom_line()

## Find Best Tuning Parameters13
bestTune <- CV_results %>%
  select_best("rmse")

final_wf <-
  randf_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=featuredata)

## stacking

library(stacks)
my_recipe <- recipe(count~., data=cleandata) %>% # Set model formula and dataset2
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = as.numeric(datetime_hour)) %>% 
  update_role(datetime, new_role = "id") # how to change between id and predictor
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet14
featuredata <- bake(prepped_recipe, new_data=cleandata)


folds <- vfold_cv(featuredata, v = 2, repeats=1)
untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

## Penalized regression model10
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning12
  set_engine("glmnet") # Function to fit in R13

## Set Workflow15
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model)

## Grid of values to tune over20
preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 10) ## L^2 total tuning possibilities


preg_model <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=preg_tuning_grid,
            metrics=metric_set(rmse, mae, rsq),
            control = untunedModel) # including the control grid in the tuning ensures you can6
# call on it later in the stacked model

lin_reg <-
  linear_reg() %>%
  set_engine("lm")
lin_reg_wf <-
  workflow() %>%
  add_model(lin_reg) %>%
  add_recipe(my_recipe)
lin_reg_model <-
  fit_resamples(
    lin_reg_wf,
    resamples = folds,
    metrics = metric_set(rmse),
    control = tunedModel
  )

my_stack <- stacks() %>%
add_candidates(preg_model) %>%
add_candidates(lin_reg_model)

stack_mod <- my_stack %>%
blend_predictions() %>% # LASSO penalized regression meta-learner10
  fit_members()

stackData <- as_tibble(my_stack)

bike_predictions <- stack_mod %>% predict(new_data=myNewData)

# bart

library(dbarts)

cleandata <- rawdata %>% 
  filter(weather != 4) %>% 
  mutate(count = log(count)) %>% 
  select(-casual,-registered,-workingday)


my_recipe <- recipe(count~., data=cleandata) %>% # Set model formula and dataset2
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = as.factor(datetime_hour)) %>% 
  step_date(datetime, features=c("year")) %>% #create time variable
  step_mutate(datetime_year = as.factor(datetime_year)) %>%
  update_role(datetime, new_role = "id") # how to change between id and predictor
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet14
featuredata <- bake(prepped_recipe, new_data=cleandata)

my_mod <- bart(trees=300) %>% #Type of model6
  set_engine("dbarts") %>% # What R function to use7
  set_mode("regression")

bart_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)


final_wf <-
  bart_wf %>%
  fit(data=featuredata)


## Predict7

myNewData<-vroom("test.csv")

bike_predictions<- final_wf %>%
  predict(new_data = myNewData)


roundto0 <- function(x){
  if(x < 0)
    x <- 0
  else
    x <- x
}


final<-bike_predictions %>% 
  mutate(datetime = as.character(format(myNewData$datetime))) %>% 
  mutate(count = map(.pred,roundto0)) %>% 
  mutate(count = as.numeric(count)) %>% 
  mutate(count = exp(count))%>% 
  select(datetime,count)

vroom_write(final,"preds.csv",delim = ",")



# year a factor
# hour as a factor
# model to predict registered and another for casual
# bart gives low scores try that if fail


# barts tuned with 300 trees
# predicts casual and registerd speratly and add them together
# log the repsonse