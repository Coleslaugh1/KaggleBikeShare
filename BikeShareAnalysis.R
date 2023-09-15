library(tidyverse)
library(vroom)
library(tidymodels)
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

roundto0 <- function(x){
  if(x <0)
    x <- 0
  else
    x <- x
}

final<-bike_predictions %>% 
  mutate(datetime = myNewData$datetime) %>% 
  mutate(count = map(.pred,roundto0)) %>% 
  mutate(count = as.numeric(count)) %>% 
  select(datetime,count)

vroom_write(final,"preds.csv",delim = ",")

