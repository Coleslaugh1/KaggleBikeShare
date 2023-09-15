library(tidyverse)
library(vroom)
library(tidymodels)
rawdata <- vroom("KaggleBikeShare/train.csv")
cleandata <- rawdata %>% 
  filter(weather != 4) %>% 
  select(-casual,-registered,-workingday)
my_recipe <- recipe(count~., data=cleandata) %>% # Set model formula and dataset2
  step_time(datetime, features=c("hour", "minute"))#create time variable8
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet14
featuredata <- bake(prepped_recipe, new_data=cleandata)




