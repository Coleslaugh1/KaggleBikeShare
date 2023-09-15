library(tidyverse)
library(vroom)
library(GGally)
library(DataExplorer)
library(patchwork)
train <- vroom("KaggleBikeShare/train.csv")
View(train)

plot1<-ggplot(data = train, mapping = aes(x=count,y=registered))+geom_point()+geom_smooth(se=FALSE)
plot2<-plot_correlation(train)
plot3<-ggplot(data = train, aes(datetime,windspeed))+geom_line()
plot4<-plot_intro(train)

(plot4 + plot2) / (plot1 + plot3)
