library(tidyverse)
library(readr)

#Need to discuss differences between these results and presentation results

#Read in data
chun.data <- read_csv('data/chun_data.csv')
#Extract data from test phase, filtering out incorrect trial responses
test.data <- chun.data %>% filter(phase=='test') %>% filter(correct==1)

#Convert block from <chr> to <dbl> and add the variable epoch to test.data
test.data$block <- as.numeric(test.data$block)
blockToEpochFunction <- function(x){
  sapply(x, function(x){ #Needed because block is a vector
  if(x >= 0 && x <= 4) 1
  else if(x >= 5 && x <= 9) 2
  else if(x >= 10 && x <= 14) 3
  else NULL
  })}
test.data <- mutate(test.data, epoch = blockToEpochFunction(block))

#Create a tibble called test.means with the mean reaction time for each 
 #participant in each condition
test.means <-
  test.data %>%
  arrange(subject_id, oldORnew, epoch) %>%
  group_by(subject_id, oldORnew, epoch) %>%
  summarise(reaction_time = mean(rt))
test.means

#Perform an ANOVA with Error term() to account for between-subjects variation
test.aov <- with(test.means,
                 aov(reaction_time ~ oldORnew * epoch +
                       Error(subject_id / (oldORnew * epoch)))
)
summary(test.aov)

#Compute effect size by computing the standardized difference between 
 #the cueing effect for epochs 1 and 3

epoch1.cueing.effect <-
  mean(filter(test.data, oldORnew == "new", epoch==1)$rt) -
  mean(filter(test.data, oldORnew == "old", epoch==1)$rt)

epoch3.cueing.effect <-
  mean(filter(test.data, oldORnew == "new", epoch==3)$rt) -
  mean(filter(test.data, oldORnew == "old", epoch==3)$rt)

#Note: If aggregating by subject_id first, the code is slightly different:

#epoch1.cueing.effect <-
  #mean(filter(test.means, oldORnew == "new", epoch==1)$reaction_time) -
  #mean(filter(test.means, oldORnew == "old", epoch==1)$reaction_time)
#epoch3.cueing.effect <-
  #mean(filter(test.means, oldORnew == "new", epoch==3)$reaction_time) -
  #mean(filter(test.means, oldORnew == "old", epoch==3)$reaction_time)

diff <- epoch3.cueing.effect - epoch1.cueing.effect
MSE.interaction <- 22272
d <- diff/sqrt(MSE.interaction)
d
