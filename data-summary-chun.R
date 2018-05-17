library(tidyverse)
library(readr)

#Read in data
chun.data <- read_csv('data/chun_data.csv')
#Extract data from test phase, filtering out incorrect trial responses
test.data <- chun.data %>% filter(phase=='test') %>% filter(correct==1)

#Exclude subjects 'jwne38k3p6nbxh8' and 'rfe3y2ty3vvg1w5'
test.data <- test.data %>% filter((subject_id != 'jwne38k3p6nbxh8') & 
                                    (subject_id != 'rfe3y2ty3vvg1w5'))
#Exclude reaction time outliers (over 4000)
test.data <- test.data %>% filter(rt <= 4000)

#Convert block from <chr> to <dbl> and add the variable epoch to test.data
test.data$block <- as.numeric(test.data$block)
blockToEpochFunction <- function(x){
  sapply(x, function(x){ #Needed because block is a vector
    #Blocks 0-4 correspond to Epoch 1
    if(x >= 0 && x <= 4) 1
    #Blocks 5-9 correspond to Epoch 2
    else if(x >= 5 && x <= 9) 2
    #Blocks 10-14 correspond to Epoch 3
    else if(x >= 10 && x <= 14) 3
    else NULL
  })}
test.data <- mutate(test.data, epoch = blockToEpochFunction(block))
test.data

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

#Compute effect size by computing the standardized mean difference in
 #reaction time for new and old stimuli in Epoch 3 (cueing effect)

#Store the reaction times for new and old stimuli in Epoch 3
epoch3.new <- filter(test.means, oldORnew == "new", epoch==3)$reaction_time
epoch3.old <- filter(test.means, oldORnew == "old", epoch==3)$reaction_time

#Compute the pairwise differences
epoch3.diff <- epoch3.new - epoch3.old

#Effect size = mean difference / sd(pairwise differences)
d <- mean(epoch3.diff)/sd(epoch3.diff)
d
