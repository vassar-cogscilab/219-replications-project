library(tidyverse)
library(readr)

#Need to confirm which blocks correspond to which epochs

#Read in data and extract data from test phase
chun.data <- read_csv('data/chun_data.csv')
test.data <- chun.data %>% filter(phase=='test')

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

#Compute Cohen's d