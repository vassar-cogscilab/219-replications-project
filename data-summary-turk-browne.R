library(tidyverse)
library(readr)

# TODO: confirm that these subject IDs are NOT Mturk worker ideas before releasing
# this data at all.

tb.data <- read_csv('data/turkbrowne_data.csv')
tb.data$correct <- as.numeric(tb.data$correct)
tb.data$attended <- factor(tb.data$attended)

# this subject was excluded in Ben T's analysis, not sure why so we need
# to check on that

excluded.subjects <- c('0c5yfae4oc8t7tk')

# exclude subject and get test data

test.data <- tb.data %>% filter(!subject_id %in% excluded.subjects) %>% filter(phase=='test')

# summarize test data

summary.data <- test.data %>% group_by(subject_id, attended) %>% summarise(mean.correct = mean(correct))
summary.data.main <- test.data %>% group_by(subject_id) %>% summarize(mean.correct = mean(correct))
summary.data.moderator <- summary.data %>% group_by(subject_id) %>% mutate(mean.diff = (mean.correct - lag(mean.correct))) %>% filter(!is.na(mean.diff))

# cohen's d

d.main <- (mean(summary.data.main$mean.correct) - 0.5) / sd(summary.data.main$mean.correct)
d.moderator <- mean(summary.data.moderator$mean.diff) / sd(summary.data.moderator$mean.diff)

# t tests

t.test(summary.data.main$mean.correct, mu=0.5)
t.test(summary.data.moderator$mean.diff)
