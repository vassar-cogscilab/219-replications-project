library(tidyverse)
library(readr)

# TODO: confirm that these subject IDs are NOT Mturk worker ideas before releasing
# this data at all.

cunillera.data <- read_csv('data/cunillera_data.csv')
cunillera.data$correct <- as.numeric(cunillera.data$correct)
cunillera.data$anchor_condition <- factor(cunillera.data$anchor_condition, levels=c("1","0"), labels=c("anchor", "nonanchor"))

# extract data from test phase
# excluding all cases where the anchor word was tested

test.data <- cunillera.data %>% filter(phase=='test', is.na(is_anchor))

# summarize data

summary.data <- test.data %>% group_by(subject_id, anchor_condition, language_version) %>% summarize(mean.correct = mean(correct))

# main effect

cunillera.d.main <- (mean(summary.data$mean.correct) - 0.5) / sd(summary.data$mean.correct)

t.test(summary.data$mean.correct, mu = 0.5)

# moderator

summary.moderator.group <- summary.data %>% group_by(anchor_condition) %>% summarize(m = mean(mean.correct), sd = sd(mean.correct), se=sd(mean.correct)/n())

cunillera.d.moderator <- (summary.moderator.group$m[1] - summary.moderator.group$m[2]) / sd(summary.data$mean.correct)

