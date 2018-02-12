library(tidyverse)
library(readr)

# TODO: confirm that these subject IDs are NOT Mturk worker ideas before releasing
# this data at all.

saffran.data <- read_csv('data/saffran_data.csv')
saffran.data$correct <- as.numeric(saffran.data$correct)
saffran.data$button_pressed <- as.numeric(saffran.data$button_pressed)
saffran.data$assigned_condition <- factor(saffran.data$assigned_condition)

# get original number of subjects

length(unique(saffran.data$subject_id))

# check for reports of technical difficulties or more than 1(?) year of musical training
# TODO: confirm what the exclusion criteria should be here. I am trying to reverse
# engineer it from student code.

survey.data <- saffran.data %>% filter(trial_type == 'survey-text')

# manually inspecting the above data frame shows that no one had critical technical problems,
# but three subjects did have more than 1 year of musical training.

excluded.subjects <- c('fz8lzr7s4gpvw1n','2kmdygjk1kbfj25','lbfhnfqgj7dlt7t')

# exclude above subjects from the data, and select the relevant test data

test.data <- saffran.data %>% filter(!subject_id %in% excluded.subjects) %>% filter(phase=='test')

# confirm remaining number of subjects

length(unique(test.data$subject_id))

# summarize test data, subject level

summary.data <- test.data %>% group_by(subject_id, assigned_condition) %>% summarize(mean.correct = mean(correct))

# summary data, group level

summary.data.group <- summary.data %>% group_by(assigned_condition) %>% summarize(m = mean(mean.correct), sd = sd(mean.correct), se = sd(mean.correct)/sqrt(n()))

# plotting the data

ggplot(summary.data, aes(x=assigned_condition, y=mean.correct))+
  geom_boxplot()+
  geom_dotplot(binaxis = 'y', binwidth=0.02)+
  theme_light()

# overall cohen's d

saffran.cohens.d <- (mean(summary.data$mean.correct) - 0.5) / sd(summary.data$mean.correct)

t.test(summary.data$mean.correct, mu=0.5)

# moderator analysis: are tone words with higher TP better learned?


# L1 High:  GG#A (1), D#ED (.75), CC#D (.75)
# L1 Low:   ADB (.375), DFE (.375), FCF# (.5)
# 
# L2 High:  GCD# (1), F#G#E (.75), G#BA (.75)
# L2 Low:   AC#E (.33), C#BA (.67), C#FD (.67)

high.tp.words <- c("GGsharpA", "DsharpED", "CCsharpD", "GCDsharp", "FsharpGsharpE","GsharpBA")

# first need to recover which target the stimulus was and whether it was high or low TP

test.data.moderator <- saffran.data %>% filter(!subject_id %in% excluded.subjects) %>% 
  mutate(word1=lag(stimulus,2), word2=lag(stimulus,1)) %>% filter(phase=='test') %>%
  mutate(target=ifelse(correct==1, button_pressed, 1-button_pressed)) %>%
  mutate(triple=ifelse(target==0, sub(".mp3", "", sub(".*/.*/", "", word1)), sub(".mp3", "", sub(".*/.*/", "", word2)))) %>%
  mutate(tp=ifelse(triple%in%high.tp.words, "high", "low"))

# now do analysis 

summary.data.moderator <- test.data.moderator %>% group_by(subject_id, assigned_condition, tp) %>% summarize(mean.correct = mean(correct))
t.test(mean.correct ~ tp, data=summary.data.moderator, paired=T)

ggplot(summary.data.moderator, aes(x=tp,y=mean.correct))+
  geom_boxplot()+
  geom_point()+
  geom_line(aes(group=subject_id), alpha=0.4)

summary.data.moderator.diff <- summary.data.moderator %>% group_by(subject_id, assigned_condition) %>% 
  mutate(diff = lag(mean.correct) - mean.correct) %>% filter(!is.na(diff))

mean(summary.data.moderator.diff$diff) / sd(summary.data.moderator.diff$diff)


