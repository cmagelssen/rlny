


# Set working to directory to the default folder
setwd("C:/Users/cmage/Desktop/project_reinforcementlearning")

# Load in custom functions
source("code/cleandata.R")
source("code/readcsv.R")

# Load in libraries for data manipulation
library(tidyverse)
library(openxlsx)
library(readxl)
library(hms)




d <- readcsv("C:/Users/cmage/Desktop/project_reinforcementlearning/data")
d <- change_session(d) 


d |>
  ggplot(aes(x=strategy, y=finish, color=strategy)) +
  geom_jitter(width=0.1) +
  theme_bw()


d <- d |>
  arrange(experiment, skier, session, run)

#d <- clean_data(d) 


d |>
  filter(strategy == "sg") |>
  ggplot(aes(x=session, y=finish, group=skier, color=skier)) +
  geom_point() +
  geom_line() +
  facet_wrap(~experiment, nrow=4)

d

d |>
  filter(finish > 20) 

d <- add_lostdata(d) 


str(d)


d <- d |>
  mutate(run = as.double(run),) |>
  arrange(experiment, skier, session, run)
str(d)

d <- d |>
  mutate(run = as.numeric(run),
         skier = as.numeric(skier))|>
  arrange(experiment, skier, session, run)


d |>
  filter(strategy == "a" | strategy == "b" | strategy == "c" | strategy == "d" | strategy == "ns") |>
  ggplot(aes(x=session, y=finish, group=skier, color=as.factor(skier))) +
  geom_jitter(width=0.2, alpha=0.5) +
  facet_wrap(~experiment, nrow=1) +
  theme_bw()



d <- remove_errordata(d) 

d <- d |>
  mutate(run = as.numeric(run),
         skier = as.numeric(skier))|>
  arrange(experiment, skier, session, run)



d <- swap_strategies(d, "c", "12","block1") 
#d <- change_session(d) 
d <- remove_dnf(d)
d <- remove_screentrials(d)


d |>
  filter(strategy == "a" | strategy == "b" | strategy == "c" | strategy == "d" | strategy == "ns") |>
  ggplot(aes(x=session, y=finish, group=skier, color=as.factor(skier))) +
  geom_jitter(width=0.2, alpha=0.5) +
  facet_wrap(~experiment, nrow=1) +
  theme_bw()







d |>
  filter(strategy == "a" | strategy == "b" | strategy == "c" | strategy == "d" | strategy == "ns") |>
  ggplot(aes(x=session, y=finish, group=skier, color=as.factor(skier))) +
  geom_jitter(width=0.2, alpha=0.5) +
  facet_wrap(~experiment, nrow=1) +
  theme_bw()



t <- d |>
  filter(strategy == "99") |>
  group_by(experiment, skier, session) |>
  summarise(n = n()) |>
  pivot_wider(names_from = session, values_from=n)



a <- d |>
  filter(experiment=="c", session=="baseline") |>
  filter(strategy == "ns")

lmer(finish ~ treatment )


str(d)

str(d)

d 

d |>
  filter(experiment=="c" & skier=="2") |>
  ggplot(aes(x=strategy, y=finish, color=strategy)) + 
  geom_jitter(width=0.1) +
  facet_wrap(~session, nrow=1)



  group_by(strategy) |>
  summarise(n = mean(finish, na.rm=TRUE))



d |>
  ggplot(aes(x=strategy, y=finish, color=strategy)) +
  geom_jitter(width=0.1) +
  theme_bw()





expc <- d |>
  filter(strategy == "a" | strategy == "b" | strategy == "c" | strategy == "d" | strategy == "ns") 
  
  
  
expc |>
  ggplot(aes(x=strategy, y=finish, color=strategy)) +
  geom_jitter(width=0.1) +
  theme_bw()





?as.numeric
d <- d |>
  arrange(experiment, skier, session, run)

#mod <- lmer(finish ~ session * treatment + (1 + session * treatment | experiment) + (1 + session | experiment:skier), data=d)
d
summary(mod)

expc <- d |>
  filter(strategy == "a" | strategy == "b" | strategy == "c" | strategy == "d" | strategy == "ns") |>
  


expc |>
  ggplot(aes(x=strategy, y=finish, color=strategy)) +
  geom_jitter(width=0.1) +
  theme_bw()


library(lmerTest)

mod <- lmer(finish ~ session * treatment + (1  | experiment/skier), data=expc, REML=FALSE)

summary(mod)


se <- lm(finish ~ treatment, data=expc)
summary(se)






d |>
  ggplot(aes(x=run, y=finish, color=skier, group=interaction(experiment,skier))) +
  geom_point(width=0.1) + 
  geom_line() +
  facet_wrap(~session)
 



d <- d |>
  arrange(experiment, skier, session, run)


write.xlsx(d, "d.xlsx")



b <-  d |>
  filter(strategy != "sg") |>
  filter(!(skier == "20" & experiment == "b")) |>
  filter(!(skier == "50" & experiment == "b")) |>
  group_by(experiment, skier, session) |>
  reframe(avg = mean(finish, na.rm = TRUE),
          treatment = unique(treatment), 
          experiment = unique(experiment)) |>
  filter(session == "baseline")



a <- d |>
  filter(strategy != "sg") |>
  filter(!(skier == "20" & experiment == "b")) |>
  filter(!(skier == "50" & experiment == "b")) |>
  filter(session == "retention")




data <- a %>%
  left_join(b, by = c("skier","experiment"))

unique(data$skier)


mod <- lmer(finish ~ avg + treatment.x + (1 | experiment) + (1| experiment:skier), REML=FALSE, data=data)

summary(mod) 

pp_check(mod, ndraws = 50) 


lmer(retention ~ baseline + treatment + (1 | experiment) + (1|experiment:skier), data=data, REML=FALSE)

data 

summary(mod) 



