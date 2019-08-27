pacman::p_load(caTools, tidyverse)
source("r_square_viewer.r")
set.seed(123)
sample = sample.split(dat,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train = subset(dat,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test = subset(dat, sample==FALSE)
trainlm <- lm(satisfaction~., train)
summary(trainlm)

testlm <- lm(satisfaction~., test)
summary(testlm)

### Vizualize and test model with just significant

dat %>% 
  dplyr::select(WORKFAST, LOTOFSAY, MYSKILLS, TRUSTMAN, WKFREEDM, JOBSECOK, WKPRAISE, FAIREARN, satisfaction) %>% 
  gather(WORKFAST:JOBSECOK, `FAIREARN`, key = "Variable", value ="Score") %>% 
  ggplot(aes(x = Score, y = satisfaction)) + 
  geom_point(alpha = 0.01)+
  geom_smooth(method = "lm", aes(color = factor(WKPRAISE)))+
  facet_wrap(~ Variable, nrow = 1)+
  theme_classic()

dat1 <- dat %>% 
  dplyr::select(WORKFAST, LOTOFSAY, MYSKILLS, TRUSTMAN, WKFREEDM, JOBSECOK, WKPRAISE, FAIREARN, satisfaction)
set.seed(123)
sample = sample.split(dat,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train = subset(dat,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test = subset(dat, sample==FALSE)
trainlm <- lm(satisfaction~., train)
summary(trainlm)

testlm <- lm(satisfaction~., test)
summary(testlm)


## Lets see what model will be best of not over or under fitting
dat2 <- dat %>% 
  dplyr::select(LOTOFSAY, MYSKILLS, TRUSTMAN, WKFREEDM, JOBSECOK, WKPRAISE, FAIREARN, satisfaction)

dat3 <- dat %>% 
  dplyr::select(LOTOFSAY, MYSKILLS, TRUSTMAN, WKFREEDM, JOBSECOK, WKPRAISE, satisfaction)

r_1 <- r_square_viewer(dat, satisfaction~., 100) %>% 
  mutate(model = "M1")
r_2 <- r_square_viewer(dat2, satisfaction~., 100) %>% 
  mutate(model = "M2")
r <- r_square_viewer(dat3, satisfaction~., 100) %>% 
  mutate(model = "M3") %>% 
  full_join(r_1) %>%  
  full_join(r_2) %>% 
  mutate(difference = tester - trainer)

difference_lm <- aov(difference ~ model, r)
summary(difference_lm)

r %>% 
  ggplot(aes(y = difference, x = model))+
  geom_boxplot(fill = "purple", color = 'firebrick')+
  theme_bw()+
labs(title = "comparing models")

ggsave("ModelComparison.jpeg")

