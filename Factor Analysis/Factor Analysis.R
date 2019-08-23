pacman::p_load(psych, nFactors, factanal, FactoMineR, caTools)
### EigenValues
ev <- eigen(cor(Predictors))
ap <- parallel(subject = nrow(Predictors), var = ncol(Predictors), rep = 100, cent = .05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

## 5 suggested factors
fit <- factanal(EF, 4, rotation = "varimax")
print(fit, digits=2, cutoff=.4, sort=TRUE)

fit <- factanal(EF, 4, rotation = "promax")
print(fit, digits=2, cutoff=.4, sort=TRUE)

## Factors and satisfaction
EF_dat <- dat %>% 
  mutate(F1 = KNOWWHAT + SAFETYWK + RESPECT + TRUSTMAN + MYSKILLS,
        F2 = WKDECIDE, 
        F3 = LEARNNEW + WORKDIFF + WORKFAST,
        F4 = LOTOFSAY + WKFREEDM) %>% 
  dplyr::select(F1:F4, satisfaction)

set.seed(123)
sample = sample.split(EF_dat,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train = subset(EF_dat,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test = subset(EF_dat, sample==FALSE)

train_lm <- lm(satisfaction ~., train)
summary(train_lm)

test_lm <- lm(satisfaction ~., test)
summary(test_lm)

trainlm2 <- lm(satisfaction ~ F1 +F4, train)
summary(trainlm2)

testlm2 <- lm(satisfaction ~ F1 + F4, test)
summary(testlm2)

## Vizualaization

EF_dat %>% 
  dplyr::select(satisfaction, F1:F4) %>% 
  gather(F1:F4, key = "factor", value ="score") %>% 
  ggplot(aes(x = score, y = satisfaction))+
    geom_point(alpha = 0.01)+
    geom_smooth(method = "lm", color = "black")+
    facet_wrap(~factor, scales = "free", nrow = 1 )+
  theme_classic()+
  labs(x = " Factor Score", y = "Satisfaction Score", title = "Work Factors and Life Satisfaction Scores")
  
ggsave("Factor_LM.jpeg")
