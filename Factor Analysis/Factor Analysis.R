### EigenValues
ev <- eigen(cor(EF))
ap <- parallel(subject = nrow(EF), var = ncol(EF), rep = 100, cent = .05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

## 5 suggested factors
fit <- factanal(EF, 5, rotation = "varimax")
print(fit, digits=2, cutoff=.4, sort=TRUE)

fit <- factanal(EF, 5, rotation = "promax")
print(fit, digits=2, cutoff=.4, sort=TRUE)

## Factors and satisfaction
EF_dat <- dat %>% 
  mutate(F1 = KNOWWHAT + MYSKILLS + RESPECT + TRUSTMAN,
        F2 = LOTOFSAY + SETTHNGS + TRAINOPS, 
        F3 = WKPRAISE + PROMTEOK,
        F4 = LEARNNEW + WORKDIFF + WORKFAST,
        F5 = WKDECIDE) %>% 
  dplyr::select(F1:F5, satisfaction)

set.seed(123)
sample = sample.split(EF_dat,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train = subset(EF_dat,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test = subset(EF_dat, sample==FALSE)

train_lm <- lm(satisfaction ~., train)
summary(train_lm)

test_lm <- lm(satisfaction ~., test)
summary(test_lm)
