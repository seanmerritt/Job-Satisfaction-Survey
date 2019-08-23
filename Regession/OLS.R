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
  dplyr::select(WORKFAST, LOTOFSAY, MYSKILLS, TRUSTMAN, WKFREEDM, JOBSECOK, WKPRAISE, FAIREARN, satisfaction) 
