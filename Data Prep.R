pacman::p_load(tidyverse, psycho)

WorkSatisfaction <- read_csv("Data/WorkSatisfaction.txt")
sub.data <- WorkSatisfaction
## Filter Dat set
n <- 23
for(i in 1:n){
  if(i > 1){
    if(colnames(sub.data[,i]) != "HRSRELAX"){
      sub.data <- sub.data %>% 
        filter(sub.data[,i] > 0, sub.data[,i] <= 5)
    }
    else{
       sub.data <- sub.data %>% 
      filter(sub.data[,i] >= 0, sub.data[,i] <= 30)
    }
}
}

## Recode the data
sub.data$SATJOB1 <- recode(sub.data$SATJOB1, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$WKTOPSAT <- recode(sub.data$WKTOPSAT, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$TRAINOPS <- recode(sub.data$TRAINOPS, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$LEARNNEW <- recode(sub.data$LEARNNEW, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$RESPECT <- recode(sub.data$RESPECT, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$TRUSTMAN <- recode(sub.data$TRUSTMAN, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$WKDECIDE <- recode(sub.data$WKDECIDE, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$WORKFAST <- recode(sub.data$WORKFAST, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$WORKDIFF <- recode(sub.data$WORKDIFF, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$LOTOFSAY<- recode(sub.data$LOTOFSAY, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$KNOWWHAT <- recode(sub.data$KNOWWHAT, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$SETTHINGS <- recode(sub.data$SETTHNGS, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$PROMTEOK <- recode(sub.data$PROMTEOK, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$SAFETYWK <- recode(sub.data$SAFETYWK, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$WKFREEDM <- recode(sub.data$WKFREEDM, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$MYSKILLS <- recode(sub.data$MYSKILLS, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$WKVSFAM <- recode(sub.data$WKVSFAM, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$JOBSECOK <- recode(sub.data$JOBSECOK, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$TRAINOPS <- recode(sub.data$TRAINOPS, `4`= 0L, `3`= 1L, `2`= 2L, `1`= 3L)
sub.data$WKPRAISE <- recode(sub.data$WKPRAISE, `3`= 0L, `2`= 0L, `1` = 1L)
sub.data$FAIREARN <- recode(sub.data$FAIREARN, `5`= 4L, `4`= 3L, `3`= 2L, `2`= 1L, `1` = 0L)

## Create Satisfaction variable and filter contributors

dat <- sub.data %>% 
  mutate(satisfaction = (SATJOB1 + WKTOPSAT)/2) %>% 
  dplyr::select(WKVSFAM:LOTOFSAY, KNOWWHAT:HEALTH1, satisfaction)

## Create a predictors data set to use only predictors for EF, LASSO and Clusters
Predictors <- dat %>% 
  dplyr::select(-satisfaction)


## Standardize with z-scores
Std_score <- Predictors %>%
  psycho::standardize()

write_csv(Std_score, "Std_score.csv")

x <- model.matrix(satisfaction~., data = dat)[,1]
y <- dat$satisfaction
