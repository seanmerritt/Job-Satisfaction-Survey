pacman::p_load(tidyverse, car, mosiac, skimr, psycho, psych, nfactors, factanal())

WorkSatisfaction <- read_csv("Data/WorkSatisfaction.txt")

## Filter Dat set
n <- 23
for(i in 1:n){
  dat <- WorkSatisfaction %>% 
    filter(WorkSatisfaction[,i] > 0, WorkSatisfaction[,i] <=5)
}
  

write_csv(WorkSatisfaction, "WorkSatisfaction.csv")

dat <- dat %>% 
  mutate(satisfaction = (SATJOB1 + WKTOPSAT)/2)

EF <- dat[,-c(1,8,23,24)]

EFEwrite_csv(dat, "dat.csv")

## Standardize with z-scores
Std_score <- dat %>% 
  standardize()

write_csv(Std_score, "Std_score.csv")



