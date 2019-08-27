r_square_viewer <- function(data, model, iterations){
  trainer <- 0
  tester <- 0
  
  n <- iterations
  for(i in 1:n){
    set.seed(i)
    sample = sample.split(data,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
    train = subset(dat,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
    test = subset(data, sample==FALSE)
    trainlm <- lm(model, train)
    trainer[i] <-  summary(trainlm)$adj.r.squared
    
    testlm <- lm(model, test)
    tester[i] <- summary(testlm)$adj.r.squared
  }
  

  r_squared <- data.frame(trainer, tester)
  

}



