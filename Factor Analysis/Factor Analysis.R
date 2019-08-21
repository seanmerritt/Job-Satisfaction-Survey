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
