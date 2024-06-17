library(survival)
library(rpart)
library(partykit)

head(stagec)
# pgtime pgstat age eet    g2 grade gleason     ploidy
# 1    6.1      0  64   2 10.26     2       4    diploid
# 2    9.4      0  62   1    NA     3       8  aneuploid
# 3    5.2      1  59   2  9.99     3       7    diploid
# 4    3.2      1  62   2  3.57     2       4    diploid
# 5    1.9      1  64   2 22.56     4       8 tetraploid
# 6    4.8      0  69   1  6.14     3       7    diploid

dat <- stagec
tfit <- rpart(Surv(pgtime,pgstat)~., data = dat)

# Decision Tree
p.fit <- as.party(tfit)
plot(p.fit)

# survival rate in nodes
d5 <- dat[tfit$where==5,]
summary(survfit(Surv(d5$pgtime,d5$pgstat)~1))

# Survival Fit, Broken by Node in Tree:
dat$node = as.factor(tfit$where)
# #1) plot K-M curves by node (black):
plot( survfit(Surv(dat$pgtime, event = dat$pgstat)~dat$node) )


#2) plot exponential survival with rates = e0 * RPart rates (red):
s0 <- survreg(Surv(pgtime,pgstat)~ 1, data =  dat)
e0 <- exp(-summary(s0)$coefficients[1])
rates <- unique(predict(tfit))

for (rate in rates) {
  grid= seq(0,max(dat$pgtime),length.out = 100)
  lines(x= grid, y= exp(-e0*rate*(grid)), col=2)
}

#3) plot partykit survival curves based on RPart tree (green)
tfit2 <- as.party(tfit)
col_n = 1
for (node in names(table(dat$node))){
  predict_curve = predict(tfit2, newdata = dat[dat$node == node, ], type = "prob")  
  surv_esitmated = approxfun(predict_curve[[1]]$time, predict_curve[[1]]$surv)
  lines(x= grid, y= surv_esitmated(grid), col = 2+col_n)
  col_n=+1
}
# Add legend
legend("topright", legend = c("Node", "Exponential", "Tree"),
       col = c("black", "red", "green"), lty = 1, cex = 0.8)
# dev.off()
