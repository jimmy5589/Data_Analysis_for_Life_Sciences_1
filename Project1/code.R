# How to download a file using a URL

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)

#

femaleMW = read.csv("femaleMiceWeights.csv")
str(femaleMW)
femaleMW[12,2]
femaleMW$Bodyweight[11]
length(femaleMW$Bodyweight)

mean(femaleMW[femaleMW$Diet=="hf",]$Bodyweight)

set.seed(1)
sample(13:24, 1)
femaleMW$Bodyweight[16]

# dplyr
install.packages("dplyr")
library(dplyr)

install.packages("downloader")
library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

dat=read.csv("msleep_ggplot2.csv")
class(dat)

dat = read.csv("femaleMiceWeights.csv")
str(dat)

# Random VAriables Exercise #1
dat=read.csv("femaleControlsPopulation.csv")

summary(dat)
set.seed(1)
sam=sample(dat$Bodyweight, 5)
abs(mean(sam)-mean(dat$Bodyweight))

set.seed(5)
sam=sample(dat$Bodyweight, 5)
abs(mean(sam)-mean(dat$Bodyweight))

# Conclusion: Abs diff using 2 different samples in a random variable

# Null distributions exeercises

set.seed(1)
n=10000
avgs=vector("numeric",n)
for (i in 1:n){ 
  avgs[i]=mean(sample(dat$Bodyweight,50))
}

sum(abs(avgs-mean(dat$Bodyweight))>1)/n


#

install.packages("gapminder")
library(gapminder)
data("gapminder")
head(gapminder)

x=gapminder[gapminder$year==1952,]
head(x)
summary(x)
hist(x$lifeExp)

# We can compute F in two ways: the simplest way is to type mean(x <= a). 
# This calculates the number of values in x which are less than or equal a, 
# divided by the total number of values in x, in other words the proportion of values 
# less than or equal to a.

mean(x$lifeExp <= 40)

mean(x$lifeExp <= 60) - mean(x$lifeExp <= 40)

prop = function(q) {
  mean(x$lifeExp <= q)
}
class(prop)
prop(40)

qs = seq(from=min(x$lifeExp), to=max(x$lifeExp), length=20)
props=sapply(qs,prop)

plot(qs,props)

# Or

props = sapply(qs, function(q) mean(x$lifeExp <= q))

#Last was an inline function or anonymous

# Compare with the built in function
plot(ecdf(x$lifeExp))

#
dat=read.csv("femaleControlsPopulation.csv")
summary(dat)

set.seed(1)

avgs5=vector("numeric", 1000)
n=1000
for (i in 1:n){
  avgs5[i]=mean(sample(dat$Bodyweight,5))
}
avgs50=vector("numeric",1000)
for (i in 1:n){
  avgs50[i]=mean(sample(dat$Bodyweight,50))
}

hist(avgs5)
hist(avgs50)

mean(avgs50<=25)-mean(avgs50<=23)

z23=(23-23.9)/0.43
z25=(25-23.9)/0.43

z25-z23

pnorm(25,23.9,0.43)-pnorm(23,23.9,0.43)
# same as above:
pnorm(z25)-pnorm(z23)
# basically the Z is normalized to Normal(0,1)

#

dat=read.csv("mice_pheno.csv")
dat=na.omit(dat)
str(dat)
# MALES
malechow=dat[dat$Sex=="M" & dat$Diet=="chow",]
xmean=mean(dat[dat$Sex=="M" & dat$Diet=="chow",]$Bodyweight)
xsd=sd(dat[dat$Sex=="M" & dat$Diet=="chow",]$Bodyweight)

set.seed(1)
X=sample(malechow$Bodyweight, 25)
Xmean=mean(X)

# Now males HF
malehf=dat[dat$Sex=="M" & dat$Diet=="hf",]
ymean=mean(malehf$Bodyweight)

install.packages("rafalib")
library(rafalib)
ysd=popsd(malehf$Bodyweight)

set.seed(1)
Y=sample(malehf$Bodyweight, 25)
Ymean=mean(Y)

(ymean-xmean)-(Ymean-Xmean)

# FEMALES
femalechow=dat[dat$Sex=="F" & dat$Diet=="chow",]
xmean=mean(femalechow$Bodyweight)
xsd=sd(femalechow$Bodyweight)

set.seed(1)
X=sample(femalechow$Bodyweight, 25)
Xmean=mean(X)

# Now females HF
femalehf=dat[dat$Sex=="F" & dat$Diet=="hf",]
ymean=mean(femalehf$Bodyweight)

ysd=popsd(femalehf$Bodyweight)

set.seed(1)
Y=sample(femalehf$Bodyweight, 25)
Ymean=mean(Y)

abs((ymean-xmean)-(Ymean-Xmean))

(ymean-xmean)
(Ymean-Xmean)
#

# CTL exercises
dat <- na.omit( read.csv("mice_pheno.csv"))
str(dat)

str(malechow)
y=malechow$Bodyweight
str(y)
ymean=mean(y)
ysd=popsd(y)
c(ymean, ysd)

mean(y>ymean-ysd & y<=ymean+ysd)

mean(y>ymean-2*ysd & y<=ymean+2*ysd)

mean(y>ymean-3*ysd & y<=ymean+3*ysd)


#
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

mean(avgs)
popsd(avgs)

#CLT exercice 1
# Dices

dat=read.csv("femaleMiceWeights.csv")

mypar()
avgs=vector("numeric",10000)

mypar(1,2)
N=10000  #Number of samples
n=100  # Number of dice
set.seed(1)

avgs=replicate(N,mean(sample(1:6, n, replace=TRUE)==6))

hist(avgs)
p1=mean(avgs)
sd1=popsd(avgs)

# z = (mean(x==6) - p) / sqrt(p*(1-p)/n) 

p=1/6
sd=sqrt(p*(1-p)/100)

z=(avgs-p)/sd
qqnorm(z)
abline(0,1)#confirm it's well approximated with normal distribution

mean(abs(z)>2)

mean(z)

# Code from the solution
set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)
# End code from Solution
install.packages("rafalib")
library(rafalib)

mypar(2,2)

set.seed(1)
p <- 0.01
n <- 100
zs <- replicate(10000,{
  x <- sample(1:6,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
hist(zs)

#It sucks, solution is a hidden parameter sides


ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}

######################

rm(list=ls())

dat=read.csv("femaleMiceWeights.csv")
X=dat[dat$Diet=="chow",]$Bodyweight
Y=dat[dat$Diet=="hf",]$Bodyweight
Xavg=mean(X)
Xavg

Xvar=var(X)
Xvar
Xsd=sqrt(Xvar)
Xsd
# that is the same as sd(X)

sd1=sd(X)*sqrt((length(X)-1)/(length(X)))
mean1=mean(X)
z=2/sd1
z
1-pnorm(z)
2*(1-pnorm(z))

# Exerc 7
w=sqrt(length(X))*(2)/sd(X)
sd(X)
length(X)
w
2*(1-pnorm(w))

# Exerc 8
# Under the null Hypotesis
# should be muX=muY
# SE(avgX-avgY)=sqrt(var(X)/12+var(Y)/12))
# This is the standard deviation of this statitistic 
# also known as standard error or SE.
#
length(X)
length(Y)
varX=var(X)
varY=var(Y)
serror=sqrt(varX/length(X)+varY/length(Y))
serror

# T-stats
# T-stat is realtive to 1 sample
# If you consider many samples you end up with T-Distribution
mean(Y)-mean(X)
tstat=(mean(Y)-mean(X))/serror
tstat
# The larger the absolute value of the t-value, the smaller the p-value, 
# and the greater the evidence against the null hypothesis
2*(1-pnorm(tstat))

1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)

#t.test calculates all the values
t.test(X)
t.test(Y)

t.test(Y,X) # Computes the T-value and Degrees of freedom and p-value

# the test based on the CLT approximation is more likely to incorrectly reject the
# null hypothesis (a false positive), while the t-distribution is more likely to incorrectly accept the null
# hypothesis (false negative)

# Week 3
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke=babies[babies$smoke==0,]$bwt
bwt.smoke=babies[babies$smoke==1,]$bwt

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
dat.ns=sample(bwt.nonsmoke, 25)
dat.s=sample(bwt.smoke, 25)

t.test(dat.ns, dat.s)
tval=2.1209

# Solution

N=25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)

X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)

X.s <- mean(dat.s)
sd.s <- sd(dat.s)

sd.diff <- sqrt(sd.ns^2/N+sd.s^2/N)
tval <- (X.ns - X.s)/sd.diff
tval

#########

2*pnorm(-abs(tval))

# Confidence interval of 99%
# Find the t-stat
tval99=abs(qnorm(0.01/2))
tval99  # represents the number of standard deviations from the mean

sd.diff

# Lets multiply by the standard deviation to find the value to add to the mean
tval99*sd.diff
# 12.0478
# Interval will be:
X.ns - X.s  +- tval99*sd.diff

####
qtval99=abs(qt(0.01/2,2*(N-2)))
qtval99  # represents the number of standard deviations from the mean

sd.diff

# Lets multiply by the standard deviation to find the value to add to the mean
qtval99*sd.diff

# 12.56783

N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)

t.test(dat.ns, dat.s)

#
# Power:
# Probability of rejecting the null hipotesis when the alternative is true.
#

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)


N=5    # sample size
alpha=0.05  # Cutoff to reject H0
B=10000  # No. of times we repeat the sample

reject<-function(N, alpha=0.01){
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.ns, dat.s)$p.value < alpha
}
reject(5)

set.seed(1)
rejections<-replicate(B, reject(N))
mean(rejections)
# 0.0984

Ns<-c(30, 60, 90, 120)
set.seed(1)

power<-sapply(Ns, function(N){
  rejections<-replicate(B, reject(N))
  mean(rejections)
  }
)

plot(Ns, power, type="b")
power
table(power, Ns)

# Monte Carlo simulations
set.seed(1)
X<-rnorm(5)
t=sqrt(5)*mean(X)/sd(X)
t

#Monte Carlo Exercise 2

mytstat<-function(){
  X<-rnorm(5)
  sqrt(5)*mean(X)/sd(X) > 2
}

set.seed(1)
result<-replicate(1000, mytstat())
mean(result)
head(result)


# Solution
set.seed(1)
N <- 5
B<- 100

tstats <- replicate(B,{
  X <- rnorm(N)
  sqrt(N)*mean(X)/sd(X)
})
mean(tstats>2)
##############

# Monte Carlo Exercise 3
B=100; ps = seq(1/(B+1), 1-1/(B+1),len=B)
N<-50

#### Solution

library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 
######

# Monte Carlo Exercise 4

library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
set.seed(1)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N,0,1)
    Y <- rnorm(N,0,1)
#    (mean(X)-mean(Y))/sqrt(var(X)/N+var(Y)/N)
    t.test(X,Y,var.equal = TRUE)$statistic
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
#  qqnorm(ts,main=N,
                xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 

# Monte Carlo Exercise 5

library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
Ns=c(1000)
B <- 1000
mypar(1,1)
LIM <- c(-4.5,4.5)
set.seed(1)

for(N in Ns){
  ts <- replicate(B, {
    X <- sample(c(-1,1), N, replace=TRUE)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 

# Monte Carlo Exercise 7


library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
Ns<-c(5, 10, 50, 1000, 10000, 20000)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)

set.seed(1)
mn.sd<-sapply(Ns,function(N){
  ts <- replicate(B, {
    X <- rnorm(N)
    #    sqrt(N)*median(X)/sd(X)
    mean(X)
  })
    sd(ts)
})
set.seed(1)
md.sd<-sapply(Ns, function(N){
  ts <- replicate(B, {
    X <- rnorm(N)
    #    sqrt(N)*median(X)/sd(X)
    median(X)
  })
  sd(ts)
})

set.seed(1)
md.mean<-sapply(Ns, function(N){
  ts <- replicate(B, {
    X <- rnorm(N)
    #    sqrt(N)*median(X)/sd(X)
    median(X)
  })
  mean(ts)
})

mn.sd
#[1] 0.463164960 0.307540795 0.142648147 0.031244522 0.009817387 0.006968280
md.sd
#[1] 0.541248504 0.354307837 0.175427479 0.039361979 0.012388655 0.008616991
md.sd-mn.sd
#[1] 0.078083544 0.046767042 0.032779332 0.008117457 0.002571268 0.001648711
1/sqrt(Ns)
#[1] 0.447213595 0.316227766 0.141421356 0.031622777 0.010000000 0.007071068
md.sd
md.mean

### Solution: Monte Carlo exercise 7 

set.seed(1)
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", 
                 sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}

##there is an asymptotic result that says SD is sqrt(N*4*dnorm(0)^2)

##########
# Permutations Exercise 1
library(rafalib)

babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- babies[babies$smoke==0,]$bwt 
bwt.smoke <- babies[babies$smoke==1,]$bwt 


N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)

# reshufle the data

dat <- c(smokers,nonsmokers)

set.seed(1)
null<-replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
})
mypar(1,1)
hist(null)
abline(v=obs, col="red", lwd=2)
mean(null<=obs)

# The proportion of permutaions with larger difference
(sum(abs(null) > abs(obs))+1) / (length(null)+1)


# Permutations Exercise 2

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- median(smokers) - median(nonsmokers)

# reshufle the data

dat <- c(smokers,nonsmokers)

set.seed(1)
null<-replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
mypar(1,1)
hist(null)
abline(v=obs, col="red", lwd=2)
mean(null<=obs)

# The proportion of permutaions with larger difference
(sum(abs(null) > abs(obs))+1) / (length(null)+1)

# Association Tests Exercise 1 and 2

d = read.csv("assoctest.csv")
head(d)

table(d$allele)
table(d$case)
t=table(d$allele, d$case)
t
chisq.test(t)
fisher.test(t)

# Chapter 4

load("skew.RData")
dim(dat)
par(mfrow = c(3,3))
for (i in 1:9) {
   qqnorm(dat[,i])
}
par(mfrow=c(1,2))
hist(dat[,4])
hist(dat[,9])
str(dat)

####
head(InsectSprays)
str(InsectSprays)
summary(InsectSprays)
t=split(InsectSprays, InsectSprays$spray)

par(mfrow=c(1,1))

boxplot(InsectSprays$count ~ InsectSprays$spray)

###
install.packages("UsingR")
library(dplyr)
data(nym.2002, package="UsingR")
summary(nym.2002)

#par(mfrow=c(1,1,2,3))
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))

boxplot(nym.2002$time ~ nym.2002$gender)
hist(nym.2002[nym.2002$gender=="Female",]$time, main="Female")
hist(nym.2002[nym.2002$gender=="Male",]$time, main="Male")

# Solution

mypar(1,3)
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))

### Scatter plots
data("father.son", package="UsingR")

x=father.son$fheight
y=father.son$sheight

mypar(1,1)
plot(x,y, xlab="Father's height in inches",ylab="Son's height in inches")

boxplot(split(y,round(x)))

print(mean(y[round(x)==72]))

x=(x-mean(x))/sd(x)
y=(y-mean(y))/sd(y)
# trick round(x*4)/4 to squash the values into quartiles
means=(tapply(y, round(x*4)/4, mean))
# turn into numbers the names of the vector to display in the plot
fatherheights=as.numeric(names(means))
plot(fatherheights, means, ylab="average of strata son height")
abline(0,cor(x,y))

# this is true for normal disributed data

# if is not normal distributed the correlation gives a wrong indication
# example
set.seed(1)
a=rnorm(100); a[1]=25
b=rnorm(100); b[1]=26
plot(a,b, main=paste("correlation=", round(cor(a,b), digits = 2)))
# coreelation in the chart is 0.88
cor(a,b, method = "spearman")
# 0.05
# The added points make the correlation very high altough there is no correlation
# all this because the data is not normal distributed

## Exercises
summary(nym.2002)
males=subset(nym.2002, gender=="Male")
females=subset(nym.2002, gender=="Female")

# default correlation in R is Pearson
cor(males$age, males$time)
# 0.2432273
cor(females$age, females$time)
#0.2443156

mypar(1,2)
plot(males$age%/%5*5, males$time)
boxplot(split(males$time, males$age%/%5*5))
r=round(tapply(males$time, males$age%/%5*5, mean))
r[i]-r[i-1]

# Symmetry
time = sort(nym.2002$time)
time[1]/median(time)
time[1000]/median(time)

# compare
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)
# We use logs for chart symmetry around 0

# Robust Summaries
mypat(1,1)
data("ChickWeight")
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)

chick=na.omit(chick)

day4=chick$weight.4
day4out=c(day4, 3000)
day4out[46]
# how sensitive the mean is to outliers
mean(day4out)/mean(day4)
# median in contrast is not sensitive
median(day4out)/median(day4)
# how sensitive is the Standard Deviation to the outliers
sd(day4out)/sd(day4)
# what is the MAD (Median Absolute Deviation)
mad(day4out)/mad(day4)

day21=chick$weight.21
day21out=c(day21, 3000)
cor(day4,day21)

cor(day4out,day21out)/cor(day4,day21)


# Last at last ...
# Mann-Whitney-Wilcoxon exercises
x=subset(chick, chick$Diet==1)$weight.4
head(x)
y=subset(chick, chick$Diet==4)$weight.4

t.test(x,y)

wilcox.test(x,y)
# Add an outlier to x
x.out=c(x,200)
t.test(x.out, y)$p.value
wilcox.test(x.out,y)$p.value

# Exercise 3
# investigate possible downside to Wilcox-Mann-Witney test statitic
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

t.test(x,y+10)$statistic - t.test(x,y+100)$statistic

# Now Wilcox
# Because the Wilcoxon works on ranks, once the two groups show complete separation, 
# that is all points from group 'y' are above all points from group 'x', 
# the statistic will not change, regardless of how large the difference grows.

# Likewise, the p-value has a minimum value, regardless of how far apart the groups are.
# This means that the Wilcoxon test can be considered less powerful than the t-test in 
# certain contexts. 

z=c(1,2,3)
w=c(4,5,6)
wilcox.test(z,w)$p.value

v=c(400,500,600)
wilcox.test(z,v)$p.value
