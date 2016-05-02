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

#