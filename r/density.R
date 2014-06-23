######
### The goal of this project is to examine voting patterns of Congressman, and its
### relationship with their district's population density/urban vs rural nature
######

rm(list=ls())

#### file containing density measures for all districts from American Fact Finder
cd_density <- read.csv("C:/Users/mcast_000/Documents/Github/Urban Politics/data/cd_density.csv")
#### file containing congressional voting ideal point measures
#### courtesy of Simon Jackman (http://jackman.stanford.edu/blog/) 3/29/14
### one of the things to keep in mind is that Jackman's ideal points are only estimates,
### and there are fairly wide margins of error
estimates <- read.csv("C:/Users/mcast_000/Documents/Github/Urban Politics/data/estimates.csv")

colnames(cd_density) <- tolower(colnames(cd_density))
colnames(cd_density)
colnames(estimates)

#### merge datasets together
dense_ideal <- merge(cd_density, estimates,
			   by.x=c("geography", "district"), by.y=c("state", "district"),
			   all=T)
is.na(dense_ideal)
dense_ideal$icpsr.id[is.na(dense_ideal$icpsr.id)] <- 0
dense_ideal[!complete.cases(dense_ideal),]
dense_ideal[396,"gender"] <- "M"

attach(dense_ideal)
rownames(dense_ideal) <- label

#### a basic OLS model seems to fit well
dense_lm <- lm(idealPoint~density.land)
summary(dense_lm)

##### but when we plot density against the ideal point, the line clearly doesn't fit well based on an eyetest
plot(density.land, idealPoint)
abline(reg=dense_lm)

#### histogram and jarque-bera test confirm density is not normal
library(moments)
hist(density.land)
jarque.test(density.land)

### clearly we need to transform our independent variable
### a log transformation of density should help since the data are right-skewed
dense_ideal <- transform(dense_ideal, log_dense = log(density.land))
detach(dense_ideal)
attach(dense_ideal)

hist(log_dense)
jarque.test(log_dense)

### and we do find that it improves our model
dense_lm_log <- lm(idealPoint~log_dense)
summary(dense_lm_log)

#### republicans are in the upper half of this plot while dems are in bottom
#### since republicans tend to be from more rural districts while dems are from more urban districts
plot(log_dense, idealPoint)
abline(reg=dense_lm_log)

#### so lets color accordingly
library(ggplot2)
library(scales)
ggplot(data=dense_ideal, aes(log_dense, idealPoint, colour=factor(party))) +
geom_point() + geom_smooth() + scale_color_manual(values=alpha(c("blue", "red"), .4))
#### looks like density appears to be correlated with Democratic voting patterns, but not GOP

### a partitioned linear model confirms this phenomenon
library(lme4)
dense_lm_log_party <- lmList(data=dense_ideal, idealPoint~log_dense | factor(party))
summary(dense_lm_log_party)

### let's take a closer look at Republicans
r_fitted <- fortify(dense_lm_log_party$R)
r_fitted$label <- rownames(r_fitted)
tail(arrange(r_fitted, log_dense))
### Michael Grimm of NY-11 (Staten Island) and Dana Rohrabacher of CA-48 (Orange County)
### are the Congressmen in the 2 densest districts controlled by the GOP

### let's take a closer look at democrats
d_fitted <- fortify(dense_lm_log_party$D)
ggplot(data=d_fitted, aes(log_dense, idealPoint)) +
geom_point() + geom_smooth()

detach(dense_ideal)
attach(d_fitted)
### which democrats' voting patterns are least representative of their district's density?
d_fitted <- transform(d_fitted, extreme = ifelse(abs(.stdresid)>1.5,1,0))
d_fitted$label <- rownames(d_fitted)
ggplot(data=d_fitted, aes(log_dense, idealPoint)) +
geom_point() + geom_smooth() +
geom_text(data=d_fitted[d_fitted$extreme == 1,], aes(label=label, size=abs(.stdresid), angle=45)) +
scale_size(range=c(3,5))

library(plyr)
arrange(d_fitted, idealPoint)
arrange(d_fitted, log_dense)

### The important thing to consider is that density is not a perfect predictor of the ideology of a district
### Other factors can play a part such as educational attainment and economic factors
### Just because a representative is more liberal/conservative relative to what you would
### expect from a district's density does not necessarily mean that the representative is
### more liberal/conservative than their constituency

### Let's take a look at the Democrats who are most conservative relative to what you would
### expect from their district's density.
arrange(d_fitted, desc(.stdresid))[1:14,]
### The representative who is most conservative relative to their district's density is Jim Matheson of UT-4 (also the most conservative Democrat overall)
### This district is fairly dense (contains a large part of Salt Lake City), but has a Cook PVI of R+14. (http://en.wikipedia.org/wiki/Utah's_4th_congressional_district)

### A fascinating result is Kyrsten Sinema of AZ-9. The district is largely based in dense Phoenix,
### and Sinema describes herself as progressive (http://kyrstensinema.com/record/). But her voting record
### places her as the 11th most conservative Democrat in the House.

### How about Democrats who are most liberal relative to their district's density?
arrange(d_fitted, .stdresid)[1:14,]

### The representative who is most liberal relative to their district's density is Mark Pocan of WI-2 (also the most liberal representative overall)
### This district incorporates the very progressive college town of Madison
### while being  the 43rd least dense Democrat-controlled district

### Perhaps most interesting though is Raul Grijalva of AZ-3.
### Incorporating a large swath of Southern Arizona, this district is the 9th least
### dense district controlled by a Democrat, yet Grijalva is more progressive
### than all but 41 representatives. But upon observing the district's map (http://upload.wikimedia.org/wikipedia/commons/c/c8/Arizona_3rd_Congressional_District.png)
### we see that "land density" does not paint the whole picture. Much of the population
### for AZ-3 is concentrated in Tucson, the second largest city in Arizona. Arizona
### has a history of creating strange districts (see the former AZ-2 http://upload.wikimedia.org/wikipedia/commons/e/e1/AZ-districts-109-02.gif).
### While AZ-3 is relatively less dense than other districts, the vast majority (about 88%)
### of it's population is found in urban areas (http://factfinder2.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=DEC_10_113_H2&prodType=table).
### Essentially there are wide stretches of land in AZ-3 that are empty (it is the desert after all).

### An important thing to do when analyzing data is to understand the context of the data. In this case, a district's population density
### does not truly reflect the concentration of inhabitants within that district

### So let's look at Urban vs Rural numbers

cd_urban <- read.csv("C:/Users/mcast_000/Documents/Github/Urban Politics/data/cd_urban.csv")

dense_ideal_urb <- merge(dense_ideal, cd_urban,
				 by.x="target.geo.id", by.y="Id",
				 all.x=T)
dense_ideal_urb <- transform(dense_ideal_urb, pct_urban = Urban / Total)

detach(d_fitted)
attach(dense_ideal_urb)

### again we have a significant linear model when looking at idealPoint vs pct_urban
urb_lm <- lm(idealPoint ~ pct_urban) 
summary(urb_lm)
### again we have a significant linear model when looking at idealPoint vs pct_urban
urb_lm <- lm(idealPoint ~ pct_urban) 
summary(urb_lm)

### but again we should take into account by party
plot(pct_urban, idealPoint)
abline(urb_lm)

ggplot(data=dense_ideal_urb, aes(pct_urban, idealPoint, colour=factor(party))) +
geom_point() + geom_smooth() + scale_color_manual(values=alpha(c("blue", "red"), .4))

### it looks like we have pct_urban only affecting democrats, but we don't even have
### a good estimate for the intercept
urb_lm_party <- lmList(data=dense_ideal_urb, idealPoint~pct_urban | factor(party))
summary(urb_lm_party)

### we should incorporate log_dense to see if we are actually improving on the
### previous model
urb_log_lm_party <- lmList(data=dense_ideal_urb, idealPoint~pct_urban+log_dense | factor(party))
summary(urb_log_lm_party)
#### Using both pct_urban and log_dense only yields log_dense as a significant predictor
#### for Democrats

### When we add an interaction term, none of the predictors are significant
urb_log_int_lm_party <- lmList(data=dense_ideal_urb, idealPoint~pct_urban*log_dense | factor(party))
summary(urb_log_int_lm_party)

### the original model looking only at log_dense appears to be the best model actually
summary(dense_lm_log_party)

#### So what's the takeaway of all this? If one were to confuse correlation with causation,
#### they would automatically suggest the Democrats invest more resources into taking over
#### districts like TX-7 where we find the solid conservative John Culberson in the dense,
#### but wealthy, white suburbs of Houston. One might also suggest more heavily protecting
#### districts like MA-2, where we find solid progressive Jim McGovern in relatively rural,
#### but Progressive Central Massachussetts.