---
title       : Growth Curve Fitting
subtitle    : Understanding trends and predicting outcomes
author      : Matt Nelson
job         : Student
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax, quiz]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Motivation

1. The progression of a child's weight, height, and head circumference are all important factors that help doctors to diagnose:
	* Failure to thrive: a condition in which the child doesn't follow normal growth patterns, which can result in delayed mental and physical development
	* Overweight/obese child: a condition in which the child's Body Mass Index is higher than normal ranges, which increases their risk for diabetes, adult obesity (and the risks that come with it), heart disease, cancer, and a host of other medical ailments
2. Parents and doctors who are dealing with abnormal growth trends need tools to help them understand how different treatments affect growth trends so that they can adjust their care plans accordingly.
3. Parents and doctors also need to have an idea of where the child's growth will be in the future if they follow their existing patterns so that they can predict whether the current treatment will be sufficient to meet care plan goals.

--- #personal 

## Personal Motivation

* My daughter was born with Focal Dermal Hypoplasia, or Goltz syndrome. Failure to thrive is common in these children.
* We have tried a number of different treatments to increase her weight gain, but it is difficult to get a feel for what is working because normal variations in measurements make it hard to see the overall trend.
* The doctors give us goals for her to reach but it's difficult for us to monitor her progress between appointments.

![plot of chunk unnamed-chunk-1](assets/fig/unnamed-chunk-1.png) 

<style>
#personal {
  background-image:url(./assets/img/Megan.jpg); 
  background-repeat: no-repeat;
  background-position: center center;
  background-size: cover;
}
</style>

--- &radio

## Prevalence

Is this health issue a concern for many people? How prevalent are failure to thrive and childhood obesity?

1. 30% of children are considered obese in the USA, but only 5% of infants are considered to have failure to thrive
2. As both failure to thrive and childhood obesity are extreme situations, estimated prevalence for both is 5% or less
3. _Childhood obesity is estimated to affect 18% of children in the US, while failure to thrive could be anywhere from 1% to 27%_

*** .hint There is a fair amount of uncertainty on how to accurately diagnose failure to thrive directly from growth data, so there is a large range of prevalence values depending on the tests being used

*** .explanation The [CDC states](http://www.cdc.gov/healthyyouth/obesity/facts.htm) that 18% of children aged 6-11 in the US in 2012 were obese. A [study in Denmark](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2083342/) found that depending on the test used to diagnose failure to thrive, anywhere from 1% to 27% of the infants were identified as failure to thrive. However, the same study showed that 17-20% of infants were considered to have failure to thrive according to at least one of the seven tests. Based on these figures, dealing with growth trends is something that a large portion of the population will have to deal with at some point.

--- 

## Proposal

* Create an application (available [here](http://mattnelson.shinyapps.io/Shiny)) that allows users to enter their own data, then plot a growth curve and predict a future value
* Allow them to also specify a time frame when they tried a new treatment (such as diet change, medicine, etc.), then show how that affected the curve going forward
* Growth curves are often modeled using a Gompertz curve model:
$y(t) = A*e^{-e^{\frac{\mu*e}{A}(\lambda-t)+1}}$




```r
# fit the mean weights for girls to a Gompertz curve
library(grofit)
fit <- gcFitModel(time=train[, 1], data=train[, 2], 
	control=grofit.control(model=c("gompertz"), suppress.messages=TRUE))
test$prediction <- gompertz(test[, 1], A = fit$parameters$A[1], 
	mu = fit$parameters$mu[1], lambda = fit$parameters$lambda[1])
rmse <- sqrt(mean((test$prediction - test[, 2])^2))
rmse
```

```
## [1] 0.2619
```
