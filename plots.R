library(survival)
data("ERSPC")
ERSPC$ScrArm<-factor(ERSPC$ScrArm)
gModel <- casebase::fitSmoothHazard(DeadOfPrCa ~ Follow.Up.Time + ScrArm, data=ERSPC, ratio = 100)
eModel <- casebase::fitSmoothHazard(DeadOfPrCa ~ ScrArm, data=ERSPC, ratio = 100)
wModel <- casebase::fitSmoothHazard(DeadOfPrCa ~ log(Follow.Up.Time)+ ScrArm, data=ERSPC, ratio = 100)
sModel <- casebase::fitSmoothHazard(DeadOfPrCa ~ bs(Follow.Up.Time) + ScrArm, data=ERSPC, ratio = 100)
coxModel<-coxph(Surv(Follow.Up.Time,DeadOfPrCa ) ~ ScrArm, data=ERSPC)
compareHazard=as.data.frame(cbind(coxModel$coefficients, gModel$coefficients[3] ,eModel$coefficients[2],wModel$coefficients[3],sModel$coefficients[3]))

colnames(compareHazard)=c("Cox","Gompertz","Exponential","Weibull","splines")
summary(coxModel)
time_points <- seq(0,15, 1)
newData=as.data.frame(cbind(factor(0),2,0))
colnames(newData)=c("ScrArm","Follow.Up.Time","DeadOfPrCa")
newData$ScrArm=factor(newData$ScrArm)
# calculate cumulative incidence using casebase model
wRisk <- absoluteRisk(object = wModel,time = time_points,newdata = newData)
#plot(wRisk)
coxRisk=survfit(coxModel, newdata = newData)
#plot(coxRisk$time,coxRisk$cumhaz)





#######################



## Dr. Hernan's perspective
![](hernan_HR_tweet.png)

##Dr. Cox's perspective
![](coxquote.png)

#Data on the men in the European Randomized Study of Prostate Cancer Screening (ERSPC)

## ERSPC Data

* ~150 000 men ages 55-69
* First start: 1991
* End: 2006 

![](erspcdata.png)

## ERSPC Data

```{r , echo=FALSE, warning=FALSE,message=FALSE}

repSample<-c(3,1682,40176,146154,159814)
repSample<-casebase::ERSPC[repSample,]
knitr::kable(repSample,row.names = FALSE)

ERSPC$ScrArm <- factor(ERSPC$ScrArm, 
                       levels = c(0,1), 
                       labels = c("Control group", "Screening group"))
```

##Recall
* Justin wants to know his two year risk for prostate cancer.
* As justin was not part of the study, we will consider him part of the control group where no screening occured
* **We will determine Justin's absolute risk using CaseBase!**

#Casebase

##Casebase Overview

1. Clever sampling.
2. Indirectly deals with censoring.
3. Allows a parametric fit using *logistic regression*.

* Casebase is parametric, and allows different parametric fits by incorporation of the time component.
* Package contains an implementation for generating *population-time* plots.

##Casebase: Sampling
```{r , echo=FALSE, warning=FALSE,message=FALSE}
ERSPC=casebase::ERSPC
nobs <- nrow(ERSPC)
ftime <- ERSPC$Follow.Up.Time
ord <- order(ftime, decreasing = FALSE)
yCoords <- cbind(cumsum(ERSPC[ord, "DeadOfPrCa"] == 1),
cumsum(ERSPC[ord, "DeadOfPrCa"] == 0))
yCoords <- cbind(yCoords, nobs - rowSums(yCoords))
aspectRatio <- 0.75
height <- 8.5 * aspectRatio; width <- 11 * aspectRatio
cases <- ERSPC[, "DeadOfPrCa"] == 1
comps <- ERSPC[, "DeadOfPrCa"] == 2

# randomly move the cases vertically
moved_cases <- yCoords[cases[ord], 3] * runif(sum(cases))
moved_comps <- yCoords[comps[ord], 3] * runif(sum(comps))
plot(0, type = 'n', xlim = c(0, max(ftime)), ylim = c(0, nobs),
xlab = 'Follow-up time (years)', ylab = 'Population')

```

##Casebase: Sampling
```{r , echo=FALSE, warning=FALSE,message=FALSE}
plot(0, type = 'n', xlim = c(0, max(ftime)), ylim = c(0, nobs),
xlab = 'Follow-up time (years)', ylab = 'Population')
polygon(c(0, 0, ftime[ord], max(ftime), 0),
c(0, nobs, yCoords[,3], 0, 0),
col = "grey80")
```

##Casebase: Sampling
```{r , echo=FALSE, warning=FALSE,message=FALSE}
plot(0, type = 'n', xlim = c(0, max(ftime)), ylim = c(0, nobs), legend=TRUE,
xlab = 'Follow-up time (years)', ylab = 'Population')
polygon(c(0, 0, ftime[ord], max(ftime), 0),
c(0, nobs, yCoords[,3], 0, 0),
col = "grey80")
points((ftime[ord])[cases[ord]], yCoords[cases[ord],3], pch = 19,
col = "firebrick3", cex = 0.5)
legend("right", legend = c("Event"),
col = c("firebrick3", "dodgerblue2", "black"),
pch = 19)

```

##Casebase: Sampling
```{r , echo=FALSE, warning=FALSE,message=FALSE}

plot(0, type = 'n', xlim = c(0, max(ftime)), ylim = c(0, nobs),
xlab = 'Follow-up time (years)', ylab = 'Population')
polygon(c(0, 0, ftime[ord], max(ftime), 0),
c(0, nobs, yCoords[,3], 0, 0),
col = "grey80")
points((ftime[ord])[cases[ord]], moved_cases, pch = 19,
col = "firebrick3", cex = 0.5)
legend("topright", legend = c("Relapse"),
col = c("firebrick3", "dodgerblue2", "black"),
pch = 19)
death="DeadOfPrCa"
ftime="Follow.Up.Time"
```
```{r,echo=TRUE,eval=FALSE}
casebase::popTime(Data,Event,Time)
```

##Casebase: Sampling
```{r,echo=FALSE}
ftime <- ERSPC$Follow.Up.Time
plot(0, type = 'n', xlim = c(0, max(ftime)), ylim = c(0, nobs),
xlab = 'Follow-up time (years)', ylab = 'Population')
polygon(c(0, 0, ftime[ftime %in% ftime[cases[ord]]], 0, 0),
c(0, 0, yCoords[ftime %in% ftime[cases[ord]],3], 0, 0),
col = "grey80")
points((ftime[ord])[cases[ord]], yCoords[cases[ord],3], pch = 19,
col = "firebrick3", cex = 0.5)
legend("right", legend = c("Event"),
col = c("firebrick3", "dodgerblue2", "black"),
pch = 19)
```

##Casebase: Sampling
```{r,echo=FALSE}
plot(0, type = 'n', xlim = c(0, max(ftime)), ylim = c(0, nobs),
xlab = 'Follow-up time (years)', ylab = 'Population')
points((ftime[ord])[cases[ord]], yCoords[cases[ord],3], pch = 19,
col = "white", cex = 0.5)
lines(rep((ftime[ord])[cases[ord]], each=3), t(matrix(c(yCoords[cases[ord],3], rep(c(0,NA),each=length(yCoords[cases[ord],3]))), ncol=3)),col="grey80") 
points((ftime[ord])[cases[ord]], moved_cases, pch = 19,
col = "firebrick3", cex = 0.5)

ERSPC$ScrArm<-factor(ERSPC$ScrArm)
ERSPC$DeadOfPrCa<-as.numeric(ERSPC$DeadOfPrCa)
```

#Casebase: Hazard fits


##Casebase: Parametric families
* We can now fit models of the form:
$$\ log(h(t;\alpha,\beta))=g(t;\alpha)+\beta X$$
* By changing the function $g(t;\alpha)$, we can model different parametric families easily:


##Casebase: Parametric models
*Exponential*: $g(t;\alpha)$ is equal to a constant
```{r,echo=TRUE,eval=FALSE}
casebase::fitSmoothHazard(status ~ var)
```
*Gompertz*: $g(t;\alpha)=\alpha t$
```{r,echo=TRUE,eval=FALSE}
casebase::fitSmoothHazard(status ~ time + var)
```
*Weibull*: $g(t;\alpha) = \alpha log(t)$
```{r,echo=TRUE,eval=FALSE}
casebase::fitSmoothHazard(status ~ log(time) + var)
```

# ERSPC Hazard

##ERSPC Hazard
```{r,echo=TRUE, warning=FALSE,message=FALSE}
wModel<-fitSmoothHazard(DeadOfPrCa ~
log(Follow.Up.Time) + 
ScrArm, data=ERSPC, ratio = 100)
```
<font size="6">
```{r echo=FALSE, warning=FALSE,message=FALSE,eval=FALSE}
summary(wModel)
```

![](wmodel_out.png)

##ERSPC Hazard comparison
```{r echo=FALSE, warning=FALSE,message=FALSE}
ERSPC$ScrArm<-factor(ERSPC$ScrArm)
gModel <- casebase::fitSmoothHazard(DeadOfPrCa ~ Follow.Up.Time + ScrArm, data=ERSPC, ratio = 100)
eModel <- casebase::fitSmoothHazard(DeadOfPrCa ~ ScrArm, data=ERSPC, ratio = 100)
wModel <- casebase::fitSmoothHazard(DeadOfPrCa ~ log(Follow.Up.Time)
+ ScrArm, data=ERSPC, ratio = 100)
sModel <- casebase::fitSmoothHazard(DeadOfPrCa ~ bs(Follow.Up.Time) + ScrArm, data=ERSPC, ratio = 100)
coxModel<-coxph(Surv(Follow.Up.Time,DeadOfPrCa ) ~ ScrArm, data=ERSPC)
compareHazard=as.data.frame(cbind(coxModel$coefficients, gModel$coefficients[3] ,eModel$coefficients[2],wModel$coefficients[3],sModel$coefficients[5]))

colnames(compareHazard)=c("Cox","Gompertz","Exponential","Weibull","Splines")
knitr::kable(compareHazard,row.names = FALSE)
```

#Absolute Risk

## Absolute Risk
* we have a bunch of different parametric Hazard models now.
* to get the absolute risk, we need to evaluate the following equation in relation to the hazard:
$$CI(x,t)=1-e^{-\int^{t}_{0}h(x,u)du}$$

* Lets use the weibull hazard


##Casebase: Absolute Risk comparison
```{r echo=FALSE,warning=FALSE,message=FALSE}
time_points <- seq(0,15, 1)
newData=as.data.frame(cbind(factor(0),2,0))
colnames(newData)=c("ScrArm","Follow.Up.Time","DeadOfPrCa")
newData$ScrArm=factor(newData$ScrArm)
# calculate cumulative incidence using casebase model
wRisk <- absoluteRisk(object = wModel,time = time_points,newdata = newData)
#plot(wRisk)
coxRisk=survfit(coxModel, newdata = newData)
#plot(coxRisk$time,coxRisk$cumhaz)


# cumulative incidence function for the Cox model
plot(coxRisk$time,coxRisk$cumhaz, type="l",lwd=3,
xlab = "Years", ylab = "Cumulative Incidence (%)", fun = "event",
xlim = c(0,15), conf.int = F, col = "red", 
main = sprintf("Estimated Cumulative Incidence (risk) With No Screening"))

# add casebase curve with legend
lines(wRisk[,1], wRisk[,2], type = "l", col = "blue",lwd=3)
legend("bottomright", 
legend = c("semi-parametric (Cox)", "parametric (casebase)"), 
col = c("red","blue"),
lty = c(1, 1), 
bg = "gray90")

```
```{r eval=FALSE,echo=TRUE}
casebase::absoluteRisk(object, time, newdata)
```

#Competing Risks

##Competing Risks
* Current methods:
* Fine-Gray
* Kaplan-Meier
* Proposed method:
* Case-Base

##Competing Risks: Data
* Two diseases:
* Lymphoblastic leukemia (ALL)
* Myeloblastic leukemia (AML)
* Contains a competing event.


```{r,warning=FALSE,message=FALSE}
sample=head(casebase::bmtcrr)
sample=sample[,c("D","Status","ftime")]
knitr::kable(head(sample),row.names = FALSE)
```
```{r,echo=FALSE,warning=FALSE,message=FALSE}
newdata <- data.frame("Sex" = factor(c("F", "F"), 
levels = c("F", "M")),
"D" = c("ALL", "AML"),
"Phase" = factor(c("Relapse", "Relapse"), 
levels = c("CR1", "CR2", "CR3", "Relapse")),
"Age" = c(35, 35),
"Source" = factor(c("PB", "PB"), 
levels = c("BM+PB", "PB")))
model_cb <- fitSmoothHazard(Status ~ ftime + Sex + D + Phase + Source + Age,
data = bmtcrr, time = "ftime")
model_fg <- comp.risk(Event(ftime, Status) ~ const(Sex) + const(D) + 
const(Phase) + const(Source) + const(Age), 
data = bmtcrr, cause = 1, model = "fg")
time_points <- (0:100)*60/100
risk_cb <- absoluteRisk(object = model_cb, time = time_points, method = "montecarlo", newdata = newdata)
risk_cb <- bind_rows(data.frame(Time = time_points, Method = "Case-base", Risk = risk_cb[,2], Disease = "ALL",
stringsAsFactors = FALSE),
data.frame(Time = time_points, Method = "Case-base", Risk = risk_cb[,3], Disease = "AML",
stringsAsFactors = FALSE))

risk_fg <- predict(model_fg, newdata, times = time_points)
risk_fg <- bind_rows(data.frame(Time = time_points, Method = "Fine-Gray", Risk = risk_fg$P1[1,], Disease = "ALL",
stringsAsFactors = FALSE),
data.frame(Time = time_points, Method = "Fine-Gray", Risk = risk_fg$P1[2,], Disease = "AML",
stringsAsFactors = FALSE))
risk_km <- map_df(c("ALL", "AML"), function(disease) {
foo <- bmtcrr %>% 
filter(D == disease) %>% 
survfit(Surv(ftime,Status == 1) ~ 1, data = .)
data.frame(Time = foo$time, Method = "Kaplan-Meier", Risk = 1 - foo$surv, Disease = disease,
stringsAsFactors = FALSE) %>%
filter(Time <= 60)
})
disease_names <- c("ALL" = "Acute Lymphoid Leukemia",
"AML" = "Acute Myeloid Leukemia")
p <- risk_cb %>%
bind_rows(risk_fg,
risk_km) %>% 
arrange(Disease, Risk) %>% 
ggplot(aes(Time, Risk, colour = Method)) + 

geom_step(data = . %>% filter(Method != "Case-base"),size=2) + 
geom_line(data = . %>% filter(Method == "Case-base"),size=1.5) + 
facet_grid(Disease ~ ., labeller = as_labeller(disease_names)) + ylim(c(0,1)) + theme(legend.position = "top") +
xlab("Time (in Months)") + ylab("Relapse risk")
```
##Competing Risks: Absolute Risk
```{r echo=TRUE,eval=FALSE,warning=FALSE,message=FALSE}
model_cb <- casebase::fitSmoothHazard(Status ~ ftime 
+ ... , data = bmtcrr)
risk_cb <- absoluteRisk(Model, Time, Newdata)
```

##Competing Risks: Absolute Risk
```{r,warning=FALSE,message=FALSE}
p
```

#Summary

##Summary
* CaseBase sampling implicitly incorporates censoring and permits the use of GLMs and the tools associated with them
* The casebase package contains tools to generate:
* Population-Time plots
* Hazard functions
* Absolute Risk
* Casebase can deal with competing risks.


##References

http://sahirbhatnagar.com/casebase/
Math paper
Hanley paper
Max Presentation slides
Olli presentation slides
data reference
data reference

# cumulative incidence function for the Cox model
plot(coxRisk$time,coxRisk$cumhaz, type="l",
     xlab = "Years", ylab = "Cumulative Incidence (%)", fun = "event",
     xlim = c(0,15), conf.int = F, col = "red", 
     main = sprintf("Estimated Cumulative Incidence (risk)"))

# add casebase curve with legend
lines(wRisk[,1], wRisk[,2], type = "l", col = "blue")
legend("bottomright", 
       legend = c("semi-parametric (Cox)", "parametric (casebase)"), 
       col = c("red","blue"),
       lty = c(1, 1), 
       bg = "gray90")
