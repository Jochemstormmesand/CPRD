
practices<- select_events(db=db,tab="Practice_001", convert_dates=TRUE)
prevalence_dat2<- left_join(prevalence_dat,practices, by = c("practid" = "pracid"))
cohort<- build_cohort(prevalence_dat,cohort_type=c("incid"),
                      cohort_start="1995-01-01", cohort_end="2015-12-31"
                      , diagnosis_start="eventdate")
cohort <- tbl_df(merge(x = cohort, y = ISInfo[ , c("patid", "ageatstart", "agegroups")], by = "patid", all.x=TRUE))

## Add a logical column for death during cohort
#####DONT USE DEATH DATE BUT PD DIAGNOSIS!!!!!
require('GGally')
cohort$PD<- with(cohort,
                    ifelse(!is.null(eventdate)&
                             (eventdate> as.Date("1995-01-01") &
                                eventdate< as.Date("2015-12-31")),
                           1, 0))
cohort$PD[is.na(cohort$PD)]<- 0
cohort_PD <- cohort %>% filter(case>0)
#library(survival)
#including IS patients
surv_obj<- with(cohort,Surv(start/365.25,end/365.25, PD))
#excluding IS patients. Purely looking at survival time of the PD patients. 
surv_obj_PD <- with(cohort_PD, Surv(start/365.25,end/365.25,PD))
coxph_PD <- coxph(surv_obj~gender,data=cohort)
coxph_PD2 <- coxph(surv_obj~gender+ageatstart,data=cohort)
coxph_PD_PD <- coxph(surv_obj_PD~gender+ageatstart, data=cohort_PD)
sf.Immunosup <-survfit(coxph_PD, type = "kaplan-meier") 
plot(survfit(coxph_PD,type = "kaplan-meier"),ylim= c(0.995,1), xlab="time",
     ylab="Proportion")
plot(survfit(coxph_PD_PD,type = "kaplan-meier"), xlab="time",
     ylab="Proportion")

ggsurv(sf.Immunosup, CI = T, plot.cens = T, ylab = "PD free survival", xlab = "Time to PD (y)", main ="PD free survival of immunosuppressant cohort" )




sfit <- survfit(coxph_PD)
cumhaz.upper <- -log(sfit$upper)
cumhaz.lower <- -log(sfit$lower)
cumhaz <- sfit$cumhaz
CIF <- plot(cumhaz, xlab="years", ylab="cumulative hazard",
     ylim=c(min(cumhaz.lower), max(cumhaz.upper)))
lines(cumhaz.lower) 
lines(cumhaz.upper)

sfit <- survfit(coxph_PD_PD)
cumhaz.upper <- -log(sfit$upper)
cumhaz.lower <- -log(sfit$lower)
cumhaz <- sfit$cumhaz
CIF <- plot(cumhaz, xlab="years", ylab="cumulative hazard",
            ylim=c(min(cumhaz.lower), max(cumhaz.upper)))
lines(cumhaz.lower) 
lines(cumhaz.upper)


#IBD
cohort_IBD <- build_cohort(prevalence_dat_IBD,cohort_type=c("incid"),
                           cohort_start="1995-01-01", cohort_end="2015-12-31"
                           , diagnosis_start="eventdate")
cohort_IBD <- tbl_df(merge(x = cohort_IBD, y = ISInfo[ , c("patid", "ageatstart", "agegroups")], by = "patid", all.x=TRUE))

## Add a logical column for death during cohort
#####DONT USE DEATH DATE BUT PD DIAGNOSIS!!!!!

cohort_IBD$PD<- with(cohort_IBD,
                 ifelse(!is.null(eventdate)&
                          (eventdate> as.Date("1995-01-01") &
                             eventdate< as.Date("2015-12-31")),
                        1, 0))
cohort_IBD$PD[is.na(cohort_IBD$PD)]<- 0
cohort_IBD_PD <- cohort_IBD %>% filter(case>0)
#library(survival)
#including IS patients
surv_IBD_obj<- with(cohort_IBD,Surv(start,end, PD))
#excluding IS patients. Purely looking at survival time of the PD patients. 
surv_IBD_obj_PD <- with(cohort_IBD_PD, Surv(start,end,PD))
coxph_IBD_PD <- coxph(surv_IBD_obj~gender,data=cohort_IBD)
coxph_IBD_PD2 <- summary(coxph(surv_IBD_obj~gender+ageatstart,data=cohort_IBD))
coxph_IBD_PD_PD <- coxph(surv_IBD_obj_PD~gender+ageatstart, data=cohort_IBD_PD)
plot(survfit(coxph_IBD_PD,type = "kaplan-meier"),ylim= c(0.995,1), xlab="time",
     ylab="Proportion")
plot(survfit(coxph_IBD_PD_PD,type = "kaplan-meier"), xlab="time",
     ylab="Proportion")




#INFLAMMATORY
cohort_Inflam <- build_cohort(prevalence_dat_Inflam,cohort_type=c("incid"),
                           cohort_start="1995-01-01", cohort_end="2015-12-31"
                           , diagnosis_start="eventdate")
cohort_Inflam <- tbl_df(merge(x = cohort_Inflam, y = ISInfo[ , c("patid", "ageatstart", "agegroups")], by = "patid", all.x=TRUE))

cohort_Inflam$PD<- with(cohort_Inflam,
                     ifelse(!is.null(eventdate)&
                              (eventdate> as.Date("1995-01-01") &
                                 eventdate< as.Date("2015-12-31")),
                            1, 0))
cohort_Inflam$PD[is.na(cohort_Inflam$PD)]<- 0
cohort_Inflam_PD <- cohort_Inflam %>% filter(case>0)
#library(survival)
#including IS patients
surv_Inflam_obj<- with(cohort_Inflam,Surv(start,end, PD))
#excluding IS patients. Purely looking at survival time of the PD patients. 
surv_Inflam_obj_PD <- with(cohort_Inflam_PD, Surv(start,end,PD))
coxph_Inflam_PD <- coxph(surv_Inflam_obj~gender,data=cohort_Inflam)
coxph_Inflam_PD2 <- summary(coxph(surv_Inflam_obj~gender+ageatstart,data=cohort_Inflam))
coxph_Inflam_PD_PD <- coxph(surv_Inflam_obj_PD~gender+ageatstart, data=cohort_Inflam_PD)
plot(survfit(coxph_Inflam_PD,type = "kaplan-meier"),ylim= c(0.995,1), xlab="time",
     ylab="Proportion")
plot(survfit(coxph_Inflam_PD_PD,type = "kaplan-meier"), xlab="time",
     ylab="Proportion")

sf.Inflammation <- survfit(coxph_Inflam_PD, type= "kaplan-meier")
sf.IBD <- survfit(coxph_IBD_PD, type= "kaplan-meier")
fit <- list(sf.Inflammation, sf.IBD)
inflam_IBD <- rbind((cbind(cohort_IBD, type = "IBD")),
                    (cbind(cohort_Inflam, type = "Other Inflammations")))
sf.inflam_IBD <- survfit(Surv(start/365,end/365,PD) ~ type, data = inflam_IBD)
inflam_IBD_dif <- survdiff(Surv(start,end,PD) ~ type, data = inflam_IBD)
Surv_Inflam_IBD_obj_PD <- with(inflam_IBD, Surv(start, end, PD))
inflam_IBD_dif <- survdiff(sf)
coxph_inflam_IBD <- coxph(Surv_Inflam_IBD_obj_PD~type, data = inflam_IBD)
inflam_IBD_dif <- survdiff(Surv(start,end,PD)~type, data= inflam_IBD)

plot(survfit(coxph_inflam_IBD, type = "kaplan-meier"), ylim = c(0.995,1),xlab = "time", ylab = "proportion")
plot(survfit(Surv(start,end,PD)~type, data = inflam_IBD),lty = 1:2,mark.time = FALSE,xlab = "time", ylab = "proportion of PD free survival",  ylim = c(0.995,1), main = "")
title(main = "PD free survival of IBD vs other inflammatory diseases")
legend(250, 0.9965, legend = c("Other inflammation", "IBD"), lty = c(2, 1),
       title = "Type of Inflammation", bty = "n")
plot(sf.inflam_IBD, ylim = c(0.995,1))

ggsurv(sf.inflam_IBD, CI = T, plot.cens = T, xlab = "Time to PD (y)",ylab = "PD free survival", main ="PD free survival of IBD vs other immunosuppressants" )













haz <- survexp(~followup, data = cohort,
               rmap = list(sex = gender, year = start, age = ageatstart),
               method = 'individual.h')


bmt <- Surv(cohort$end-cohort$start, cohort$case)
fitbmt <- survfit(bmt ~1)
summary(fitbmt)
plot(fitbmt,ylim = c(0.994,1),
     main = 'Kaplan Meyer Plot with confidence bands')
legend(1000, 0.99, legend = c('K-M survival estimate',
                              'pointwise intervals', 'Hall-Werner conf bands'), lty = 1:3)

form <- formula(bmt ~ patid+practid+gender+yob+crd+year+followup+end_date+ageatstart+agegroups)
coxbmt <- coxph(form, data = cohort)
summary(coxbmt)
r_fit_bmt <- ranger(form,
                    data = cohort,
                    importance = "permutation",
                    seed = 1234)

pyears(Surv(case/365.25,case) ~ cut(ageatstart+48, c(0,50,60,70,100))+case, data = cohort, scale =1)
personyears <- pyears(surv_obj ~ cut(ageatstart, c(c(0,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))), data = cohort, data.frame = TRUE)


personyears$data$pyearsyears <- personyears$data$pyears/365.25
personyears2 <- pyears(surv_obj ~ cut(ageatstartdays, c(0, 50, 60, 70,150)*365.25, labels=c("0-50 yr", "50-60 yr", "60-70 yr", "70+ yr")), data = cohort, scale = 1, data.frame = TRUE)
personyears$data

 c(0, 50, 60, 70,100)*365.25, labels=c("0-50 yr", "50-60 yr", "60-70 yr", "70+ yr")

