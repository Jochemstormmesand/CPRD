
practices<- select_events(db=db,tab="Practice_001", convert_dates=TRUE)
prevalence_dat2<- left_join(prevalence_dat,practices, by = c("practid" = "pracid"))
cohort<- build_cohort(prevalence_dat,cohort_type=c("incid"),
                      cohort_start="1995-01-01", cohort_end="2015-12-31"
                      , diagnosis_start="eventdate")
cohort <- tbl_df(merge(x = cohort, y = ISInfo[ , c("patid", "ageatstart", "agegroups")], by = "patid", all.x=TRUE))

## Add a logical column for death during cohort
#####DONT USE DEATH DATE BUT PD DIAGNOSIS!!!!!

cohort$PD<- with(cohort,
                    ifelse(!is.null(eventdate)&
                             (eventdate> as.Date("1995-01-01") &
                                eventdate< as.Date("2015-12-31")),
                           1, 0))
cohort$PD[is.na(cohort$PD)]<- 0
cohort_PD <- cohort %>% filter(case>0)
#library(survival)
surv_obj<- with(cohort,Surv(start,end, PD))
coxph_PD <- summary(coxph(surv_obj~gender,data=cohort))
coxph_PD2 <- summary(coxph(surv_obj~gender+ageatstart,data=cohort))

plot(survfit(coxph_PD,type = "kaplan-meier"),ylim= c(0.995,1), xlab="time",
     ylab="Proportion")


sfit <- survfit(coxph_PD)
cumhaz.upper <- -log(sfit$upper)
cumhaz.lower <- -log(sfit$lower)
cumhaz <- sfit$cumhaz
CIF <- plot(cumhaz, xlab="years", ylab="cumulative hazard",
     ylim=c(min(cumhaz.lower), max(cumhaz.upper)))
lines(cumhaz.lower) 
lines(cumhaz.upper)

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

