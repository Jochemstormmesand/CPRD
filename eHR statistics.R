#eHR Statistics
install.packages("plotly")
library(plotly)

#Patient Demographics
PatientInfo <- tbl_df(sqldf("SELECT * FROM Patient_001 ORDER BY patid", connection = db))%>%
  group_by(patid, yob) %>%
  distinct(patid, .keep_all = TRUE) %>%
  ungroup() %>%
  semi_join(., three_and_two)
#Patient Demographics less columns for left_join
PatientInfo2 <- tbl_df(select_events(db, tab = "Patient_001", columns = c("patid", "gender", "yob", "deathdate")))%>%
  group_by(patid, yob) %>% 
  distinct(patid, .keep_all = TRUE) %>% 
  ungroup() %>%
  semi_join(., three_and_two)
#Patient demographics for those on IS medication. Added age at time of first prescription. 
ISInfo <- left_join(SelFirstImmuneProducts, PatientInfo2) %>%
  semi_join(., three_and_two)%>%
  mutate(ageatstart = year(eventdate)-yob)
#Create age groups  
agebreaks <- c(0,30,40,50,60,70,80,500) #make another exclusion criterium earlier on, because now very very young patients are included...
agelabels <- c("0-29","30-39","40-49","50-59","60-69","70-79","80+")
setDT(ISInfo)[ , agegroups := cut(ageatstart, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]
table1 <- CreateTableOne("gender", data = ISInfo, factorVars = "gender", strata =c("agegroups"))

PDInfotest <- left_join(SelFirstImmuneProducts, PatientInfo2) %>%
  semi_join(., Final_True_PD) %>%
  mutate(ageatstart = year(eventdate)-yob)

PDInfo <- left_join(SelFirstParkinsoncodes, PatientInfo2) %>% 
  semi_join(., Final_True_PD) %>% 
  group_by(patid, medcode) %>% 
  distinct(eventdate, .keep_all = TRUE) %>% 
  ungroup %>%
  mutate(AgeatOnset = year(eventdate) - yob)
#PDInfoAge <- PDInfo %>% mutate(deathdateyear = as.numeric(format(as.Date(PDInfo$deathdate, format="%Y-%m-%d"),"%Y")))
#PDInfoAge <- PDInfoAge %>% mutate(eventdateyear = as.numeric(format(as.Date(PDInfoAge$eventdate, format="%Y-%m-%d"),"%Y")))
#PDInfoAge <- PDInfoAge %>% mutate(AgeatOnset = PDInfoAge$eventdateyear - PDInfoAge$yob)
setDT(PDInfo)[ , agegroups := cut(AgeatOnset, 
                                  breaks = agebreaks, 
                                  right = FALSE, 
                                  labels = agelabels)]
table2 <- CreateTableOne("gender", PDInfo, factorVars = "gender", strata = c("agegroups"))
print(table2, showAllLevels = TRUE)
#PDInfoAge <- PDInfoAge %>% mutate(todyear = as.numeric(format(as.Date(PDInfoAge$tod, format="%Y-%m-%d"),"%Y")))

## Possibly needed to calculate age at other timepoint.
# PatientInfo <- PatientInfo %>% mutate(deathdateyear = as.numeric(format(as.Date(PatientInfo$deathdate, format="%Y-%m-%d"),"%Y")))
# PatientInfo <- PatientInfo %>% mutate(todyear = as.numeric(format(as.Date(PatientInfo$tod, format="%Y-%m-%d"),"%Y")))
# PatientInfo <- PatientInfo %>% mutate(age3 = PatientInfo$todyear - PatientInfo$yob)
# PatientInfo <- PatientInfo %>% mutate(age2 = Startyear - PatientInfo$yob)
# PatientInfo <- PatientInfo %>% mutate(age1 = PatientInfo$deathdateyear - PatientInfo$yob)
# PatientInfo$age <- PatientInfo$age1
# my.na <- is.na(PatientInfo$age)
# PatientInfo$age[my.na] <- PatientInfo$age2[my.na]
## Can I do this? maybe they didnt die.. could be inaccurate. 

#Age
#PD patients age at PD onset
hist(PDInfo$AgeatOnset)
plot_ly(x = PDInfo$AgeatOnset, type = "histogram", name = "Age at onset")%>% layout(title = "Age at PD onset",
         yaxis = list(title = 'Frequency'),
         xaxis = list(title = 'Age'))
ageatonset<- ggplotly(ggplot(PDInfo, aes(x=AgeatOnset)) + geom_histogram(binwidth=5, fill="#CCCCCC") +
  scale_x_continuous(breaks=seq(0,max(PDInfo$AgeatOnset), 5))+
  geom_vline(data=PDInfo, aes(xintercept=mean(PDInfo$AgeatOnset, na.rm = T)),
             linetype="dashed", size=1, colour="red") +theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))

as.integer(PDInfo$AgeatOnset)
min(PDInfo$AgeatOnset, na.rm = TRUE)
max(PDInfo$AgeatOnset, na.rm = TRUE) 
mean(PDInfo$AgeatOnset, na.rm = TRUE)
median(PDInfo$AgeatOnset, na.rm = TRUE)
table(PDInfo$AgeatOnset)

#age
#IS patients age at First IS treatment
plot_ly(x = ISInfo$ageatstart, type = "histogram", name = "Age at first IS treatment")%>% layout(title = "Age at first IS treatment",
                                                                                       yaxis = list(title = 'Frequency'),
                                                                                       xaxis = list(title = 'Age'))
ageatfirstIS<- ggplotly(ggplot(ISInfo, aes(x=ageatstart)) + geom_histogram(binwidth=5, fill="#CCCCCC") +
  scale_x_continuous(breaks=seq(0,max(ISInfo$ageatstart), 5))+
  geom_vline(data=ISInfo, aes(xintercept=mean(ISInfo$ageatstart, na.rm = T)),
             linetype="dashed", size=1, colour="red") +theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))

min(ISInfo$ageatstart, na.rm = TRUE)
max(ISInfo$ageatstart, na.rm = TRUE) 
mean(ISInfo$ageatstart, na.rm = TRUE)
median(ISInfo$ageatstart, na.rm = TRUE)

ageatfirstISPD<- ggplotly(ggplot(PDInfotest, aes(x=ageatstart)) + geom_histogram(binwidth=5, fill="#CCCCCC") +
                          scale_x_continuous(breaks=seq(0,max(PDInfotest$ageatstart), 5))+
                          geom_vline(data=PDInfotest, aes(xintercept=mean(PDInfotest$ageatstart, na.rm = T)),
                                     linetype="dashed", size=1, colour="red") +theme_bw() + 
                          theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))
min(PDInfotest$ageatstart, na.rm = TRUE)
max(PDInfotest$ageatstart, na.rm = TRUE) 
mean(PDInfotest$ageatstart, na.rm = TRUE)
median(PDInfotest$ageatstart, na.rm = TRUE)

PD_ageatstart <- summarise(PDInfotest, mean_age = mean(ageatstart), sd = sd(ageatstart))
PD_ageatonset <- summarise(PDInfo, mean_age = mean(AgeatOnset), sd = sd(AgeatOnset))
IS_ageatstart <- summarise(ISInfo, mean_age = mean(ageatstart), sd = sd(ageatstart))

table2 <- CreateTableOne("gender", PDInfo, factorVars = "gender", strata = c("agegroups"))

#Gender
Gender = table(ISInfo$gender)
GenderPD = table(PDInfo$gender)
table(ISInfo$gender, PDInfo$gender)

barplot(Gender, main = "Gender", xlab = "Gender", ylab = "Number of Patients")


summary_pp_IS <- three_and_two %>% 
  ungroup() %>% group_by(Drug) %>%
  summarise(nr_pp_immunosup = n_distinct(patid))%>%
  arrange(desc(nr_pp_immunosup))
summary_pp_IS_PD <- three_and_two %>% semi_join(.,Final_True_PD, by ="patid") %>%
  ungroup() %>% group_by(Drug) %>%
  summarise(nr_pp_immunosup = n_distinct(patid))%>%
  arrange(desc(nr_pp_immunosup))










#Incidence and Prevalence of PD
nParkinsonpatients <- length(unique(SelParkinsoncodes$patid))
table(registered_patients$year)
table(incident_cases$year)
totals$prevalence$year_counts
totals$incidence$year_counts
#hist(totals$prevalence$year_counts$prevalence)
plot(totals$incidence$year_counts$year, totals$incidence$year_counts$numerator)
barplot(totals$prevalence$year_counts$numerator)

prevalence <- plot_ly(
  x = totals$prevalence$year_counts$year,
  y = totals$prevalence$year_counts$prevalence,
  name = "Prevalence",
  type = "bar"
)

incidence <- plot_ly(
  x = totals$incidence$year_counts$year,
  y = totals$incidence$year_counts$incidence,
  name = "Incidence",
  type = "bar"
)


nr_of_pp_Parkinsoncode <- filter(ppm, ppm$medcode %in% Parkinsoncodes)
nr_of_pp_ParkinsonEXcode <- filter(ppm, ppm$medcode %in% ParkinsonExclusioncodes)
nr_of_PDcodes_PP <- tbl_df(sqldf("SELECT patid, medcode, COUNT(DISTINCT medcode) AS nr_of_PDcodes FROM SelParkinsoncodes GROUP BY patid ORDER BY nr_of_PDcodes DESC"))
nr_of_pp_Immunosup <- tbl_df(sqldf("SELECT prodcode, COUNT(DISTINCT patid) AS patients FROM SelImmuneProducts GROUP BY prodcode ORDER BY patients DESC"))

three_and_two <- three_and_two %>% mutate(drug_grouped = case_when(
  grepl("adalimumab", three_and_two$Drug, ignore.case = TRUE) ~ "Adalimumab",
  grepl("alemtuzumab", three_and_two$Drug, ignore.case = TRUE) ~"Alemtuzumab",
  grepl("azathioprine", three_and_two$Drug, ignore.case = TRUE) ~ "Azathioprine",
  grepl("ciclosporin", three_and_two$Drug, ignore.case = TRUE) ~ "Ciclosporin",
  grepl("cyclophosphamide", three_and_two$Drug, ignore.case = TRUE) ~ "Cyclophosphamide",
  grepl("etanercept", three_and_two$Drug, ignore.case = TRUE) ~ "Etanercept",
  grepl("infliximab", three_and_two$Drug, ignore.case = TRUE) ~ "Infliximab",
  grepl("methotrexate", three_and_two$Drug, ignore.case = TRUE) ~ "Methotrexate",
  grepl("mycophenolate", three_and_two$Drug, ignore.case = TRUE) ~ "Mycophenolate",
  grepl("rituximab", three_and_two$Drug, ignore.case = TRUE) ~ "Rituximab",
  grepl("tacrolimus", three_and_two$Drug, ignore.case = TRUE) ~ "Tacrolimus"))
  
nr_of_Immunosup_PP <- tbl_df(sqldf("SELECT patid, COUNT(DISTINCT drug_grouped) AS nr_of_drugs FROM three_and_two GROUP BY patid ORDER BY nr_of_drugs DESC"))
table(nr_of_Immunosup_PP$nr_of_drugs)



