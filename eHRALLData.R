###
library(stringr)
numextract <- function(string){
  str_extract(string,"\\-*\\d+\\.*\\d*")
}
letterextract <- function(string){
  sub("[^[:alpha:]]+", "", string)}

###3.1
#cancer_codes <- clinical_codes[clinical_codes$list=="Cancer",]
#diabetes_codes <- clinical_codes[clinical_codes$list=="Diabetes",]
Parkinsoncodes = as.numeric(unlist(strsplit(c("1691 4321 8956	9509 10718 14912 16860 17004 53655 59824 86062 96860 101090"), "\\s+")[[1]]))
ParkinsonExclusioncodes = as.numeric(unlist(strsplit(c("19478	24001	26181	33544	51105	52589	72879	97170	100128"), "\\s+")[[1]]))
ImmuneMed = tbl_df(sqldf("SELECT * FROM Prodcodes_Cut", connection = db))
PD_medcodes <- tbl_df(sqldf("SELECT medcode, readcode, desc FROM MedicalReadcodes WHERE medcode IN (1691, 4321, 8956,	9509, 10718, 14912, 16860, 17004, 53655, 59824, 86062, 96860, 101090)", connection = db))
All_medcodes <- tbl_df(sqldf("SELECT * FROM MedicalReadcodes", connection = db))
All_prodcodes <- tbl_df(sqldf("SELECT * FROM MedicalProdcodes", connection =db))
PD_prodcodes <- tbl_df(sqldf("SELECT cprd_prodcode, description2 FROM prod_pd_treat", connection = db)) 
colnames(PD_prodcodes)[1] <- "prodcode"


###
#find ImmuneMed in prodcode
SelImmuneProducts <- tbl_df(select_events(db,tab="Therapy_All", columns = '*',
                                          where = "prodcode %in% .(ImmuneMed$Prodcode)"))
SelFirstImmuneProducts <- tbl_df(first_events(db, tab="Therapy_All", columns = c("patid", "eventdate", "prodcode"), 
                                              where = "prodcode %in% .(ImmuneMed$Prodcode)"))
#SelOtherProducts <- tbl_df(select_events(db,tab = "Therapy_All", columns = '*', 
#                                 where = "!prodcode %in% .(ImmuneMed$Prodcode)"))
nImmuneProducts <- tbl_df(unique(SelImmuneProducts$patid))
#nOtherProducts <- tbl_df(unique(SelOtherProducts$patid))

DrugInfo <- tbl_df(select_events(db, tab="Prodcodes_Cut", columns = '*')) 
colnames(DrugInfo)[1] <- "prodcode" #DO THIS IN BASH ALREADY!! NEEDS TO BE DONE EVERYTIME OTHERWISE!
#ADD PRODUCT NAME AND STRENGTH TO THERAPY FILE
MedCalculation <- tbl_df(left_join(SelImmuneProducts, DrugInfo))
colnames(MedCalculation)[11] <- "strength"
#############################



#####################
# GO to 3months_2consecutive script from here. 
#####################







###
#Calculate duration of each prescription and total duration from eventdate variable 
MedCalculation <- MedCalculation %>%
  group_by(patid, prodcode) %>% #not group by eventdate, but order by eventdate
  mutate(BETWEEN0=as.numeric(difftime(eventdate,lag(eventdate,1))),BETWEEN=ifelse(is.na(BETWEEN0),0,BETWEEN0),FIRST=cumsum(as.numeric(BETWEEN)))%>%
  select(-BETWEEN0)
#calculate gap
MedCalculation <- MedCalculation %>%
  mutate(GapDuration = as.integer(lead(BETWEEN) - medduration))
MedCalculation <- MedCalculation %>%
  mutate(daily_dose = MedCalculation$ndd*MedCalculation$Concentration)
MedCalculation <- MedCalculation %>%
  mutate(ndd_minimum = case_when(MedCalculation$ndd == 0 ~ MedCalculation$Concentration,
                                 MedCalculation$ndd >0 ~ MedCalculation$ndd * MedCalculation$Concentration))
#USEFUL UPGRADE FOR ndd_minimum: 
#STILL FIGURE OUT HOW TO IMPROVE THIS AS IN FORBES AS ALL FIGURE 3.
#MedCalculation21 <- MedCalculation21 %>%
# mutate(ndd_minimum = case_when(MedCalculation21$ndd == 0 & qty_median == 0 & qty_binary == 0 ~ MedCalculation21$Concentration,
#                                MedCalculation21$ndd == 0 & qty_median >0 ~ MedCalculation21$qty_median * MedCalculation21$Concentration,
#                                MedCalculation21$ndd >0 ~ MedCalculation21$ndd * MedCalculation21$Concentration)

MedCalculation <- MedCalculation %>%
  mutate(tot_daily_dose = MedCalculation$daily_dose*MedCalculation$medduration)


#!!!!!!!!Why does this line give NA for some of the cumtot_daily dose? Seems not to be dependent on the variables I have given, becuase mostly it works perfect..
MedCalculation <- MedCalculation %>%
  group_by(patid, prodcode) %>%
  mutate(cumtot_daily_dose = cumsum(as.numeric(daily_dose)*as.numeric(medduration))) %>% ungroup -> MedCalculation 

### 
# EDUCATED GUESSES ON DRUG USE;
library(tidyr)
#Seems not to work for no reason..If possible, group by less. + TAKES REALLLY LONG, less grouping = faster
#MedCalculation_filled <- MedCalculation %>% 
#  group_by(patid, prodcode, Formulation, Concentration, qty) %>% 
#  fill(ndd)

drugs <- c("Etanercept","Azathioprine","Methotrexate sodium","Cyclophosphamide monohydrate","Mycophenolate mofetil",
           "Mycophenolate sodium","Tacrolimus monohydrate","Infliximab","Rituximab","Tacrolimus","Mycophenolate mofetil hydrochloride",
           "Ciclosporin","Methotrexate","Cyclophosphamide","Alemtuzumab")
Formulation <-  c("Injection Solution","Tablet","Solution for injection","Injection","Capsule","Gastro-resistant tablet","Modified-release capsule",
                  "Powder for solution for infusion","Solution for infusion","Oral suspension","Solution For Injection",
                  "Once Daily Modified Release Capsules","Powder For Solution For Injection","Powder for solution for injection",
                  "Powder and solvent for solution for injection","Tablets","Oral solution","Capsules","Concentrate For Solution For Infusion","Granules")  
paste(drugs,collapse="")
#FIll out the right values for each drug, and then we should be able to work with daily dose. 
#problem still: might not be accurate for every value (sometimes the value should be very small because it was done per weekor per day or so..)
MedCalculation24 <- MedCalculation %>%
  mutate(ndd = case_when(MedCalculation$ndd > 0 ~ MedCalculation$ndd,
                         MedCalculation$Drug == "Adalimumab" ~ 0.1428571,
                         
                         MedCalculation$Drug == "Alemtuzumab" ~,
                         
                         #makes up 250.000 of the missing ones
                         MedCalculation$Drug == "Azathioprine" & (MedCalculation$Formulation == "Capsule" | MedCalculation$Formulation == "Capsules"),
                         MedCalculation$Drug == "Azathioprine" & MedCalculation$Formulation =="Oral solution",  
                         MedCalculation$Drug == "Azathioprine" & MedCalculation$Formulation =="Oral suspension",
                         MedCalculation$Drug == "Azathioprine" & MedCalculation$Formulation == "Powder for solution for injection",
                         MedCalculation$Drug == "Azathioprine" & (MedCalculation$Formulation =="Tablet" | MedCalculation$Formulation =="Tablets"),
                         
                         MedCalculation$Drug == "Ciclosporin" & (MedCalculation$Formulation == "Oral solution" | MedCalculation$Formulation == "Capsule") ~ 2,
                         MedCalculation$Drug == "Ciclosporin" & (MedCalculation$Formulation == "Concentrate For Solution For Infusion" | MedCalculation$Formulation == "Solution for infusion"),
                         
                         #makes up 3.000 of the missing ones
                         (MedCalculation$Drug == "Cyclophosphamide" | MedCalculation$Drug =="Cyclophosphamide monohydrate") & (MedCalculation$Formulation == "Tablet" | MedCalculation$Formulation == "Tablets"),
                         MedCalculation$Drug == "Cyclophosphamide monohydrate" & MedCalculation$Formulation == "Powder for solution for injection ",
                         
                         MedCalculation$Drug == "Etanercept" & (MedCalculation$Formulation == "Injection Solution " | MedCalculation$Formulation == "Powder and solvent for solution for injection" | MedCalculation$Formulation == ignore.case("Solution for injection")),
                         
                         MedCalculation$Drug == "Infliximab" ~,
                         
                          #makes up 23.000 of the missing ones
                         MedCalculation$Drug == "Methotrexate" & MedCalculation$Formulation != "Solution for injection" ~ 0.1428571, 
                         (MedCalculation$Drug == "Methotraxate sodium" | MedCalculation$Drug =="Methotrexate")~,
                        
                         #makes up 38.000 of the missing ones
                         MedCalculation$Drug == "Mycophenolate mofetil" ~ 2,
                         MedCalculation$Drug == "Mycophenolate mofetil hydrochloride" ~,
                         
                         MedCalculation$Drug == "Rituximab" ~ 
                         
                          #makes up 3.500 of the missing ones
                         (MedCalculation$Drug == "Tacrolimus" | MedCalculation$Drug == "Tacrolimus monohydrate") & MedCalculation$Formulation != "Once Daily Modified Release Capsules" ~ 2,
                         
                         TRUE ~ 0))

Drug_Formulation <- tbl_df(sqldf("SELECT Drug, Formulation, Concentration, unit FROM MedCalculation GROUP BY Drug, Formulation, Concentration"))
Drug_Formulation2 <- tbl_df(sqldf("SELECT Drug, Formulation, ndd FROM MedCalculation GROUP BY Drug, Formulation, Concentration, ndd"))
Drug_Formulation3 <- tbl_df(sqldf("SELECT Drug, Formulation FROM MedCalculation GROUP BY Drug, Formulation"))
Drug_Formulation4 <- tbl_df(sqldf("SELECT Drug, Formulation, ndd, Concentration, unit FROM MedCalculation GROUP BY Drug, Formulation, Concentration, ndd"))
Missing <- tbl_df(sqldf("SELECT * FROM MedCalculation_filled WHERE ndd == 0"))


      #Medcalculation21 excludes ndd = 0
      MedCalculation2 <- tbl_df(sqldf("SELECT * FROM MedCalculation WHERE ndd IS NOT NULL"))
      MedCalculation21 <- tbl_df(sqldf("SELECT * FROM MedCalculation WHERE patid IN (SELECT patid FROM MedCalculation2)"))
      With <- tbl_df(sqldf("SELECT * FROM MedCalculation21 WHERE NOT ndd == 0"))
      Missing1 <- tbl_df(sqldf("SELECT * FROM MedCalculation21 WHERE ndd == 0"))
      MedCalculation3 <- tbl_df(sqldf("SELECT * FROM MedCalculation WHERE patid NOT IN (SELECT patid FROM MedCalculation2)"))
      
      
Avg_Dose_Perdrug <- tbl_df(sqldf("SELECT patid, prodcode, avg(daily_dose) AS avg_daily_dose, avg(tot_daily_dose)/avg(medduration) AS weighed_daily_dose FROM MedCalculation GROUP BY patid, prodcode")) %>% ungroup 
Total_Duration <- tbl_df(sqldf("SELECT patid, prodcode, sum(medduration) AS Total_Duration FROM MedCalculation GROUP BY patid, prodcode")) %>% ungroup %>% arrange(patid)


#calculate first and last to see increase vs decrease
MedCalculation <- MedCalculation %>% group_by(patid, prodcode)
DoseChange <- tbl_df(sqldf("SELECT eventdate, patid, prodcode, Concentration 
                           FROM MedCalculation21")) 
DoseChange <- DoseChange %>%
  group_by(patid, prodcode) %>%
  slice(c(1, n())) %>%
  ungroup()
#must check whether this line is correct when applied to the large dataset. 
#Seems not to work..
  # DoseChange2 <- DoseChange %>%
  #   group_by(patid, prodcode) %>%
  #   mutate_each(funs(diff = Concentration - lag(Concentration)),Concentration) %>%
  #   na.omit() %>%
  #   arrange(desc(diff)) %>%
  #   ungroup() 



#print(tbl_df(MedInterval), n = 100)
              MedCalculation <- MedCalculation %>%
                mutate(qty_binary = case_when(MedCalculation$qty < MedCalculation$qty_median ~ "low",
                                              MedCalculation$qty == MedCalculation$qty_median ~ "median",
                                              MedCalculation$qty > MedCalculation$qty_median ~ "high"))
              MedCalculation %>% arrange(Drug)
              
              
             
#Disp all substance.strengths
unique(MedCalculation$Substance.strength)

#Transform ALL Substance.strengths to mg. 
MedCalculation$Substance.strength[MedCalculation$Substance.strength == "500microgram"] <- "0.5mg"
MedCalculation$Substance.strength[MedCalculation$Substance.strength == "200microgram"] <- "0.2mg"
MedCalculation$Substance.strength[MedCalculation$Substance.strength == "1gram"] <- "1000mg"
MedCalculation$Substance.strength[MedCalculation$Substance.strength == "100microgram/1ml"] <- "0.1mg/1ml"
MedCalculation$Substance.strength[MedCalculation$Substance.strength == "500microgram/1ml"] <- "0.5mg/1ml"


############



##########
#find parkinsoncodes amongst medcodes
SelParkinsoncodes = tbl_df(select_events(db,tab="Clinical_All", columns = '*',
                                  where = "medcode %in% .(Parkinsoncodes)"))
SelFirstParkinsoncodes = tbl_df(first_events(db, tab="Clinical_All", columns= c("patid", "eventdate", "medcode"), 
                                             where = "medcode %in% .(Parkinsoncodes)"))
#SelImmuneParkinsoncodes= tbl_df(select_events(db, tab = c("Clinical_All_Cut","Therapy_All_Cut"), columns = '*', where "medcode %in% .(Parkinsoncodes) & prodcode %in% .(ImmuneMed)"))
SelOthercodes = tbl_df(select_events(db,tab ="Clinical001", columns = '*', 
                                     where = "!medcode %in% .(Parkinsoncodes)"))
countPP = nrow(SelParkinsoncodes)
nParkinsoncodes = length(unique(SelParkinsoncodes$medcode))
nParkinsonpatients <- length(unique(SelParkinsoncodes$patid))
nOthercodes = length(unique(SelOthercodes$medcode))
nOtherpatients <- length(unique(SelOthercodes$patid))

#Number of patients per medcode
ppm <- tbl_df(patients_per_medcode(db, clinical_table = "Clinical_All", patid = "patid", medcode = "medcode"))
#patients per Parkinsoncode and exclusioncodes
nr_of_pp_Parkinsoncode <- filter(ppm, ppm$medcode %in% Parkinsoncodes) #some patients are diagnosed by more than one medcode..
nr_of_pp_ParkinsonEXcode <- filter(ppm, ppm$medcode %in% ParkinsonExclusioncodes)
nr_of_PDcodes_PP <- tbl_df(sqldf("SELECT patid, medcode, COUNT(DISTINCT medcode) AS nr_of_PDcodes FROM SelParkinsoncodes GROUP BY patid ORDER BY nr_of_PDcodes DESC"))

#number of patients per Immunosuppressive drug
nr_of_pp_Immunosup <- tbl_df(sqldf("SELECT prodcode, COUNT(DISTINCT patid) AS patients FROM SelImmuneProducts GROUP BY prodcode ORDER BY patients DESC"))
#Total number of distinct patients on all drugs. The discrepancy between both numbers indicates that some patients take multiple drugs..
sum(nr_of_pp_Immunosup$patients)
length(unique(SelImmuneProducts$patid))





#############
##3.2
#Calculate duration
first_DM<- first_events(db,tab="Clinical_All", columns=c("patid", "eventdate", "medcode"),
                        where="medcode %in% .(Parkinsoncodes)")
last_DM<- last_events(db,tab="Clinical_All", columns=c("patid", "eventdate", "medcode"),
                      where="medcode %in% .(Parkinsoncodes)")
first_events(SelImmuneProducts)
head(first_DM)
head(last_DM)

First_and_Last <- left_join(first_DM, last_DM, by = c("patid", "medcode"))
Duration <- First_and_Last%>%
  mutate(DateDiff = as.Date(as.character(First_and_Last$eventdate.y), format="%Y-%m-%d")-
  as.Date(as.character(First_and_Last$eventdate.x), format="%Y-%m-%d"))


##3.3.1
#WATCH OUT: THESE VALUES ARE CUMULATIVE OVER YEARS.IF A PATIENT IS IN THE CLINIC FOR MORE THAN 1 YEAR IT WILL BE COUNTED FOR EVERY YEAR PRESETN!
registered_patients<- select_by_year(db=db, tables="Patient_001",
                                     columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
                                     where="crd < STARTDATE", year_range=c(1995 : 2015),year_fn =standard_years, first_events)

str(registered_patients)

table(registered_patients$year)
table(registered_patients$gender) #adds over the years though. So if a male is there for longer than a female, this will affect the counting...
table(registered_patients$yob)

incident_cases<- select_by_year(db=db, tables=c("Clinical_All", "Referral_001"),
                                columns=c("patid", "eventdate", "medcode"),
                                where="medcode %in% .(Parkinsoncodes) & eventdate <= ENDDATE",
                                year_range=c(1995: 2015), year_fn=standard_years, selector_fn =first_events)
str(incident_cases)
table(incident_cases$year)
table(incident_cases$medcode)

## All patientsare kept (equivalentto merge(all.x= TRUE))
prevalence_dat<- left_join(registered_patients, incident_cases)
## Removeduplicatesacrossclinicaland referraltables:
incident_cases%>% group_by(patid,year) %>% arrange(eventdate)%>% distinct() %>% ungroup-> incident_cases

prevalence_dat<- prev_terms(prevalence_dat)
totals<- prev_totals(prevalence_dat)
#Prevalence and incidence of Parkinson from 1995 to 2015
totals$prevalence$year_counts
totals$incidence$year_counts








###############################################################################################
###############################################################################################

## 4.0

practices<- select_events(db=db,tab="Practice001", convert_dates=TRUE)
prevalence_dat<- left_join(prevalence_dat,practices)
cohort<- build_cohort(prevalence_dat,cohort_type="prev",
                      cohort_start="2006-01-01", cohort_end="2012-12-31"
                      , diagnosis_start="eventdate")

## Add a logical column for death during cohort
cohort$death<- with(cohort,
                    ifelse(!is.null(deathdate)&
                             (deathdate> as.Date("2006-01-01") &
                                deathdate< as.Date("2012-12-31")),
                           1, 0))
cohort$death[is.na(cohort$death)]<- 0
library(survival)
surv_obj<- with(cohort,Surv(start,end, death))
coxph(surv_obj~gender+ case,data=cohort)


##4.1.
cohort2<- build_cohort(prevalence_dat,cohort_type="incid",
                       cohort_start="2006-01-01",
                       cohort_end="2012-12-31",
                       diagnosis_start = "eventdate")

IDM_controls<- get_matches(cases=filter(cohort2, case == 1),
                           control_pool=filter(cohort2,case == 0),
                           match_vars=c("gender", "region"),   ##extra_conditions for more complex matching strategies
                           n_controls=4, cores =1,
                           method="incidence_density",
                           diagnosis_date="eventdate")

IDM_controls2<- get_matches(cases=filter(cohort2, case == 1),
                            control_pool=filter(cohort2,case == 0),
                            match_vars=c("gender", "region"),
                            extra_conditions = "yob>=(.(CASE&yob)-2)
                            &yob<=(.(CASE$yob)+2)",
                            n_controls=4, cores =1,
                            method="incidence_density",
                            diagnosis_date="eventdate")

exact_controls3 <- get_matches(cases=filter(cohort2, case == 1),
                               control_pool=filter(cohort2,case == 0),
                               match_vars=c("gender", "region"),
                               n_controls=4, cores =1,
                               method="exact",
                               diagnosis_date="eventdate")

consultation_dir<- "~/R/rEHR_testing"
flat_files(db,out_dir=consultation_dir,file_type="csv")
index_controls<- match_on_index(cases=filter(cohort2,case == 1),
                                control_pool=filter(cohort2, case == 0),
                                index_var="eventdate",
                                match_vars=c("gender","region"),
                                index_diff_limit=90,
                                consult_path=consultation_dir,
                                n_controls=4,
                                import_fn=function(x)
                                  convert_dates(read.csv(x)))

# clean up constructed dirs after analysis
unlink(consultation_dir,recursive=TRUE)

tv_test<- data.frame(id=1 : 5, start=rep(0,5),
                     end=c(1000, 689, 1000, 874,777),
                     event=c(0, 1, 0, 1, 1),
                     drug_1=c(NA,NA, NA, 340, 460),
                     drug_2=c(NA,234, 554,123, NA),
                     drug_3_start=c(110, 110,111, 109,110),
                     drug_3_stop=c(400,400, 400,400, 400),
                     stage_1=c(300,NA, NA, NA, NA),
                     stage_2=c(450,NA, NA, NA, NA))

## Multiplebinarychroniccovariates:
tv_out1<- cut_tv(tv_test,
                 entry=start,
                 exit = end,
                 cut_var=drug_1,
                 id_var=id,
                 tv_name=drug_1_state)
tv_out1<- cut_tv(tv_out1,start,end,drug_2,id_var=id, drug_2_state)
head(tv_out1)


## Binarycovariates:
tv_out3<- cut_tv(tv_test,start,end, drug_3_start,id_var= id, drug_3_state)
tv_out3<- cut_tv(tv_out3,start,end, drug_3_stop,id_var= id, drug_3_state)
head(tv_out3)

## incrementalcovariates:
inc_1<- cut_tv(tv_test,start,end, stage_1,id_var=id, disease_stage, on_existing ="inc")
inc_1 <- cut_tv(inc_1,start,end, stage_2,id_var=id, disease_stage, on_existing ="inc")
head(inc_1)

## Chainingcombinationsof the above using %>% 
library(dplyr) 
tv_test%>% 
  cut_tv(start,end, drug_1,id_var=id, drug_1_state)%>% 
  cut_tv(start,end, drug_2,id_var=id, drug_2_state)%>% 
  cut_tv(start,end, drug_3_start,id_var=id, drug_3_state)%>% 
  cut_tv(start,end, drug_3_stop,id_var=id, drug_3_state)%>% 
  cut_tv(start,end, stage_1,id_var=id, disease_stage, 
         on_existing="inc") %>%
  cut_tv(start,end, stage_2,id_var=id, disease_stage, 
         on_existing="inc") %>%
  head

## Exampleconstructionof a clinicalcode list
def <- MedicalDefinition(
  terms=list( "peripheral vascular disease", "peripheral gangrene", " -wrong answer", "intermittent claudication", "thromboangiitis obliterans","thromboangiitis obliterans", "diabetic peripheral angiopathy",
              c("diabetes", "peripheral angiopathy"),# single AND expression
              c("buerger", "disease presenile_ gangrene"), " - excepted", # exclusion
              codes=list("G73"), tests=NULL, drugs=list("insulin", "diabet","aspirin")))

## Use fileEncoding= "latin1"to avoid issueswith non-asciicharacters
medical_table<- read.delim("Lookups=medical:txt", fileEncoding="latin1", stringsAsFactors=FALSE)
drug_table<- read.delim("Lookups=product:txt", fileEncoding="latin1", stringsAsFactors=FALSE)
draft_lists<- build_definition_lists(def,medical_table = medical_table, drug_table=drug_table)
