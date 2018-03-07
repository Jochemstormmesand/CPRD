
# Use the overloaded`head` function to view a list of
# tables or the head of individual tables:
head(db)
head(db, table="Clinical_All_Cut")

library(stringr)
numextract <- function(string){
  str_extract(string,"\\-*\\d+\\.*\\d*")
}
letterextract <- function(string){
  sub("[^[:alpha:]]+", "", string)}

##3.1
#cancer_codes <- clinical_codes[clinical_codes$list=="Cancer",]
#diabetes_codes <- clinical_codes[clinical_codes$list=="Diabetes",]
Parkinsoncodes = as.numeric(unlist(strsplit(c("1691 4321 8956	9509 10718 14912 16860 17004 53655 59824 86062 96860 101090"), "\\s+")[[1]]))
ParkinsonExclusioncodes = as.numeric(unlist(strsplit(c("19478	24001	26181	33544	51105	52589	72879	97170	100128"), "\\s+")[[1]]))
ImmuneMed = as.numeric(unlist(strsplit(c("23850	6882	50996	52833	42273	30495	39115	13320	55773	53869	54982	36792	671	451	32101	43077	34816	41670	19072	12339	26261	1899	270	14395	571	34687	43562	53956	53797	21899	42988	34451	29340	31215	41620	22982	35518	51181	770	39787	55021	47042	3896	54975	42449	46395	47192	973	13556	48556	54134	52615	13494	1905	1626	16035	16137	53969	19222	38525	42924	47377	2838	55116	42637	49958	46637	53175	47471	3920	972	48763	52743	2837	54974	42448	53176	47102	47047	4231	15596	48798	54867	38056	26790	19370	26322	31193	3984	47752	29840	44309	26066	44273	16105	34728	3985	10729	47843	15921	14886	36008	35419	36556	35126	26387	19257	50998	49856	41058	16822	22392	877	34929	21753	32865	37117	36800	36849	28041	27404	46156	46265	30780	823	51120	32111	20951	13428	49951	52606	53385	41104	17035	40273	40292	45165	46129	46197	40328	45558	24634	44908	50950	46039	35402	35865	35752	18424	41585	12816	40371	40356	7337	46152	46098	7336	40281	40284	16540	27400	18890	36167	46407	27342	51667	34258	14347	26064	14348	17672	40293	16519	33601	8583	27642	30703	32229	29069	24783	51321	53696	8327	49547	41086	30932	9528	40301	40280	16570	14748	21732	52488	16919	45489	47789	18804	45043	45393	16879	4438	54317	53255	4230	50669	47746	30581	7077	27290	27289	35301	26097	47502	28490	39111	36294	47240	47852	44640	47416	2839	48339	3683	47239	52993	47512	54198	51184	44804	47276	37985	37155	55066	44926	40964	6495	51790	5870	51185	54048	5089	13271	55010	47984	44641	47506	33123	47432	6633	5817	6950	5838	46324	39633	46325	43081	37506	43082	40765	38113	38989	38919"), "\\s+")[[1]]))
PD_medcodes <- tbl_df(sqldf("SELECT medcode, desc FROM MedicalReadcodes WHERE medcode IN (1691, 4321, 8956,	9509, 10718, 14912, 16860, 17004, 53655, 59824, 86062, 96860, 101090)", connection = db))
PD_prodcodes <- tbl_df(sqldf("SELECT cprd_prodcode, description2 FROM treat", connection = db))
colnames(PD_prodcodes)[1] <- "prodcode"


########
#find ImmuneMed in prodcode
SelImmuneProducts <- tbl_df(select_events(db,tab="Therapy_All", columns = '*',
                             where = "prodcode %in% .(ImmuneMed)"))
SelFirstImmuneProducts <- tbl_df(first_events(db, tab="Therapy_All", columns = c("patid", "eventdate", "prodcode"), 
                                              where = "prodcode %in% .(ImmuneMed)"))
SelOtherProducts <- tbl_df(select_events(db,tab = "Therapy", columns = '*', 
                                 where = "!prodcode %in% .(ImmuneMed)"))
nImmuneProducts <- tbl_df(unique(SelImmuneProducts$patid))
nOtherProducts <- tbl_df(unique(SelOtherProducts$patid))

#Change textid column from character to integer...
#MedCalculation$textid <- as.numeric(as.character(MedCalculation$textid))
#SELECT COLUMNS FROM common_dosages file
#Common_Dosages <- tbl_df(select_events(db, tab = "Common_Dosages", columns = c("textid", "text", "daily_dose")))
#MedCalculation <- tbl_df(left_join(MedCalculation, Common_Dosages))

DrugInfo <- tbl_df(select_events(db, tab="Prodcodes_Cut", columns = '*'))
colnames(DrugInfo)[1] <- "prodcode" #DO THIS IN BASH ALREADY!! NEEDS TO BE DONE EVERYTIME OTHERWISE!
#ADD PRODUCT NAME AND STRENGTH TO THERAPY FILE
MedCalculation <- tbl_df(left_join(SelImmuneProducts, DrugInfo))


#############################
colnames(MedCalculation)[10] <- "strength"
MedCalculation <- MedCalculation %>% 
  ungroup %>%
  mutate(Concentration = as.numeric(as.character(numextract(MedCalculation$strength))))
MedCalculation <- MedCalculation %>%
  ungroup %>% 
  mutate(unit = letterextract(MedCalculation$strength))


MedCalculation <- MedCalculation %>%
  group_by(Drug, Formulation)%>%
  mutate(qty_median = median(qty)) %>%
  ungroup()
MedCalculation <- MedCalculation %>%
  mutate(qty_binary = case_when(MedCalculation$qty < MedCalculation$qty_median ~ "low",
                                MedCalculation$qty == MedCalculation$qty_median ~ "median",
                                MedCalculation$qty > MedCalculation$qty_median ~ "high"))
MedCalculation %>% arrange(Drug)

tbl_df(sqldf("SELECT Patid, Drug, Formulation FROM MedCalculation GROUP BY Drug, Formulation"))

                  #######
                  ## left here
                  Does the patient have other therapy records with the same quantity and dose?
                    Use the median NDD from these records
                  MedCalculation %>% group_by(patid) %>%
                    mutate(ndd = replace(ndd, ndd == 0 &))
                  
                    mutate(mpg=replace(mpg, cyl==4, NA))
                    mutate(var = replace(var, var != "Candy", "Not Candy"))
                    MedCalculation[which(MedCalculation[,6]== 0 )]
                    df[which(df[,7]==min(df[,7])), 2]
                  case_when(MedCalculation$Drug == "Azathioprine" ~ median(Drug == "Azathioprine",MedCalculation$qty, na.rm = TRUE),
                                                  MedCalculation$Drug == "Ciclosporin" ~ median(MedCalculation$qty, na.rm = TRUE)))
                  ##
                  #####

                  MedCalculation %>%
                    mutate(NDD = case_when(ndd > 0 ~ ndd,
                                           ndd == 0 & qty > 0 & Concentration>0 ~
                    ))
                  if NDD = 0 
                   ifelse other therapy record with same qty and concentration
                      use median NDD from these
                    ifelse other therapy with same binary quantity variable and dose

####
MedCalculation <- tbl_df(sqldf("SELECT patid, eventdate, prodcode, qty, ndd, Drug, Concentration, unit, Formulation FROM MedCalculation ORDER BY patid, prodcode, eventdate"))
#Maybe add issueseq, drug and Formulation when needed later strength...?
#MedCalculation files filtered on NDD present or not. 

MedCalculation2 <- tbl_df(sqldf("SELECT * FROM MedCalculation WHERE NOT ndd == 0"))
MedCalculation21 <- tbl_df(sqldf("SELECT * FROM MedCalculation WHERE patid IN (SELECT patid FROM MedCalculation2)"))
With <- tbl_df(sqldf("SELECT * FROM MedCalculation21 WHERE NOT ndd == 0"))
Missing <- tbl_df(sqldf("SELECT * FROM MedCalculation21 WHERE ndd == 0"))
MedCalculation3 <- tbl_df(sqldf("SELECT * FROM MedCalculation WHERE patid NOT IN (SELECT patid FROM MedCalculation2)"))
###
#IMPORTANT: 
# WORK WITH MedCalculation21. That contains the right values, excluding patients with ONLY NO ndd. 
###

MedCalculation21 <- MedCalculation21 %>%
  mutate(medduration= qty/ndd)
####!!!###!!!##!! REALLY IMPORTANT: some of the durations/ndds are missing because text ID is missing/invalid. get median ndd to get here. 
MedCalculation21 <- MedCalculation21 %>%
  group_by(patid, prodcode) %>% #not group by eventdate, but order by eventdate
  mutate(BETWEEN0=as.numeric(difftime(eventdate,lag(eventdate,1))),BETWEEN=ifelse(is.na(BETWEEN0),0,BETWEEN0),FIRST=cumsum(as.numeric(BETWEEN)))%>%
  select(-BETWEEN0)
MedCalculation21 <- MedCalculation21 %>%
  mutate(GapDuration = as.integer(lead(BETWEEN) - medduration))
MedCalculation21 <- MedCalculation21 %>%
  mutate(daily_dose = MedCalculation21$ndd*MedCalculation21$Concentration)
MedCalculation21 <- MedCalculation21 %>%
  mutate(ndd_minimum = case_when(MedCalculation21$ndd == 0 ~ MedCalculation21$Concentration,
                                 MedCalculation21$ndd >0 ~ MedCalculation21$ndd * MedCalculation21$Concentration))
#USEFUL UPGRADE FOR ndd_minimum: 
#STILL FIGURE OUT HOW TO IMPROVE THIS AS IN FORBES AS ALL FIGURE 3.
#MedCalculation21 <- MedCalculation21 %>%
    # mutate(ndd_minimum = case_when(MedCalculation21$ndd == 0 & qty_median == 0 & qty_binary == 0 ~ MedCalculation21$Concentration,
    #                                MedCalculation21$ndd == 0 & qty_median >0 ~ MedCalculation21$qty_median * MedCalculation21$Concentration,
    #                                MedCalculation21$ndd >0 ~ MedCalculation21$ndd * MedCalculation21$Concentration)
MedCalculation21 <- MedCalculation21 %>%
  mutate(tot_daily_dose = MedCalculation21$daily_dose*MedCalculation21$medduration)
MedCalculation21 <- MedCalculation21 %>% ungroup -> MedCalculation21
#calculate first and last to see increase vs decrease

#!!!!!!!!Why does this line give NA for some of the cumtot_daily dose? Seems not to be dependent on the variables I have given, becuase mostly it works perfect..
MedCalculation21 <- MedCalculation21 %>%
  group_by(patid, prodcode) %>%
  mutate(cumtot_daily_dose = cumsum(as.numeric(daily_dose)*as.numeric(medduration))) %>% ungroup -> MedCalculation21 


Total_Duration <- tbl_df(sqldf("SELECT patid, prodcode, sum(medduration) AS Total_Duration FROM MedCalculation21 GROUP BY patid, prodcode")) %>% ungroup %>% arrange(patid)
Avg_Dose_Perdrug <- tbl_df(sqldf("SELECT patid, prodcode, avg(daily_dose) AS avg_daily_dose, avg(tot_daily_dose)/avg(medduration) AS weighed_daily_dose FROM MedCalculation21 GROUP BY patid, prodcode")) %>% ungroup 


MedCalculation21 <- MedCalculation21 %>% group_by(patid, prodcode)
DoseChange <- tbl_df(sqldf("SELECT eventdate, patid, prodcode, Concentration 
                            FROM MedCalculation21")) 
DoseChange <- DoseChange %>%
                       group_by(patid, prodcode) %>%
                       slice(c(1, n())) %>%
                       ungroup()
#must check whether this line is correct when applied to the large dataset. Seems ok, but no difference in concentration in sample file. 
DoseChange2 <- DoseChange %>%
  group_by(patid, prodcode) %>%
  mutate_each(funs(diff = Concentration - lag(Concentration)),Concentration) %>%
  na.omit() %>%
  ungroup()
 
 



daily_dose = Concentration*ndd
cum_daily_dose = cumsum(daily_dose) #must be grouped on patid and prodcode!
#print(tbl_df(MedInterval), n = 100)


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

#########
#Patient Demographics
PatientInfo <- tbl_df(sqldf("SELECT * FROM Patient_001 ORDER BY patid", connection = db))
PatientInfo <- PatientInfo %>% group_by(patid, yob) %>% distinct(patid, .keep_all = TRUE) %>% ungroup -> PatientInfo

PatientInfo2 <- tbl_df(select_events(db, tab = "Patient_001", columns = c("patid", "gender", "yob", "deathdate")))
PatientInfo2 <- PatientInfo2 %>% group_by(patid, yob) %>% distinct(patid, .keep_all = TRUE) %>% ungroup -> PatientInfo2

#####CALCULATE AGE!!!!
#what to do with patients that have another toreason than "death"? Some have become 130 otherwise, 
#and that seems illogical. Presumably TRA code 13 = "other reason" is used for death as well.
#Calculate age of patients with parkinson at moment of first admission
PDInfo <- left_join(SelFirstParkinsoncodes, PatientInfo2)
PDInfo <- PDInfo %>% group_by(patid, medcode) %>% distinct(eventdate, .keep_all = TRUE) %>% ungroup -> PDInfo
PDInfoAge <- PDInfo %>% mutate(deathdateyear = as.numeric(format(as.Date(PDInfo$deathdate, format="%Y-%m-%d"),"%Y")))
PDInfoAge <- PDInfoAge %>% mutate(eventdateyear = as.numeric(format(as.Date(PDInfoAge$eventdate, format="%Y-%m-%d"),"%Y")))
PDInfoAge <- PDInfoAge %>% mutate(AgeatOnset = PDInfoAge$eventdateyear - PDInfoAge$yob)
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

#### to do to make it work: 
#run library and install for devtools, rEHR and 
#get package with packageurl. 
#before installing dplyr package, make sure rtools is installed https://cran.r-project.org/bin/windows/Rtools/ 
#in user library, make sure that: devtools, dplyr, gsubfn, proto, rEHR, RSQLite, sqldf are selected. 
#IF NOT WORKING: untick, rEHR, untick dplyr, then tick them. 

# ############## WHAT YOU SHOULD INSTALL ACCORDING TO PAPER. SKIP THESE, CAUSE VERSIONS INCORRECT!
# Jochem Stormmesand
# Research Project on Immunosuppressive Therapy and Parkinson Risk
#
# Package used as described by Springate et al. 2017

# ############## PACKAGE INSTALLATION AS DESCRIBED BY PAPER. DOESN'T WORK
# library(devtools)
# install_github("rOpenHealth/rEHR")
# library(rEHR)
# ##############
# 
# ############## INSTALL THESE PACKAGES FOR PROPER FUNCTIONALITY
# install.packages("digest")
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_0.5.0.tar.gz"  ##version0.5.0
# install.packages(packageurl, repos = NULL, type="source")
# ##install.packages("dplyr") ##version0.7.4
# packageurldevtools <- "https://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.12.0.tar.gz"
# install.packages(packageurldevtools, repos = NULL, type="source")
# library(devtools)
# library(dplyr)
# install_github("rOpenHealth/rEHR") 
# library(rEHR)
# ##############



#TO DO with the script
#from therapy file get all immunosuppressant codes and see how many are other. 
#cumulative dose (difficult --> multiply dose, with duration with start date, end date etc)



#STARTING THE SCRIPT
##2

## Use simulated ehr files supplied with the package to build database
ehr_path <- dirname(system.file("ehr_data", "ehr_Clinical.txt",  #MAKE SURE THAT SAMPLE_ TEXT FILES ARE STORED IN C:\Users\Stormezand\Documents\R\win-library\3.3\rEHR\ehr_data 
                                package = "rEHR"))
## create a new database connection to a temporary file
db <- database(tempfile(fileext = ".sqlite"))
## Import multiple data files into the database
import_CPRD_data(db, data_dir = ehr_path,         
                 filetypes = c("Additional_001","Clinical_001","Clinical_002","Clinical_003","Consultation_001","Consultation_002",,"Consultation_003", "Immunisation_001",
                               "Patient_001", "Practice_001", "Referral_001", "Therapy_001", "Therapy_002", "Therapy_003", "Therapy_004", "Test_001","Test_002","Test_003","Test_004","Test_005","Test_006", "Staff_001"),
                 dateformat = "%d/%m/%Y",
                 yob_origin = 1800,
                 regex = "PD_immunosup_Extract",
                 recursive = TRUE)
## Individual files can also be added:
add_to_database(db, files = system.file("ehr_data", "productandmedicalcodelists.txt",
                                        package = "rEHR"),
                table_name = c("ProdandMedcodelist", dateformat = "%b-%d"))

## Use the overloaded`head` function to view a list of
## tables or the head of individual tables:
head(db)
head(db, table="Clinical_002")

##3.1
#cancer_codes <- clinical_codes[clinical_codes$list=="Cancer",]
#diabetes_codes <- clinical_codes[clinical_codes$list=="Diabetes",]
Parkinsoncodes = as.numeric(unlist(strsplit(c("1691 4321 8956	9509 10718 14912 16860 17004 53655 59824 86062 96860 101090"), "\\s+")[[1]]))
ParkinsonExclusioncodes = as.numeric(unlist(strsplit(c("19478	24001	26181	33544	51105	52589	72879	97170	100128"))))
ImmuneMed = as.numeric(unlist(strsplit(c("23850	6882	50996	52833	42273	30495	39115	13320	55773	53869	54982	36792	671	451	32101	43077	34816	41670	19072	12339	26261	1899	270	14395	571	34687	43562	53956	53797	21899	42988	34451	29340	31215	41620	22982	35518	51181	770	39787	55021	47042	3896	54975	42449	46395	47192	973	13556	48556	54134	52615	13494	1905	1626	16035	16137	53969	19222	38525	42924	47377	2838	55116	42637	49958	46637	53175	47471	3920	972	48763	52743	2837	54974	42448	53176	47102	47047	4231	15596	48798	54867	38056	26790	19370	26322	31193	3984	47752	29840	44309	26066	44273	16105	34728	3985	10729	47843	15921	14886	36008	35419	36556	35126	26387	19257	50998	49856	41058	16822	22392	877	34929	21753	32865	37117	36800	36849	28041	27404	46156	46265	30780	823	51120	32111	20951	13428	49951	52606	53385	41104	17035	40273	40292	45165	46129	46197	40328	45558	24634	44908	50950	46039	35402	35865	35752	18424	41585	12816	40371	40356	7337	46152	46098	7336	40281	40284	16540	27400	18890	36167	46407	27342	51667	34258	14347	26064	14348	17672	40293	16519	33601	8583	27642	30703	32229	29069	24783	51321	53696	8327	49547	41086	30932	9528	40301	40280	16570	14748	21732	52488	16919	45489	47789	18804	45043	45393	16879	4438	54317	53255	4230	50669	47746	30581	7077	27290	27289	35301	26097	47502	28490	39111	36294	47240	47852	44640	47416	2839	48339	3683	47239	52993	47512	54198	51184	44804	47276	37985	37155	55066	44926	40964	6495	51790	5870	51185	54048	5089	13271	55010	47984	44641	47506	33123	47432	6633	5817	6950	5838	46324	39633	46325	43081	37506	43082	40765	38113	38989	38919"), "\\s+")[[1]]))

select_events(db,tab="Clinical_002", columns = c("patid", "eventdate", "medcode"),
              where = "medcode %in% .(ImmuneMed)")

#find ImmuneMed in prodcode
SelImmuneMed = select_events(db,tab="Therapy001", columns = '*',
                             where = "prodcode %in% .(ImmuneMed)")
countIMP =  nrow(SelImmuneMed)
UniqueIMP = length(unique(SelImmuneMed$patid))

#find parkinsoncodes amongst medcodes
SelParkinsoncodes = select_events(db,tab="Clinical_002", columns = '*',
                                  where = "medcode %in% .(Parkinsoncodes)")
countPP = nrow(SelParkinsoncodes)
UniquePP = length(unique(SelParkinsoncodes$medcode))

#Calculate drug duration * dose *
UsedMed <- SelImmuneMed %>% 
  mutate(MedMultiplication = ndd*numdays)



# sqldf("SELECT patid, eventdate, medcode from Clinical001 WHERE medcode > 500 AND medcode<1000 ", connection=db)
# medlist = 180:200
# sqldf("SELECT patid, eventdate, medcode FROM Clinical001 WHERE .(medcode) = medlist", connection=db)
# 
# sqldf("SELECT patid, eventdate, medcode from Clinical001 WHERE medcode %in% parkinson_codes", connection=db)
# 
# wrap_sql_query("SELECT patid, eventdate, medcode from Clinical001 WHERE medcode in #1",cancer_codes1)
# expand_string("SELECT patid, eventdate, medcode from Clinical001 WHERE medcode in .(cancer_codes1)")


# Asthma_codes <- clinical_codes[clinical_codes$list=="Asthma",]
# q <- select_events(db,tab="Clinical001",columns=c("patid","eventdate","medcode"),
#                    where="medcode %in% .(Asthma_codes$medcode)",
#                    sql_only=TRUE)
# temp_table(db,tab_name="Asthma",select_query =q)
# head(db,temp=TRUE)
# head(db,table="Asthma")

# sqldf("SELECT patid, practid, gender, yob, deathdate from Patient001 WHERE gender IS NOT NULL LIMIT 6",
#       connection=db)
# 
# medcodes1<- 1:5
# practice<- 255
# expand_string("SELECT * FROM clinical001 WHERE practid == .(practice)")

# wrap_sql_query(-
# "SELECT * FROM clinical001 WHERE practid == # 1 AND medcodes in # 2",practice,medcodes1)

##3.2

first_DM<- first_events(db,tab="Clinical001", columns=c("patid", "eventdate", "medcode"),
                        where="medcode %in% .(Parkinsoncodes)")
last_DM<- last_events(db,tab="Clinical001", columns=c("patid", "eventdate", "medcode"),
                      where="medcode %in% .(Parkinsoncodes)")
head(first_DM)
head(last_DM)


##3.3.1
registered_patients<- select_by_year(db=db, tables="Patient001",
                                     columns=c("patid", "practid", "gender", "yob", "crd", "tod","deathdate"),
                                     where="crd < STARTDATE", year_range=c(1900 : 2012),year_fn =standard_years, first_events)

str(registered_patients)

table(registered_patients$year)

incident_cases<- select_by_year(db=db, tables=c("Clinical001", "Referral001"),
                                columns=c("patid", "eventdate", "medcode"),
                                where="medcode %in% .(Parkinsoncodes) & eventdate <= ENDDATE",
                                year_range=c(1900: 2012), year_fn=standard_years, selector_fn =first_events)
str(incident_cases)
table(incident_cases$year)

## All patientsare kept (equivalentto merge(all.x= TRUE))
prevalence_dat<- left_join(registered_patients, incident_cases)
## Removeduplicatesacrossclinicaland referraltables:
incident_cases%>% group_by(patid,year) %>% arrange(eventdate)%>% distinct() %>% ungroup-> incident_cases

prevalence_dat<- prev_terms(prevalence_dat)
totals<- prev_totals(prevalence_dat)
#Prevalence and incidence of Parkinson from 1900 to 2012
totals$prevalence$year_counts
totals$incidence$year_counts

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
