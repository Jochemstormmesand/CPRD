#File to find which patients have received IS medication for 
  # more than 3 months consecutive 
  # with at least 2 prescriptions

# THIS SIMPLY FINDS WHETHER THEY HAVE RECEIVED IT FOR LONG ENOUGH. NOT HOW LONG EXACTLY!!!!

    # Calculated with first NDD 
        ##cum_duration >= 90 AND cum_ndd0 >1
    # if no NDD, eventdate differences were used.
        ##BETWEEN1 > 1 AND BETWEEN1 < 8000 AND FIRST >= 90
    # looking at the values that were not initially included:
        ##* FROM alternative_duration WHERE patid NOT IN (SELECT patid FROM Inclusion_patid2
      # if value for BETWEEN1 is weirdly higher than 8000 (some error), work with a counter that measures consecutive eventdates for IS per patient per medication resets at 0s 
    # if they had 3 or more consecutive evendates, than it is likely that this will add up to 3 months
        ##SELECT DISTINCT(patid) FROM Exclusion_patid2 WHERE BETWEEN1 >= 8000 AND counter >= 3 
    # finally, it can be assumed that if the counter is larger than 3, they will have had enough IS
        ##DISTINCT(patid) FROM Exclusion_patid2 WHERE ndd = 0 AND counter >= 3)) 
####################

IMPORTANT: THESE CALCULATIONS ARE NOT MADE PER DRUG TYPE? 
NOW ALL DRUGS AT ONCE, AND IT DOESNT MAKE SENSE FOR FOR EXAMPLE INJECTIONS. SO injections will likely be filtered out of this. 


####################
  MedCalculation <- MedCalculation %>% 
    ungroup %>%
    mutate(Concentration = as.numeric(as.character(numextract(strength))),
           unit = letterextract(MedCalculation$strength),
           daily_dose = ndd * Concentration) %>%
    arrange(patid, prodcode, eventdate) %>%
    select(-textid, -issueseq, -practid) 
  #Maybe add issueseq, drug and Formulation when needed later strength...?
  #MedCalculation files filtered on NDD present or not. 
  
  #find duration on each med using qty and ndd
  #Calculate cumulative duration via a cumulative sum of ndd but without qty 
  #Grouped by patid, prodcode. The summation will restart each time ndd =0
  MedCalculation <- MedCalculation %>%
    mutate(medduration = qty/ndd,
           ndd0 = case_when(MedCalculation$ndd > 0 ~ 1,
                            TRUE ~ 0),
           try = c(0,diff(ndd0) ==0))%>% 
    group_by(patid, prodcode, idx = cumsum(try == 0L)) %>% 
    mutate(cum_duration = cumsum(medduration),
           cum_ndd0 = cumsum(ndd0)) %>% ungroup %>% 
    select(-idx, -ndd0, - try)
  ##################################  
  
  #first, SELECT unique patients that have cum_ndd0 > 1 AND cum_duration > 90 to match inclusion criteria. 
  Inclusion_patid1 <- MedCalculation %>% filter(cum_duration >= 90 & cum_ndd0 >2)%>% distinct(patid)
  
  #second, NO ndd, so alternative methods needed to calculate duration to find which patids to include. 
  #Correct. Only needs improvement to keep the counter going if the eventdate is the same as previous eventdate.
  #so something needs to change in the as.period statement to inlcude all of htese. 
  alternative_duration <- tbl_df(anti_join(MedCalculation, Inclusion_patid1, by = "patid")) %>% 
    select(-medduration, -cum_duration, -cum_ndd0) %>% ungroup()%>%
    group_by(patid, prodcode) %>%
    mutate(BETWEEN0=as.numeric(as.period(interval(lag(eventdate,1),eventdate)), unit = "days")) %>% 
    mutate(BETWEEN1=ifelse(is.na(BETWEEN0),0,BETWEEN0))%>% ungroup 
  alternative_duration$FIRST <- with(alternative_duration, ave(BETWEEN1, cumsum(BETWEEN1== 0), FUN = cumsum))
  alternative_duration$idx <- as.numeric(alternative_duration$BETWEEN1 > 0)
  alternative_duration <- alternative_duration %>% group_by(patid,prodcode, idx)
  alternative_duration$counter <- with(alternative_duration, ave(idx, cumsum(idx == 0), FUN = cumsum)) 
  alternative_duration <- alternative_duration %>% ungroup() %>% select(-BETWEEN0, -idx)
  
  

  #doesn't take gaps into account, so it could be that the 2 consecutive is not correct because of a long gap in between..
  Inclusion_patid2 <- alternative_duration %>% filter(BETWEEN1 > 2 & FIRST >= 90) %>% distinct(patid)
  Inclusion_patid3 <- MedCalculation %>% filter(Drug == "Infliximab"| Drug == "Rituximab" | Drug == "Etanercept"| Drug == "Alemtuzumab" | Drug == "Adalimumab")  %>% distinct(patid)
  
  #Perhaps also include the patients where ndd is known, but n >=3, between1<8000.. That would include a few more. alhtough there ndd is a little bit lower, it would often add up to a month of prescription.  
  All_Inclusion_patid <- full_join(Inclusion_patid1, Inclusion_patid2) %>% 
    full_join(., Inclusion_patid3) %>% distinct(patid)
  
  three_and_two <- tbl_df(semi_join(MedCalculation, All_Inclusion_patid, by = "patid"))
  three_and_two_marker <- three_and_two %>% distinct(patid) #select All patids of patients that are properly on IS
  Excluded <- tbl_df(anti_join(MedCalculation, All_Inclusion_patid, by = "patid")) %>% arrange(patid)%>% 
    select(-medduration, -cum_duration, -cum_ndd0)
  
  #################################
  # go to incidence script here
  #################################
now: 
  included = 74620 patients 72913
  excluded = 10085 patients 11792
####
length(unique(three_and_two$patid))
length(unique(Excluded$patid))




#TOTAL DURATION OF EACH PATIENT ON EACH DRUG 
#1x for alternative duration, then for medcalculation then joined
#better: Do this for each patient with their total amount of drugs taken: duration *amount

#of 46508 patients. the rest should be obtained with alternative duraiton
Medcalculation_summary <- MedCalculation %>% semi_join(three_and_two_marker)%>% semi_join(Inclusion_patid1)%>% filter(ndd > 0 &qty >0) %>% group_by(patid, prodcode) %>%
  summarise(total_duration = max(cum_duration),
            mean_dose = mean(daily_dose),
            total_dose = sum(daily_dose),
            total_meds = total_duration * mean_dose)
alternative_duration_summary <- alternative_duration %>% semi_join(three_and_two_marker)%>% group_by(patid, prodcode) %>%
  summarise(total_duration = max(FIRST),
            mean_dose = mean(Concentration*ndd[ndd!=0]),
            total_dose = sum(Concentration*ndd[ndd!=0]),
            total_meds = sum(total_duration*mean_dose))
Meds_duration_summary <- full_join(Medcalculation_summary, alternative_duration_summary)            
Meds_duration_summary <- Meds_duration_summary %>% mutate(category=cut(total_meds, breaks=c(-Inf, 1000, 10000, Inf), labels=c("low","middle","high"))) %>%
  group_by(category) %>% summarise(categories = n())






#calculate total duration. still find out how to NOT have overlap and make sure everything is correct...
Duration1 <- three_and_two %>% group_by(patid, prodcode) %>% filter(cum_duration == max(cum_duration[is.finite(cum_duration)])) %>% ungroup()
Duration2 <- alternative_duration %>% group_by(patid, prodcode) %>% filter(FIRST == max(FIRST[is.finite(FIRST)])) %>% ungroup()
Duration <- full_join(Duration1, Duration2)
sqldf("SELECT * FROM Duration2 WHERE patid IN (SELECT patid FROM Duration1)")
  






Drug_Duration <- function(drug, formulation){







# SELECT first two lines per drug
Firsttwo1 <- function(drugname){
  filter(MC_filtered, grepl(drugname, Drug) & grepl('Capsule|Tablet', Formulation)) %>%
  group_by(patid) %>%
  slice(c(1:2)) %>%
  ungroup()}

#calculate the difference between first two lines 
Firsttwo_diff <- function(drugname){
  filter(MC_filtered, grepl(drugname, Drug) & grepl('Capsule|Tablet', Formulation)) %>%
  group_by(patid) %>%
  summarize(timediff = difftime(eventdate[2], eventdate[1], units = "days"))  %>%
  ungroup()}

Firsttwo1("Ciclosporin")
Firsttwo_diff("Ciclosporin")

mean(, na.rm = TRUE)
sd(, na.rm = TRUE)

#####################################################

##
FIRST do this calculation for ndd. so find for which patid have a duration longer than 3 months consecutive with at least 2 consecutive prescriptions. 
make sure gap difference is smaller than 90 days. consecutive can be found by GAP.  once you know this, work with all the patids in the medcalculation dataframe that have not also a patid
in the newly calculated variable. 
##

three_months_and2 <- function(drugname){
  dropcols <- c("qty", "ndd", "Concentration", "unit", "medduration", "cum_duration")
  MC_filtered %>% select(-one_of(dropcols)) %>%
  filter(grepl(drugname, Drug) & grepl('Capsule|Tablet', Formulation)) %>%
    group_by(patid) %>%
    mutate(timediff = as.numeric(difftime(eventdate,lag(eventdate,1)))) %>%
    mutate(cumsum = cumsum(ifelse(is.na(timediff), 0, timediff)) + timediff*0)
  
  if cumsum > 90 & timediff that make up the cumsum value are smaller than 44
  print patid, prodcode, formulation
  else continue calculation
  } 


    for (i in cumsum) {
      if (i != 90){
        next
      }
      stop()
      mutate(valid = i)
    }
    ungroup()}
diff(eventdate)

