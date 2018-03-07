#Real PD Confirmation

SelParkinsoncodes = tbl_df(select_events(db,tab="Clinical_All", columns = '*',
                                         where = "medcode %in% .(Parkinsoncodes)"))
SelParkinsonprodcodes <- tbl_df(select_events(db, tab= "Therapy_All", columns = c("patid", "eventdate", "prodcode"),
                                              where = "prodcode %in% .(PD_prodcodes$prodcode)"))

nr_of_PDcodes_PP <- tbl_df(sqldf("SELECT patid, medcode, COUNT(DISTINCT medcode) AS nr_of_PDcodes FROM SelParkinsoncodes GROUP BY patid ORDER BY nr_of_PDcodes DESC"))
nr_of_PDcodes_PP %>% ungroup
Check_Patients <- tbl_df(sqldf("SELECT *FROM nr_of_PDcodes_PP WHERE nr_of_PDcodes < 2")) #medcode != 4321 AND


Check_Patients_Meds <- tbl_df(select_events(db, tab = "Therapy_All", columns =  c("patid", "eventdate", "prodcode"),
                                           where = "patid %in% nr_of_PDcodes_PP$patid & prodcode %in% PD_prodcodes$cprd_prodcode"))


#Very slow but works!
Check_Patients_Meds <- sqldf("SELECT patid, eventdate, prodcode FROM Therapy_all", connection = db) %>%
  group_by(patid, prodcode) %>% filter(prodcode %in% PD_prodcodes$prodcode & patid %in% nr_of_PDcodes_PP$patid) %>% distinct() %>% ungroup -> Check_Patients_Meds
def_not_PD <-  tbl_df(sqldf("SELECT DISTINCT patid FROM Clinical_All WHERE medcode IN (19478, 24001, 26181, 33544, 97170, 100128)", connection =db))
Not_real_PD <- tbl_df(sqldf("SELECT patid, medcode FROM nr_of_PDcodes_PP WHERE patid NOT IN (SELECT patid FROM def_not_PD)"))
Not_real_PD <- tbl_df(sqldf("SELECT patid, medcode FROM Not_real_PD WHERE medcode NOT IN (4321, 101090, 96860, 9509)"))
Not_real_PD <- tbl_df(sqldf("SELECT patid, medcode FROM Not_real_PD WHERE (SELECT nr_of_PDcodes FROM nr_of_PDcodes_PP WHERE nr_of_PDCodes ==1) AND patid NOT IN (SELECT patid FROM Check_Patients_Meds)")) 
          # select from all PD patients the ones that..
          # ..have 1 of the PD exclusioncodes OR  
          # ..don't have one of the PD definite PD diagnoses 
          # ..have only 1 PD code AND  
          # ..don't have a patid in received parkinson medication OR 
          
          
          
Check_PD <- tbl_df(select_events(db, tab = "Clinical_All", columns = '*', 
                                 where = "patid %in% .(Not_real_PD$patid)"))
Real_PD <- tbl_df(sqldf("SELECT patid, medcode FROM nr_of_PDcodes_PP WHERE patid NOT IN (SELECT patid FROM Not_real_PD)"))
PD_AND_NOT_PDcode <- tbl_df(unique(select_events(db, tab = "Clinical_All", columns = "patid", where = "patid %in% .(def_not_PD$patid) & patid %in% .(SelParkinsoncodes$patid)")))

  
Check_Patients_Meds <- tbl_df(sqldf("SELECT patid, eventdate prodcode FROM Therapy_All GROUP BY 
                                    patid WHERE prodcode IN (SELECT prodcode FROM PD_prodcodes)", connection =db))
# or more generally SELECT * FROM table INNER JOIN otherTable ON ( table.x = otherTable.a AND table.y = otherTable.b) 








All_prodcodes <- tbl_df(select_events(db,tab="Drugproductreadcodes", columns = c("prodcode", "productname", "drugsubstance", "strength", "formulation")))

Check_Patients_Meds <- left_join(Check_Patients_Meds, All_prodcodes) 
#Select the information on the medication they received on the day they were diagnosed as PD. That should likely be anti-PD medication...



