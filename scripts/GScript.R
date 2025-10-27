library(googlesheets4)
library(tidyverse)
library(magrittr)
library(lubridate)
library(httpuv)

#update to charging from attendance

#attendance_url<-"https://docs.google.com/spreadsheets/d/12PsCDKaGrkmgbdbzN0iFtNPCDD0-frYBvi4i-5Ez_5Y/edit?gid=31993042#gid=31993042"

charging_url<-"https://docs.google.com/spreadsheets/d/12PsCDKaGrkmgbdbzN0iFtNPCDD0-frYBvi4i-5Ez_5Y/edit?gid=843633111#gid=843633111"

session_url<-"https://docs.google.com/spreadsheets/d/12PsCDKaGrkmgbdbzN0iFtNPCDD0-frYBvi4i-5Ez_5Y/edit?gid=1558804232#gid=1558804232"

#gs4_auth()

#attendanceDat<-read_sheet(attendance_url, sheet="attendance")
chargingDat<-read_sheet(charging_url , sheet = "charging" , col_types = "cTcccccclcccccccccnnnnnnnnnnTccc")
sessionDat<-read_sheet(session_url, sheet="session_info")

#cleanAttendance<-attendanceDat
cleanCharging<-chargingDat
cleanSession<-sessionDat

#need to think more clearly about what dat I actually want here
# categorisation etc.
#need schools
#event name
#event duration
#categorisation - teaching, exam running, hosting - practical, lectures, exams, 



# cleanAttendance ---------------------------------------------------------


#Clean Variables

#programme
cleanCharging %<>%
  mutate(programme = case_when(
    programme == "BBSRC" ~ "BBSRC DTP",
    programme == "NERC" ~ "NERC DTP",
    programme == "MRC-U" ~ "MRC DTP",
    programme == "MRC" ~ "MRC DTP",
    programme == "SBS MPhil A" ~ "SBS MPhil",
    programme == "SBS MPhil B" ~ "SBS MPhil",
    programme == "BTN" ~ "BTN MPhil",
    programme == "CATS" ~ "CATS MPhil",
    programme == "CSCI" ~ "Stem Cell MPhil",
    programme == "SCM MPhil (CBU)" ~ "Cog Neuro MPhil",
    programme == "Dev Biol" ~ "Dev Biol MPhil",
    programme == "Pharmacology PG" ~ "Pharm PG",
    programme == "Pharmacology UG" ~ "Pharm UG",
    event_id == "20230417_GATK" ~ "Open",
    event_id == "20230918_StatsSchool" ~ "Open",
    event_id == "20240415_Analysing_own_data_main" ~ "Open",
    event_id == "20240425_Analysing_own_data_extra" ~ "Open",
    .default = programme
  ))


#charge_status
cleanCharging %<>%
  mutate(charge_status = case_when(
    charge_status == "internal" ~ "academic - Cambridge",
    charge_status == "external" ~ "academic - External",
    .default = charge_status
  ))

#event_title
event_name_lookup<-read_csv("data_raw/251027_event_title.csv")
nName<-length(event_name_lookup$old_name)
for(iName in 1:nName){
  cleanCharging %<>%
    mutate(event_title = case_when(
      event_title == event_name_lookup$old_name[iName] ~ event_name_lookup$new_name[iName],
      .default = event_title
    ))
}


#remove all cancelled bookings
cleanCharging %<>%
  filter(paid_status != "cancelled")


#Make New Variables


#make Course_type Column
#default values are open, cohort or special event
cleanCharging %<>%
  mutate(course_type = case_when(
    programme == "Open" ~ "open",
    programme == "Special event" ~ "special event",
    .default = "cohort"))



#make Session_type Column
#default values are teaching, exam or clinic
cleanCharging %<>%
  mutate(session_type = case_when(
    event_title == "Core statistics exam" ~ "exam",
    event_title == "Data analysis in R clinic" ~ "clinic",
    event_title == "Data analysis in Python clinic" ~ "clinic",
    event_title == "Core statistics exam clinic" ~ "clinic",
    .default = "teaching"
  ))



#Fix individual records
cleanCharging %<>%
  mutate(attended = case_when(
    crsid == "lw708" & event_id == "20240301_ProgML" ~ TRUE,
    .default = attended
  ))


attendance_fix<-read_csv("data_raw/251027_Attendance_Fix.csv")
nAttend<-length(attendance_fix$event_id)
for(iAttend in 1:nAttend){
  cleanCharging %<>%
    mutate(attended = case_when(
      event_id == attendance_fix$event_id[iAttend] &  (participant == attendance_fix$participant[iAttend] | crsid == attendance_fix$crsid[iAttend]) ~ attendance_fix$attended[iAttend],
      .default = attended
    ))
}



# cleanEvent ------------------------------------------------------------

#Create an Event dataframe that aggregates individual sessions
#accurate calculate session hours using start and end times
cleanEvent<-cleanSession %>%
  mutate(duration_hrs = round((hour(cleanSession$end_time) + minute(cleanSession$end_time)/60 - hour(cleanSession$start_time) - minute(cleanSession$start_time)/60)/0.25)*0.25) %>%
  group_by(event_id) %>%
  summarise(hours=sum(duration_hrs , na.rm = TRUE) , 
            slots=sum(slot_duration , na.rm=TRUE) , 
            ay=first(ay) , 
            event_date=first(event_date) , 
            category=first(category) , 
            programme = first(programme) , 
            event_title=first(event_title_internal))


#make Course_type Column
#default values are open, cohort or special event
cleanEvent %<>%
  mutate(course_type = case_when(
    programme == "Open" ~ "open",
    programme == "Special event" ~ "special event",
    .default = "cohort"))

#make Session_type Column
#default values are teaching, exam or clinic
cleanEvent %<>%
  mutate(session_type = case_when(
    event_title == "Dev Biol stats exam" ~ "exam",
    event_title == "SBS MPhil stats exam (IBaMI)" ~ "exam",
    event_title == "SBS MPhil stats exam (SSD IBaMI)" ~ "exam",
    event_title == "SCM MPhil stats exam (SSD)" ~ "exam",
    event_title == "BTN MPhil Core Assessment Exam" ~ "exam",
    event_title == "SBS MPhil stats exam (5 x pathways)" ~ "exam",
    event_title == "SBS MPhil stats exam (SSD)" ~ "exam",
    event_title == "SCM MPhil stats exam (main)" ~ "exam",
    event_title == "Helpdesk" ~ "clinic",
    event_title == "Drop-in session" ~ "clinic",
    event_title == "Revision session" ~ "clinic",
    .default = "teaching"
  ))


# Filter Data -------------------------------------------------------------

#filter data
yearSel <- c("AY2223" , "AY2324" , "AY2425")
#monthSel <- c(9,10,11,12,1,2,3) ; myperiod="Sep - Mar"
monthSel <- 1:12 ; myperiod="Full Year"
#monthSel <- c(3,4,5,6,7,8) ; myperiod="Mar - Aug" %>%

filterCharging<-cleanCharging %>%
  filter(ay %in% yearSel) %>%
  filter(month(event_date) %in% monthSel)



filterEvent<-cleanEvent %>%
  filter(ay %in% yearSel) %>%
  filter(month(event_date) %in% monthSel)


# Exploration -------------------------------------------------------------



# Visuals ---------------------------------------------------------


# Bookings ----------------------------------------------------------
## Total
filterCharging %>%
  filter(session_type == "teaching") %>%
  group_by(ay) %>%
  summarise(Bookings=n()) %>%
  mutate(Change = floor((Bookings/first(Bookings) - 1 )*100)) %>%
  ggplot(aes(x=ay , y=Bookings , label=paste("atop(" , Bookings , "," , "paste(",Change,",'%')" , ")"))) + 
  geom_bar(stat="identity" , fill="seagreen") +
  geom_text(parse=TRUE , position = position_stack(vjust=0.5)) +
  labs(
    title = paste0("Total Bookings: ",myperiod),
    y = "Bookings",
    x = "Academic Year"
  )



## Cohort vs Open
filterCharging %>%
  filter(session_type == "teaching") %>%
  mutate(course_type=factor(course_type)) %>%
  group_by(ay , course_type , .drop = FALSE) %>%
  summarise(Bookings=n()) %>%
  ungroup() %>%
  group_by(course_type) %>%
  arrange(course_type, ay) %>%
  mutate(Change = floor((Bookings/first(Bookings) - 1 )*100)) %>%
  ggplot(aes(x=course_type , y=Bookings , fill=ay , label=paste("atop(" , Bookings , "," , "paste(",Change,",'%')" , ")"))) +
  geom_bar(stat="identity" , position = "dodge") + 
  geom_text(parse = TRUE , position = position_dodge(width=0.9)) +
  labs(
    title = paste0("Total Bookings: ",myperiod),
    y = "Bookings",
    fill = "Academic Year",
    x = "Course Type"
  )


## Participant Type
filterCharging %>%
  filter(session_type == "teaching") %>%
  group_by(ay , charge_status) %>%
  summarise(Bookings=n()) %>%
  ungroup() %>%
  group_by(charge_status) %>%
  arrange(ay , charge_status) %>%
  mutate(Change = floor((Bookings/first(Bookings) - 1 )*100)) %>%
  ggplot(aes(x=charge_status , y=Bookings , fill=ay , label=paste("atop(" , Bookings , "," , "paste(",Change,",'%')" , ")"))) +
  geom_bar(stat="identity" , position = "dodge") + 
  geom_text(parse = TRUE , position = position_dodge(width=0.9)) + 
  labs(
    title = paste0("Total Bookings: ",myperiod),
    y = "Bookings",
    fill = "Academic Year",
    x = "Participant Type"
  )





# Attendees ---------------------------------------------------------------
#Total
filterCharging %>%
  filter(attended == TRUE) %>%
  filter(session_type == "teaching") %>%
  group_by(ay) %>%
  summarise(Attendees=sum(attended == TRUE)) %>%
  mutate(Change = floor((Attendees/first(Attendees) - 1 )*100)) %>%
  ggplot(aes(x=ay , y=Attendees , label=paste("atop(" , Attendees , "," , "paste(",Change,",'%')" , ")"))) + 
  geom_bar(stat="identity" , fill="skyblue3") + 
  geom_text(parse=TRUE , position = position_stack(vjust=0.5)) +
  labs(
    title = paste0("Total Attendees: ",myperiod),
    y = "Attendees",
    x = "Academic Year"
  )


## Cohort vs Open
filterCharging %>%
  filter(attended = TRUE) %>%
  filter(session_type == "teaching") %>%
  mutate(course_type=factor(course_type)) %>%
  group_by(ay , course_type , .drop = FALSE) %>%
  summarise(Attendees=n()) %>%
  ungroup() %>%
  group_by(course_type , .drop = FALSE) %>%
  arrange(course_type, ay) %>%
  mutate(Change = floor((Attendees/first(Attendees) - 1 )*100)) %>%
  ggplot(aes(x=course_type , y=Attendees , fill=ay , label=paste("atop(" , Attendees , "," , "paste(",Change,",'%')" , ")"))) +
  geom_bar(stat="identity" , position = "dodge") + 
  geom_text(parse = TRUE , position = position_dodge(width=0.9)) +
  labs(
    title = paste0("Total Attendees: ",myperiod),
    y = "Attendees",
    fill = "Academic Year",
    x = "Course Type"
  )


## Participant Type
filterCharging %>%
  filter(attended == TRUE) %>%
  filter(session_type == "teaching") %>%
  group_by(ay , charge_status) %>%
  summarise(Attendees=n()) %>%
  ungroup() %>%
  group_by(charge_status) %>%
  arrange(charge_status, ay) %>%
  mutate(Change = floor((Attendees/first(Attendees) - 1 )*100)) %>%
  ggplot(aes(x=charge_status , y=Attendees , fill=ay , label=paste("atop(" , Attendees , "," , "paste(",Change,",'%')" , ")"))) +
  geom_bar(stat="identity" , position = "dodge") + 
  geom_text(parse = TRUE , position = position_dodge(width=0.9)) + 
  labs(
    title = paste0("Total Attendees: ",myperiod),
    y = "Attendees",
    fill = "Academic Year",
    x = "Participant Type"
  )





# Attendance Rates --------------------------------------------------------
## Overall 
filterCharging %>%
  filter(session_type == "teaching") %>%
  group_by(ay) %>%
  summarise(Bookings=n() , Attendees=sum(attended == TRUE) , Attendance = Attendees/Bookings) %>%
  ggplot(aes(x=ay , y=Attendance , label=paste( "paste(" , round(Attendance*100,0) , ",'%')"))) + 
  geom_bar(stat="identity" , fill="wheat") + 
  geom_text(parse=TRUE , position = position_stack(vjust=0.5)) +
  ylim(0,1) +
  labs(
    title = paste0("Overall Attendance Rate: ",myperiod),
    y = "Attendance Rate",
    x = "Academic Year"
  )
  


## Attendance by year and course type
filterCharging %>%
  filter(session_type == "teaching") %>%
  mutate(ay=factor(ay)) %>%
  mutate(course_type=factor(course_type)) %>%
  group_by(ay,course_type, .drop=FALSE) %>%
  summarise(Bookings=n() , Attendees=sum(attended == TRUE) , Attendance = Attendees/Bookings) %>%
  ggplot(aes(fill=ay , y=Attendance , x=course_type , label=paste( "atop(paste(" , round(Attendance*100,0) , ",'%'),)"))) +
  geom_bar(stat="identity" , position="dodge") +
  geom_text(parse = TRUE , position = position_dodge(width=0.9)) +
  scale_x_discrete() +
  ylim(0,1) +
  labs(
    title = paste0("Attendance Rates: ",myperiod),
    y = "Attendance Rate",
    fill = "Academic Year",
    x = "Course Type"
  )

## Attendance by year and cohort
filterCharging %>%
  filter(session_type == "teaching") %>%
  mutate(ay=factor(ay)) %>%
  mutate(programme=factor(programme)) %>%
  group_by(ay,programme, .drop=FALSE) %>%
  summarise(Bookings=n() , Attendees=sum(attended == TRUE) , Attendance = Attendees/Bookings) %>%
  ggplot(aes(y=Attendance , x=ay , fill=ay , label=paste( "atop(, paste(" , round(Attendance*100,0) , ",'%'))"))) +
  geom_bar(stat="identity") +
  geom_text(parse = TRUE , position = position_dodge(width=0.9)) +
  facet_wrap(vars(programme)) +
  labs(
    title = paste0("Attendance Rates: ",myperiod),
    y = "Attendance Rate",
    fill = "Academic Year",
    x = "Academic Year"
  )
  

cleanCharging %>% filter(programme == "SCM MPhil") %>%
  group_by(ay) %>%
  summarise(n())

# Hours -------------------------------------------------------------------

filterEvent %>%
  filter(category == "cohort" | category == "open") %>%
  group_by(ay) %>%
  summarise(total = sum(slots)*8) %>%
  mutate(Change = floor((total/first(total) - 1 )*100)) %>%
  ggplot(aes(x=ay , y=total , label=paste("atop(" , total , "," , "paste(",Change,",'%')" , ")"))) +
  geom_bar(stat = "identity" , fill = "seagreen") +
  geom_text(parse=TRUE , position = position_stack(vjust=0.5)) +
  labs(
    title = paste0("Total Hours: ",myperiod),
    y = "Hours",
    x = "Academic Year"
  )

filterEvent %>%
  filter(category == "cohort" | category == "open") %>%
  group_by(ay) %>%
  summarise(total = sum(hours)) %>%
  mutate(Change = floor((total/first(total) - 1 )*100)) %>%
  ggplot(aes(x=ay , y=total , label=paste("atop(" , total , "," , "paste(",Change,",'%')" , ")"))) +
  geom_bar(stat = "identity" , fill = "seagreen") +
  geom_text(parse=TRUE , position = position_stack(vjust=0.5)) +
  labs(
    title = paste0("Total Hours: ",myperiod),
    y = "Hours",
    x = "Academic Year"
  )


filterEvent %>%
  filter(category == "cohort" | category == "open") %>%
  distinct(event_id , .keep_all = TRUE) %>%
  group_by(ay , category) %>%
  summarise(n())
  
filterEvent %>%
  filter(category == "cohort" | category == "open") %>%
  distinct(event_id , .keep_all = TRUE) %>%
  group_by(ay) %>%
  view()



