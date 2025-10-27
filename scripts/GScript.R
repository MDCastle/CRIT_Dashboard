library(googlesheets4)
library(tidyverse)
library(magrittr)
library(lubridate)
library(httpuv)

#update to charging

attendance_url<-"https://docs.google.com/spreadsheets/d/12PsCDKaGrkmgbdbzN0iFtNPCDD0-frYBvi4i-5Ez_5Y/edit?gid=31993042#gid=31993042"

session_url<-"https://docs.google.com/spreadsheets/d/12PsCDKaGrkmgbdbzN0iFtNPCDD0-frYBvi4i-5Ez_5Y/edit?gid=1558804232#gid=1558804232"

#gs4_auth()

attendanceDat<-read_sheet(attendance_url, sheet="attendance")
sessionDat<-read_sheet(session_url, sheet="session_info")

cleanAttendance<-attendanceDat
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
cleanAttendance %<>%
  mutate(programme = case_when(
    programme == "BBSRC" ~ "BBSRC DTP",
    programme == "NERC" ~ "NERC DTP",
    programme == "MRC-U" ~ "MRC DTP",
    programme == "MRC" ~ "MRC DTP",
    programme == "SBS MPhil A" ~ "SBS MPhil",
    programme == "SBS MPhil B" ~ "SBS MPhil",
    programme == "BTN" ~ "BTN MPhil",
    programme == "CATS" ~ "CATS MPhil",
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
cleanAttendance %<>%
  mutate(charge_status = case_when(
    charge_status == "internal" ~ "academic - Cambridge",
    charge_status == "external" ~ "academic - External",
    .default = charge_status
  ))

#Clean Naming for event_title
cleanAttendance %<>%
  mutate(event_title = case_when(
    event_title == "Advanced statistics in Python" ~ "Advanced statistics",
    event_title == "Advanced statistics in R" ~ "Advanced statistics",
    event_title == "An Introduction to Machine Learning" ~ "Introduction to Machine Learning",
    event_title == "Analysis of expression proteomics data in R" ~ "Expression Proteomics analysis in R",
    event_title == "BTN MPhil Core Assessment Exam" ~ "Core Statistics Exam",
    event_title == "SBS MPhil stats exam (5 x pathways)" ~ "Core Statistics Exam",
    event_title == "SBS MPhil stats exam (IBaMI)" ~ "Core Statistics Exam",
    event_title == "SBS MPhil stats exam (SSD IBaMI)" ~ "Core Statistics Exam",
    event_title == "SBS MPhil stats exam (SSD)" ~ "Core Statistics Exam",
    event_title == "SCM MPhil stats exam (main)" ~ "Core Statistics Exam",
    event_title == "SCM MPhil stats exam (SSD)" ~ "Core Statistics Exam",
    event_title == "Dev Biol stats exam" ~ "Core Statistics Exam",
    event_title == "Core statistics in Python" ~ "Core Statistics",
    event_title == "Core statistics in R" ~ "Core Statistics",
    event_title == "Core statistics in R or Python" ~ "Core Statistics",
    event_title == "Experience Postgraduate Life Sciences" ~ "Using R and effectively communicating data",
    event_title == "Expression Proteomics analysis in R" ~ "Expression proteomics analysis in R",
    event_title == "Fundamentals of stats/AI link" ~ "Fundamentals of Statistics",
    event_title == "Intro to R drop-in" ~ "Data Analysis in R clinic",
    event_title == "Introducing experimental design" ~ "Experimental design for statistical analysis",
    event_title == "Introducing generalised linear models" ~ "Generalised linear models",
    event_title == "Introducing linear mixed effect models" ~ "Linear mixed effects models",
    event_title == "Introducing visual data communication" ~ "Visual data communication",
    event_title == "Introduction to Metabolomics" ~ "Metabolomics data analysis",
    event_title == "Introduction to Metagenomics" ~ "Metagenomics data analysis",
    event_title == "Introduction to Python" ~ "Data Analysis in Python",
    event_title == "Introduction to Python drop-in session" ~ "Data Analysis in Python clinic",
    event_title == "Introduction to R" ~ "Data Analysis in R",
    event_title == "Introduction to R drop-in session" ~ "Data Analysis in R clinic",
    event_title == "Introduction to Statistical Analysis" ~ "Fundamentals of Statistics",
    event_title == "Reproducible Research and Experimental Design" ~ "Reproducible research in R",
    event_title == "Reproducible Research in R" ~ "Reproducible research in R",
    event_title == "SBS MPhil post-exam Q&A" ~ "Core Statistics Exam clinic",
    event_title == "Working with Bacterial Genomes" ~ "Working with bacterial genomes",
    .default = event_title
  ))
    


#Make New Variables

#make Course_type Column
#default values are open, cohort or special event
cleanAttendance %<>%
  mutate(course_type = case_when(
    programme == "Open" ~ "open",
    programme == "Special event" ~ "special event",
    .default = "cohort"))

#make Session_type Column
#default values are teaching, exam or clinic
cleanAttendance %<>%
  mutate(session_type = case_when(
    event_title == "Core Statistics exam" ~ "exam",
    event_title == "Data Analysis in R clinic" ~ "clinic",
    event_title == "Data Analysis in Python clinic" ~ "clinic",
    event_title == "Core Statistics Exam clinic" ~ "clinic",
    .default = "teaching"
  ))



#Fix Individual records


#Remove Jonathan Aaron (no other information apart from name)
cleanAttendance %<>% filter(participant != "Jonathan Aaron" | event_id != "20221014_IntroR")

#Remove S. Camara (on waiting list but absent so sholdn't be on here at all)
cleanAttendance %<>% filter (booking_status != "waiting" | attn_status != "absent")

#fix booking status for Saur Hajiev, E.C. Harding
cleanAttendance %<>% 
  mutate(booking_status = case_when(
    participant == "Saur Hajiev" & event_id== "20240617_bioinfo-unix2" ~ "booked",
    participant == "Dr E.C. Harding" & event_id== "20220912_IntroPython" ~ "booked",
    participant == "Aisha Al Amri" & event_id== "20230112_UNIX" ~ "not booked",
    booking_status == "offered" ~ "booked",
    booking_status == "provisional" ~ "booked",
    .default = booking_status
  ))

#fix charge_status
cleanAttendance %<>%
  mutate(charge_status = case_when(
    event_id == "20230918_StatsSchool" & participant == "Elisabeth Murphy" ~ "academic - External",
    event_id == "20230918_StatsSchool" & participant == "Katherine Symons" ~ "academic - Cambridge",
    event_id == "20230918_StatsSchool" & participant == "Max Koko" ~ "student",
    event_id == "20230918_StatsSchool" & participant == "Muruj Ishaq Tukruni" ~ "academic - External",
    charge_status == "no_charge" & participant == "Laura Millett" ~ "academic - External",
    charge_status == "no_charge" & participant == "Laura Mincarelli" ~ "academic - External",
    .default = charge_status
  ))





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

filterAttendance<-cleanAttendance %>%
  filter(ay %in% yearSel) %>%
  filter(month(event_date) %in% monthSel) #%>%
  #filter(primary_inst_school == "School of Clinical Medicine")


filterEvent<-cleanEvent %>%
  filter(ay %in% yearSel) %>%
  filter(month(event_date) %in% monthSel)


# Exploration -------------------------------------------------------------


cleanAttendance %>%
  group_by(course_type) %>%
  summarise(n())


cleanAttendance %>% filter(is.na(charge_status)) %>% view()
# Visuals ---------------------------------------------------------


# Bookings ----------------------------------------------------------
## Total
filterAttendance %>%
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
filterAttendance %>%
  filter(session_type == "teaching") %>%
  group_by(ay , course_type) %>%
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
filterAttendance %>%
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
filterAttendance %>%
  filter(session_type == "teaching") %>%
  group_by(ay) %>%
  summarise(Attendees=sum(attn_status=="attended" | attn_status=="async", na.rm=TRUE)) %>%
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
filterAttendance %>%
  filter(attn_status == "attended" | attn_status == "async") %>%
  filter(course_type != "Special event") %>%
  filter(session_type == "teaching") %>%
  group_by(ay , course_type) %>%
  summarise(Attendees=n()) %>%
  ungroup() %>%
  group_by(course_type) %>%
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
filterAttendance %>%
  filter(attn_status == "attended" | attn_status == "async") %>%
  filter(course_type != "Special event") %>%
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
filterAttendance %>%
  filter(booking_status == "booked" | booking_status == "booked late" | booking_status == "joined" | attn_status=="attended" | attn_status=="async") %>%
  filter(session_type == "teaching") %>%
  group_by(ay) %>%
  summarise(Bookings=n() , Attendees=sum(attn_status=="attended" | attn_status=="async" , na.rm=TRUE) , Attendance = Attendees/Bookings) %>%
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
filterAttendance %>%
  filter(booking_status == "booked" | booking_status == "booked late" | booking_status == "joined" | attn_status=="attended" | attn_status=="async") %>%
  filter(session_type == "teaching") %>%
  filter(course_type != "Special event") %>%
  mutate(ay=factor(ay)) %>%
  mutate(course_type=factor(course_type)) %>%
  group_by(ay,course_type, .drop=FALSE) %>%
  summarise(Bookings=n() , Attendees=sum(attn_status=="attended" | attn_status=="async" , na.rm=TRUE) , Attendance = Attendees/Bookings) %>%
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
filterAttendance %>%
  filter(booking_status == "booked" | booking_status == "booked late" | booking_status == "joined" | attn_status=="attended" | attn_status=="async") %>%
  filter(session_type == "teaching") %>%
  filter(course_type != "Special event") %>%
  mutate(ay=factor(ay)) %>%
  mutate(programme=factor(programme)) %>%
  group_by(ay,programme, .drop=FALSE) %>%
  summarise(Bookings=n() , Attendees=sum(attn_status=="attended" | attn_status=="async" , na.rm=TRUE) , Attendance = Attendees/Bookings) %>%
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



