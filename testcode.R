test <- Institutions2016 %>%
  select(INSTNM,CLOSEDAT) %>%
  filter(!str_detect(CLOSEDAT,"/"))#%>%  droplevels()


open_Institutions2016 <- anti_join(Institutions2016,closed_institutions_2016, by = "UNITID")
  