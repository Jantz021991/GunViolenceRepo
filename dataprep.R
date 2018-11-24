# compile data from gunviolence archive, ping google api to get lat lon
if (!require('plyr')) install.packages('plyr')
if (!require('dplyr')) install.packages('dplyr')
if (!require('ggmap')) install.packages('ggmap')

DataPrep <- function(all) {
  det <-fread("gun-violence-data_01-2013_03-2018.csv")%>%
    filter(date >= "2014-01-01")
  
  all <- det %>% 
    select("date","state","n_killed","n_injured")%>%
    rename(Date = date,Address=state,Killed=n_killed,Injured =n_injured)%>%
    filter(!is.na(Address)| Address==" ")
  
  all$Date <- as.Date(all$Date)
  all <- all%>%
    group_by(Date,Address)%>%
    summarise(Killed=sum(Killed),
              Injured=sum(Injured))
  
  
  state<- read.csv("statelatlong.csv")
  state <- state%>%
    rename(Address=City,lat=Latitude,lon=Longitude)
  all <- merge(all,state,by="Address")
  all$Content <- paste("</b>Date:</b>",all$Date,"<br/>",
                       "</b>Killed:</b>",all$Killed,"<br/>",
                       "</b>Injured:</b>",all$Injured,"<br/>",
                       "</b>Location:</b>",all$Address,"<br/>")
df <-  all
df
}

saveRDS(all, "Data/GunViolence.rds")

