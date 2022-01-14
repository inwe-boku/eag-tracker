library(tidyverse)
library(lubridate)
library(tidyquant)
library(zoo)
library(twitteR)
library(feather)


COLORS3<-c("#c72321",
           "#0d8085",
           "#efc220")

FILENAME_FIGURE <- "figures/eag-tracker.png"

get_e_control_data<-function(){
  
  download.file("https://www.e-control.at/documents/1785851/8165594/el_dataset_mn.csv",
                destfile="data/production-e-control.csv")
  
  production<-read_delim("data/production-e-control.csv",delim=";")
  technology<-production[5,] %>% unlist()
  fuel<-production[6,] %>% unlist()
  names<-paste0(technology,fuel)
  
  names(production)<-names
  production<-production[c(14:nrow(production)),][c("Hydr_",
                                                    "Gliederung1Gliederung2",
                                                    "KalBioFe",
                                                    #"KalBioFl",
                                                    "KalBioG",
                                                    #"KalSoBio",
                                                    #"KalKDG",
                                                    "WindWind",
                                                    "PV_")]
  
  
  
  production<-production %>% 
    gather(Variable,Value,-Gliederung1Gliederung2) %>% 
    dplyr::select(Date=Gliederung1Gliederung2,Variable,Value) %>% 
    mutate(Technology=ifelse(startsWith(Variable,"Kal"),"Biomasse",Variable)) %>% 
    mutate(Value=as.numeric(str_replace(Value,",","."))) %>% 
    mutate(Date=ymd(Date)) %>% 
    group_by(Date,Technology) %>% 
    summarize(Value=sum(Value)) %>% 
    mutate(Technology=ifelse(Technology=="Hydr_","Wasserkraft",Technology)) %>% 
    mutate(Technology=ifelse(Technology=="PV_","PV",Technology)) %>% 
    mutate(Technology=ifelse(Technology=="WindWind","Wind",Technology)) 
  
  
  production_annual<-production %>% 
    group_by(Year=year(Date),Technology) %>% 
    summarize(Value=sum(Value,na.rm=TRUE))
  
  production_2020 <- production_annual %>% 
    filter(Year==2020) %>% 
    mutate(Value=Value/10^6)
  
  additional_production<-tibble(Technology=c("Biomasse","Wasserkraft","PV","Wind"),
                                Target_Add=c(1,5,11,10))  
  
  
  timeseries_target<-tibble(
    Date=rep(seq(as.POSIXct("2020-01-01"),as.POSIXct("2030-12-01"),by="month"),each=4),
    Technology=rep(c("Biomasse","Wasserkraft","PV","Wind"),11*12))  
  
  production_2020_2030<-full_join(production_2020,additional_production) %>% 
    mutate(Target=Value+Target_Add) 
  
  target<-bind_rows(production_2020_2030 %>% 
                      ungroup() %>%
                      mutate(Date=as.POSIXct("2020-01-01")) %>% 
                      dplyr::select(Date,Technology,Value),
                    production_2020_2030 %>% 
                      ungroup() %>% 
                      mutate(Date=as.POSIXct("2030-12-01")) %>% 
                      dplyr::select(Date,Technology,Value=Target)) %>% 
    full_join(timeseries_target) %>% 
    arrange(Date) %>% 
    group_by(Technology) %>% 
    mutate(Value=na.approx(Value,maxgap=11*12)) %>% 
    mutate(Type="Ziel")
  
  bind_rows(target,
            production %>% 
              ungroup() %>% 
              #filter(year(Date)>2018) %>% 
              group_by(Technology) %>% 
              mutate(Value=rollmean(12*Value,k=12,align="right",fill=NA)) %>% 
              #filter(year(Date)>2019) %>% 
              mutate(Type="Realisiert e-control") %>% 
              mutate(Value=Value/10^6)) %>% 
    return()

  
}

get_entso_e_data<-function(startyear=2015, startmonth=1){
  
  protocol <- "sftp"
  server <- "sftp-transparency.entsoe.eu/"
  source("data/passwd-entso-e")
  
  
  for(year in startyear:2022){
    for(month in startmonth:12){
      filename <- paste0(year,"_",sprintf("%02d", month),"_AggregatedGenerationPerType_16.1.B_C.csv")
      full_url <- paste0("TP_export/AggregatedGenerationPerType_16.1.B_C/",filename)
      dest_file <- paste0("data/entso-e/entso-e-generation-",year,"-",sprintf("%02d", month),".csv")  
      file_connection <- file(dest_file)
      print(paste0("Downloading: ",full_url))
      url <- paste0(protocol, "://", server, full_url)
      out <- tryCatch(
        
        {getURL(url=url,userpwd=userpwd) %>% 
          writeLines(file_connection)
        },
        error=function(cond){
          print("File not found")
          message(cond)
        }
      )
        
      close.connection(file_connection)
      
    }
  }
  
  entso_e_data<-NULL
  for(year in 2015:2021){
    for(month in 1:12){
      
      filename <- paste0("data/entso-e/entso-e-generation-",year,"-",sprintf("%02d", month),".csv")
      monthly_data<-read_delim(filename,delim="\t") %>% 
        group_by(year=year(DateTime),month=month(DateTime),AreaName,ProductionType) %>% 
        summarize(Value=sum(ActualGenerationOutput)/4)  %>% 
        filter(AreaName %in% c("AT CTY")) %>% 
        mutate(Date=as.POSIXct(paste0(year,"-",sprintf("%02d",month),"-01"))) %>% 
        dplyr::select(Date,Technology=ProductionType,Value) %>% 
        mutate(Type="Realisiert entso-e")
      entso_e_data<-bind_rows(entso_e_data,
                              monthly_data)
      
      
    }
  }
  
  entso_e_data_final <- entso_e_data %>% 
    ungroup() %>% 
    dplyr::select(Date,Technology,Value) %>% 
    mutate(Type="Realisiert entso-e") %>% 
    mutate(Technology=ifelse(startsWith(Technology, "Hydro"),"Wasserkraft",Technology)) %>% 
    mutate(Technology=ifelse(Technology=="Solar","PV",Technology)) %>% 
    mutate(Technology=ifelse(Technology=="Wind Onshore","Wind",Technology)) %>% 
    mutate(Technology=ifelse(Technology=="Biomass","Biomasse",Technology)) %>% 
    group_by(Date,Technology,Type) %>%
    summarize(Value=sum(Value)/10^6) %>% 
    filter(Technology %in% c("Wasserkraft", "PV", "Wind", "Biomasse")) 
    
  entso_e_data_final<-entso_e_data_final %>% 
    group_by(Technology) %>% 
    arrange(Date) %>% 
    mutate(Value=rollmean(12*Value,k=12,align="right",fill=NA)) 
    
  
  return(entso_e_data_final)
  
}


tweet_result<-function(){
  
  
  
  authentification <- feather("data/authentification")
  
  setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                      access_token = authentification$access_token[1],
                      consumer_secret = authentification$consumer_secret[1],
                      access_secret = authentification$access_secret[1])
  
  
  tweet1<-updateStatus(text = "EAG Tracker: Wie geht der Ausbau der Erneuerbaren in Österreich voran?",
                       mediaPath = FILENAME_FIGURE)
  
  tweet2<-updateStatus(text = "Die Grafik zeigt die erneuerbare Produktion in Österreich im Vergleich zu einem linearen Ausbaupfad konsistent mit dem EAG.",
                       inReplyTo=tweet1$id)
  
  tweet3<-updateStatus(text = "Dabei wird die Summe der Produktion in den jeweils letzten 12 Monaten dargestellt, es handelt sich also um Jahreswerte.",
                       inReplyTo=tweet2$id)
  
  tweet4<-updateStatus(text = "Die Wind- und Wasserkraftdaten werden rund 6-8 Wochen verspätet veröffentlicht, PV und Biomassedaten gibt es nur jährlich.",
                       inReplyTo=tweet3$id)
  
  tweet5<-updateStatus(text = "Der #RStats Source Code ist verfügbar auf https://github.com/inwe-boku/eag-tracker",
                       inReplyTo=tweet4$id)
  
}


