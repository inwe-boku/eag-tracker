library(tidyverse)
library(lubridate)
library(tidyquant)
library(zoo)
library(twitteR)
library(feather)
library(RCurl)
library(irenabpdata)

#irenabpdata::download_clean_save_irena()


COLORS3<-c("#c72321",
           "#0d8085",
           "#efc220")

FILENAME_FIGURE <- "figures/eag-tracker.png"

DATA_DIR<- "data/"
CACHE_AGP_DATA_FILE<-"data-apg.csv"
CACHE_AGP_DATA_PATH_FILE<-paste0(DATA_DIR,CACHE_AGP_DATA_FILE)

create_cache_agp<-function(year, type="realized"){
  
  #startdate<-as.POSIXct("2015-01-01")
  startdate<-as.POSIXct(paste0(year,"-01-01"))
  enddate<-as.POSIXct(paste0(year,"-12-31"))
  table<-get_apg_data(startdate, enddate, type)
  table %>% write_csv(filename_cache(year, type))
  
}

filename_cache<-function(year, type){
  
  return(paste0(DATA_DIR, year, "-", type, "-", CACHE_AGP_DATA_FILE))
  
}

filename_cache_all_years<-function(type){
  paste0(DATA_DIR, type, "-", CACHE_AGP_DATA_FILE)
  
}

create_full_cache_apg<-function(type = "realized"){
  years<-2015:2021
  for(year in years){
    print(year)
    filename_cache<-filename_cache(year, type) 
    if(!file.exists(filename_cache)){
      create_cache_agp(year, type)
    }
  }
  

  all_data<-NULL
  for(year in years){
    file<-filename_cache(year, type)
    table<-read_csv(file)
    all_data<-bind_rows(all_data,table)
  }
  
  all_data %>% write_csv(filename_cache_all_years(type))
  
  
  
}

download_and_clean_e_control_data<-function(){
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
  
    return(production)
}

production_annual_econtrol<-function(production){
  
  production_annual<-production %>% 
    group_by(Year=year(Date),Technology) %>% 
    summarize(Value=sum(Value,na.rm=TRUE))
  
  return(production_annual)
  

}

production_annual_bp<-function(){
  bp <- load_db(irenabpdata::get_bp_db_files()[1])
  bp %>% 
    filter(Year==2020) %>% 
    filter(Country=="Austria") %>% 
    filter(Variable %in% c("Solar Generation - TWh",
                           "Wind Generation - TWh",
                           "Hydro Generation - TWh",
                           "Geo Biomass Other - TWh"))
}

production_2020<-function(production_econtrol){
  
  econtrol_2020<-production_annual_econtrol(production_econtrol) %>% 
    filter(Year==2020) %>% 
    dplyr::select(Year,Technology,econtrol=Value)
  
  bp <- load_db(irenabpdata::get_bp_db_files()[1])
  generation_2020_joint<-bp %>% 
    filter(Year==2020) %>% 
    filter(Country=="Austria") %>% 
    filter(Variable %in% c("Solar Generation - TWh")) %>% 
    mutate(Technology="PV") %>%
    mutate(Value=Value*10^6) %>% 
    dplyr::select(Year,Technology,BP=Value) %>% 
    right_join(econtrol_2020) %>% 
    mutate(Value=ifelse(Technology=="PV",BP,econtrol)) %>% 
    dplyr::select(Year,Technology,Value) 
  
  return(generation_2020_joint)
  
  
}

get_bp_pv_monthly<-function(){
  bp <- load_db(irenabpdata::get_bp_db_files()[1])
  
  all_dates <- tibble(Country="Austria",Variable="PV",Unit="Terawatt-hours",Date=seq(as.POSIXct(paste0("2010-01-01")),as.POSIXct(paste0("2030-12-01")),by="month")) %>% 
    mutate(Year=year(Date))
  
  bp %>% 
    filter(Country=="Austria") %>% 
    filter(Variable %in% c("Solar Generation - TWh")) %>% 
    mutate(Variable="PV") %>% 
    filter(Year>2008) %>% 
    mutate(Date=as.POSIXct(paste0(Year,"-12-01"))) %>% 
    full_join(all_dates) %>% 
    arrange(Date) %>% 
    mutate(Value=na.approx(Value,maxgap=14)) %>%
    filter(Year>2009) %>% 
    return()
  
}

eag_tracker_econtrol<-function(){
  
  production<-download_and_clean_e_control_data()
  
  production_2020_value <- production_2020(production) %>% 
    mutate(Value=Value/10^6)
  
  additional_production<-tibble(Technology=c("Biomasse","Wasserkraft","PV","Wind"),
                                Target_Add=c(1,5,11,10))  
  
  
  timeseries_target<-tibble(
    Date=rep(seq(as.POSIXct("2020-01-01"),as.POSIXct("2030-12-01"),by="month"),each=4),
    Technology=rep(c("Biomasse","Wasserkraft","PV","Wind"),11*12))  
  
  production_2020_2030<-full_join(production_2020_value,additional_production) %>% 
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
  
  bp_pv_monthly<-get_bp_pv_monthly() %>% 
    dplyr::select(Date,Technology=Variable,Value) %>% 
    mutate(Type="BP World Review")
  
  bind_rows(target,
            production %>% 
              ungroup() %>% 
              #filter(year(Date)>2018) %>% 
              group_by(Technology) %>% 
              mutate(Value=rollmean(12*Value,k=12,align="right",fill=NA)) %>% 
              #filter(year(Date)>2019) %>% 
              mutate(Type="Realisiert") %>% 
              mutate(Value=Value/10^6),
            bp_pv_monthly) %>% 
    return()

  
}

get_entso_e_data<-function(startyear=2015, 
                           endyear=2022, 
                           startmonth=1, 
                           endmonth=12){

  INTERIM_FEATHER_FILE<-"data/entsoe-interim.feather"
  
  protocol <- "sftp"
  server <- "sftp-transparency.entsoe.eu/"
  source("data/passwd-entso-e")
  
 
  
  for(year in startyear:endyear){
    for(month in startmonth:endmonth){
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
 
  entso_e_data <- NULL
  if(file.exists(INTERIM_FEATHER_FILE)){
    entso_e_data <- feather(INTERIM_FEATHER_FILE) %>% as_tibble()
  }

  for(year in 2015:endyear){
    for(month in 1:12){
      
      n<-entso_e_data %>% filter(year(Date)==year & month(Date)==month) %>% nrow()
      if(n==0){
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
  
  entsoe_e_data_final %>% write_feather(INTERIM_FEATHER_FILE)
  
  return(entso_e_data_final)
  
}


tweet_result<-function(additional_text,
                       additional_text_2){
  
  
  
  authentification <- feather("data/authentification")
  
  setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                      access_token = authentification$access_token[1],
                      consumer_secret = authentification$consumer_secret[1],
                      access_secret = authentification$access_secret[1])
  
  
  tweet1<-updateStatus(text = paste0("EAG Tracker: Wie geht der Ausbau der Erneuerbaren in Österreich voran?",
                                     "Datenstand Wind-, Wasserkraft: ",additional_text, " Datenstand PV, Biomasse: ", additional_text_2),
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


get_apg_data<-function(startdate, enddate, type="realized"){

  base_url<-"https://transparency.apg.at/transparency-api/api/v1/Data/AGPT/German/M60/"
  if(type=="forecast"){
    base_url<-"https://transparency.apg.at/transparency-api/api/v1/Data/DAFTG/German/M60/"
  }
  
  daysequence<-seq(startdate,enddate,by="day")
  all_values<-NULL
  for(i in seq_along(daysequence)){
    day<-daysequence[i]
    year_string<-year(day)
    month_string<-sprintf("%02d", month(day))
    day_string<-sprintf("%02d", day(day))

    url<-paste0(base_url,year_string,"-",month_string,"-",day_string,"T000000/",year_string,"-",month_string,"-",day_string,"T230000")
    
    json<-fromJSON(url)
  
    datetime<-paste(json$ResponseData$ValueRows$DF,
                  json$ResponseData$ValueRows$TF) %>% 
                  dmy_hm()
  
    values<-json$ResponseData$ValueRows$V
  
    values <- mapply(function(x){return(x[,1])},values) %>% 
                t() %>% 
                as_tibble()
  
    if(type=="realized"){
    names(values)<-c("Wind", "PV", "Biomasse", "Gas", "Kohle", "Oel",
                   "Geothermie", "Pumpspeicher", "Laufwasserkraft",
                   "Speicher", "Sonstige Erneuerbare", "Muell", "Andere")
    }else
    {
      names(values)<-c("Gesamt", "Wind", "PV")
      
    }
    values<-bind_cols(tibble(datetime),values)
  
    all_values<-bind_rows(all_values, values)
    
    
  }
  
  return (all_values)

}

get_full_apg_dataset<-function(startdate, enddate){
  cached_data_apg_realized <- read_csv(filename_cache_all_years("realized"))
  cached_data_apg_forecast <- read_csv(filename_cache_all_years("forecast"))
  
  data_apg_realized <- get_apg_data(startdate, enddate, "realized")
  data_apg_forecast <- get_apg_data(startdate, enddate, "forecast")
  
  data_apg_full_realized <- bind_rows(cached_data_apg_realized,
                                      data_apg_realized) %>% 
    mutate(Type="realized")
  
  data_apg_full_forecast <- bind_rows(cached_data_apg_forecast,
                                      data_apg_forecast) %>% 
    mutate(Type="forecast")
  
  data_apg_full <- bind_rows(data_apg_full_realized,
                             data_apg_full_forecast %>% dplyr::select(-Gesamt)) %>% 
    gather(Technology,Generation,-datetime,-Type) %>%  
    mutate(Generation=as.numeric(Generation)/10^6) %>% 
    mutate(Technology=ifelse(Technology %in% c("Laufwasserkraft", 
                                               "Speicher",
                                               "Pumpspeicher"),"Wasserkraft",Technology)) %>% 
    mutate(Technology=ifelse(Technology %in% c("Sonstige Erneuerbare", "Muell", "Andere"),"Biomasse",Technology)) %>% 
    group_by(datetime,Technology,Type) %>% 
    summarize(Generation=sum(Generation,na.rm=TRUE)) %>% 
    ungroup() %>% 
    group_by(Year=year(datetime),Month=month(datetime),Technology,Type) %>% 
    summarize(Generation=sum(Generation)) %>% 
    mutate(Date=ymd(paste0(Year,"-",Month,"-01"))) %>% 
    mutate(Generation=ifelse(Generation==0,NA,Generation)) %>% 
    filter((Type=="realized"&Technology %in% c("Biomasse",
                                               "Wasserkraft",
                                               "Wind"))|
             (Technology=="PV"&Type=="forecast")) %>% 
    mutate(Type="Realisiert")
  
  
  return(data_apg_full)
}



eag_tracker_apg<-function(data_apg_full) {

  data_apg_full %>% ggplot(aes(x=Date,y=Generation)) + 
    geom_line(aes(col=Type))+
    facet_wrap(.~Technology)
  
  data_full_agp_annual <- data_apg_full %>%
    mutate(Year=year(Date)) %>% 
    group_by(Technology,Year,Type) %>% 
    summarize(Generation=sum(Generation,na.rm=TRUE))
  
  production_2020 <- data_full_agp_annual %>% 
    group_by(Type) %>% 
    filter(Year==2020) 
    
  additional_production<-tibble(Technology=c("Biomasse",
                                             "Wasserkraft",
                                             "PV",
                                             "Wind"),
                                Target_Add=c(1,5,11,10))  
  
  
  timeseries_target<-tibble(
    Date=rep(seq(as.POSIXct("2020-01-01"),as.POSIXct("2030-12-01"),by="month"),each=4),
    Technology=rep(c("Biomasse","Wasserkraft","PV","Wind"),11*12))  
  
  production_2020_2030<-full_join(production_2020,
                                  additional_production) %>% 
                        mutate(Target=Generation+Target_Add) 
  
  target<-bind_rows(production_2020_2030 %>% 
                      ungroup() %>%
                      mutate(Date=as.POSIXct("2020-01-01")) %>% 
                      dplyr::select(Date,Technology,Generation),
                    production_2020_2030 %>% 
                      ungroup() %>% 
                      mutate(Date=as.POSIXct("2030-12-01")) %>% 
                      dplyr::select(Date,Technology,Generation=Target)) %>% 
    full_join(timeseries_target) %>% 
    arrange(Date) %>% 
    group_by(Technology) %>% 
    mutate(Generation=na.approx(Generation,maxgap=11*12)) %>% 
    mutate(Type="Ziel")
  
  data_apg_full <- data_apg_full %>% 
    ungroup() %>% 
    dplyr::select(Date, Technology, Generation, Type)
  
  bind_rows(target,
            data_apg_full %>% 
              ungroup() %>% 
              #filter(year(Date)>2018) %>% 
              group_by(Technology,Type) %>% 
              mutate(Generation=rollmean(12*Generation,k=12,align="right",fill=NA))  
              #filter(year(Date)>2019) %>% 
              )  %>% 
    return()
  
}

eag_tracker_apg_econtrol<-function(startdate,enddate){
  
  
  data_apg_full <- get_full_apg_dataset(startdate, enddate) 
  apg_data<-eag_tracker_apg(data_apg_full) %>% 
    mutate(Source="APG") 
  
  e_control_data<-eag_tracker_econtrol() %>% 
    mutate(Source="econtrol") %>% 
    dplyr::select(Date,Technology,Generation=Value,Type,Source)
  
  econtrol_apg_data<-
    bind_rows(e_control_data %>% 
                filter(Technology %in% c("Biomasse", 
                                         "Wasserkraft", 
                                         "Wind")) ,
              apg_data %>% 
                filter(Technology=="PV"))
  
  return(econtrol_apg_data)
  
  
}

