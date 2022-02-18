source("src/functions.R")

create_full_cache_apg("realized")
create_full_cache_apg("forecast")

startdate <- as.POSIXct("2022-01-01")
enddate <- as.POSIXct("2022-02-01")
apg_econtrol_data<-eag_tracker_apg_econtrol(startdate,enddate)


apg_econtrol_data %>% 
  ggplot(aes(x=Date,y=Generation))+
  geom_line(aes(col=Type),size=1)+
  facet_wrap(.~Technology,scale="free") +
  scale_color_manual(values=COLORS3) +
  theme_bw() +
  ylab("Erzeugung (TWh)") +
  ylim(c(0,NA)) +
  xlab("Datum") +
  labs(caption = "Quelle: APG, EAG, e-control")
ggsave(FILENAME_FIGURE)

tweet_result("12/21", "12/20")
