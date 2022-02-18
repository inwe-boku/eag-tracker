source("src/functions.R")

e_control_data<-eag_tracker_econtrol()
e_control_data %>% filter(year(Date)==2020) %>% tail()




entso_e_data <- get_entso_e_data(startyear = 2022, 
                                endyear = 2022, 
                                startmonth = 1, 
                                endmonth = 2)

e_control_data %>% 
  bind_rows(entso_e_data) %>% 
  ggplot(aes(x=Date,y=Value))+
  geom_line(aes(col=Type),size=1)+
  facet_wrap(.~Technology,scale="free") +
  scale_color_manual(values=COLORS3) +
  theme_bw() +
  ylab("Erzeugung (TWh)") +
  ylim(c(0,NA)) +
  xlab("Datum") +
  labs(caption = "Quelle: EAG, e-control")
  
ggsave(FILENAME_FIGURE)

tweet_result()




  
  


  
