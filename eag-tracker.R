source("src/functions.R")

e_control_data<-get_e_control_data()

e_control_data %>% 
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


  
