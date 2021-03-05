#plot data
plot.q10.portionC <- ggplot(Q10, aes(PercentRespired, Q10, fill = Depth))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(x = "Portion Respired", 
       y = expression(Q[10-q]), 
       fill = "Depth", 
       title = expression("Changes in Q"[10]*" associated with portion of sediment C respired"))+
  scale_x_continuous(breaks = seq(0, 1, by = 0.01))+
  scale_y_continuous(breaks = seq(0, 10, by = 0.25))
plot.q10.portionC

#test