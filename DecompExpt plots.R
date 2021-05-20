#####
# N.D. McTigue, Q.A. Walker, and C.A. Currin 2021
# Refining estimates of greenhouse gas emissions from salt marsh “blue carbon” erosion and decomposition
# email: quentin.walker@noaa.gov, mctigue@utexas.edu
#####

## Run this script third ##
# this script assumes that output from DecompExpt.R and Q10.R is in the global environment 

#### This script plots data from the decomp expt ##### 


decomp.data <- decomp.data %>% 
  mutate(Depth = ifelse(Depth == "M", "Deep", "Shallow"),
         Treatment = ifelse(Treatment == "IB 20", "20° C", "30° C"))

ggplot(decomp.data, aes(day, PercentCRespired,  group = Bottle))+
  geom_line()+
  geom_point(size = 5, aes(fill = Treatment, shape = Depth)) +
  scale_fill_manual(values = c("gray90", "gray20"))+
  scale_shape_manual(values = c(21, 24))+
  scale_y_continuous(labels =  function(x) paste0(x*100, "%"))+
  labs(y = "Cumulative C Respired\n(% of Initial Sediment C)", 
       x = "Incubation Time (days)", 
       title = NULL, 
       fill = NULL, 
       shape = NULL)+
  theme_bw()+
  theme(text = element_text(size = 36),
        legend.text = element_text(size = 27),
        legend.position = c(.02, .98),
        legend.justification = c(0, 1),
        legend.box = "horizontal",
        legend.background = element_blank(),
        # panel.background = element_rect(color = "black", size = 1.25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(fill = guide_legend(override.aes = list(shape = c(22))))

# Q10 Plots ####

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



ggplot(data = filter(Q10q, PortionC <=0.05), aes(x = PortionC, y = Q10q, fill = Depth))+
  geom_errorbar(aes(min=Q10q-se, max=Q10q+se), width = 0.001)+
  geom_line()+
  geom_point(size = 4, shape = 21)+
  labs(x = "Percent Cabon Respired", 
       y = expression(Q[10-q]), 
       fill = "Depth Horizon")+
  coord_cartesian(xlim = c(0.01, 0.05))+
  scale_x_continuous(labels = function(x) {paste0(round(x*100, 0), "%")})+
  scale_y_continuous(breaks = c(1,2,3))+
  scale_fill_manual(values = c("white", "gray20"))+
  theme_bw()+
  theme(panel.grid = element_blank())
ggsave( "Q10-q rates for Fig 3_revised colors.tiff", dpi = 300, height = 3, width = 4, units = "in")

ggplot(data = filter(Q10q, PortionC <=0.05), aes(x = PortionC, y = Q10q, shape = Depth))+
  geom_errorbar(aes(min=Q10q-se, max=Q10q+se), width = 0.001)+
  geom_line()+
  geom_point(size = 4, fill = "black")+
  labs(x = "Percent Cabon Respired", 
       y = expression(Q[10-q]), 
       fill = "Depth Horizon")+
  coord_cartesian(xlim = c(0.01, 0.05))+
  scale_x_continuous(labels = function(x) {paste0(round(x*100, 0), "%")})+
  scale_y_continuous(breaks = c(1,2,3))+
  scale_shape_manual(values = c(21, 24), labels = c("Deep", "Shallow"))+
  theme_bw()+
  theme(panel.grid = element_blank())
ggsave( "Q10-q rates for Fig 3_revised shapes.tiff", dpi = 300, height = 3, width = 4, units = "in")

ElapsedTime %>% 
  filter(PortionC<=.1) %>% 
  ggplot(aes(PortionC, ElapsedTime, color=as.factor(Bottle)))+
  geom_line()+
  geom_point()+
  facet_grid(Depth~Treatment)+
  scale_x_continuous(limits = c(0.01, .1))+
  labs(color="Bottle", 
       x="Portion C Respired",
       y="Incubation Time (days)")

ElapsedTime.summary %>% 
  filter(PortionC <= 0.1) %>% 
  ggplot(aes(PortionC, ElapsedTime, fill = Treatment, shape = Depth, group = paste(Depth, Treatment)))+
  geom_errorbar(aes(min = ElapsedTime - se, max = ElapsedTime + se), width = 0.002, show.legend = F)+
  geom_line( show.legend = F, color = "gray40") + 
  geom_point(size = 5, aes(fill = Treatment, shape = Depth)) +
  scale_fill_manual(values = c("gray90", "gray20"))+
  scale_shape_manual(values = c(21, 24))+
  scale_x_continuous(breaks = seq(0, .2, by= 0.02), labels =  function(x) paste0(x*100, "%"))+
  labs(x = "Percent C Respired", 
       y = "Incubation Time (days)", 
       title = expression("Time to Respire Given Portion of Sediment Carbon"))+
  theme_bw()+
  guides(fill = guide_legend(override.aes = list(shape = c(22))))
