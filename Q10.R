### Q10
# Traditional Q10
decompRate <- data.frame(Depth = NA, Treatment = NA, day = NA)

for (d in unique(decomp.data$day)) {
  decompRate <- filter(decomp.data, day <= d) %>% 
    group_by(Depth, Treatment, Rep) %>% 
    summarize(d = d, 
              slope = summary(lm(PercentCRespired ~ day))$coefficients[2]) %>% 
    rename(day = d) %>% 
    ungroup() %>% 
    bind_rows(decompRate, .) %>% 
    filter(!is.na(Depth))
}

Q10 <- decompRate %>% 
  group_by(day, Depth, Treatment) %>% 
  summarise(slope = mean(slope, na.rm = T)) %>% 
  dcast(day + Depth ~ Treatment) %>% 
  left_join(.,
            decompRate %>% 
              group_by(day, Depth, Treatment) %>% 
              summarise(dSlope = sd(slope, na.rm = T)) %>% 
              dcast(day + Depth ~ Treatment), 
            suffix = c(".slope", ".sd"), by = c("day", "Depth")) %>% 
  rename("slope.20" = "IB 20.slope", 
         "dslope.20" = "IB 20.sd", 
         "slope.30" = "IB 30.slope",
         "dslope.30" = "IB 30.sd") %>% 
  mutate_with_error(Q10 ~ slope.30/slope.20) %>% 
  ungroup() %>% 
  mutate(seQ10 = dQ10/sqrt(6)) %>% 
  select(day, Depth, dplyr::contains("Q"))

ggplot(Q10, aes(day, Q10, color = Depth))+
  geom_line()+
  geom_errorbar(aes(min = Q10 - seQ10, max = Q10 + seQ10))+
  geom_point()

#####
# Q10q
# Q10 at each nth percent of sediment C respired
# 
# This method of calculating Q10 uses the ratio of time to respire a given portion of sediment C at temperatures 10 degrees apart
# rather than the ratio of rates at a the same time after experiment start.
# See Conant et al. 2008
###

for (q in seq(0, round(max(decomp.data$PercentCRespired), 2), by=0.01)) {
  temp <- decomp.data %>% 
    filter(PercentCRespired <= q) %>% 
    group_by(Treatment, Depth, Bottle = Rep) %>% 
    summarize(PortionC = q, 
              TimeBefore = max(day, na.rm = T),
              ValueBefore = max(PercentCRespired, na.rm = T))
  
  temp <- decomp.data %>% 
    filter(PercentCRespired > q) %>% 
    group_by(Treatment, Depth, Bottle = Rep) %>% 
    summarize(PortionC = q, 
              TimeAfter = min(day, na.rm = T),
              ValueAfter = min(PercentCRespired, na.rm = T)) %>% 
    left_join(temp, ., by = c("Treatment", "Depth", "Bottle", "PortionC"))
  
  if(!exists("ElapsedTime")){
    ElapsedTime <- temp
  }else{
    ElapsedTime <- bind_rows(ElapsedTime, temp)
  }
}

for (i in 1:nrow(ElapsedTime)) {
  ElapsedTime$CalcTime[i] <- with(ElapsedTime[i, ], TimeAtGivenPortion(PortionC, c(TimeBefore, ValueBefore), c(TimeAfter, ValueAfter)))
}

ElapsedTime <- ElapsedTime %>% 
  group_by(Treatment, Depth, Bottle) %>% 
  arrange(Treatment, Depth, Bottle, PortionC) %>% 
  mutate(ElapsedTime = CalcTime - lag(CalcTime, default = 0))

ElapsedTime.summary <- ElapsedTime %>% 
  group_by(PortionC, Depth, Treatment) %>% 
  summarize(mean = mean(ElapsedTime, na.rm = T),
            sd = sd(ElapsedTime, na.rm = T),
            N = n(), 
            se = sd/sqrt(N)) %>% 
  rename(ElapsedTime = mean) %>% 
  filter(PortionC != 0)


Q10q <- left_join(filter(ElapsedTime.summary, Treatment == "IB 20")[,c("PortionC", "Depth", "N", "ElapsedTime", "sd", "se")], 
                   filter(ElapsedTime.summary, Treatment == "IB 30")[,c("PortionC", "Depth", "N", "ElapsedTime", "sd", "se")], 
                   by = c("Depth", "PortionC"), 
                   suffix = c(".20", ".30")) %>% 
  as_tibble() %>% 
  rename(dElapsedTime.30 = sd.30, dElapsedTime.20 = sd.20) %>% 
  mutate_with_error(Q10q ~ ElapsedTime.20/ElapsedTime.30) %>% 
  mutate(N = N.30 + N.20,
         se = dQ10q / sqrt(N)) %>% 
  select("PortionC", "Depth", "Q10q", "sd" = "dQ10q", "N", "se")

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
