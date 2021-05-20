#####
# N.D. McTigue, Q.A. Walker, and C.A. Currin 2021
# Refining estimates of greenhouse gas emissions from salt marsh “blue carbon” erosion and decomposition
# email: quentin.walker@noaa.gov, mctigue@utexas.edu
#####

decomp.summary %>% 
  mutate(Depth = ifelse(Depth == "M", "Deep", "Shallow"),
         Treatment = ifelse(Treatment == "IB 20", "20° C", "30° C")) %>% 
  ggplot(aes(day, PercentCRespired, fill = Treatment, shape = Depth, group = paste(Depth, Treatment)))+
    geom_errorbar(aes(min = PercentCRespired - se, max = PercentCRespired + se), width = 3, show.legend = F)+
    geom_line(size = 1, show.legend = F, color = "gray40") + 
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

ggsave(filename = "PercentCRespired_border.png", path =getwd(),
       width = 9.74, height = 9.74, units = "in", dpi = 600)
ggsave(filename = "PercentCRespired_border.jpeg", path = paste0(getwd(),"/plots/jpegs/"),
       width = 9.74, height = 9.74, units = "in", dpi = 600)
ggsave(filename = "PercentCRespired_border", path = paste0(getwd(),"/plots/tifs/"), device = "tiff",
       width = 9.74, height = 9.74, units = "in", dpi = 600)
