# Plot a map of COOPS water temperature stations and the calculated annual loss of C at each ####
# this creates a theme for the map
theme_map <- function(...) {
  theme_minimal() +
    theme(
      # text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      # plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      # panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      # legend.background = element_rect(fill = "#f5f5f2", color = NA),
      # panel.border = element_blank(),
      ...
    )
}

# station decomp map
# install.packages("rnaturalearth")
library("rnaturalearth")
# install.packages("rnaturalearthdata")  
library("rnaturalearthdata")
# install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
library(rnaturalearthhires)
# install.packages("rgeos")
library(rgeos)
# install.packages(sf)
library(sf)
# install.packages("ggrepel")
library(ggrepel) 
# install.packages("ggstar")
# library(ggstar)

# create datasets for the background map
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "united states of america", returnclass = "sf")
coast <- ne_coastline(scale = 10, returnclass = "sf") 

#only select states with COOPS stations for the dotted outlines
states1 <- filter(states, postal %in% unique(idlist$State))

#create a dataframe for plotting the labels
states2 <- left_join(states1, station.decomp.summary, by = c("postal" = "State"))
states3 <-
  st_as_sf(states2) %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

#add a variable that moves the labels to avoid overcrowding
states3$nudge_x <- 0
states3$nudge_y <- 0

x_range <- abs(Reduce("-", range(states3$COORDS_X)))
y_range <- abs(Reduce("-", range(states3$COORDS_Y)))
#set the nudge for individual states or groups of states
ix <- states3$name %in% c("New Hampshire", "Massachusetts", "Rhode Island")
states3$nudge_x[ix] <- 1 * 0.09 * x_range
states3$nudge_y[ix] <- 0 * 0.050 * y_range

ix <- states3$name %in% c("Connecticut", "New Jersey", "Delaware")
states3$nudge_x[ix] <- 1 * 0.11 * x_range
states3$nudge_y[ix] <- -1 * 0.08 * y_range

ix <- states3$name %in% c("Maryland", "District of Columbia")
states3$nudge_x[ix] <- -1 * 0.15 * x_range
states3$nudge_y[ix] <- 1 * 0.02 * y_range

ix <- states3$name %in% c("Florida") & states3$Region %in% c("Atlantic")
states3$nudge_x[ix] <- 1 * 0.075 * x_range
states3$nudge_y[ix] <- 1 * 0.05 * y_range
states3$COORDS_X[ix] <- -80.650238
states3$COORDS_Y[ix] <- 27.5
states3$postal[ix] <- "FL (Atlantic)"

ix <- states3$name %in% c("Florida") & states3$Region %in% c("Gulf")
states3$nudge_x[ix] <- -1 * 0.08 * x_range
states3$nudge_y[ix] <- -1 * 0.08 * y_range
states3$postal[ix] <- "FL (Gulf)"

ix <- states3$name %in% c("Louisiana")
# states3$nudge_x[ix] <- 1 * 0.02 * x_range
states3$nudge_y[ix] <- 1 * 0.05 * y_range

#create a df to plot background labels for states where border lines may impact the readability of the labels 
#i.e. not those displayed over the ocean
states4 <- filter(states3, !(postal %in% c("FL (Gulf)", "FL (Atlantic)", "DE", "NJ", "CT", "RI", "MA")))
#create a specific data.frame for plotting that filters out stations with less than 95% temporaly coverage in the selected year
plotdata <- filter(stationmetadata, 
                   # Region == "Pacific",
                   readings.pct >= 0.95)
#store the limits of the plotdata, which can be useful for other functions
xlims <-  range(plotdata$lon)
ylims <- range(plotdata$lat)

#store the limits of the annual decomp for plotting when zooming in to regions
dlims <- data.frame(
 mixed =  with(plotdata, range(((totalDecomp.shallow*1/3)+(totalDecomp.deep*2/3))*100)),
 shallow = range(plotdata$totalDecomp.shallow)*100,
 deep = range(plotdata$totalDecomp.deep)*100
)

# Plot a background map ####
# which can be plotted with a variety of station values 
map <- 
  ggplot(data = world) +
    geom_sf(color = "transparent", fill = "gray90")+
    geom_sf(data = states1, fill = "transparent", color = "black", linetype = "dashed")+
    # geom_sf(data = states2, aes(fill = meanDecomp.deep/10), color = "white", size = 0.25, linetype = "dotted")+
    geom_sf(data = coast, color = "gray50")+
    coord_sf(xlim = xlims,
             ylim = ylims, expand = T)+
    # ggthemes::theme_hc()
    theme_map()

# plot the mixed decomp map ####
# mixed is the map plotting 2/3 of the decomp from deep horizon and 1/3 from shallow 
mixed <- map +
  geom_label_repel(data = states4, 
                   mapping = aes(x = COORDS_X, y = COORDS_Y, label = paste0(postal, ":\n", signif(meanDecomp.shallow*100, 3), "%")),
                   nudge_x = states4$nudge_x, nudge_y = states4$nudge_y,
                   size = 3, min.segment.length = 0, point.padding = NA,
                   segment.color = "transparent", 
                   label.size = NA, label.padding = 0.1, 
                   color = NA, fill = alpha("gray90", 01)) +
  geom_label_repel(data = states3,
                   mapping = aes(x = COORDS_X, y = COORDS_Y, label = paste0(postal, ":\n", signif(((meanDecomp.shallow*1/3)+(meanDecomp.deep*2/3))*100, 3), "%")),
                   nudge_x = states3$nudge_x, nudge_y = states3$nudge_y,
                   size = 3, min.segment.length = 0, point.padding = NA,
                   segment.color = "grey50",  label.size = NA, label.padding = 0.1,
                   fill = NA) +
  geom_label_repel(data.frame(x = -77.326, y = 34.553, shallow = 22.9, deep = 21.4), 
                   mapping = aes(x = x, y = y, label = "Coring Location and\nLong-Term Water Temperature Station"), 
                   nudge_x = 1 * 0.11 * x_range,  nudge_y = -1 * 0.08 * y_range,
                   size = 3, min.segment.length = 0, point.padding = NA,
                   segment.color = "grey50",  label.size = NA, label.padding = 0.1,
                   fill = NA)+
  geom_point(data = plotdata,
             aes(x = as.numeric(lon), y = as.numeric(lat), fill = ((totalDecomp.shallow*1/3)+(totalDecomp.deep*2/3))*100),
             size = 3, color = "black", shape = 21)+
  geom_point(data = data.frame(x = -77.326, y = 34.553, shallow = 22.9, deep = 21.4),
             aes(x, y, fill = shallow*1/3 + deep*2/3), shape = 21, size = 4, inherit.aes = F, show.legend = F)+
  # geom_star(data = data.frame(x = -77.326, y = 34.553, shallow = 22.9, deep = 21.4),
  #           aes(x, y, fill = shallow*1/3 + deep*2/3),  size = 4)+
  scale_fill_continuous(type = "viridis", option = "C", limits = dlims$mixed, breaks = seq(0,40, by = 5), labels = function(x) ifelse(x%%10==0,paste0(x, "%"), ""))+
  labs(x = NULL, y = NULL, fill = "Station Annual % C Loss", shape = NULL)+
  theme(legend.position = c((-106.5 - map$coordinates$limits$x[[1]])/abs(Reduce("-", map$coordinates$limits$x)), 
                            (43.25 - map$coordinates$limits$y[[1]])/abs(Reduce("-", map$coordinates$limits$y))),
        #legend.position = c(0.01, 0.01),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        panel.border = element_rect(fill = NA, color = NA)) + 
  guides(fill = guide_colorbar(ticks.linewidth = 1.5,
                               barheight = unit(2.5, units = "mm"),
                               barwidth = unit(75, units = "mm"),
                               draw.ulim = T,
                               title.theme = element_text(size = 11),
                               title.position = 'top',
                               title.hjust = 0.5,
                               label.hjust = 0.5))+
  annotate("rect", xmin = -108, xmax = -105, ymin = 45.5, ymax = 47.0, fill = "transparent", color = "black", linetype = "dashed")+
  annotate("text", x = -104, y = 46.25, label = "Coastal State: Mean % C Loss", hjust = 0)+
  annotate("point", x = -106.5, y = 44.75, fill = "transparent", color = "black", size = 5, shape = 21)+
  annotate("text", x = -104, y = 44.75, label = "CO-OPS Water Temperature Station", hjust = 0)
  # annotate("star", x = -106.5, y = 43.25, fill = "gray90", color = "black", size = 5)+
  # annotate("text", x = -104, y = 43.25, label = "Long Term Water Temperature Station", hjust = 0)+

mixed
# save
ggsave(plot = mixed, paste0(getwd(), "/plots/Station Decomp and Mean State Decomp Map Mixed Decomp.png"),
       width = 11, height = 6, units = "in", dpi = 600)
ggsave(plot = mixed, paste0(getwd(), "/plots/jpegs/Station Decomp and Mean State Decomp Map Mixed Decomp.jpeg"), 
       width = 11, height = 6, units = "in", dpi = 600)
ggsave(plot = mixed, paste0(getwd(), "/plots/tifs/Station Decomp and Mean State Decomp Map Mixed Decomp"), device = "tiff",
       width = 11, height = 6, units = "in", dpi = 600)
# Shallow decomp ####
shallow <- map +
  geom_label_repel(data = states4, 
                   mapping = aes(x = COORDS_X, y = COORDS_Y, label = paste0(postal, ":\n", signif(meanDecomp.shallow*100, 3), "%")),
                   nudge_x = states4$nudge_x, nudge_y = states4$nudge_y,
                   size = 3, min.segment.length = 0, point.padding = NA,
                   segment.color = "transparent", 
                   label.size = NA, label.padding = 0.1, 
                   color = NA, fill = alpha("gray90", 01)) +
  geom_label_repel(data = states3,
                   mapping = aes(x = COORDS_X, y = COORDS_Y, label = paste0(postal, ":\n", signif(meanDecomp.shallow*100, 3), "%")),
                   nudge_x = states3$nudge_x, nudge_y = states3$nudge_y,
                   size = 3, min.segment.length = 0, point.padding = NA,
                   segment.color = "grey50",  label.size = NA, label.padding = 0.1,
                   fill = NA) +
  geom_point(data = plotdata,
             aes(x = as.numeric(lon), y = as.numeric(lat), fill = (totalDecomp.shallow*100)),
             size = 3, color = "black", shape = 21)+
  scale_fill_continuous(type = "viridis", option = "C")+
  labs(x = NULL, y = NULL, fill = "% Carbon Loss", shape = NULL)+
  theme(legend.position = c(0.01, 0.01),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        panel.border = element_rect(fill = NA, color = NA)) + 
  guides(fill = guide_colorbar(barheight = unit(2, units = "mm"),
                               barwidth = unit(50, units = "mm"),
                               draw.ulim = F,
                               title.position = 'top',
                               title.hjust = 0.5,
                               label.hjust = 0.5))+
  annotate("rect", xmin = -108, xmax = -105, ymin = 45.5, ymax = 47.0, fill = "transparent", color = "black", linetype = "dashed")+
  annotate("text", x = -104, y = 46.25, label = "Coastal State: Mean %C Loss", hjust = 0)+
  annotate("point", x = -106.5, y = 42.5, fill = "transparent", color = "black", size = 5, shape = 21)+
  annotate("text", x = -104, y = 42.5, label = "CO-OPS Water Temperature Station", hjust = 0)

ggsave(plot = shallow, paste0(getwd(), "/plots/Station Decomp and Mean State Decomp Map Shallow Decomp.png"), width = 11, height = 6, units = "in", dpi = 330)

# Deep decomp ####
deep <- map +
  geom_label_repel(data = states4, 
                   mapping = aes(x = COORDS_X, y = COORDS_Y, label = paste0(postal, ":\n", signif(meanDecomp.deep*100, 3), "%")),
                   nudge_x = states4$nudge_x, nudge_y = states4$nudge_y,
                   size = 3, min.segment.length = 0, point.padding = NA,
                   segment.color = "transparent", 
                   label.size = NA, label.padding = 0.1, 
                   color = NA, fill = alpha("gray90", 01)) +
  geom_label_repel(data = states3,
                   mapping = aes(x = COORDS_X, y = COORDS_Y, label = paste0(postal, ":\n", signif(meanDecomp.deep*100, 3), "%")),
                   nudge_x = states3$nudge_x, nudge_y = states3$nudge_y,
                   size = 3, min.segment.length = 0, point.padding = NA,
                   segment.color = "grey50",  label.size = NA, label.padding = 0.1,
                   fill = NA) +
  geom_point(data = plotdata,
             aes(x = as.numeric(lon), y = as.numeric(lat), fill = (totalDecomp.deep*100)),
             size = 3, color = "black", shape = 21)+
  scale_fill_continuous(type = "viridis", option = "C")+
  labs(x = NULL, y = NULL, fill = "% Carbon Loss", shape = NULL)+
  theme(legend.position = c(0.01, 0.01),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        panel.border = element_rect(fill = NA, color = NA)) + 
  guides(fill = guide_colorbar(barheight = unit(2, units = "mm"),
                               barwidth = unit(50, units = "mm"),
                               draw.ulim = F,
                               title.position = 'top',
                               title.hjust = 0.5,
                               label.hjust = 0.5))+
  annotate("rect", xmin = -108, xmax = -105, ymin = 45.5, ymax = 47.0, fill = "transparent", color = "black", linetype = "dashed")+
  annotate("text", x = -104, y = 46.25, label = "Coastal State: Mean %C Loss", hjust = 0)+
  annotate("point", x = -106.5, y = 42.5, fill = "transparent", color = "black", size = 5, shape = 21)+
  annotate("text", x = -104, y = 42.5, label = "CO-OPS Water Temperature Station", hjust = 0)

ggsave(plot = deep, paste0(getwd(), "/plots/Station Decomp and Mean State Decomp Map Deep Decomp.png"), width = 11, height = 6, units = "in", dpi = 330)

#### zoom to regions ####
plotdata <- filter(stationmetadata, 
                   # State %in% c("NC", "VA", "MD", "DC", "DE", "PA", "NJ"),
                   # State %in% c("NY", "CT", "RI", "MA", "ME"),
                   # State %in%c("FL", "GA", "SC"),
                   # State %in% c("NC"),
                   # State %in% c("WA"),
                   # Region == "Pacific",
                   # Region == "Gulf",
                   Region == "Atlantic",
                   readings.pct >= 0.95)

ggplot(data = world) +
  geom_sf(color = "transparent", fill = "gray90")+
  geom_sf(data = states1, fill = "transparent", color = "black", linetype = "dashed")+
  # geom_sf(data = states2, aes(fill = meanDecomp.deep/10), color = "white", size = 0.25, linetype = "dotted")+
  geom_sf(data = coast, color = "gray50")+
  coord_sf(xlim = c(min(as.numeric(plotdata$lon), na.rm = T), max(as.numeric(plotdata$lon), na.rm = T)),
           ylim = c(min(as.numeric(plotdata$lat), na.rm = T), max(as.numeric(plotdata$lat), na.rm = T)), expand = T)+
  # ggthemes::theme_hc()
  theme_map() +
  # geom_label_repel(data = states4, 
  #                  mapping = aes(x = COORDS_X, y = COORDS_Y, label = paste0(postal, ":\n", signif(meanDecomp.shallow*100, 3), "%")),
  #                  nudge_x = states4$nudge_x, nudge_y = states4$nudge_y,
  #                  size = 3, min.segment.length = 0, point.padding = NA,
  #                  segment.color = "transparent", 
  #                  label.size = NA, label.padding = 0.1, 
  #                  color = NA, fill = alpha("gray90", 01)) +
  # geom_label_repel(data = filter(states3, Region %in% unique(plotdata$Region)),
  #                  mapping = aes(x = COORDS_X, y = COORDS_Y, label = paste0(postal, ":\n", signif(meanDecomp.shallow*100, 3), "%")),
  #                  # nudge_x = states3$nudge_x, nudge_y = states3$nudge_y,
  #                  size = 3, 
  #                  # min.segment.length = 0, point.padding = NA,
  #                  segment.color = "grey50",  label.size = NA, label.padding = 0.1,
  #                  fill = NA) +
  geom_point(data = plotdata,
             aes(x = as.numeric(lon), y = as.numeric(lat), fill = (totalDecomp.shallow*100)),
             size = 3, color = "black", shape = 21)+
  scale_fill_continuous(type = "viridis", option = "C", limits = dlims$shallow, breaks = seq(0,40, by = 5), labels = function(x) ifelse(x%%10==0,paste0(x, "%"), ""))+
  labs(x = NULL, y = NULL, fill = "% Carbon Loss", shape = NULL)+
  theme(legend.position = 'bottom',
        # legend.justification = c(0, 0),
        legend.direction = "horizontal",
        panel.border = element_rect(fill = NA, color = NA)) +
  guides(fill = guide_colorbar(barheight = unit(2, units = "mm"),
                               barwidth = unit(50, units = "mm"),
                               draw.ulim = F,
                               title.position = 'top',
                               title.hjust = 0.5,
                               label.hjust = 0.5))#+
  # annotate("rect", xmin = -108, xmax = -105, ymin = 45.5, ymax = 47.0, fill = "transparent", color = "black", linetype = "dashed")+
  # annotate("text", x = -104, y = 46.25, label = "Coastal State: Mean %C Loss", hjust = 0)+
  # annotate("point", x = -106.5, y = 42.5, fill = "transparent", color = "black", size = 5, shape = 21)+
  # annotate("text", x = -104, y = 42.5, label = "CO-OPS Water Temperature Station", hjust = 0)+
  # geom_star(data = data.frame(x = -77.3263, y = 34.5537, shallow = 22.9, deep = 21.4),
            # aes(x, y, fill = shallow*1/3 + deep*2/3),  size = 4)

