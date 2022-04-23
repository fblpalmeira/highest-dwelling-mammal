meter<-c(0,2300,500,1500,2500,2900,2700,4000,
         6739,6400,5800,5200,5200,5500,5400,6100)
elevation<-c("Lower elevation", "Lower elevation", "Lower elevation","Lower elevation", "Lower elevation", "Lower elevation", "Lower elevation","Lower elevation",
             "Upper elevation", "Upper elevation", "Upper elevation", "Upper elevation", "Upper elevation", "Upper elevation", "Upper elevation", "Upper elevation")
species<-c("Yellow-rumped leaf-eared mouse", "Large-eared pika", "Snow leopard",
           "Himalayan tahr", "Tibetan sand fox", "Himalayan marmot",
           "Kiang", "Yak")
a<-data.frame(meter, elevation, species)

library (ggplot2)
library (ggrepel)
library (dplyr)

MySpecial <- list(  
  # move the x axis labels up top
  scale_x_discrete(position = "top"),
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top      = element_text(size=20)),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5))
)

png(file = "mammals_elevation.png", width = 1100, height = 900)

p<-ggplot(a, aes(x = elevation, y = meter, group = species, color=species)) +
  geom_line(aes(color=species), size=2) + 
  geom_point(aes(color = species), size = 4) +
  theme_minimal(base_size = 18) + 
  scale_color_brewer(palette = "Dark2") +
  geom_text_repel(data = a %>% filter(elevation == "Lower elevation"), 
                aes(label = paste0(species, " : ", meter, "m")),  
                hjust = "left", 
                fontface = "bold", 
                size = 4.5, 
                nudge_x = -.6, 
                direction = "y") +
  geom_text_repel(data = a %>% filter(elevation == "Upper elevation"), 
                  aes(label = paste0(species, " : ", meter, "m")), 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 4.5, 
                  nudge_x = .7, 
                  direction = "y") +
  #geom_label(aes(label = meter), 
            # size = 2.5, 
            # label.padding = unit(0.05, "lines"), 
            # label.size = 0.0)+
  MySpecial +
  labs(
    title = "",
    subtitle = "",
    caption = ""
  )

dev.off()

library(magick)
library(magrittr) 

# Call back the plot
plot <- image_read("mammals_elevation.png")
plot2<-image_annotate(plot, "The world's highest-dwelling mammal", 
                      color = "black", size = 25,
                      location = "10+50", gravity = "north")
plot3<-image_annotate(plot2, "Data: Storz et al 2020 (doi.org/10.1073/pnas.2005265117) and IUCN Red List  
                          Image credit: Yellow-rumped leaf-eared mouse (Marcial Quiroga-Carmona) | Visualization by @fblpalmeira", 
                      color = "gray", size = 15, 
                      location = "10+50", gravity = "southeast")
# And bring in a logo
mouse_upper <- image_read("https://raw.githubusercontent.com/fblpalmeira/highest-dwelling-mammal/main/data/072420_ja_highest-mammal_feat2-1028x579-removebg-preview.png") 
out<-image_composite(plot3,image_scale(mouse_upper,"x100"), gravity="north", offset = "+225+40")

image_browse(out)

# And overwrite the plot without a logo
image_write(out, "mammals_elevation2.png")
