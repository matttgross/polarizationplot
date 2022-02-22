####
library(tidyverse)
library(ggalt)
library(ggtext)
library(showtext)
library(patchwork)


file_path <- 

nominate <- read.csv(file_path)


house <- nominate %>%
  filter(chamber=="House" & congress>89) %>%
  filter(party_name=="Democrat" | party_name=="Republican") %>%
  select(party_name, congress, nominate_dim1_median) %>%
  pivot_wider(names_from=party_name, values_from=nominate_dim1_median)

senate <- nominate %>%
  filter(chamber=="Senate" & congress>89) %>%
  filter(party_name=="Democrat" | party_name=="Republican") %>%
  select(party_name, congress, nominate_dim1_median) %>%
  pivot_wider(names_from=party_name, values_from=nominate_dim1_median)


h <- ggplot()+
  geom_segment(data=house, aes(y=congress, yend=congress, x=-1, xend=1),color = "gray",
              size = 0.1)+
  geom_dumbbell(data=house,aes(y=congress, x=Democrat, xend=Republican), size=1, colour = "black", size_x = 4, size_xend = 4,
              colour_x = "blue", colour_xend = "red")+
  scale_y_continuous(breaks=seq(90, 115, 5))+
  labs(title="House",
       x="Median First Dimension NOMINATE Scores", 
       y="Congress")+
  geom_text(data = filter(house, congress == 116),
            aes(x = Democrat , y = congress, label = "Democrats"),
            colour = "blue", size = 3, vjust = -3.5, hjust = 0.4, fontface = "bold", family = "Montserrat") +
  geom_text(data = filter(house, congress == 116),
            aes(x = Republican, y = congress, label = "Republicans"),
            colour = "red", size = 3, vjust = -3.5, fontface = "bold",family = "Montserrat") +
  theme_minimal()+
  theme(text = element_text(family = "Montserrat"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 14,face = "bold", color = "#535657", hjust = 0.5, vjust = -1),
        axis.title.x = element_text(size = 9, face = "bold", color = "#535657", vjust = -2),
        axis.title.y = element_text(size = 9, face = "bold", color = "#535657"),
        axis.text.x = element_text(size = 10, face = "bold", color = "#535657"),
        axis.text.y = element_text(size = 10, face = "bold", color = "#535657"),
        axis.ticks.x = element_line(colour = "#b2b2b2"))
        
s <- ggplot()+
  geom_segment(data=senate, aes(y=congress, yend=congress, x=-1, xend=1),color = "gray",
               size = 0.1)+
  geom_dumbbell(data=senate,aes(y=congress, x=Democrat, xend=Republican), size=1, colour = "black", size_x = 4, size_xend = 4,
                colour_x = "blue", colour_xend = "red")+
  geom_text(data = filter(house, congress == 116),
            aes(x = Democrat , y = congress, label = "Democrats"),
            color = "blue", size = 3, vjust = -3.5, hjust = 0.4, fontface = "bold", family = "Montserrat") +
  geom_text(data = filter(house, congress == 116),
            aes(x = Republican, y = congress, label = "Republicans"),
            color = "red", size = 3, vjust = -3.5, fontface = "bold",family = "Montserrat") +
  scale_y_continuous(breaks=seq(90, 115, 5))+
  labs(title = "Senate",
       caption="Source: Voteview.org")+
  theme_minimal()+
  theme(text = element_text(family = "Montserrat"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 14,face = "bold", color = "#535657", hjust = 0.5, vjust = -1),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", color = "#535657"),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_line(color = "#b2b2b2"),
        plot.caption = element_text(size = 7, face = "bold", color = "#535657"))        


c <- h+s
c <- c + plot_annotation(title="Polarization in Congress", 
          theme = theme(text = element_text('Montserrat',  color = "#535657"),
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_markdown(size = 14, hjust = 0.5)))
    
c

ggsave('congresspolarize.png', c, width = 12, height = 8) 

