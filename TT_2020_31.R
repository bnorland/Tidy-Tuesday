# Load libraries
library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(maptools)

# Create colour palette
Background_Main="#1E2639"
Background_Light="#27455B"
Outlines="#0C0F17"
Text_1="#E9E9EB"
Text_2="#D2D4D7"
Contrast_1="#B9BEBA"
Highlight_1="#C72523"
Highlight_1_Accent="#951E22"
Highlight_2="#4D8AB5"
Highlight_3="#EFA53A"
Highlight_4="#35B535"
Highlight_5="#2C2A89"

# Load data
tuesdata=tt_load(2020,week = 31)
penguins=tuesdata$penguins
head(penguins)

penguins=penguins%>%
  mutate(Approx_Bill_Size=bill_length_mm*bill_depth_mm)

p1=penguins%>%
  ggplot(aes(x=Approx_Bill_Size,y=body_mass_g,col=species))+
  geom_point()+
  stat_smooth(method="lm",se = FALSE,fullrange = FALSE)+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size=10,colour=Text_1),
        axis.text.y = element_text(size=8,colour=Text_1),
        panel.grid.major.x = element_line(colour = Background_Light),
        panel.grid.major.y = element_line(colour = Background_Light),
        axis.text.x=element_text(size=8,colour = Text_1,angle=45),
        plot.title = element_text(size=12,colour = Text_1),
        plot.subtitle=element_text(size=10,colour = Text_1),
        plot.caption=element_text(size=8,colour = Text_1),
        plot.background = element_rect(fill=Background_Main),
        panel.background = element_rect(fill=Background_Main),
        panel.border = element_rect(colour=Background_Main,size=0,linetype = "solid"),
        legend.key = element_rect(fill=Background_Main),
        legend.background = element_rect(fill=Background_Main),
        legend.text = element_text(colour=Text_1,size=10),
        legend.title = element_text(colour=Text_1,size=10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title.position = "plot")+
  labs(title=paste0("The bigger the beak, the bigger the bird? Well, sort of..."),
       subtitle=paste0("Bigger bill size is correlated with higher body mass within each species, but the same doesn't hold true between species"),
       caption="Data source: Dr Kristen Gorman, Dr Allison Horst, and Dr Alison Hill (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-28/readme.md)",
       y="Body Mass (grams)",
       x="Approximate Bill Size (Bill Length (mm) x Bill Depth (mm))",
       col="Species")+
  scale_color_manual(values=c(Highlight_1,Highlight_2,Highlight_3))

setwd("~/Personal/Projects/Tidy Tuesday")
ggsave(p1,file="TT_2020_Wk31.png",width=10,height=8)