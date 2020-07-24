# Load libraries
library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(cowplot)

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
tuesdata=tt_load(2020,week = 30)
animal_outcomes=tuesdata$animal_outcomes
head(animal_outcomes)

# Pivot data to long form
animal_outcomes_long=animal_outcomes%>%
  pivot_longer(-c(year,animal_type,outcome),names_to = "State",values_to="Count")

# Find outcome types
unique(animal_outcomes_long$outcome)

# Group by state, animal, and year
state_totals=animal_outcomes_long%>%
  group_by(State,year,animal_type)%>%
  summarise(Total=sum(Count),
            Outcome_Achieved=sum(Count[!outcome %in% c("In Stock","Currently In Care")]),
            Euthanized=sum(Count[outcome=="Euthanized"]),
            Reclaimed=sum(Count[outcome=="Reclaimed"]),
            Rehomed=sum(Count[outcome=="Rehomed"]),
            Outcome_Not_Reclaimed=sum(Count[!outcome %in% c("In Stock","Currently In Care","Reclaimed")]))%>%
  mutate(Positive_Outcome=1-round(Euthanized/Outcome_Achieved,3),
         Reclaim_Prop=round(Reclaimed/Outcome_Achieved,3),
         Rehome_Prop=round(Rehomed/Outcome_Not_Reclaimed,3))

# Filter to focus on dogs and cats only
dogs_cats=state_totals%>%
  filter(animal_type %in% c("Dogs","Cats"))

# Visualise total positive outcome rate for dogs and cats by year
dogs_cats%>%
  filter(State=="Total")%>%
  ggplot(aes(x=factor(year),y=Positive_Outcome,col=animal_type))+
  geom_line(aes(group=animal_type),size=1)+
  geom_point(size=2)+
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
        legend.text = element_text(colour=Text_1,size=8),
        legend.title = element_text(colour=Text_1,size=10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title.position = "plot")+
  labs(title=paste0("Dogs consistently see more positive outcomes than cats, although the gap is narrowing"),
       subtitle=paste0("Total % of animals with definitive outcomes (not 'in stock' or 'currently in care') that are not euthanized"),
       caption="Data source: Australian RSPCA (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-21/readme.md)",
       y="Proportion with a positive outcome",
       x=NULL,
       color="Animal Type")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_color_manual(values=c(Highlight_1,Highlight_2))

# Visualise total reclaim rate for dogs and cats by year
dogs_cats%>%
  filter(State=="Total")%>%
  ggplot(aes(x=factor(year),y=Reclaim_Prop,col=animal_type))+
  geom_line(aes(group=animal_type),size=1)+
  geom_point(size=2)+
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
        legend.text = element_text(colour=Text_1,size=8),
        legend.title = element_text(colour=Text_1,size=10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title.position = "plot")+
  labs(title=paste0("Dogs are far more likely to be reclaimed than cats"),
       subtitle=paste0("Total % of animals reclaimed"),
       caption="Data source: Australian RSPCA (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-21/readme.md)",
       y="Relcaim Rate",
       x=NULL,
       color="Animal Type")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_color_manual(values=c(Highlight_1,Highlight_2))

# Visualise positive outcome rate for dogs and cats by year, by state
dogs_cats%>%
  filter(State!="Total")%>%
  ggplot(aes(x=factor(year),y=Positive_Outcome,col=animal_type))+
  geom_line(aes(group=animal_type),size=1)+
  geom_point(size=2)+
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
        legend.text = element_text(colour=Text_1,size=8),
        legend.title = element_text(colour=Text_1,size=10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title.position = "plot")+
  labs(title=paste0("Dogs consistently see more positive outcomes than cats, although the gap is narrowing"),
       subtitle=paste0("Total % of animals with definitive outcomes (not 'in stock' or 'currently in care') that are not euthanized"),
       caption="Data source: Australian RSPCA (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-21/readme.md)",
       y="Proportion with a positive outcome",
       x=NULL,
       color="Animal Type")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_color_manual(values=c(Highlight_1,Highlight_2))+
  facet_wrap(~State)

# Visualise reclaim rate for dogs and cats by year, by state
dogs_cats%>%
  filter(State!="Total")%>%
  ggplot(aes(x=factor(year),y=Reclaim_Prop,col=animal_type))+
  geom_line(aes(group=animal_type),size=1)+
  geom_point(size=2)+
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
        legend.text = element_text(colour=Text_1,size=8),
        legend.title = element_text(colour=Text_1,size=10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title.position = "plot")+
  labs(title=paste0("Dogs are far more likely to be reclaimed than cats"),
       subtitle=paste0("Total % of animals reclaimed"),
       caption="Data source: Australian RSPCA (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-21/readme.md)",
       y="Relcaim Rate",
       x=NULL,
       color="Animal Type")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_color_manual(values=c(Highlight_1,Highlight_2))+
  facet_wrap(~State)
