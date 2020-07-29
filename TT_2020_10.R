# Load libraries
library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(ggrepel)

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
tuesdata=tt_load(2020,week = 10)
df1=tuesdata$season_goals

# Limit to NHL seasons
NHL=df1%>%
  filter(league=="NHL")

# Create Player-Age Metrics Summary
Player_Age=NHL%>%
  group_by(player,age,status,headshot)%>%
  summarise(Games=sum(season_games),
            Goals=sum(goals),
            Assists=sum(assists),
            Plus_Minus=sum(plus_minus))%>%
  mutate(Goals_Per_Game=ifelse(Goals==0,0,round(Goals/Games,2)),
         Assists_Per_Game=ifelse(Assists==0,0,round(Assists/Games,2)),
         `G+A_Per_Game`=Goals_Per_Game+Assists_Per_Game)

# Create Player Career Summary
Player=Player_Age%>%
  group_by(player,status,headshot)%>%
  summarise(Age=max(age),
            Total_Games=sum(Games),
            Total_Goals=sum(Goals),
            Total_Assists=sum(Assists))%>%
  mutate(Goals_Per_Game=ifelse(Total_Goals==0,0,round(Total_Goals/Total_Games,2)),
         Assists_Per_Game=ifelse(Total_Assists==0,0,round(Total_Assists/Total_Games,2)),
         `G+A_Per_Game`=Goals_Per_Game+Assists_Per_Game)%>%
  arrange(desc(`G+A_Per_Game`))

Median_Assists=median(Player$Assists_Per_Game)
Median_Goals=median(Player$Goals_Per_Game)

Player=Player%>%
  mutate(Type=case_when(
    Assists_Per_Game > Median_Assists & Goals_Per_Game > Median_Goals ~ "All-Rounders",
    Goals_Per_Game > Median_Goals ~ "Finishers",
    Assists_Per_Game > Median_Assists ~ "Creators",
    TRUE ~ "Mere Mortals"
  ))

Greats=Player%>%
  head(10)

Labels=Player%>%
  group_by(Type)%>%
  mutate(Rank=rank(-`G+A_Per_Game`,ties.method = "first"))%>%
  filter(Rank<=5,
         Type!="Mere Mortals")

p1=Player%>%
  ggplot(aes(x=Goals_Per_Game,y=Assists_Per_Game))+
  geom_point(aes(col=Type,size=Total_Games),alpha=0.6,show.legend=FALSE)+
  geom_vline(xintercept = Median_Goals,linetype="dashed",col=Text_1)+
  geom_hline(yintercept = Median_Assists,linetype="dashed",col=Text_1)+
  scale_color_manual(values=c(Highlight_1,Highlight_2,Highlight_3,Contrast_1))+
  scale_size_continuous(range=c(0.5,5))+
  geom_text_repel(data=Labels,aes(x=Goals_Per_Game,y=Assists_Per_Game,label=player),col=Text_1,size=ifelse(Labels$player=="Wayne Gretzky",3,2),alpha=ifelse(Labels$player=="Wayne Gretzky",1,0.7),min.segment.length = unit(0, 'lines'),segment.size = 0.05)+
  scale_x_log10()+
  scale_y_log10()+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size=10,colour=Text_1),
        axis.text.y = element_text(size=8,colour=Text_1),
        axis.line = element_line(colour = Contrast_1),
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
  labs(title=paste0("The Great One: Few players even come close to Gretzky's goal involvement"),
       subtitle=paste0("NHL career goals and assists per game for the top 250 goalscorers since 1979-80 season (bigger dots indicate more games played)"),
       caption="Data source: HockeyReference.com via TidyTuesday (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-03/readme.md)",
       y="Assists Per Game",
       x="Goals Per Game")+
  annotate(geom="text",x=0.2,y=0.2,label="Mere Mortals",col=Text_2,size=4,alpha=0.3,fontface=2)+
  annotate(geom="text",x=0.2,y=1.2,label="Creators",col=Text_2,size=4,alpha=0.3,fontface=2)+
  annotate(geom="text",x=0.6,y=1.2,label="All-Rounders",col=Text_2,size=4,alpha=0.3,fontface=2)+
  annotate(geom="text",x=0.6,y=0.2,label="Finishers",col=Text_2,size=4,alpha=0.3,fontface=2)

setwd("~/Personal/Projects/Tidy Tuesday")
ggsave(p1,file="TT_2020_Wk10.png",width=9,height=6)