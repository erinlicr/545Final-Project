install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("tibble")
install.packages("gridExtra")
install.packages("readxl")
install.packages("tidyr")
install.packages("maps")
install.packages("mapproj")

library(dplyr)
library(tidyverse)
library(ggcorrplot)
library(ggplot2)
library(tibble)
library(gridExtra)
library(readxl)
library(tidyr)
library(maps)
library(mapproj)

athletes=read_xlsx('/Users/Acer/Desktop/22Spring-16278-DSO545/final project-winter olympics/Athletes.xlsx')
coaches=read_xlsx('/Users/Acer/Desktop/22Spring-16278-DSO545/final project-winter olympics/Coaches.xlsx')
CHE=read_csv('/Users/Acer/Desktop/22Spring-16278-DSO545/final project-winter olympics/CHE.csv')
medals=read_xlsx('/Users/Acer/Desktop/22Spring-16278-DSO545/final project-winter olympics/Medals_2022.xlsx')
gdp=read_csv('/Users/Acer/Desktop/22Spring-16278-DSO545/final project-winter olympics/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3840542.csv')
interest =read_xlsx('/Users/Acer/Desktop/22Spring-16278-DSO545/final project-winter olympics/interest in the Olympics.xlsx')
happiness = read_csv('/Users/Acer/Desktop/22Spring-16278-DSO545/final project-winter olympics/Happiness2019.csv')


##merge the main table1 ##
amount_athletes=athletes%>%
  group_by(NOC)%>%
  summarise(athletes_amount=n())
amount_athletes
amount_coaches=athletes%>%
  group_by(NOC)%>%
  summarise(coaches_amount=n())
amount_coaches

CHE=CHE%>%
  filter(Period==2019)%>%
  select(Location,FactValueNumeric)

gdp=gdp%>%
  select(`Country Name`,`2019`)

table1=medals%>%
  left_join(amount_athletes,NOC=NOC)%>%
  left_join(amount_coaches,NOC=NOC)%>%
  left_join(interest,NOC=NOC)%>%
  left_join(happiness,NOC=NOC)
table1=merge(x=table1,y=CHE,by.x='NOC',by.y='Location',all.x=TRUE)
table1=merge(x=table1,y=gdp,by.x='NOC',by.y='Country Name',all.x=TRUE)

names(table1)[19]='CHE_values_2019'
names(table1)[20]='gdp_2019'
table1 = table1 %>%
  select(NOC:`Interested Rate`, Score, 'CHE_values_2019':'gdp_2019')
colnames(happiness)[2] = "NOC"


ggcorrplot(cor(table1 %>% select_if(is.numeric)),lab = FALSE)

table1=select(table1,-coaches_amount)
table1

write.csv(table1,"table1.csv")
summary(table1)

summarize_numeric= function(dataset){
  dataset=select_if(dataset, is.numeric)
  summary.table=data.frame(Attribute=names(dataset))
  
  summary.table=summary.table %>%
    mutate('Missing Values'= apply(dataset, 2, function(x) sum(is.na(x))),
           'Unique Values'=apply(dataset,2,function(x)length(unique(x))),
           'Mean'=apply(dataset,2,function(x)max(x,na.rm=TRUE)),
           'Min'=apply(dataset,2,function(x)max(x,na.rm=TRUE)),
           'SD'=apply(dataset,2,function(x)sd(x,na.rm=TRUE))
    )
  summary.table
   }


  table1$Gold=as.numeric(table1$Gold)
  table1$Silver=as.numeric(table1$Silver)
  table1$Bronze=as.numeric(table1$Bronze)
  table1$Total=as.numeric(table1$Total)
  summarize_numeric(select(table1,-NOC,-Rank,-`Rank by Total`))

  table1
  
  
  ####6.2 medals per country  ###
  
  table10 = medals %>%
    select(NOC:Bronze) %>%
    head(12) 
    
    
  table1edit = table10 %>%
    gather(`Gold`,`Silver`,`Bronze` , key = "Medals", value = "count" ) %>%
    filter(count != 0) 
  table1edit$Medals=as.factor(table1edit$Medals)
  table1edit$count=as.numeric(table1edit$count)
  
  ggplot(table1edit, aes(x = NOC, y = count, group = NOC, fill = Medals)) +
    geom_col()+
    scale_fill_manual(values = c("tan3", "gold1", "gainsboro"))+
    xlab("NOC") +
    ylab("Medals") +
    ggtitle("Medals per country") +
    geom_text(aes(x= NOC, y= count, label = count), position = position_stack(vjust=0.5), alpha = 0.3) 
  
  ###6.3 gold medal per country (spacial) ###
  world_map = map_data("world")
  tablemap = table1
  colnames(world_map)[5] = "NOC"
  tablemap = left_join(tablemap,world_map, by = "NOC")
  ggplot(tablemap, aes(long, lat, group = group, fill = Gold)) +  
    geom_polygon(colour = "grey50") +
    scale_fill_gradient(low = "white", high = "orange")+
    coord_map()   +
    ggtitle("Gold medal per country (spacial)") 

  ###6.4 gdp per country (spacial) ###
  ggplot(tablemap, aes(long, lat, group = group, fill = gdp_2019)) +  
    geom_polygon(colour = "grey50") +
    scale_fill_gradient(low = "white", high = "purple")+
    ggtitle("GDP per country (spacial)") +
    coord_map()  
  
  ###6.8 cor ###
  
  g1=ggplot(table1) + geom_histogram(aes(x=Gold))
  g1
  
  g2=ggplot(table1) + geom_histogram(aes(x=Silver))
  g2
  
  g3=ggplot(table1) + geom_histogram(aes(x=Bronze))
  g3
  
  g4=ggplot(table1) + geom_histogram(aes(x=Total))
  g4
  
  g5=ggplot(table1) + geom_histogram(aes(x=athletes_amount))
  g5
  
  g6=ggplot(table1) + geom_histogram(aes(x=CHE_values_2019))
  g6
  
  g7=ggplot(table1) + geom_histogram(aes(x=gdp_2019))
  g7
  
  #Bivariate Analysis   NOC and Rank
  table1$Rank=as.numeric(table1$Rank)
  toprank=head(arrange(table1,Rank),10)
  g8=ggplot(toprank,aes(x=reorder(NOC,Rank),y=Rank))+geom_col()
  g8

  topgold=head(arrange(table1,desc(Gold)),10)
  g9=ggplot(topgold,aes(x=reorder(NOC,-Gold),y=Gold))+geom_col()
  g9
  
  topsilver=head(arrange(table1,desc(Silver)),10)
  g10=ggplot(topsilver,aes(x=reorder(NOC,-Silver),y=Silver))+geom_col()
  g10
  
  topbronze=head(arrange(table1,desc(Bronze)),10)
  g10=ggplot(topbronze,aes(x=reorder(NOC,-Bronze),y=Bronze))+geom_col()
  g10
  
  toptotal=head(arrange(table1,desc(Total)),10)
  g11=ggplot(toptotal,aes(x=reorder(NOC,-Total),y=Total))+geom_col()
  g11
  
  g12=ggplot(table1,aes(x=Total,y=athletes_amount))+geom_point()
  g12
  
  g13=ggplot(table1,aes(x=Total,y=CHE_values_2019))+geom_point()
  g13
  
  g14=ggplot(table1,aes(x=Total,y=gdp_2019))+geom_point()
  g14
