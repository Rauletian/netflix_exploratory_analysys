library(tidyverse)
library(dplyr)
library(readxl)
library(ggthemes)
netflix_titles <- read_csv("R/datasets/netflix_titles.csv")
hdro_statistical_data_tables_1_15_d1_d5 <- read_excel("R/datasets/hdro_statistical_data_tables_1_15_d1_d5.xlsx")

hdro_statistical_data_tables_1_15_d1_d5%>%
  select(2,3)->aux

names(aux)[names(aux) == "Table 1. Human Development Index and its components"] <- "main_country"
names(aux)[names(aux)=="...3"]<-"values"


netflix_titles%>%
  distinct(show_id,.keep_all = TRUE)->netflix_titles

netflix_titles%>%
  separate(country,
           into=c("main_country", "other_country"),
           sep=",",
           convert=F,
           fill="right",
           extra="merge",
           remove = T)->netflix_titles

netflix_titles%>%
  left_join(aux, by="main_country")->netflix_titles



netflix_titles%>%
  distinct(show_id,.keep_all = TRUE)->netflix_titles

  


netflix_titles%>%
  separate(date_added, into=c("month", "dayear"), 
           extra = "merge", sep = " ")%>%
  separate(dayear, into=c("day","year"), 
           sep=",")->netflix_titles

netflix_titles%>%
  separate(listed_in, 
           into=c("listed_main","listed_other1","
                  listed_other2"),sep=",",
           extra="merge")->netflix_titles

netflix_titles%>%
  mutate(idh_ranking=(case_when(values>0.8~"Very High",values>0.7~"High",values>0.55~"Medium",values>0.35~"Low")))->netflix_titles



netflix_titles%>%
  filter(type=="Movie")%>%
  group_by(main_country)%>%
  separate(duration,into=c("dur","min"))->netflix_movies

transform(netflix_movies, durInt = as.numeric(dur))->netflix_movies

#netflix_movies%>%
  #na.exclude()->netflix_movies



##----count--------
netflix_movies%>%
  group_by(main_country)%>%
  mutate(isna=is.na(main_country))%>%
  filter(isna==FALSE)%>%
  summarise(count=n(), meanDur=mean(durInt))->resume


resume%>%
  slice_max(order_by=count, n=10)%>%
  ggplot(aes(y=main_country))+
  geom_col(aes(x=count),fill="sky blue")+
  xlab("country")+
  ylab("Number of movies on the platform")+
  ggtitle("Main country of origin of films")+
  theme_gdocs(base_size = 7, base_family = "Courier")+
  theme(title = element_text(hjust = 0.5))
 


netflix_movies%>%
  group_by(idh_ranking)%>%
  summarise(count=n(), meanDur=mean(durInt))%>%
  na.exclude()->resume_idh

resume_idh%>%
  ggplot(aes(y=idh_ranking))+
  geom_col(aes(x=count), fill="sky blue",width=0.6)+
  xlab("IDH")+
  ylab("Number of movies on the platform")+
  ggtitle("Number of movies by IHD ranking")+
  theme_gdocs(base_size = 7)+
  theme(title = element_text(hjust = 0.5))

netflix_movies%>%
  filter(idh_ranking=="Medium")%>%
  ggplot(aes(y=main_country))+
  geom_bar(fill="sky blue")+
  ggtitle("Number of movies for Medium IDH countries")+
  xlab("Country")+
  ylab("Number of movies")+
  theme_gdocs(base_size = 7)+
  theme(title = element_text(hjust = 0.5))

netflix_movies%>%
  filter(idh_ranking=="High")%>%
  ggplot(aes(y=main_country))+
  geom_bar(fill="sky blue")+
  ggtitle("Number of movies for High IDH countries")+
  xlab("Country")+
  ylab("Number of movies")+
  theme_gdocs(base_size = 7)+
  theme(title = element_text(hjust = 0.5))



####------Géneros--------- <- -

netflix_movies%>%
  group_by(listed_main)%>%
  summarise(count=n())%>%
  filter(count>30)%>%
  ggplot(aes(y=listed_main))+
  geom_col(aes(x=count), fill="sky blue", width = 0.8)+
  ggtitle("Number of movies by type")+
  ylab("Count")+
  xlab("Main Genre")+
  theme_classic()+
  labs(fill = "Genre")+
  theme(legend.key.size=unit(0.8, "lines"))+
  theme_gdocs(base_size = 7)+
  theme(title = element_text(hjust = 0.5))

netflix_movies%>%
  group_by(listed_main, idh_ranking)%>%
  summarise(count=n())%>%
  filter(count>5)%>%
  ggplot(aes(y=listed_main))+
  geom_col(aes(x=count), fill="sky blue")+
  ggtitle("Number of movies by type")+
  ylab("Count")+
  xlab("Main Genre")+
  labs(fill = "Genre")+
  facet_wrap(~idh_ranking, 2)+
  theme_gdocs(base_size = 10)+
  theme(title = element_text(hjust = 0.5))

  



##----Duración-------------




netflix_movies%>%
  filter(main_country!="United States")->netflix_movies_int

netflix_movies%>%
  ggplot(aes(x=durInt))+
  geom_freqpoly(binwidth=40, color="tan")+
  geom_freqpoly(data=netflix_movies_int, binwidth=40, color="blue")+
  ggtitle("Distribution of movies acording to duration")+
  xlab("Duration(Min)")+
  ylab("Number of movies")+
  theme(plot.title = element_text(lineheight=.3, face="bold"))+
  theme_minimal()+
  theme_gdocs(base_size = 7)+
  theme(title = element_text(hjust = 0.5))


netflix_movies%>%
  mutate(IDH_ERROR=is.na(idh_ranking))%>%
  filter(IDH_ERROR==FALSE)%>%
  ggplot(aes(x=durInt, color=idh_ranking))+
  geom_freqpoly(binwidth=50)+
  ggtitle("Distribution of movies acording to duration")+
  xlab("Duration(Min)")+
  ylab("Number of movies")+
  theme(plot.title = element_text(lineheight=.3, face="bold"))+
  theme_minimal()+
  theme_gdocs(base_size = 8)+
  theme(title = element_text(hjust = 0.5))


###-Release date-----

netflix_movies$month<-factor(netflix_movies$month, 
                             levels = c("January","February","March",
                                        "April","May","June",
                                        "July","August",
                                        "September","October",
                                        "November", "December"))


netflix_movies%>%
  mutate(IDH_ERROR=is.na(idh_ranking))%>%
  filter(IDH_ERROR==FALSE)%>%
  ggplot(aes(y=month,fill=idh_ranking))+
  geom_bar()+
  ggtitle("Distribution of movies acording to release date")+
  xlab("Month")+
  ylab("Number of movies")+
  theme(plot.title = element_text(lineheight=.3, face="bold"))+
  theme_minimal()+
  theme_gdocs(base_size=7)+
  labs(fill="IDH")+
  theme(title = element_text(hjust = 0.5))

#---------types-grupos_idh-----------

