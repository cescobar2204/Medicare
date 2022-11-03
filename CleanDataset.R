# Cargo las librerías que utilizaremos para el manejo de la data
library(dplyr)

# Abro el documento desde la web
url= "https://docs.google.com/spreadsheets/d/e/2PACX-1vRjyv0fjRaVIQ36opIoPa9I1P63aqP2GUS3jifA_mQWfBbsCPJNlCglV34BNrAji1bI8Rny8wxNUP1C/pub?output=csv"
df<-read.csv(url, sep=",",header = F,encoding = "UTF-8")

#Renombro las columnas
colnames(df)<- c("countyname","state","contract","healthplanname","typeofplan","countyssa","eligibles","enrollees","penetration","ABrate")

#Reemplazo los valores vacíos de eligibles, enrollees y penetration con 0
df<-df%>%
  mutate(eligibles=ifelse(is.na(eligibles),0,eligibles),
         enrollees=ifelse(is.na(enrollees),0,enrollees),
         penetration=ifelse(is.na(penetration),0,penetration))

#Creo la variable numberofplans1
df1<-df%>%
  group_by(countyname,healthplanname)%>%
  mutate(numberofplans1=ifelse(enrollees>10,1,0))%>%
  ungroup()%>%
  filter(numberofplans1==1)%>%
  group_by(countyname)%>%
  summarize(numberofplans1=n())

#Creo la variable numberofplans2
df2<-df%>%
  group_by(countyname,healthplanname)%>%
  mutate(numberofplans2=ifelse(penetration>0.5,1,0))%>%
  ungroup()%>%
  filter(numberofplans2==1)%>%
  group_by(countyname)%>%
  summarize(numberofplans2=n())
  
#Creo la variable eligibles, totalenrollees y totalpenetration por county
df3<-df%>%
  group_by(countyname)%>%
  summarize(eligibles=sum(eligibles),totalenrollees=sum(enrollees),totalpenetration=totalenrollees/eligibles)%>%
  ungroup()%>%
  left_join(df1,by="countyname")%>% #Lo uno con la data de numberofplans1 y numberofplans2
  left_join(df2,by="countyname")%>%
  mutate(numberofplans1=ifelse(is.na(numberofplans1),0,numberofplans1),
         numberofplans2=ifelse(is.na(numberofplans2),0,numberofplans2))

# Uno la data con las variables state, countyssa. En este caso el countyname se duplica en algunos ya que hay countys con mas de un state.
df4<-df%>%
  dplyr::select(countyname,state,countyssa)%>%
  unique()%>%
  right_join(df3,by="countyname")%>%
  arrange(state,countyname)%>% #ordeno por state y luego por county
  filter(!countyname %in% c("UNDER-11","Unusual SCounty Code")) #filtro los casos que no son county


