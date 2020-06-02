#RECT_west2019<-left_join(read.csv("data/SurvRects_W_2019.csv")%>%distinct(),RECTall_2019, by=c("HALFST"="RECT"))%>%
rename(RECT=HALFST)%>%
  select(RECT,lat,lon)%>%
  distinct()%>%
  bind_rows(select(RECT_west_df,RECT,lat,lon))%>%
  distinct()%>%
  left_join(RECTall_2019)%>%
  bind_rows(RECT_NOR)%>% ##adding Norway waters
  distinct()%>%
  select(-radian,-cos)%>%
  mutate(sea_ratio=replace_na(sea_ratio,1),RECT=as.factor(as.character(RECT)),R2=as.factor(as.character(R2)) )%>%
  rowwise()%>%
  mutate( Area_minus_land=Area*sea_ratio, Year=2019)%>%
  droplevels()%>%
  data.frame

summary(RECT_west2019)

