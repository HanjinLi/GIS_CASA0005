library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(stats)
library(tmaptools)
library(spatstat)
library(here)
library(rgeos)
library(maptools)
library(GISTools)
library(geojson)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(spdep)
library(psych)
library(reticulate)
library(forecast)
library(MLmetrics)
library(Rcpp)
library(raster)
library(lctools)
county <- st_read("/Users/HanjinLee/Desktop/gis/shp/US_county.shp")
covid <- read_csv("/Users/HanjinLee/Desktop/gis/Data.csv")

#covid_sub <- covid %>%
#  dplyr::filter(str_detect(date, "0826"))


Join <- county%>%
  left_join(.,covid,  by = c("Location" = "location"))

#graduate color preview of 0826 covid cases
tm_shape(Join) + 
  tm_polygons("PctCases",style="quantile", 
              palette=get_brewer_pal(palette="OrRd", plot=FALSE))


#leaflet web interface of 0826 covid cases
m <- leaflet(covid) %>% setView(lng = -98, lat = 41, zoom = 4)
m %>% 
  addProviderTiles(providers$CartoDB.Positron)%>% 
  addCircleMarkers(
    radius = ~ PctCases/80,
    stroke = FALSE, fillOpacity = 0.5
  )


points_joined <- county%>%
  left_join(.,covid,  by = c("Location" = "location"))%>%
  add_count(PctCases)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(density=n/area)

points_joined<- na.omit(points_joined)


#global Moran'I, Geary's C for autocorrelation
points_coordsW <- points_joined%>%
  st_centroid()%>%
  st_geometry()
points_nb <- points_joined %>%
  poly2nb(., queen=T)
summary(points_nb)
plot(points_nb, st_geometry(points_coordsW), col="red")
plot(points_joined$geometry, add=T)
points_lw <- points_nb %>%
  nb2mat(., style="B", zero.policy = TRUE)
sum(points_lw)
points_lw <- points_nb %>%
  nb2listw(., style="C", zero.policy = TRUE)
sagd <- points_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., points_lw, zero.policy = TRUE)

sagd

gearyC <- 
  points_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., points_lw, zero.policy = TRUE)

gearyC

#local Moran's I for hotspot

local_c <- points_joined %>%
  pull(pct_cases) %>%
  as.vector()%>%
  localmoran(., points_lw, zero.policy = TRUE)%>%
  as_tibble()

local_d <- points_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., points_lw, zero.policy = TRUE)%>%
  as_tibble()

points_joined <- points_joined %>%
  mutate(lc = as.numeric(local_c$Ii))%>%
  mutate(lcz =as.numeric(local_c$Z.Ii))%>%
  mutate(di =as.numeric(local_d$Ii))%>%
  mutate(diz =as.numeric(local_d$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MColor<- rev(brewer.pal(8, "Spectral"))
tm_shape(points_joined) +
  tm_polygons("lcz",
              style="fixed",
              breaks=breaks1,
              palette=MColor,
              midpoint=NA,
              title="Local Moran's I")



#multicolinearity 
covid <- read_csv("/Users/HanjinLee/Desktop/gis/Data.csv")
covid <- subset(covid, select = -location)
model <-  lm(PctCases~.,data=covid)
vif(model)

#Homoscedasticity
par(mfrow = c(2, 3))
model1 <-  lm(PctCases~Area,data=covid)
model2 <-  lm(PctCases~PctVentila,data=covid)
model3 <-  lm(PctCases~Traffic,data=covid)
model4 <-  lm(PctCases~AgeDistrib,data=covid)
model5 <-  lm(PctCases~PctBeds,data=covid)
model6 <-  lm(PctCases~temp,data=covid)

plot(model1,3)
plot(model2,3)
plot(model3,3)
plot(model4,3)
plot(model5,3)
plot(model6,3)



#GWR 

covid <- read_csv("/Users/HanjinLee/Desktop/gis/Data.csv")
covid<- na.omit(covid)
gwpr <- gw.glm(PctCases ~ Area + PctVentila + Traffic + AgeDistrib + PctBeds + temp,  
               "poisson", covid, 33, kernel = 'adaptive', cbind(covid$long,covid$lat))

pred <- gwpr$GGLM_GofFit$GLM_yfit
res <- gwpr$GGLM_GofFit$GLM_Res

data <- data.frame(cbind(covid$location,covid$PctCases,
                         gwpr$GGLM_LPvalues,gwpr$GGLM_GofFit,gwpr$GGLM_LEst))
#colnames(data) <- c('location','PctCases','pred',"res")
#write.csv(data,"/Users/HanjinLee/Desktop/gis/result_4889.csv", row.names = FALSE)


#graduate color preview of 0826 covid cases
county <- st_read("/Users/HanjinLee/Desktop/gis/shp/US_county.shp")
Join <- county%>%
  left_join(.,data,  by = c("Location" = "covid.location"))


#residual plot
tm_shape(Join) + 
  tm_polygons("GLM_Res",palette = "Reds", 
              border.alpha = 0, plot=FALSE)

#plot significant P value and coefficient 
breaks <- c(0,0.05,0.1,1)
tm1 <- tm_shape(Join) + 
  tm_polygons("P_Area",palette = "Reds", breaks = breaks,
              border.alpha = 0, plot=FALSE)
tm2 <- tm_shape(Join) + 
  tm_polygons("P_PctVentila",palette = "Reds", breaks = breaks,
              border.alpha = 0, plot=FALSE)
tm3 <- tm_shape(Join) + 
  tm_polygons("P_Traffic",palette = "Reds", breaks = breaks,
              border.alpha = 0, plot=FALSE)
tm4 <- tm_shape(Join) + 
  tm_polygons("P_AgeDistrib",palette = "Reds", breaks = breaks,
              border.alpha = 0, plot=FALSE)
tm5 <- tm_shape(Join) + 
  tm_polygons("P_PctBeds",palette = "Reds", breaks = breaks,
              border.alpha = 0, plot=FALSE)
tm6 <- tm_shape(Join) + 
  tm_polygons("P_temp",palette = "Reds", breaks = breaks,
              border.alpha = 0, plot=FALSE)
tmap_arrange(tm1, tm2,tm3, tm4,tm5, tm6)

#coefficient plot
tm1 <- tm_shape(Join) + 
  tm_polygons("Area",palette = "Reds",
              border.alpha = 0, plot=FALSE)
tm2 <- tm_shape(Join) + 
  tm_polygons("PctVentila",palette = "Reds",
              border.alpha = 0, plot=FALSE)
tm3 <- tm_shape(Join) + 
  tm_polygons("Traffic",palette = "Reds", 
              border.alpha = 0, plot=FALSE)
tm4 <- tm_shape(Join) + 
  tm_polygons("AgeDistrib",palette = "Reds", 
              border.alpha = 0, plot=FALSE)
tm5 <- tm_shape(Join) + 
  tm_polygons("PctBeds",palette = "Reds", 
              border.alpha = 0, plot=FALSE)
tm6 <- tm_shape(Join) + 
  tm_polygons("temp",palette = "Reds", 
              border.alpha = 0, plot=FALSE)
tmap_arrange(tm1, tm2,tm3, tm4,tm5, tm6)

