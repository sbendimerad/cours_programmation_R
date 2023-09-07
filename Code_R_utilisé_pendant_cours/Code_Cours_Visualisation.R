setwd("C:/Users/J00380/Desktop/Documents_cours_Programmation_R/cours")

library(tidyverse)
library(ggplot2)





head(mpg)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))





head(diamonds)
ggplot(diamonds)
ggplot(diamonds, aes(carat))
ggplot(diamonds, aes(carat)) + geom_histogram()
       


g = ggplot(diamonds, aes(carat))+geom_histogram()
g


g = ggplot(diamonds, aes(carat))+geom_histogram()
g+ggtitle("Distribution des carats")+theme_classic()


?theme_light

g = ggplot(diamonds, aes(carat))+geom_histogram()
g+labs(title="Distribution des carats",
       x="Carats", y="Effectif")


ggplot(diamonds, aes(x=""))+geom_bar()

ggplot(diamonds, aes(x="", fill=cut))+geom_bar()


library(scales)
ggplot(diamonds, aes(x="", fill=cut))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = percent)




ggplot(diamonds, aes(x="", fill=cut))+
  geom_bar(aes(y=..count../sum(..count..)))


ggplot(diamonds, aes(x="", fill=cut))+
  geom_bar(position = "fill")



library(scales)
ggplot(diamonds, aes(x="", fill=cut))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = percent)


ggplot(diamonds, aes(x = "", fill = cut)) +
  geom_bar(position = "fill", width = 1) +
  scale_y_continuous(labels = percent) +
  coord_polar(theta = "y")


ggplot(diamonds, aes(x = "", fill = cut)) +
  geom_bar(position = "fill", width = 1) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = percent) +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  labs(fill = "Variable cut")



g = ggplot(diamonds, aes(carat, price))+geom_point()
g
g = g+geom_rug()
g
g = g+geom_smooth()
g

g = ggplot(diamonds, aes(carat, price))+geom_point()
g+geom_smooth(method = "lm", se = FALSE)


install.packages("geojsonio")
library(geojsonio)
g = ggplot(diamonds, aes(carat, price, shape=cut, col=cut))+geom_point()
g+geom_smooth(se = FALSE)

g = ggplot(diamonds, aes(carat, price))+geom_point()
g
g+geom_smooth(se=FALSE)+facet_wrap(facets=~cut)

g = ggplot(diamonds, aes(carat, price))+geom_point()
g+geom_smooth(se=FALSE)+facet_wrap(facets=~cut, ncol=2)

g = ggplot(diamonds, aes(color, carat))+geom_boxplot()
g+facet_grid(facets=~cut)

g = ggplot(diamonds, aes(carat))+geom_histogram()
g+facet_grid(facets=cut~.)


g = ggplot(diamonds, aes(carat))+geom_histogram()
g+facet_grid(facets=cut~., scales="free_y")




g = ggplot(diamonds, aes(carat, price)) + geom_point() +
  geom_smooth(method = "lm", fullrange = T)
g+facet_grid(facets = color ~ cut)


library(dplyr)
library(tidyr)



g = ggplot(iris, aes(x="", fill=Species))+
  geom_bar()+
  labs(
    title="Distribution des especes d'iris", 
    x=" ", 
    y="Effectif"
  )



g
g1 = g+theme(legend.position = "right")
g1
g2 = g+labs(fill="Espece")
g2



diamonds1 = diamonds %>% select(carat, price, depth, cut) %>%  gather()


diamonds2 = diamonds %>% select(carat, price, depth, cut) %>%
  gather(carat, key="key", value="value")

diamonds3 = diamonds %>% select(carat,price,depth) %>% 
  gather(-carat, key = "var", value = "value")  


diamonds %>% select(carat,price,depth) %>% 
  gather(-carat, key = "var", value = "value") %>% 
  ggplot(aes(carat, value))+geom_point()+
  facet_grid(facets=var~., scales="free_y")

diamonds4 = diamonds %>% select(carat,price,depth, cut) %>% 
  gather(-carat, -cut, key = "var", value = "value")

diamonds %>% select(carat,price,depth, cut) %>% 
  gather(-carat, -cut, key = "var", value = "value") %>% 
  ggplot(aes(carat, value, color=cut))+geom_point()+
  facet_grid(facets=var~., scales="free_y")



df = diamonds %>%
  group_by(cut) %>%
  summarise(
    mean = mean(price, na.rm = T),
    sd = sd(price, na.rm = T)
  )





ggplot(diamonds, aes(cut, price, color = cut)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  geom_boxplot(show.legend = FALSE) +

   geom_point(data = df, 
              mapping = aes(y = mean), 
              col = "steelblue", size = 2) +
 theme_light()



library(geojsonio)
etats = geojson_read(
  'https://datahub.io/core/geo-countries/r/countries.geojson',
  what = "sp")  
  

etats_f = fortify(etats)


ggplot(etats_f, aes(long, lat, group = group)) +
  geom_polygon(color = "white") +
  theme_void()




library(tibble) # pour rownames_to_column
etats@data$var = runif(nrow(etats@data))

etats_f = fortify(etats)

etats_fj = etats_f %>%
  inner_join(etats@data %>% rownames_to_column("id"),
             by = "id") 

ggplot(etats_fj,
       aes(long, lat, group = group, fill = var)
) +
  geom_polygon(color = "white") +
  theme_void()



library(leaflet)
leaflet(etats) %>%
  addTiles() %>%
  addPolygons()


palette = colorBin(
  "magma",
  domain = etats$var,
  bins = seq(0, 1, by = .2)
)


leaflet(etats) %>%
  addTiles() %>%
  addPolygons(fillColor = ~palette(var),
              fillOpacity = .5)



leaflet(etats) %>%
  setView(lat = 0, lng = 0, zoom = 1) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette(var),
    fillOpacity = .5,
    color = "gray30",
    weight = 1.5,
    opacity = 1,
    dashArray = "2"
  ) %>%
  addLegend(
    pal = palette,
    values = ~var,
    opacity = 0.8,
    title = "Variable aleatoire",
    position = "bottomright"
  )
