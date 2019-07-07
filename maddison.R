library(maddison)
library(tidyverse)
library(directlabels)

library(gganimate)
library(gifski)
library(png)

library(ggthemes)
theme_set(theme_economist())

head(maddison)

dados <- subset(maddison, 
                year >= as.Date("1800-01-01"))

dados_tibble <- as_tibble(dados)

dados_tibble

projeto <- dados_tibble %>% 
  select(year, iso3c, gdp_pc,country, continent)
  
projeto_2 <- projeto %>% 
  filter(is.na(gdp_pc)==F, is.na(iso3c) ==F, is.na(continent)==F,
         is.na(year)==F, year >= 1500) 

paises <-  c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore", "Australia") 



p2 <- ggplot(projeto_2) +
 aes(x = year, y = gdp_pc, colour = country) +
 geom_line(size = 1L) + 
  scale_color_viridis_d(option = "inferno") +
 theme_classic() + theme(legend.position = "none") +
 facet_wrap(vars(continent), scales = "free") 

p2 

p2 + geom_text(data=projeto_2 %>% group_by(country) %>%
                 filter(country %in% paises) %>% 
                  slice(1) ,
                aes(x = max(projeto_2$year) + 10 , label=country), hjust=1)

direct.label(p2, method="last.points")



projeto_3 <- projeto_2 %>% 
            filter(country %in% paises) %>% 
            mutate(log.gdp = log(gdp_pc))
            
            
  p3 <-     ggplot(projeto_3) +
              aes(x = year, y = log.gdp, colour = country) +
              geom_line(size = 1L) + 
              theme_bw() +
              facet_wrap(vars(continent), scales = "free") +
    theme(strip.background = element_blank(), 
          strip.placement = "outside") +
    xlim(as_date("1970-01-01"),as_date("2020-01-01"))
  
  p3
  
  direct.label(p3,"last.bumpup")
  
  
  
  projeto_4 <- projeto_2 %>% 
    filter(country %in% paises) 
    
  
  p3 <-     ggplot(projeto_4) +
    aes(x = year, y = gdp_pc, colour = country) +
    geom_line(size = 1L) +
    facet_wrap(vars(continent), scales = "fixed") +
    theme(strip.background = element_blank(), 
          strip.placement = "outside") +
    theme_economist() +
    xlim(as_date("1970-01-01"),as_date("2020-01-01")) +
    ggtitle("Evolução da Renda per capita no Mundo. Países Selecionados", "1970 - 2010")
  
  p4 <- direct.label(p3,"last.bumpup")
  
  p4 + transition_reveal(year)

  anim_save(filename = "gdp_mundo.gif", path = "C:\\Users\\JRCD\\Dropbox")  
  
  
  