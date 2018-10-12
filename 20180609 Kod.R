# Deklaracja sciezki do bilbiotek
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "en")

##########################################################################
#### upewnienie siê ¿e nie ma ¿adnych pakietów za³adowanych ####
gc(reset = TRUE)
rm(list = ls())
#od³¹czeni wszytkich pakietów - stowrzebnuie funkcji
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo³anie funkcji 


##########################################################################
#### za³adowanie pakietów ####

library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
library(readxl)
#library(reshape2)
#library(gridExtra)
#library(grid)

#library(ggmap) #do zbierania wspó³rzednych
#library(sp)  #do zbierania wspó³rzednych
library(grid) #do polaczenia wykresow w jeden
library(gridExtra) #do polaczenia wykresow w jeden
#install.packages("extrafont")
library(extrafont) #do czcionek
#font_import()
loadfonts()
#################################################################\
#potrzebne funkcje do wykresów

#funkcja do ³adnego formaowania liczb
space <- function(x, ...) { 
  format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)
}
# Multiple plot function
# Ÿród³o: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


##########################################################################
#### wczytanie danych ####
setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\Przynale¿nosæ Wroc³awia")

#kraj
Country  <- read.csv2("czyj.csv") %>%
  mutate(Year    = Od,
         Country = Kraj) %>%
  select(Year, Country) %>%
  arrange(Year) 

#Populacja
Pop      <- read.csv2("Ludnosc.csv") %>%
  mutate(Year = as.numeric(Rok),
         Pop = as.numeric(Ludnosc)) %>%
  select(Year, Pop) %>%
  arrange(Year) %>%
  group_by(Year) %>%
  summarise(Pop = mean(Pop)) %>%
  mutate(Year_lead = lead(Year),
         Pop_lead  = lead(Pop),
         Year_diff = Year_lead - Year,
         Pop_diff  = Pop_lead  - Pop,
         Pop_add   = Pop_diff / Year_diff) %>%
    select(Year, Pop, Pop_add) 

#powierzchnia
Land <- read.csv2("Powierzchnia.csv")  %>%
  mutate(Year = as.numeric(Rok),
         Land = as.numeric(Powierzchnia)) %>%
  select(Year, Land) %>% 
  filter(!is.na(Year)) %>%
  arrange(Year) %>%
  group_by(Year) %>%
  summarise(Land = mean(Land)) %>%
  mutate(Year_lead = lead(Year),
         Land_lead = lead(Land),
         Year_diff = Year_lead - Year,
         Land_diff = Land_lead  - Land,
         Land_add  = Land_diff / Year_diff) %>%
  select(Year, Land, Land_add) 

##########################################################################
#### stworzenie tabeli danych ####

Data_long <- data.frame(Year = 850:2018) %>%
  merge(Pop, all.x = T)  %>%
  merge(Land, all.x = T) %>%
  merge(Country, all.x = T) %>%
  mutate(temp = 1,
         Country = as.character(Country),
         Country = factor(Country, levels = c("Polska", "Czechy", "Œl¹sk", "Wêgry", "Niemcy", "Francja")))
  

for(i in 1:nrow(Data_long)){
  print(i)
  if(is.na(Data_long[i, "Pop"])) {
    Data_long[i, "Pop"]     <- Data_long[i - 1, "Pop_add"] + Data_long[i - 1, "Pop"] 
    Data_long[i, "Pop_add"] <- Data_long[i - 1, "Pop_add"] 
  }
  if(is.na(Data_long[i, "Land"])) {
    Data_long[i, "Land"]     <- Data_long[i - 1, "Land_add"] + Data_long[i - 1, "Land"] 
    Data_long[i, "Land_add"] <- Data_long[i - 1, "Land_add"] 
  }
  if(is.na(Data_long[i, "Country"])) {
    Data_long[i, "Country"]     <- Data_long[i - 1, "Country"] 
  }
}



##########################################################################
#### Wykresy ####


Theme <-  theme(legend.position="bottom",
                legend.key.width = unit(1,"cm"),
                legend.title = element_blank(),
                #axis.title   = element_text(family = "Ubuntu", size = 14, color = "#22211d"),
                #axis.title   = element_blank(),
                axis.title = element_blank(),
                axis.text  = element_blank(),
                axis.ticks = element_blank(),
                text = element_text(family = "Ubuntu", color = "#22211d"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.text       = element_text(family = "Ubuntu", size = 13, hjust = 0, color = "#22211d"),
                plot.title    = element_text(family = "Ubuntu", size = 21,  hjust = 0.5,  color = "#4e4d47"),
                plot.subtitle = element_text(family = "Ubuntu", size = 13,  hjust = 0.01,  face = "italic", color = "#4e4d47"),
                plot.caption  = element_text(family = "Ubuntu", size = 11,  hjust = 0.99, color = "#4e4d47"),
                panel.border = element_blank()
)  


ggplot(Data_long, aes(x = Year, col = Country, fill = Country)) +
  geom_bar() +
  coord_cartesian(xlim = c(1000, 2018)) + 
  expand_limits(x = 0, y = 0) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette  = "Spectral")  + 
  labs(title = "blabla",
       subtitle = "bla",
       x = "",
       y = "",
       caption = "Autor: WroData" ) +
  Theme


