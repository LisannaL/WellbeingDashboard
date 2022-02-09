library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(shinyjs)
#install.packages('shinydashboardPlus')
library(zeallot)
library(plotly)
library(tidyverse)
library(lemon)
library(scales)
library(ggrepel)
library(highcharter)
#install.packages('rsconnect')
library(rsconnect)


Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

### Import data
andmed = read.csv2(text = readLines("andmed1.csv", warn = FALSE),header=T)
andmed$riik = c("Austria","Belgia","Bulgaaria","Tšehhi","Küpros",
                "Saksamaa",
                "Taani",
                'Eesti',
                'Hispaania',
                'Soome',
                'Prantsusmaa',
                'Ühendkuningriigid (UK)',
                'Horvaatia',
                'Ungari',
                'Iirimaa',
                'Itaalia',
                'Leedu',
                'Läti',
                'Montenegro',
                'Holland',
                'Poola',
                'Portugal',
                'Serbia',
                'Rootsi',
                'Sloveenia',
                'Slovakkia')

# Colors
GRAY1 = "#231F20"
GRAY2 = "#414040"
GRAY3 = "#555655"
GRAY4 = "#646369"
GRAY5 = "#76787B"
GRAY6 = "#828282"
GRAY7 = "#929497"
GRAY8 = "#A6A6A5"
GRAY9 = "#BFBEBE"
BLUE1 = "#174A7E"
BLUE2 = "#4A81BF"
BLUE3 = "#94B2D7"
BLUE4 = "#94AFC5"
BLUE5 = "#22435e"
BLUE6 = "#95B3D7"
RED1 = "#C3514E"
RED2 = "#E6BAB7"
RED3 = "#800000"
GREEN1 = "#0C8040"
GREEN2 = "#9ABB59"
GREEN3 = "#31859C"
GREEN4 = "#4BACC6"
GREEN5 = "#93CDDD"
ORANGE1 = "#F79747"
ORANGE2 = "#FAC090"


### Scaling data
skaleerija_1 = data.frame(row.names = c("1","2","3", "4", "5") , val = c(10, 7.5, 5, 2.5, 0))
skaleerija_2 = data.frame(row.names = c("1","2","3", "4") , val = c(10, 6.7, 3.3, 0))
skaleerija_3 = data.frame(row.names = c("1","2","3", "4", "5", "6", "7") , val = c(0, 1.666, 3.332, 4.998, 6.664, 8.33, 10))
skaleerija_4 = data.frame(row.names = c("1","2","3", "4", "5", "6") , val = c(0, 2, 4, 6, 8, 10))
skaleerija_5 = data.frame(row.names = c("1","2","3", "4", "5", "6") , val = c(10, 8, 6, 4, 2 ,0))


skaleeri_andmed = function(vastused){
  c(kog_rahulolu, euda_usaldus, euda_vaartus, euda_auto, kog_majandus, kog_sotstoetus, afek_masendus, afek_room, euda_huvi, kog_tervis, kog_turvalisus, afek_onnelikkus) %<-% vastused
  
  euda_vaartus = skaleerija_1[euda_vaartus,]  
  euda_auto = skaleerija_1[euda_auto,]
  kog_majandus = skaleerija_2[kog_majandus, ]
  kog_sotstoetus = skaleerija_3[kog_sotstoetus, ]
  afek_masendus = skaleerija_4[afek_masendus, ]
  afek_room = skaleerija_5[afek_room, ]
  euda_huvi = skaleerija_5[euda_huvi, ]
  kog_tervis = skaleerija_1[kog_tervis, ]
  kog_turvalisus = skaleerija_2[kog_turvalisus, ]
  
  return(c(kog_rahulolu, euda_usaldus, euda_vaartus, euda_auto, kog_majandus, kog_sotstoetus, afek_masendus, afek_room, euda_huvi, kog_tervis, kog_turvalisus, afek_onnelikkus))
}


### Add user data to dataset
lisa_vastaja_rida = function(andmed, vastused){
  vastused = c('Teie', vastused, leia_vastaja_kog_kesk(vastused), leia_vastaja_afek_kesk(vastused), leia_vastaja_euda_kesk(vastused), leia_vastaja_yldskoor(vastused))
  print(vastused)
  andmed[nrow(andmed) + 1,] = vastused
  andmed$total_heaoluskoor = as.numeric(andmed$total_heaoluskoor)
  andmed$kog_kesk = as.numeric(andmed$kog_kesk)
  andmed$afek_kesk = as.numeric(andmed$afek_kesk)
  andmed$euda_kesk = as.numeric(andmed$euda_kesk)#
  
  andmed$kog_rahulolu = as.numeric(andmed$kog_rahulolu)
  andmed$kog_tervis = as.numeric(andmed$kog_tervis)
  andmed$kog_majandus = as.numeric(andmed$kog_majandus)
  andmed$kog_turvalisus = as.numeric(andmed$kog_turvalisus)
  andmed$kog_sotstoetus = as.numeric(andmed$kog_sotstoetus)#
  andmed$afek_onnelikkus = as.numeric(andmed$afek_onnelikkus)
  andmed$afek_masendus = as.numeric(andmed$afek_masendus)
  andmed$afek_room = as.numeric(andmed$afek_room)#
  andmed$euda_vaartus = as.numeric(andmed$euda_vaartus)
  andmed$euda_auto = as.numeric(andmed$euda_auto)
  andmed$euda_huvi = as.numeric(andmed$euda_huvi)
  andmed$euda_usaldus = as.numeric(andmed$euda_usaldus)
  
  return(andmed)
}


### Leida heaolu dimensioonide keskmised
# 1. kognitiivne
leia_vastaja_kog_kesk = function(vastused){
  c(kog_rahulolu, euda_usaldus, euda_vaartus, euda_auto, kog_majandus, kog_sotstoetus, afek_masendus, afek_room, euda_huvi, kog_tervis, kog_turvalisus, afek_onnelikkus) %<-% vastused
  kog_kesk = 0.2 * as.numeric(kog_rahulolu) + 0.2 * as.numeric(kog_sotstoetus) + 0.2* as.numeric(kog_tervis) + 0.2 * as.numeric(kog_majandus) + 0.2 * as.numeric(kog_turvalisus)
  return(kog_kesk)
}

# 2. afektiivne
leia_vastaja_afek_kesk = function(vastused){
  c(kog_rahulolu, euda_usaldus, euda_vaartus, euda_auto, kog_majandus, kog_sotstoetus, afek_masendus, afek_room, euda_huvi, kog_tervis, kog_turvalisus, afek_onnelikkus) %<-% vastused
  afek_kesk = 0.33 * as.numeric(afek_onnelikkus) + 0.33 * as.numeric(afek_room) + 0.33 * as.numeric(afek_masendus)
  return(afek_kesk)
}

# 3. eudaineemiline
leia_vastaja_euda_kesk = function(vastused){
  c(kog_rahulolu, euda_usaldus, euda_vaartus, euda_auto, kog_majandus, kog_sotstoetus, afek_masendus, afek_room, euda_huvi, kog_tervis, kog_turvalisus, afek_onnelikkus) %<-% vastused
  euda_kesk = 0.25 * as.numeric(euda_vaartus) + 0.25 * as.numeric(euda_auto) + 0.25 * as.numeric(euda_huvi) + 0.25 * as.numeric(euda_usaldus)
  return(euda_kesk)
}

#4. heaolu yldskoor
leia_vastaja_yldskoor = function(vastused){
  return(leia_vastaja_kog_kesk(vastused) + leia_vastaja_afek_kesk(vastused) + leia_vastaja_euda_kesk(vastused))
}


### SWD visualization
theme_swd = function() {
  theme_minimal(base_size = 11, base_family = "Helvetica") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(size = .13, color = GRAY9),
      axis.text = element_text(color = GRAY7),
      axis.ticks.x = element_line(size = 0.5, color = GRAY9),
      axis.ticks.y = element_line(size = 0.5, color = GRAY9),
      axis.title = element_text(color = GRAY3),
      axis.title.y = element_text(hjust = 1, margin = margin(0, 6, 0, 15, "pt")),
      axis.title.x = element_text(hjust = 0, margin = margin(6, 0, 15, 0, "pt")),
      plot.subtitle = element_text(color = GRAY4, size= 11),
      plot.title = element_text(color = GRAY4, size= 15),
      plot.title.position = "plot", 
      plot.caption = element_text(hjust = 0, color = GRAY6),
      plot.caption.position = "plot",
      plot.margin = margin(.5,.5,.5,.5,"cm"),
      strip.text = element_text(color = GRAY7)) 
}


### Sõned
get_heaolu_sõne = function(tase){
  heaolu_sõne = ''
  
  if (tase >= 9) {
    heaolu_sõne = 'väga kõrge'
  } else if (tase >= 7) {
    heaolu_sõne = 'kõrge'
  } else if (tase >= 4) {
    heaolu_sõne = 'keskmine'
  } else if (tase >= 2) {
    heaolu_sõne = 'madal'
  } else {
    heaolu_sõne = 'väga madal'
  }
  return(heaolu_sõne)
}

get_masendus_sõne = function(tase){
  heaolu_sõne = ''
  
  if (tase >= 9) {
    heaolu_sõne = 'väga madal'
  } else if (tase >= 7) {
    heaolu_sõne = 'madal'
  } else if (tase >= 4) {
    heaolu_sõne = 'keskmine'
  } else if (tase >= 2) {
    heaolu_sõne = 'kõrge'
  } else {
    heaolu_sõne = 'väga kõrge'
  }
  return(heaolu_sõne)
}

### 1. plot: yldskoorid
plot_yldine_heaolu = function(vastaja_data){
  
  joonis = ggplot(vastaja_data, aes(total_heaoluskoor, reorder(riik, +total_heaoluskoor), 
                              fill=factor(ifelse(riik=="Eesti" , 'HOME', ifelse(riik=="Teie" , "USER" ,'DEFAULT'))),
                              text = paste("Riik: ", riik, "</br></br>", "Üldtulemus: ", round(total_heaoluskoor, 2)))) +
    geom_bar(stat='identity') + 
    scale_fill_manual(name = "riik", values=c("#BFBEBE","#4A81BF","#C3514E")) + #gray9, green4/blue2, red1
    labs(y = "", x = "", title = "") +
    theme_swd() 
  
  joonis = ggplotly(joonis, tooltip = c('text')) %>% hide_legend() %>% 
    layout(annotations = list(x = 22.5, y = 3.5, 
                              text = " 0-5.99 Väga madal \n 6-11.99 Madal\n 12-17.99 Keskmine\n 18-23.99 Kõrge\n 24-30 Väga kõrge", 
                              showarrow = F,
                              align = 'left',
                              font = list(size = 13, color = "#76787B")),
           title = list(text = 'Üldine subjektiivne heaolu', 
                        x = 0.18,
                        y = 1,
                        font = list(size = 22, color = "#646369"),
                        pad = list(t = 15)
                        ))
  return(joonis)
}

render_heaolu_tase = function(vastaja_andmed) {
  heaolu = vastaja_andmed %>% filter(riik == 'Teie')
  heaolu = heaolu$total_heaoluskoor
  heaolu_sõne = ''
  
  if (heaolu >= 24) {
    heaolu_sõne = 'väga kõrge'
  } else if (heaolu >= 18) {
    heaolu_sõne = 'kõrge'
  } else if (heaolu >= 12) {
    heaolu_sõne = 'keskmine'
  } else if (heaolu >= 6) {
    heaolu_sõne = 'madal'
  } else {
    heaolu_sõne = 'Väga madal'
  }
  
  return(heaolu_sõne)
}

render_heaolu_tase_number = function(vastaja_andmed) {
  heaolu = vastaja_andmed %>% filter(riik == 'Teie')
  heaolu = round(heaolu$total_heaoluskoor,2)
  
  return(heaolu)
}



### 2. plot: heaolud
plot_heaolud = function(vastaja_data){
  heaolud_andmed = vastaja_data %>% 
    select(kog_kesk, afek_kesk, euda_kesk, riik) %>% 
    filter(riik == 'Teie') %>% 
    select(kog_kesk, afek_kesk, euda_kesk) %>% 
    gather(heaolud, skoor, kog_kesk:euda_kesk) 
  
  joonis2 = ggplot(heaolud_andmed, aes(heaolud,  y = round(skoor, digits = 2), fill = heaolud,
                                       text = paste("Teie skoor: ", round(skoor, 2),
                                                    '\n<b>Skaala:</b>\n0-1.99 Väga madal\n2-3.99 Madal\n4-6.99 Keskmine\n7-8.99 Kõrge\n9-10 Väga kõrge'))) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
    labs(y="", x = "") +
    #ylim(0, 10) +
    scale_fill_manual(name = "heaolud", values=c("#F79747", "#4A81BF", "#C3514E")) + #orange2, blue6, red2 # uus: ORANGE1, BLUE2, RED1 
    scale_x_discrete(labels = c("Emotsionaalne heaolu", "Toimetuleku heaolu", "Hinnanguline heaolu")) +
    theme_swd() 
  
  
  
  joonis2 = ggplotly(joonis2, tooltip = c('text')) %>% 
    hide_legend() %>% 
    layout(autosize=T)
  
  return(joonis2)
  
}

render_afektiivne = function(vastaja_andmed) {
  heaolu = vastaja_andmed %>% filter(riik == 'Teie')
  heaolu = as.numeric(heaolu$afek_kesk)

  return(get_heaolu_sõne(heaolu))
}

render_afektiivne_number = function(vastaja_andmed) {
  heaolu = vastaja_andmed %>% filter(riik == 'Teie')
  heaolu = round(heaolu$afek_kesk,2)
  
  return(heaolu)
}

render_eudaineemiline = function(vastaja_andmed) {
  heaolu = vastaja_andmed %>% filter(riik == 'Teie')
  heaolu = heaolu$euda_kesk
  return(get_heaolu_sõne(heaolu))
}

render_eudaineemiline_number = function(vastaja_andmed) {
  heaolu = vastaja_andmed %>% filter(riik == 'Teie')
  heaolu = round(heaolu$euda_kesk,2)
  
  return(heaolu)
}

render_kognitiivne = function(vastaja_andmed) {
  heaolu = vastaja_andmed %>% filter(riik == 'Teie')
  heaolu = heaolu$kog_kesk
  return(get_heaolu_sõne(heaolu))
}

render_kognitiivne_number = function(vastaja_andmed) {
  heaolu = vastaja_andmed %>% filter(riik == 'Teie')
  heaolu = round(heaolu$kog_kesk,2)
  
  return(heaolu)
}


### 3. plot: koik heaolud

plot_koik_heaolud = function(vastaja_data){
  
  koik_heaolud2 = vastaja_data %>% 
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus, 
           afek_onnelikkus, afek_masendus, afek_room, 
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>% 
    filter(riik== 'Teie') %>% 
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus, 
           afek_onnelikkus, afek_masendus, afek_room, 
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus) %>% 
    gather(heaolud, skoor, kog_rahulolu:euda_usaldus) %>% 
    mutate(group=c( rep('Hinnanguline', 5), rep('Emotsionaalne', 3), rep('Toimetuleku', 4))) %>% 
    arrange(group, skoor) %>% 
    mutate(soned = c("Heas tujus olemine ja rõõmu tundmine viimasel 2 nädalal", 
                     "Üldine õnnelikkuse\n tunne", 
                     "Masenduse ja depressiooni\n tundmine viimasel 2 nädalal",
                     "Positiivsed suhted:\n teiste usaldamine",
                     "Igapäevaelu huvipakkuvus\n viimasel 2 nädalal",
                     "Autonoomia tunnetus", 
                     "Üldine enda tegevuste väärtuslikkuse tunnetus",
                     "Hinnang suhetele:\n sotsiaalne toetus", 
                     "Hinnang tervisele", 
                     "Hinnang majanduslikule\n toimetulekule", 
                     "Hinnang turvalisusele", 
                     "Üldine eluga rahulolu")) 
  
  joonis3 = highchart() %>%
    hc_chart(polar = TRUE) %>% 
    hc_xAxis(categories = koik_heaolud2$soned) %>% 
    hc_series(list(
      name = 'Skoor',
      data = round(koik_heaolud2$skoor, 2),
      colorByPoint = T,
      type = "column", # afek orange2, euda blue6, kog red2 # uus: ORANGE1, RED1, BLUE2
      colors = ifelse(koik_heaolud2$group=="Emotsionaalne" , "#F79747", ifelse(koik_heaolud2$group=="Hinnanguline" , "#C3514E" , "#4A81BF")),
      showInLegend = F)) %>% 
    hc_tooltip(shared = F,
               borderColor = "black")
  
  return(joonis3)
  
}

### 4. plot: koik heaolud + riigid

plot_koik_heaolud_riigid = function(vastaja_data, input_riik){ 
  # print(input_riik)
  add_closed_trace <- function(p, r, theta, ...)
  {
    plotly::add_trace(p, r = c(r, r[1]), theta = c(theta, theta[1]), ...)
  }
  
  koik_vastaja = vastaja_data %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    filter(riik== 'Teie') %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    gather(heaolud, skoor, kog_rahulolu:euda_usaldus) %>%
    mutate(group=c( rep('Hinnanguline', 5), rep('Emotsionaalne', 3), rep('Toimetuleku', 4))) %>%
    arrange(group, skoor) %>%
    mutate(soned = c("Heas tujus olemine ja\n rõõmu tundmine\n viimasel 2 nädalal",
                     "Üldine õnnelikkuse\n tunne",
                     "Masenduse ja depressiooni\n tundmine viimasel 2 nädalal",
                     "Positiivsed suhted: teiste usaldamine",
                     "Igapäevaelu huvipakkuvus\n viimasel 2 nädalal",
                     "Autonoomia tunnetus",
                     "Üldine enda tegevuste\n väärtuslikkuse\n tunnetus",
                     "Hinnang suhetele:\n sotsiaalne toetus",
                     "Hinnang tervisele",
                     "Hinnang majanduslikule\n toimetulekule",
                     "Hinnang turvalisusele",
                     "Üldine eluga rahulolu"))
  
  koik_eesti = vastaja_data %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    filter(riik== 'Eesti') %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    gather(heaolud, skoor, kog_rahulolu:euda_usaldus) %>%
    mutate(group=c( rep('Hinnanguline', 5), rep('Emotsionaalne', 3), rep('Toimetuleku', 4))) %>%
    arrange(group, skoor) %>%
    mutate(soned = c("Heas tujus olemine ja\n rõõmu tundmine\n viimasel 2 nädalal",
                     "Üldine õnnelikkuse\n tunne",
                     "Masenduse ja depressiooni\n tundmine viimasel 2 nädalal",
                     "Positiivsed suhted: teiste usaldamine",
                     "Igapäevaelu huvipakkuvus\n viimasel 2 nädalal",
                     "Autonoomia tunnetus",
                     "Üldine enda tegevuste\n väärtuslikkuse\n tunnetus",
                     "Hinnang suhetele:\n sotsiaalne toetus",
                     "Hinnang tervisele",
                     "Hinnang majanduslikule\n toimetulekule",
                     "Hinnang turvalisusele",
                     "Üldine eluga rahulolu"))
  
  koik_taani = vastaja_data %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    filter(riik== 'Taani') %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    gather(heaolud, skoor, kog_rahulolu:euda_usaldus) %>%
    mutate(group=c( rep('Hinnanguline', 5), rep('Emotsionaalne', 3), rep('Toimetuleku', 4))) %>%
    arrange(group, skoor) %>%
    mutate(soned = c("Heas tujus olemine ja\n rõõmu tundmine\n viimasel 2 nädalal",
                     "Üldine õnnelikkuse\n tunne",
                     "Masenduse ja depressiooni\n tundmine viimasel 2 nädalal",
                     "Positiivsed suhted: teiste usaldamine",
                     "Igapäevaelu huvipakkuvus\n viimasel 2 nädalal",
                     "Autonoomia tunnetus",
                     "Üldine enda tegevuste\n väärtuslikkuse\n tunnetus",
                     "Hinnang suhetele:\n sotsiaalne toetus",
                     "Hinnang tervisele",
                     "Hinnang majanduslikule\n toimetulekule",
                     "Hinnang turvalisusele",
                     "Üldine eluga rahulolu"))
  
  koik_bulgaaria = vastaja_data %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    filter(riik== 'Bulgaaria') %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    gather(heaolud, skoor, kog_rahulolu:euda_usaldus) %>%
    mutate(group=c( rep('Hinnanguline', 5), rep('Emotsionaalne', 3), rep('Toimetuleku', 4))) %>%
    arrange(group, skoor) %>%
    mutate(soned = c("Heas tujus olemine ja\n rõõmu tundmine\n viimasel 2 nädalal",
                     "Üldine õnnelikkuse\n tunne",
                     "Masenduse ja depressiooni\n tundmine viimasel 2 nädalal",
                     "Positiivsed suhted: teiste usaldamine",
                     "Igapäevaelu huvipakkuvus\n viimasel 2 nädalal",
                     "Autonoomia tunnetus",
                     "Üldine enda tegevuste\n väärtuslikkuse\n tunnetus",
                     "Hinnang suhetele:\n sotsiaalne toetus",
                     "Hinnang tervisele",
                     "Hinnang majanduslikule\n toimetulekule",
                     "Hinnang turvalisusele",
                     "Üldine eluga rahulolu"))
  
  vali_riik = vastaja_data %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    filter(riik== input_riik) %>%
    select(kog_rahulolu, kog_tervis, kog_majandus, kog_turvalisus, kog_sotstoetus,
           afek_onnelikkus, afek_masendus, afek_room,
           euda_vaartus, euda_auto, euda_huvi, euda_usaldus, riik) %>%
    gather(heaolud, skoor, kog_rahulolu:euda_usaldus) %>%
    mutate(group=c( rep('Hinnanguline', 5), rep('Emotsionaalne', 3), rep('Toimetuleku', 4))) %>%
    arrange(group, skoor) %>%
    mutate(soned = c("Heas tujus olemine ja\n rõõmu tundmine\n viimasel 2 nädalal",
                     "Üldine õnnelikkuse\n tunne",
                     "Masenduse ja depressiooni\n tundmine viimasel 2 nädalal",
                     "Positiivsed suhted: teiste usaldamine",
                     "Igapäevaelu huvipakkuvus\n viimasel 2 nädalal",
                     "Autonoomia tunnetus",
                     "Üldine enda tegevuste\n väärtuslikkuse\n tunnetus",
                     "Hinnang suhetele:\n sotsiaalne toetus",
                     "Hinnang tervisele",
                     "Hinnang majanduslikule\n toimetulekule",
                     "Hinnang turvalisusele",
                     "Üldine eluga rahulolu"))
  
  
  joonis4 = plot_ly(
    type = 'scatterpolar',
    mode = 'lines',
    fill = 'toself',
    line = list(shape = 'spline')) %>%
    add_closed_trace(
      name = 'Eesti',
      mode = 'lines+markers',
      r = round(as.numeric(koik_eesti$skoor), 2),
      theta = koik_eesti$soned) %>%
    add_closed_trace(
      name = 'Taani',
      mode = 'lines+markers',
      r = round(as.numeric(koik_taani$skoor), 2),
      theta = koik_taani$soned,
      visible = "legendonly") %>%
    add_closed_trace(
      name = 'Bulgaaria',
      mode = 'lines+markers',
      r = round(as.numeric(koik_bulgaaria$skoor), 2),
      theta = koik_bulgaaria$soned,
      visible = "legendonly") %>%
    add_closed_trace(
      name = 'Teie',
      mode = 'lines+markers',
      r = round(as.numeric(koik_vastaja$skoor), 2),
      theta = koik_vastaja$soned) %>%
    add_closed_trace(
      name = input_riik,
      mode = 'lines+markers',
      r = round(as.numeric(vali_riik$skoor), 2),
      theta = vali_riik$soned,
      visible = "legendonly") %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,10))),
      showlegend = T,
      legend = list(x =0, y = 1, title = list(text = '<b> Joonisele lisamiseks kliki riigil: </b>')))
  
  return(joonis4)
  
}

render_heaoluEesti = function(vastaja_andmed){
  tase = vastaja_andmed %>% filter(riik == 'Teie')
  tase = ifelse(tase$total_heaoluskoor > 20.09259, 'kõrgem', 'madalam')
  return(tase)
}


### SOOVITUSED
render_dimensiooniTekstid = function(output, vastaja_andmed){
  vastaja = vastaja_andmed %>% filter(riik == 'Teie')
  
  vektor = vector()
  soovitused = ""
  
  if(vastaja$kog_rahulolu < 4) {
    soovitused = paste("<b>Teie üldine eluga rahulolu on ", get_heaolu_sõne(vastaja$kog_rahulolu) ,"</b>– mõelge, mis mõjutab negatiivselt Teie hinnangut enda elule hetkel kõige rohkem ja mis on see, mida Te saate ise selle asja juures muuta. Pange kirja 3 esimest sammu koos tähtaegadega, mida plaanite teha selle asja muutmiseks.<hr>") # <h1></h1>
  } else {
    vektor = c(vektor, 'üldine eluga rahuloluga')  
  }
  if(vastaja$kog_tervis < 4) {
    soovitused = paste(soovitused, "<b>Teie hinnang oma tervisele on ", get_heaolu_sõne(vastaja$kog_tervis) ,"</b>– proovige tõsta enda füüsilist aktiivsust. Isegi kui Teil on liikumine piiratud, on Teil võimalik kasvõi tubastes tingimustes endale sobilikke harjutusi teha. Kujundage sellest meeldiv harjumus.<hr>")
  }else {
    vektor = c(vektor, 'hinnang tervisele')  
  }
  
  if(vastaja$kog_majandus  < 4) {
    soovitused = paste(soovitused, "<b>Teie hinnang majanduslikule toimetulekule on ", get_heaolu_sõne(vastaja$kog_majandus ) ,"</b>– võimalusel omandage uusi teadmisi ja oskusi. Juhul kui arvate, et materiaalne jõukus ja tarbimine on märgid õnnelikkusest, siis soovitame sellest arusaamast loobuda, sest see vähendab Teie heaolu. Mõelge, kellega Te ennast võrdlesite, kui majanduslikule  toimetulekule hinnangu andsite, kui mõistlik see võrdlus tundub ja kellega võiksite ennast veel võrrelda.<hr>")
  }else {
    vektor = c(vektor, 'hinnang majanduslikule toimetulekule')  
  }
  
  if(vastaja$kog_turvalisus  < 4) {
    soovitused = paste(soovitused,"<b>Teie hinnang turvalisusele on ", get_heaolu_sõne(vastaja$kog_turvalisus ) ,"</b>–  mõelge, mis Teis ebaturvalisuse tunnet kõige enam tekitab, kes saaks midagi teha, et seda vähendada ning milline võiks Teie roll selles olla.  Rääkige oma muredest ja kartustest lähedastele inimestele ja mõelge koos, kas turvalisust saaks suurendada.<hr>")
  }else {
    vektor = c(vektor, 'hinnang turvalisusele')  
  }
  
  if(vastaja$kog_sotstoetus  < 4) {
    soovitused = paste(soovitused,"<b>Teie hinnang sotsiaalsele toetusele on ", get_heaolu_sõne(vastaja$kog_sotstoetus ) ,"</b>– Mõelge, miks pole Teil hetkel võimalik lähedaste või sõpradega avatult rääkida ja kas näete võimalust olukorda muuta. Proovige teha ise esimene samm olukorra muutmiseks: näiteks leida samade huvidega inimesi, kellega suhelda. Võimalik, et nii leiate inimese, kellega saate üksteist vajadusel toetada.<hr>")
  }else {
    vektor = c(vektor, 'hinnang suhetele: sotsiaalne toetus')  
  }
  
  if(vastaja$afek_onnelikkus  < 4) {
    soovitused = paste(soovitused,"<b>Teie hinnang üldisele õnnelikkusele on ", get_heaolu_sõne(vastaja$afek_onnelikkus ) ,"</b>– mõelge, mis teeb Teid õnnetumaks ja mis õnnelikuks ja mis on see, mida Te saate ise selle juures muuta. Pange kirja 3 esimest sammu koos tähtaegadega, mida saaks ise teha.<hr>")
  }else {
    vektor = c(vektor, 'üldine õnnelikkuse tunne')  
  }
  
  if(vastaja$afek_masendus < 4) {
    soovitused = paste(soovitused,"<b>Teie masenduse ja depressiooni tundmine viimasel 2 nädalal oli ", get_masendus_sõne(vastaja$afek_masendus ) ,"</b>–  uurige järgevalt veebilehelt, mis on vaimse tervise vitamiinid ja kuidas need Teid võiksid aidata. https://peaasi.ee/vaimse-tervise-vitamiinid/ <hr>")
  }else {
    vektor = c(vektor, 'masenduse ja depressiooni tundmine viimasel 2 nädalal')  
  }
  
  if(vastaja$afek_room  < 4) {
    soovitused = paste(soovitused,"<b>Teie heas tujus olemine ja rõõmu tundmine viimasel 2 nädalal oli ", get_heaolu_sõne(vastaja$afek_room ) ,"</b>– mõelge, mis olukorrad tekitavad Teis tavaliselt hea tuju ja panevad rõõmu tundma. Valige välja mõni olukord ning proovige teadlikult neid olukordi enda jaoks tekitada.<hr>")
  }else {
    vektor = c(vektor, 'heas tujus olemine ja rõõmu tundmine viimasel 2 nädalal')  
  }
  
  if(vastaja$euda_vaartus  < 4) {
    soovitused = paste(soovitused,"<b>Teie üldine enda tegevuste väärtuslikkuse tunnetus on ", get_heaolu_sõne(vastaja$euda_vaartus ) ,"</b>– mõelge, millised on Teie põhilised eesmärgid elus ja milliseid tegevusi peate tegema, et neid eesmärke täita. Pange need eesmärgid ja tegevused kirja koos tähtaegadega ning proovige seda plaani täitma asuda.<hr>")
  }else {
    vektor = c(vektor, 'üldine enda tegevuste väärtuslikkuse tunnetus')  
  }
  
  if(vastaja$euda_auto < 4) {
    soovitused = paste(soovitused,"<b>Teie sõltumatuse tunnetus (asendada igal pool läbivalt autonoomia sõltumatuseks)  ", get_heaolu_sõne(vastaja$euda_auto) ,"</b>– mõelge, mis on põhiline, mis ei lase Teil vabalt otsustada, kuidas oma elu elada ning mida peaks tegema, et olukorda muuta. Kui Te ise ei suuda olukorda muuta, rääkige oma vajadustest ja soovidest tuttavatele, sõpradele, lähedastele.<hr>")
  }else {
    vektor = c(vektor, 'autonoomia tunnetus')  
  }
  
  if(vastaja$euda_huvi  < 4) {
    soovitused = paste(soovitused,"<b>Teie igapäevaelu huvipakkuvus viimasel 2 nädalal oli ", get_heaolu_sõne(vastaja$euda_huvi ) ,"</b>– esimesena mõelge, millised igapäevaelu olukorrad Teile huvi ei pakkunud; teisena, kuidas muuta igapäevaelus ettetulevaid vältimatuid olukordi endale huvipakkuvamaks; kolmandaks loobuge mitte huvipakkuvatest igapäevaelu olukordadest, mis pole vältimatud.<hr>")
  }else {
    vektor = c(vektor, 'igapäevaelu huvipakkuvus viimasel 2 nädalal')  
  }
  
  if(vastaja$euda_usaldus  < 4) {
    soovitused = paste(soovitused,"<b>Teiste usaldamine on ", get_heaolu_sõne(vastaja$euda_usaldus ) ,"</b>– proovige lähedaste ja sõpradega või ka (tulevaste) tuttavatega teha rohkem koostööd nõudvaid ühistegevusi. Need võivad olla nii praktilist laadi (nt talgud) kui ka puhtalt meelelahutuslikud. Seejuures proovige vältida usaldamatuse eelhoiakut.<hr>")
  }else {
    vektor = c(vektor, 'positiivsed suhted: teiste usaldamine')  
  }
  
  if(length(vektor) != 0) {
    #output$hasti = renderText({paste("Teie", paste(vektor,collapse=", ") ,"on Teil kõik hästi, jätkake samas vaimus.")})
    output$hasti = renderText({paste("<b>Järgnevate heaolu komponentidega: </b>", paste(vektor,collapse=", ") ," <b>- on Teil kõik hästi, jätkake samas vaimus.</b>")})
  }
  
  output$soovitused = renderText({soovitused})
  
}



### app.R ###

ui <- dashboardPage(
  skin='black-light',
  dashboardHeader(title = "Heaolumeeter"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")), # make navbar and sidebar sticky
    tags$style(
      '
        @media (min-width: 768px){
          .sidebar-mini.sidebar-collapse .main-header .logo {
              width: 180px;
          }
          .sidebar-mini.sidebar-collapse .main-header .navbar {
              margin-left: 180px;
          }
        }
        '
    ),
    useShinyjs(),
    
    ## Add custom CSS rules
    tags$head(tags$style(HTML('
      @import url("https://fonts.googleapis.com/css2?family=Montserrat:wght@500&display=swap");
    
      .landing-page {
        max-width: 1400px
      }
      
      .aboutPage {
        max-width: 1200px;
        text-align: justify;
      }
      
      h1, h2, h3, h4, h5, strong, p, button, li, span {
        font-family: Montserrat;
      }
      
      .sidebar-mini.sidebar-collapse 
      .main-header
      .logo{
        font-family: Montserrat;
      }
      
      .input-page {
        padding: 16px;
        height: 460px;
        position: relative;
      }
      
      .marginZero {
        margin-top: 0;
      }
      
      .highcharts-root { 
        background: white;
      }
      
      .infoBoxContainer {
        display: flex;
        flex-direction: column;
      }
      
      .infoBoxContainer p {
        text-align: initial;
      }
      
      .marginBottomZero {
        margin-bottom: 0;
      }
      
      .btnContainer {
        display: flex;
        justify-content: center;
        margin-top: 64px;
      }
      
      .actionBtnContainer {
        display: flex;
        justify-content: center;
        margin-top: 64px;
        position: absolute;
        width: 100%;
        bottom: 0;
      }
      
      .logoContainer {
        display: flex;
        justify-content: center;
        margin-top: 96px;
      }
      
      .hide {
        visibility: hidden;
      }
      
      .center-text {
        text-align: center;
      }
      
      .input-container {
        display: flex;
        justify-content: center;
        margin-top: 64px;
        margin-left: 16px;
      }
      
      .radio-input-container {
        display: flex;
        justify-content: center;
        margin-top: 64px;
        margin-left: 16px;
      }
      
      .radio-input-container span {
       font-size: 20px;
      }
      
     .radio-input-container input {
       margin-top: 10px;
     }
      
      .graph-container {
        display: flex;
        margin-bottom: 16px;
      }
      
      .horizontalCenter {
        display: flex;
        flex-direction: column;
        align-items: center;
        width: 100%
      }
      
      .graph-container-reverse {
        flex-direction: row-reverse;
      }
      
      .graph-text {
        margin-right: 16px;
        background: white;
        padding: 32px;
        width: 33%;
        
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      
      .graph-text-evenly {
        margin-right: 16px;
        background: white;
        padding: 32px;
        width: 33%;
        
        display: flex;
        flex-direction: column;
        justify-content: space-evenly;
      }
      
      .scale-container {
        text-align: left;
        width: 150px;
      }
      
      .graph-container-reverse .graph-text {
        margin-right: unset;
        margin-left: 16px;
        width: 50%;
      }
      
      .graph-container-reverse .graph-text-evenly {
        margin-right: unset;
        margin-left: 16px;
      }
      
      .white-graph-container {
        display: flex;
        justify-content: center;
        margin-bottom: 16px;
        width: 100%;
        background: white;
      }
      
      .polar-graph-text {
        width: 38% !important;
      }
      
      .doubleLineHeight {
        line-height: 2;
      }
      
      .marginBottom48 {
        margin-bottom: 48px;
      }
      
      .highchart-graph-container {
        display: flex;
        justify-content: center;
        align-items: center;
        background: white;
        width: 100%;
      }

      p {
        text-align: justify
      }
      
      img {
        margin: 32px 48px 0 32px
      }
      
      .highcharts-axis-labels text {
        font-size: 14px !important;
      }
      
      hr {
        height: 1px;
      }
    

      .irs-grid-text {
          font-size: 16px;
      }
      .small {
        display: none
      }
      
      .tab-pane {
        padding: 32px;
      }
      
      .fa-info {
        margin-left: 4px;
      }
      
      .fa-question {
        margin-left: 0;
      }
      
      .fa-bar-chart {
        margin-left: 0;
      }
      
      .fa-network-wired {
        margin-left: -2px;
      }
      
      .small-box {
        margin-bottom: 0;
        height: 200px;
      }
      
      .small-box p {
        font-size: 18px;
      }
      
      .col-sm-4 {
        padding-left: 0;
      }
      
      @media (min-width: 768px) {
        .col-sm-4 {
          width: initial;
        }
      }
}
    '))),
    
    tabItems(
      # First tab content
      tabItem(tabName = "tutvustus",
              div(
                class='landing-page',
                h1(class= 'marginBottomZero', strong('HEAOLUMEETER')),
                h1(class= 'marginBottomZero', strong('Kui hästi mul läheb? Kui õnnelik ma olen? Kuidas tõsta enda heaolu?')),
                h2(''),
                h2(class = 'marginZero', 'Kui soovite vastuseid nendele küsimustele, siis olete õiges kohas.'),
                h4(class = 'marginBottomZero', 'Heaolumeeter aitab Teil kindlaks teha enda isikliku heaolu taseme, võrrelda enda tulemusi keskmiste Eesti ja teiste Euroopa riikide täisealiste inimestega, välja selgitada enda tugevad ja nõrgad kohad ning saada soovitusi enda heaolu tõstmiseks.'),
                p(' '),
                h4('Heaolumeeter on välja töötatud Euroopa Sotsiaaluuringu (ja väiksemal määral Euroopa elukvaliteedi uuringu) andmete põhjal.'),
                #h2('Mis on Euroopa Sotsiaaluuring?'),
                h2(''),
                h4('Euroopa Sotsiaaluuring (ESS - European Social Survey) on rahvusvaheline sotsiaalteaduslik uuring ja ühiskonnateaduste taristu, mille eesmärgiks on ühiskondade arengu seaduspärasuste uurimise võimaldamine. Andmeid kogutakse alates 2002. aastast. Eri riikide andmete võrreldavuse tagamiseks nõutakse rangete protseduurireeglite järgimist. Kogutud andmed võimaldavad analüüsida hoiakute, institutsioonide arengu ja inimeste käitumise vahelist seost ning mitmeid muid protsesse ühiskondades. Vaata lähemalt ', a('siit.', href='https://www.yti.ut.ee/et/ess/euroopa-sotsiaaluuring')),
                h2('Mis on heaolu?'),
                #
                span(HTML('<h4>Heaolu on see, kuidas meil oma elus läheb. Räägime <b>objektiivsest heaolust</b>, kui vaatleme silmaga nähtavaid ja ühtselt määratletud elutingimusi ehk kuidas minu elu paistab väljastpoolt vaadatuna.<br><br> <b>Subjektiivne heaolu</b> on see, kuidas me ise oma elu näeme, täpsemalt milliseid tundeid ja emotsioone me tunneme, kuidas toimime isiklikul ja sotsiaalsel tasandil ning kuidas ise hindame „kuidas meil oma elus läheb“. Heaolumeeter aitab mõõta Teie subjektiivse heaolu taset.</h4>')
                ),
                div(
                  class = 'btnContainer',
                  actionButton('switchtab', 'Mine küsimustikku täitma')
                ),
                div(
                  class = 'logoContainer',
                  img(src='ut_logo.png', height=100, width=246),
                  img(src='ess_logo.png', height=75, width=160),
                  img(src='strukt.png', height=100, width=246)
                )
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "kysimused",
              div(
                class='input-page',
                h2(class = 'marginZero center-text', "Järgnevalt palume Teil vastata 12-le küsimusele."),
                h4(class = 'marginBottomZero center-text', 'Vastamine on anonüümne. Teie vastuseid kasutatakse ainult siin ja praegu, et välja selgitada Teie heaolu tase. '),
                div(
                  class = 'btnContainer',
                  actionButton('edasi', 'Edasi')
                )
              )
      ),
      
      #1
      tabItem(tabName = "kysimus_1",
              div(
                class='input-page',
                h3(
                  class='center-text',
                  '1. Kõike kokkuvõttes, kuivõrd rahul Te oma eluga üldiselt olete praegu?'), 
                h3(
                  class='center-text',
                  'Üldse mitte rahul 0 ... 10 väga rahul'),
                div(
                  class='input-container',
                  sliderInput("slider1", min = 0, max = 10, value = 1, label = NULL, width=400)
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_1', 'Tagasi'),
                  actionButton('edasi_1', 'Edasi')
                )
              )
      ),
      
      #2
      tabItem(tabName = "kysimus_2",
              div(
                class='input-page',
                h3(class='center-text','2. Üldiselt hinnates, kas Teie arvates võib enamikku inimesi usaldada?'),
                h3(class='center-text','Enamikku inimesi ei saa usaldada 0 ... 10 Enamikku inimesi võib usaldada'),
                div(
                  class='input-container',
                  sliderInput("slider2", min = 0, max = 10, value = 1, label = NULL, width=400)
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_2', 'Tagasi'),
                  actionButton('edasi_2', 'Edasi')
                )
              )
      ),
      
      #3
      tabItem(tabName = "kysimus_3",
              div(
                class='input-page',
                h3(class='center-text','3. Kuivõrd Te nõustute või ei nõustu järgmise väitega? '),
                h3(class='center-text','Tunnen üldiselt, et see, mida ma teen, on väärtuslik.'),
                div(
                  class='radio-input-container',
                  radioButtons("radio1",
                               choices = list("1 - Nõustun täielikult" = 1, "2 - Nõustun" = 2, "3 - Ei seda ega teist" = 3, "4 - Ei nõustu" = 4, "5 - Ei nõustu üldse" = 5),
                               selected = 3, label = NULL, width='inherit')
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_3', 'Tagasi'),
                  actionButton('edasi_3', 'Edasi')
                )
              )
      ),
      
      #4
      tabItem(tabName = "kysimus_4",
              div(
                class='input-page',
                h3(class='center-text','4. Kuivõrd Te nõustute või ei nõustu järgmise väitega?'),
                h3(class='center-text', 'Tunnen, et saan vabalt otsustada, kuidas oma elu elada. '),
                div(
                  class='radio-input-container',
                  radioButtons("radio2",
                               choices = list("1 - Nõustun täielikult" = 1, "2 - Nõustun" = 2, "3 - Ei seda ega teist" = 3, "4 - Ei nõustu" = 4, "5 - Ei nõustu üldse" = 5),
                               selected = 3, label = NULL, width = 'inherit')
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_4', 'Tagasi'),
                  actionButton('edasi_4', 'Edasi')
                )
              )
      ),
      
      #5
      tabItem(tabName = "kysimus_5",
              div(
                class='input-page',
                h3(class='center-text','5. Milline järgnevatest kirjeldustest vastab Teie arvates kõige paremini Teie leibkonna praegusele sissetulekute tasemele?'),
                div(
                  class='radio-input-container',
                  radioButtons("radio3",
                               choices = list("1 - Elan /elame mugavalt praeguse sissetuleku juures" = 1, "2 - Saame hakkama praeguse sissetuleku juures" = 2, "3 - Praeguse sissetuleku juures on raske hakkama saada" = 3, "4 - Praeguse sissetuleku juures on väga raske hakkama saada" = 4),
                               selected = 2, label = NULL, width='inherit')
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_5', 'Tagasi'),
                  actionButton('edasi_5', 'Edasi')
                )
              )
      ),
      
      #6
      tabItem(tabName = "kysimus_6",
              div(
                class='input-page',
                h3(class='center-text','6. Kui palju on inimesi, kui üldse on, kellega saate arutada oma isiklikke ja intiimseid asju? '),
                div(
                  class='radio-input-container',
                  radioButtons("radio4",
                               choices = list("Mitte ühtegi" = 1, "1" = 2, "2" = 3, "3" = 4, "4-6" = 5, "7-9" = 6, "10 või enam" = 7),
                               selected = 2, label = NULL, width='inherit')
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_6', 'Tagasi'),
                  actionButton('edasi_6', 'Edasi')
                )
              )
      ),
      
      #7
      tabItem(tabName = "kysimus_7",
              div(
                class='input-page',
                h3(class='center-text','7. Kui tihti olete viimase 2 nädala jooksul tundnud masendust ja depressiooni?'),
                div(
                  class='radio-input-container',
                  radioButtons("radio5",
                               choices = list("1 - Kogu aeg" = 1, "2 - Suurema osa ajast" = 2, "3 - Rohkem kui pool ajast" = 3, "4 - Vähem kui pool ajast" = 4, "5 - Mõnikord" = 5, "6 - Mitte kunagi" = 6),
                               selected = 3, label = NULL, width='inherit')
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_7', 'Tagasi'),
                  actionButton('edasi_7', 'Edasi')
                )
              )
      ),
      
      #8
      tabItem(tabName = "kysimus_8",
              div(
                class='input-page',
                h3(class='center-text','8. Kui tihti olete viimase 2 nädala jooksul olnud rõõmus ja heas tujus? '),
                div(
                  class='radio-input-container',
                  radioButtons("radio6",
                               choices = list("1 - Kogu aeg" = 1, "2 - Suurema osa ajast" = 2, "3 - Rohkem kui pool ajast" = 3, "4 - Vähem kui pool ajast" = 4, "5 - Mõnikord" = 5, "6 - Mitte kunagi" = 6),
                               selected = 3, label = NULL, width='inherit')
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_8', 'Tagasi'),
                  actionButton('edasi_8', 'Edasi')
                )
              )
      ),
      
      #9
      tabItem(tabName = "kysimus_9",
              div(
                class='input-page',
                h3(class='center-text','9. Kui tihti olete viimase 2 nädala jooksul tundnud, et igapäevane elu on olnud täis huvitavaid asju? '),
                div(
                  class='radio-input-container',
                  radioButtons("radio7",
                               choices = list("1 - Kogu aeg" = 1, "2 - Suurema osa ajast" = 2, "3 - Rohkem kui pool ajast" = 3, "4 - Vähem kui pool ajast" = 4, "5 - Mõnikord" = 5, "6 - Mitte kunagi" = 6),
                               selected = 3, label = NULL, width='inherit')
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_9', 'Tagasi'),
                  actionButton('edasi_9', 'Edasi')
                )
              )
      ),
      
      #10
      tabItem(tabName = "kysimus_10",
              div(
                class='input-page',
                h3(class='center-text','10. Üldiselt hinnates, milline on Teie tervislik seisund?'),
                h3(class='center-text','Kas Teie arvates on see .....? '),
                div(
                  class='radio-input-container',
                  radioButtons("radio8",
                               choices = list("1 - Väga hea" = 1, "2 - Hea" = 2, "3 - Rahuldav" = 3, "4 - Halb" = 4, "5 - Väga halb" = 5),
                               selected = 3, label = NULL, width='inherit')
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_10', 'Tagasi'),
                  actionButton('edasi_10', 'Edasi')
                )
              )
      ),
      
      #11
      tabItem(tabName = "kysimus_11",
              div(
                class='input-page',
                h3(class='center-text','11. Kui turvaliselt tunnete end või tunneksite end jalutamas üksinda oma kodu ümbruses pimedal ajal?'), 
                h3(class='center-text','Kas Te tunnete või tunneksite end...'),
                div(
                  class='radio-input-container',
                  radioButtons("radio9",
                               choices = list("1 - Väga turvaliselt" = 1, "2 - Turvaliselt" = 2, "3 - Mitte eriti turvaliselt" = 3, "4 - Üldse mitte turvaliselt" = 4),
                               selected = 3, label = NULL, width='inherit')
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_11', 'Tagasi'),
                  actionButton('edasi_11', 'Edasi')
                )
              )
      ),
      
      #12
      tabItem(tabName = "kysimus_12",
              div(
                class='input-page',
                h3(class='center-text','12. Kui õnnelikuks Te kõike kokku võttes end peate?'),
                h3(class='center-text','Väga õnnetu 0 ... 10 Väga õnnelik'),
                div(
                  class='input-container',
                  sliderInput("slider3", min = 0, max = 10, value = 1, label = NULL, width=400)
                ),
                div(
                  class = 'actionBtnContainer',
                  actionButton('tagasi_12', 'Tagasi'),
                  actionButton('edasi_12', 'Edasi')
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "tulemused-tuhi",
              h1("TULEMUSED"),
              p('Tulemuste nägemiseks vasta enne küsimustele.'),
              div(
                class = 'btnContainer',
                
                actionButton('switchtab1', 'Mine küsimustikku täitma')
              )
      ),
      
      
      # Third tab content
      tabItem(tabName = "tulemused",
              h1("TULEMUSED"),
              div(
                class='graph-container',
                div(
                  class='infoBoxContainer',
                  valueBox(
                    value = "> 300 000",
                    subtitle = "inimese andmed",
                    icon = shiny::icon("bar-chart"),
                    color = "red"
                  ),
                  valueBox(
                    value = "26",
                    subtitle = " Euroopa riigi võrdluses",
                    icon = shiny::icon("globe-europe"),
                    color = "yellow"
                  ),
                  valueBox(
                    value = "2",
                    subtitle = HTML("sotsiaalteadusliku uuringu põhjal <br><br><b>ESS</b><br><b>EQLS</b>"), 
                    color = 'orange',
                    icon = shiny::icon("book-open")
                  )
                ),
                div(
                  class='graph-text',
                  span(h3('Teie subjektiivne heaolu on ', 
                          strong(textOutput('heaoluTase', inline=T)), 
                          span(' - üldtulemus on '), 
                          strong(textOutput('heaoluTaseNumber', inline=T)), 
                          span(' punkti'))),
                  h5('Heaolu indeksi üldtulemus koosneb hinnangulise heaolu, emotsionaalse heaolu ja toimetuleku heaolu summeeritud hinnangutest.')
                ),
                # plot 1
                plotlyOutput("joonis", width = "66%", height = "600px")
              ),
              div(
                class='graph-container graph-container-reverse',
                div(
                  class="graph-text",
                  span(h3('Teie emotsionaalne heaolu on ', 
                          strong(textOutput('afektiivneTase', inline=T)), 
                          span(' - üldtulemus on '), 
                          strong(textOutput('afektiivneNumber', inline=T)),
                          span(' punkti.'))),
                  h4('Emotsionaalne heaolu puudutab meie emotsioone ehk seda, kuidas me end igal ajahetkel tunneme. Aitab mõista kui õnnelikud me oma elus oleme.'),
                  h1(''),
                  span(h3('Teie toimetuleku heaolu on ', 
                          strong(textOutput('eudaTase', inline=T)), 
                          span(' - üldtulemus on '), 
                          strong(textOutput('eudaNumber', inline=T)), 
                          span(' punkti.'))),
                  h4('Toimetuleku heaolu on seotud tunnetusega, et minu elul on eesmärk, mõte või suund, minu elu on huvitav, olen sõltumatu ja autonoomne, ning mul on teistega positiivsed suhted. Aitab mõista, kui hästi meie elu toimib, sh kuivõrd elame kooskõlas oma tõelise minaga.'),
                  h1(''),
                  span(h3('Teie hinnanguline heaolu on ', 
                          strong(textOutput('kogTase', inline=T)), 
                          span(' - üldtulemus on '), 
                          strong(textOutput('kogNumber', inline=T)), 
                          span(' punkti.'))),
                  h4('Hinnanguline heaolu on seotud hinnangutega, mida me oma elu ja selle erinevate aspektide kohta anname. Aitab mõista, kui rahul me oma eluga oleme. '),
                  h1(' '), 
                  # div(
                  #   class='horizontalCenter',
                  #   # div(
                  #   #   class='scale-container',
                  #   #   h4('Skaala:'),
                  #   #   h5('0-1.99 Väga madal;'),
                  #   #   h5('2-3.99 Madal;'),
                  #   #   h5('4-6.99 Keskmine;'),
                  #   #   h5('7-8.99 Kõrge;'),
                  #   #   h5('9-10 Väga kõrge.')
                  #   # )
                  # )
                ),
                
                # plot 2
                plotlyOutput("joonis2", width="50%", height="600px")
              ),
              
              
              div(
                class='graph-container',
                div(
                  class='graph-text polar-graph-text',
                  div(class="marginBottom48",
                  htmlOutput('hasti', inline=T),
                  ),
                  htmlOutput('soovitused', inline=T),
                ),
                
                # plot 3
                div(
                  class="highchart-graph-container",
                  highchartOutput("joonis3", width="62%", height="900px")
                  )#1140
              ),
              
              div(
                class='graph-container graph-container-reverse',
                div(
                  class="graph-text-evenly",
                  #
                  span(h3('Teie heaolu on', 
                          strong(textOutput('heaoluEesti', inline=T)), 
                          span(' kui Eesti keskmine.'))),
                  span(
                    h4(class='doubleLineHeight', 'Kõrvaloleval joonisel on võimalik võrrelda enda heaolu aladimensioonide skoore Eesti keskmise, kõrgeima heaoluga riigi (Taani) ja madalaima heaoluga riigi (Bulgaaria) omaga.')),
                  #
                  selectInput("select", label = h3("Vali võrdlemiseks riik:"), 
                              choices = list("Austria" = "Austria", "Belgia" = "Belgia", "Hispaania" = "Hispaania", "Holland" = "Holland", "Horvaatia" = "Horvaatia", "Iirimaa" = "Iirimaa",
                                             "Itaalia" = "Itaalia", "Küpros" = "Küpros", "Leedu" = "Leedu", "Läti" = "Läti", "Montenegro" = "Montenegro", "Poola" = "Poola", 
                                             "Portugal" = "Portugal", "Prantsusmaa" = "Prantsusmaa", "Rootsi" = "Rootsi", "Saksamaa" = "Saksamaa", "Serbia" = "Serbia",
                                             "Slovakkia" = "Slovakkia", "Sloveenia" = "Sloveenia", "Soome" = "Soome",  "Tšehhi" = "Tšehhi", 
                                             "Ungari" = "Ungari", "Ühendkuningriigid (UK)" = "Ühendkuningriigid (UK)"))
                ),
                
                # plot 4
                plotlyOutput("joonis4", width="66%", height="800px")
                
              ),
              div(
                p(class = 'marginBottomZero', 'Euroopa Sotsiaaluuringu andmed on kõigile vabaks kasutamiseks. Vaata lähemalt: ', a('Euroopa Sotsiaaluringu Eesti veebikodu.', href='https://www.yti.ut.ee/et/euroopa-sotsiaaluuring')),
                p(class = 'marginBottomZero', HTML('<br>Kontseptuaalne teostus: Oliver Nahkur, Mare Ainsaar')),
                p(class = 'marginBottomZero', 'Tehniline teostus: Lisanna Lehes')
              )
              
      ) #,
    )
  )
  
)

server <- function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  #store the results
  Results <- reactive(c(
    input$slider1,  input$slider2, input$radio1, input$radio2, input$radio3, input$radio4, input$radio5, input$radio6, input$radio7, input$radio8, input$radio9, input$slider3))
  
  
  observeEvent(input$select, {
    skaleeritud_andmed = skaleeri_andmed(Results())
    vastajaga_andmed = lisa_vastaja_rida(andmed, skaleeritud_andmed)
    
    output$joonis4 = renderPlotly({plot_koik_heaolud_riigid(vastajaga_andmed, input$select)})
  })
  
  # Plots
  observeEvent(input$edasi_12, {  
    skaleeritud_andmed = skaleeri_andmed(Results())
    vastajaga_andmed = lisa_vastaja_rida(andmed, skaleeritud_andmed)
    print(vastajaga_andmed)
    output$joonis = renderPlotly({plot_yldine_heaolu(vastajaga_andmed)})
    output$heaoluTaseNumber = renderText({ render_heaolu_tase_number(vastajaga_andmed) })
    output$heaoluTase = renderText({ render_heaolu_tase(vastajaga_andmed) })
    
    output$joonis2 = renderPlotly({plot_heaolud(vastajaga_andmed)})
    output$afektiivneTase = renderText({render_afektiivne(vastajaga_andmed)})
    output$afektiivneNumber = renderText({render_afektiivne_number(vastajaga_andmed)})
    output$kogTase = renderText({render_kognitiivne(vastajaga_andmed)})
    output$kogNumber = renderText({render_kognitiivne_number(vastajaga_andmed)})
    output$eudaTase = renderText({render_eudaineemiline(vastajaga_andmed)})
    output$eudaNumber = renderText({render_eudaineemiline_number(vastajaga_andmed)})
    
    
    render_dimensiooniTekstid(output, vastajaga_andmed)
    output$joonis3 = renderHighchart({plot_koik_heaolud(vastajaga_andmed)})
    
    output$joonis4 = renderPlotly({plot_koik_heaolud_riigid(vastajaga_andmed, input$select)}) 
    output$heaoluEesti = renderText({render_heaoluEesti(vastajaga_andmed)})
    
    
    
    output$menu <- renderMenu({
      sidebarMenu(
        id = 'tabs',
        menuItem("Tutvustus", tabName = "tutvustus", icon = icon("info")),
        menuItem("Küsimustik", tabName = "kysimused", icon = icon("question")),
        menuItem("Tulemused", tabName = "tulemused", icon = icon("bar-chart")),
        span(class='hide', menuItem("saladus", tabName = "kysimus_1", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_2", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_3", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_4", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_5", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_6", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_7", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_8", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_9", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_10", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_11", icon = icon("th"))),
        span(class='hide', menuItem("saladus", tabName = "kysimus_12", icon = icon("th")))
      )
      })
  })
  
  
  output$menu <- renderMenu({
    sidebarMenu(
      id = 'tabs',
      menuItem("Tutvustus", tabName = "tutvustus", icon = icon("info")),
      menuItem("Küsimustik", tabName = "kysimused", icon = icon("question")),
      menuItem("Tulemused", tabName = "tulemused-tuhi", icon = icon("bar-chart")),
      span(class='hide', menuItem("saladus", tabName = "kysimus_1", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_2", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_3", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_4", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_5", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_6", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_7", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_8", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_9", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_10", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_11", icon = icon("th"))),
      span(class='hide', menuItem("saladus", tabName = "kysimus_12", icon = icon("th")))
    )
  })
  
  
  ### back and forth BUTTONS
  
  observeEvent(input$switchtab, {
    newtab <- switch(input$tabs, "tutvustus" = "kysimused")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$switchtab1, {
    newtab <- switch(input$tabs, "tulemused-tuhi" = "kysimused")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$edasi, {
    print(input)
    newtab <- switch(input$tabs, "kysimused" = "kysimus_1")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 1
  observeEvent(input$edasi_1, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_1" = "kysimus_2")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_1, {
    newtab <- switch(input$tabs, "kysimus_1" = "kysimused")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 2
  observeEvent(input$edasi_2, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_2" = "kysimus_3")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_2, {
    newtab <- switch(input$tabs, "kysimus_2" = "kysimus_1")
    updateTabItems(session, "tabs", newtab)
  })
  # 3
  observeEvent(input$edasi_3, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_3" = "kysimus_4")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_3, {
    newtab <- switch(input$tabs, "kysimus_3" = "kysimus_2")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 4
  observeEvent(input$edasi_4, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_4" = "kysimus_5")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_4, {
    newtab <- switch(input$tabs, "kysimus_4" = "kysimus_3")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 5
  observeEvent(input$edasi_5, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_5" = "kysimus_6")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_5, {
    newtab <- switch(input$tabs, "kysimus_5" = "kysimus_4")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 6
  observeEvent(input$edasi_6, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_6" = "kysimus_7")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_6, {
    newtab <- switch(input$tabs, "kysimus_6" = "kysimus_5")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 7
  observeEvent(input$edasi_7, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_7" = "kysimus_8")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_7, {
    newtab <- switch(input$tabs, "kysimus_7" = "kysimus_6")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 8
  observeEvent(input$edasi_8, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_8" = "kysimus_9")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_8, {
    newtab <- switch(input$tabs, "kysimus_8" = "kysimus_7")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 9
  observeEvent(input$edasi_9, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_9" = "kysimus_10")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_9, {
    newtab <- switch(input$tabs, "kysimus_9" = "kysimus_8")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 10
  observeEvent(input$edasi_10, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_10" = "kysimus_11")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_10, {
    newtab <- switch(input$tabs, "kysimus_10" = "kysimus_9")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 11
  observeEvent(input$edasi_11, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_11" = "kysimus_12")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_11, {
    newtab <- switch(input$tabs, "kysimus_11" = "kysimus_10")
    updateTabItems(session, "tabs", newtab)
  })
  
  # 12
  observeEvent(input$edasi_12, {
    print(input)
    newtab <- switch(input$tabs, "kysimus_12" = "tulemused")
    updateTabItems(session, "tabs", newtab)
  })
  observeEvent(input$tagasi_12, {
    newtab <- switch(input$tabs, "kysimus_12" = "kysimus_11")
    updateTabItems(session, "tabs", newtab)
  })
  
}

shinyApp(ui, server)
