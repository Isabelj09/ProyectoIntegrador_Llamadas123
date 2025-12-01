library(sf)
library(tidyverse)
library(readxl)
library(writexl)
library(magick)
library(ggimage)
library(ggtext)

comunas <- st_read('Shapes/Comuna/Comuna.shp')
barrio   <- st_read('Shapes/Barrio/Barrio.shp')
manzana  <- st_read('Shapes/Manzanas_med/Manzana.shp')
barrios_priorizados <- read_excel('barrios_priorizados.xlsx')

componentes <- c(
  "Riña","Hurto calificado","Perturbacion",
  "Hurto entidad comercial","Hurto simple o raponazo",
  "Vehiculo hurtado","Violacion de domicilio"
)

colores_5 <- c(
  "0 (Sin casos)" = "#d9d9d9",
  "1–5"  = "#a8d7ff",
  "6–10" = "#7bbcff",
  "11–20"= "#4f97e6",
  "20+"  = "#083e87"
)

niveles_5 <- c("0 (Sin casos)","1–5","6–10","11–20","20+")

clasificar5 <- function(x){
  case_when(
    x == 0 ~ "0 (Sin casos)",
    x <= 5  ~ "1–5",
    x <= 10 ~ "6–10",
    x <= 20 ~ "11–20",
    TRUE    ~ "20+"
  )
}

# =============================
# MAPAS POR COMPONENTE (5 CAT)
# =============================
for (i in componentes) {
  tabla <- barrios_priorizados %>%
    filter(grupo_caso == i) %>%
    count(codigo_barrio, name="Apariciones")
  
  mapa <- barrio %>%
    left_join(tabla, by=c("CODIGO"="codigo_barrio")) %>%
    mutate(Apariciones = replace_na(Apariciones,0),
           Apariciones = factor(clasificar5(Apariciones), levels=niveles_5))
  
  p <- ggplot() +
    geom_sf(data=mapa, aes(fill=Apariciones), color="black", size=0.1) +
    geom_sf(data=comunas, color="black", fill=NA, size=0.4) +
    geom_richtext(data=comunas, aes(label=NOMBRE, geometry=geometry),
                  stat="sf_coordinates", size=2, label.color=NA,
                  fill=alpha("white",0.5), label.padding=unit(c(1,1,1,1),"pt")) +
    scale_fill_manual(values=colores_5, drop=FALSE) +
    ggtitle(i) + theme_void() +
    theme(plot.title=element_text(hjust=0.5,size=22,face="bold"))
  
  ggsave(paste0("Mapa_priorizacion_",gsub(" ","_",i),".jpg"),
         p, width=21, height=21, units="cm", dpi=300)
}

# =============================
# MAPA TOTAL (5 CAT)
# =============================
tabla_total <- barrios_priorizados %>% count(codigo_barrio, name="Apariciones")

mapa_total <- barrio %>%
  left_join(tabla_total, by=c("CODIGO"="codigo_barrio")) %>%
  mutate(Apariciones=replace_na(Apariciones,0),
         Apariciones=factor(clasificar5(Apariciones), levels=niveles_5))

p_total <- ggplot() +
  geom_sf(data=mapa_total, aes(fill=Apariciones), color="black", size=0.1) +
  geom_sf(data=comunas, color="black", fill=NA, size=0.4) +
  geom_richtext(data=comunas, aes(label=NOMBRE, geometry=geometry),
                stat="sf_coordinates", size=2, label.color=NA,
                fill=alpha("white",0.5), label.padding=unit(c(1,1,1,1),"pt")) +
  scale_fill_manual(values=colores_5, drop=FALSE) +
  ggtitle("Total priorización") + theme_void() +
  theme(plot.title=element_text(hjust=0.5,size=22,face="bold"))

ggsave("Mapa_priorizacion_total.jpg", p_total,
       width=21, height=21, units="cm", dpi=300)

# =============================
# MAPA POR CADA CATEGORÍA
# =============================
for (cat in niveles_5) {
  sub <- mapa_total %>%
    mutate(keep = ifelse(Apariciones == cat, cat, NA),
           keep = factor(keep, levels = niveles_5))
  
  pcat <- ggplot() +
    geom_sf(data=sub, aes(fill=keep), color="black", size=0.1, na.rm=FALSE) +
    geom_sf(data=comunas, color="black", fill=NA, size=0.4) +
    geom_richtext(
      data = comunas,
      aes(label = NOMBRE, geometry = geometry),
      stat = "sf_coordinates",
      size = 2,
      label.color = NA,
      fill = alpha("white", 0.5),
      label.padding = unit(c(1,1,1,1), "pt")
    ) +
    scale_fill_manual(
      name = "Apariciones",
      values = colores_5,
      drop = TRUE,
      na.value = "#ffffff00"
    ) +
    ggtitle(paste0("Categoría: ",cat)) +
    theme_void() +
    theme(plot.title = element_text(hjust=0.5, size=22, face="bold"))
  
  ggsave(
    paste0("Mapa_total_categoria_", gsub(" ","_",cat), ".jpg"),
    pcat, width=21, height=21, units="cm", dpi=300
  )
}


