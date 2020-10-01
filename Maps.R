library(sf)
library(sp)
library(httr)
library(rgeos)
library(tidyverse) 
library(ows4R)
library(ggplot2)
library(ggspatial)
library(utils)
library(downloader)
library(unikn)
library(randomcoloR)
library(viridis)
library(readxl)
library(openxlsx)

readSHP <- function(url){
  temp <- tempfile()
  temp2 <- tempfile()
  download(url, dest=temp, mode="wb")
  unzip(zipfile = temp, exdir = temp2)
  shpfile <- list.files(temp2, pattern = ".shp",full.names=TRUE)[1]
  shp <- read_sf(shpfile, options = "ENCODING=WINDOWS-1252")
  unlink(c(temp, temp2), recursive=TRUE)
  rm(temp, temp2, shpfile)
  return(shp)
}
readXLSX <- function(url, srow){
  temp = tempfile(fileext = ".xlsx")
  download.file(url, dest=temp, mode='wb')
  
  return(read.xlsx(temp, startRow = srow))
  
  unlink(temp, recursive=TRUE)
  rm(temp)
}
paavo_request <- function(){
  wfs_paavo <- "http://geo.stat.fi/geoserver/wfs"
  url <- parse_url(wfs_paavo)
  url$query <- list(service = "WFS",
                    version = "1.0.0",
                    request = "GetFeature",
                    typeName = "postialue:pno_tilasto",
                    outputFormat = "SHAPE-ZIP")
  return(build_url(url))
}
select_province <- function(pro.name){
  maakunta <- kunnat[kunnat$Maakunnan.nimi==pro.name, ]
  maakunta.kunnat <- unique(maakunta$Kuntanumero)
  shp <- paavo_shp[paavo_shp$kunta %in% maakunta.kunnat, ]
  shp <- merge(shp, data.frame(kunta=maakunta$Kuntanumero, kunta.nimi = maakunta$Kunnan.nimi))
  return(shp)
}
combine_areas <- function(shp.file, numeric.vars){
  new.df <- data.frame()
  names <- as.character(unique(shp.file$kunta.nimi))
  
  for(name in names){
    temp.df <- shp.file[shp.file$kunta.nimi == name, ]
    n.df <- data.frame(temp.df[, numeric.vars])[, 1:length(numeric.vars)]
    numvars <- data.frame(rbind(apply(n.df, 2, sum)))
    geometry = st_union(temp.df)
    new.row <- cbind(nimi=name, numvars, geometry=geometry)
    new.df <- data.frame(rbind(new.df, new.row))
  }
  
  return(new.df)
}


request <- paavo_request()
paavo_shp <- readSHP(request)
kunnat <- readXLSX("https://www.kuntaliitto.fi/sites/default/files/media/file/Alueluokat%20ja%20kuntanumerot%202020_0.xlsx", 9)

ppmaa <- select_province("Pohjois-Pohjanmaa")
ppmaa.combine <- combine_areas(ppmaa, c("he_vakiy", "he_naiset", "he_miehet", "pinta_ala",
                                        "pt_tyott", "pt_opisk", "pt_vakiy", "pt_0_14",
                                        "tr_pi_tul", "tr_ke_tul", "tr_ke_tul"))

xmean <- ymean <- rep(0,30)
for(i in 1:30){
  if(!is.null(dim(ppmaa.combine[i,"geometry"][[1]][1][[1]]))){
    xmean[i] <- median(ppmaa.combine[i,"geometry"][[1]][1][[1]][,1])
    ymean[i] <- median(ppmaa.combine[i,"geometry"][[1]][1][[1]][,2])
  }
  else{
    xmean[i] <- median(ppmaa.combine[i,"geometry"][[1]][1][[1]][[1]][,1])
    ymean[i] <- median(ppmaa.combine[i,"geometry"][[1]][1][[1]][[1]][,2])
  }
  
}

ppmaa.combine$xmean <- xmean
ppmaa.combine$ymean <- ymean
ppmaa.combine$vaki.5g <- factor(cut(ppmaa.combine$he_vakiy, breaks = c(0,5000, 10000, 20000,50000, Inf)),
                                labels=c("alle 5 000", "5 000-10 000", "10 000 - 20 000","20 000 - 50 0000" ,"yli 50 0000"))


suomi <- st_union(paavo_shp)

## MAPS:

ggplot(data=suomi) + geom_sf() + geom_sf(data=ppmaa.combine, aes(geometry=geometry, fill=nimi)) + coord_sf() +
  scale_fill_manual(values=distinctColorPalette(30))+ 
  labs(fill = "Pohjois-Pohjanmaan kunnat",
       title = "Pohjois-Pohjanmaa",
       subtitle = "Data: Paavo - Postinumeroalueittainen avoin tieto") + theme_bw()


ggplot() + geom_sf(data=ppmaa.combine, aes(fill=vaki.5g, geometry=geometry))+coord_sf()+
  geom_text(data=ppmaa.combine, aes(x=xmean, y=ymean, label=nimi), angle=0,
            check_overlap = TRUE)+
  scale_fill_manual(values=usecol(pal_petrol))+ 
  labs(title="Pohjois-Pohjanmaan asukasluvut", fill="Asukasluku",
       subtitle = "Data: Paavo - Postinumeroalueittainen avoin tieto")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        plot.title = element_text(size=22)) +
  annotate("text", x = 524000, y = 7226900, label = "©hannaeve",
           col="white", size=3.5,
           fontface = "bold", alpha = 0.6)


