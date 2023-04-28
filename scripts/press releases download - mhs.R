# Protocol code: Reputation management during a public health crisis: Overcompensating when all else fails. PAR, 2023.
# Varela Castro, Bustos & Saldivia Gonzatti - April 2023.

# The code covers the access and download of the press releases from the Mexican Health Secretariat

# Necessary libraries ====

library(httr) 
library(XML)  
library(ggplot2)
library(dplyr)

# Set config ====
usuario <- "Daniel Saldivia Gonzatti, Investigador WZB, R Scraper"
correo  <- "Correo: ---"

# Pages available by the time of download ====
# (last: 26-03-2022) 
urls <- paste0("https://www.gob.mx/salud/es/archivo/prensa?idiom=es&order=DESC&page=",1:689)
todos_links <- character()

# Loop for links collections ====

for(i in 1:length(urls)){  
  pagina <- httr::GET(urls[i], httr::add_headers(User = usuario, From = correo))
  arbol_resultados <- XML::htmlParse(pagina)
  links_temporales <- c(XML::xpathSApply(arbol_resultados,
                                         '//*[(@id = "prensa")]//*[contains(concat( " ", @class, " " ), concat( " ", "small-link", " " ))]', XML::xmlGetAttr, "href")) 
  todos_links <- c(todos_links,links_temporales)
  Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  print(i)
} 

# Clean environ.
rm(i, links_temporales, arbol_resultados, pagina) 

#save(todos_links, file = "links_press.releaser_HealthSecretariat_17022022.RData") 

# Paste initial URL adress
todos_links <- paste0("https://www.gob.mx/", todos_links)

# Define time zone
Sys.setenv(TZ="Europe/Berlin") # Mexico/General

# Generate local folder
dir.create("HTMLs_secretaria.salud")

# Download HTMLs and local saving
for(i in 1:length(todos_links)){ 
  html_temporal <- httr::GET(todos_links[i], httr::add_headers(User = usuario, From = correo))
  cat(httr::content(html_temporal, "text"), file = paste0("HTMLs_secretaria.salud/HTML_post-",i,".html"))
  Sys.sleep(sample(seq(2, 5, by=0.001), 1))
}

rm(html_temporal, i)

# Get all files
setwd("./HTMLs_secretaria.salud/")
list.files()


# Generate database ====

data_comunicados <- data.frame()
options(stringsAsFactors = FALSE) # Config.

# Obtain data through HTML language patterns

for(i in 1:length(todos_links)){
  tryCatch({ # Get mistakes, ignore them [usually due to empty HTMLs]
    
    temporal <- readLines(paste0("HTML_post-",i,".html"))
    temporal <- stringr::str_c(temporal, collapse = "")
    temporal <- XML::htmlParse(temporal, encoding="UTF-8")
    
    titulo <- xpathSApply(temporal, '//*[contains(concat( " ", @class, " " ), concat( " ", "bottom-buffer", " " ))]', xmlValue)
    titulo   <- ifelse(length(titulo)==0,NA,titulo)
    
    entrada <- xpathSApply(temporal,  '//*[contains(concat( " ", @class, " " ), concat( " ", "pull-left", " " ))]//h2', xmlValue)
    entrada   <- ifelse(length(entrada)==0,NA,entrada)
    
    texto_cuerpo <- xpathSApply(temporal, '//*[contains(concat( " ", @class, " " ), concat( " ", "article-body", " " ))]', xmlValue)
    texto_cuerpo   <- ifelse(length(texto_cuerpo)==0,NA,texto_cuerpo)
    
    fecha <- xpathSApply(temporal, '//dd[(((count(preceding-sibling::*) + 1) = 4) and parent::*)]', xmlValue)
    fecha   <- ifelse(length(fecha)==0,NA,fecha)
    
    tipo <- xpathSApply(temporal, '//dd[(((count(preceding-sibling::*) + 1) = 6) and parent::*)]', xmlValue) 
    tipo   <- ifelse(length(tipo)==0,NA,tipo)
    
    link <- todos_links[i] # Add link
    
    contador <- i
    
    print(i) 
    rm(temporal)
    
    if (length(texto_cuerpo) > 0 ){ # If HTML is not empty, then...
      data_comunicados <- rbind(data_comunicados,c(contador, titulo, entrada, texto_cuerpo, fecha, tipo, link))
    } # If HTML is empty, then...
  }, error=function(e){cat("ERROR :",i,conditionMessage(e), "\n")})
}

# Adapt var names and trim texts
names(data_comunicados)<- c("contador", "titulo", "entrada", "texto_cuerpo", "fecha", "tipo", "link")
data_comunicados <- data_comunicados %>% 
  mutate(titulo = trimws(titulo),
         entrada = trimws(entrada),
         texto_cuerpo = trimws(texto_cuerpo))

rm(list=setdiff(ls(), c("data_comunicados"))) 

# Clean date var in the data
data_comunicados <- data_comunicados %>% 
  mutate(date = case_when(grepl("enero", fecha) ~ "01",
                          grepl("febrero", fecha) ~ "02",
                          grepl("marzo", fecha) ~ "03",
                          grepl("abril", fecha) ~ "04",
                          grepl("mayo", fecha) ~ "05",
                          grepl("junio", fecha) ~ "06",
                          grepl("julio", fecha) ~ "07",
                          grepl("agosto", fecha) ~ "08",
                          grepl("septiembre", fecha) ~ "09",
                          grepl("octubre", fecha) ~ "10",
                          grepl("noviembre", fecha) ~ "11",
                          grepl("diciembre", fecha) ~ "12")) %>% 
  mutate(date = as.Date(paste0(
    stringr::str_extract(fecha, "\\d{4}$"),"-",
    date, "-", stringr::str_extract(fecha, "^\\d{2}")
  )))


# save(data_comunicados, file = "data/data_comunicados_salud_21022022.RData")

# Filter press releases to start 2013 and get months

data_comunicados <- data_comunicados %>% filter(date>="2013-01-01")
data_comunicados <- data_comunicados %>% mutate(date_month = as.Date(paste0(gsub("\\d{2}$","",date),"01")))

# Visualize amount of press releases over time
time_aggr <- data_comunicados %>% 
  group_by(date_month) %>% 
  tally()

ggplot(time_aggr, aes(date_month, n)) + geom_line()













