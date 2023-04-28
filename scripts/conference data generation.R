
# Protocol code: Reputation management during a public health crisis: Overcompensating when all else fails. PAR, 2023.
# Varela Castro, Bustos & Saldivia Gonzatti - April 2023.

# The code includes the press conference data generation and the generation of the data for qualitative validation.

# Necessary libraries ====
require("pacman")
p_load(ggplot2, ggcorrplot, quanteda, quanteda.textmodels, quanteda.textstats, 
       quanteda.textplots, stringi, tibble, purrr, tidyr, stm,
       readtext, openxlsx, ggcorrplot, devtools, newsmap, seededlda, stopwords, 
       ggpubr, lubridate,stringr, car,
       rmarkdown, dplyr, tidyverse, igraph, RcppRoll, patchwork, dynlm, lmtest, sandwich, vars)

# Define working directory ====
# setwd("../..")

# Conference data ====

data <- readtext("*.docx", cache = TRUE,
                 encoding = "UTF-8",
                 docvarsfrom = "filenames", dvsep="-",
                 docvarnames = c("doc"))                

data <- data %>% mutate(date = as.Date(paste0("20",stringr::str_extract(doc,"^2\\d"),"-", # year
                                              gsub("^2\\d","",stringr::str_extract(doc,"^2\\d{3}")),"-", # month
                                              gsub("^2\\d{3}","",stringr::str_extract(doc,"^2\\d{5}"))))) # day

# save(data, file = "../data-raw_joint.RData") # Local safe, not available online. Request the data by mail.

# Split code ====
strsplit2 <- function(x,split,type = "remove",perl = FALSE,...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

# Extract interventions from transcriptions ====

data_interventions <- data.frame()

for(i in 1:nrow(data)){
  
  interventions <- strsplit2(x = data$text[i],
                             "\\\n[A-Z]{4}.+?(?=[:])|\\\nDOCTOR\\s+V?CTOR\\s+HUGO|\\\nINTERLOCUTOR(|A)|\\\nPREGUNTA",
                             type = "before") %>%
    unlist() %>%
    as.data.frame() 
  
  interventions <- str_split_fixed(interventions$., "[:]|INICIA\\s+VIDEO[)]\\s+|ABURTO[.][-]", 2) %>% 
    as.data.frame() %>% rename(actor = 1, intervention_text = 2)
  
  interventions$session <- data$doc[i]
  
  data_interventions <- data_interventions %>% bind_rows(interventions)
  print(i)
  gc()
  
  if(i == nrow(data)){
    rm(i, interventions)
  }
  
}

gc()

# Order and clean interventions

data_interventions <- data_interventions %>% 
  mutate(actor = trimws(actor),
         intervention_text = trimws(intervention_text),
         actor_name = gsub("[,].*","",actor),
         actor_name = ifelse(grepl("rsi?n\\s+estenogr?|Conferencia",actor_name),NA,actor_name))

# Extract actor name
data_interventions <- data_interventions %>% 
  mutate(actor_name = ifelse(grepl("GATELL",actor_name),"HUGO L?PEZ-GATELL RAM?REZ", actor_name))

# Create actor frequency table
actors <- table(data_interventions$actor_name) %>% as.data.frame() %>% 
  arrange(-Freq) %>% mutate(total = sum(Freq),
                            share = Freq/total*100) %>% 
  mutate(cumulate_sum = cumsum(share)) %>% 
  mutate(Freq_cl = ifelse(grepl("PREGUNTA|INTERLOCUTOR",Var1),NA,Freq)) %>% 
  mutate(total_cl = sum(Freq_cl, na.rm = TRUE), share_cl = Freq_cl/total_cl*100) %>% 
  mutate(share_cl = ifelse(is.na(share_cl),0,share_cl)) %>% 
  mutate(cumulate_sum_cl = cumsum(share_cl)) 

# Most prominent actors (more frequent) and roles

# JOS? LUIS ALOM?A ZEGARRA, DIRECTOR GENERAL DE EPIDEMIOLOG?A
# HUGO L?PEZ-GATELL RAM?REZ, SUBSECRETARIO DE PREVENCI?N Y PROMOCI?N DE LA SALUD
# RICARDO CORT?S ALCAL?, DIRECTOR GENERAL DE PROMOCI?N DE LA SALUD
# RUY L?PEZ RIDAURA, DIRECTOR GENERAL DEL CENTRO NACIONAL DE PROGRAMAS PREVENTIVOS Y CONTROL DE ENFERMEDADES (CENAPRECE)
# ANA LUC?A DE LA GARZA BARROSO, DIRECTORA DE INVESTIGACI?N OPERATIVA EPIDEMIOL?GICA
# SANTA ELIZABETH CEBALLOS LICEAGA, DIRECTORA DE VIGILANCIA EPIDEMIOL?GICA DE ENFERMEDADES TRANSMISIBLES
# ALETHSE DE LA TORRE ROSAS, DIRECTORA GENERAL DEL CENTRO NACIONAL DE PREVENCI?N Y EL CONTROL DEL VIH Y EL SIDA (CENSIDA)
# KARLA BERDICHEVSKY FELDMAN, DIRECTORA GENERAL DEL CENTRO NACIONAL DE EQUIDAD DE G?NERO Y SALUD REPRODUCTIVA
# ARLETTE SAAVEDRA ROMERO, ENCARGADA DE DESPACHO LA DIRECCI?N DE ESTRATEGIAS Y DESARROLLO DE ENTORNOS SALUDABLES, DIRECCI?N GENERAL DE PROMOCI?N DE LA SALUD
# (INICIA VIDEO)
# ANTA ELIZABETH CEBALLOS LICEAGA, DIRECTORA DE VIGILANCIA EPIDEMIOL?GICA DE ENFERMEDADES TRANSMISIBLES
# CHRISTIAN ARTURO ZARAGOZA JIM?NEZ, DIRECTOR DE INFORMACI?N EPIDEMIOL?GICA DE LA DIRECCI?N GENERAL DE EPIDEMIOLOG?A
# GADY ZABICKY SIROT, TITULAR DE LA COMISI?N NACIONAL CONTRA LAS ADICCIONES (CONADIC):
# JORGE GONZ?LEZ OLVERA, DIRECTOR GENERAL DE CONADIC
# NADIA ROBLES SOTO, DIRECTORA DE LA COORDINACI?N DE PROGRAMAS NACIONALES CONTRA LAS ADICCIONES; RESPONSABLE DEL OBSERVATORIO MEXICANO DE DROGAS (OMD), CONADIC  
# ALEJANDRA FRAUSTO GUERRERO, SECRETARIA DE CULTURA
# V?CTOR HUGO BORJA ABURTO, DIRECTOR DE PRESTACIONES M?DICAS DEL INSTITUTO MEXICANO DEL SEGURO SOCIAL (IMSS)

# Get word count

data_interventions <- data_interventions %>% 
  mutate(words = ntoken(intervention_text))

summary(data_interventions$words)

# Descriptive stats
data_interventions %>% filter(actor_name=="HUGO L?PEZ-GATELL RAM?REZ") %>% 
  summarise(mean_words = mean(words))

# Generate full data with original transcripts
data_interventions <- data_interventions %>% 
  left_join(data %>% select(-text), by = c("session" = "doc"))

data_interventions <- data_interventions %>% 
  group_by(doc_id) %>% mutate(id = row_number()) %>% ungroup()


# Get dictionary ====

dictionary.reputation <- openxlsx::read.xlsx("../../D_DictionaryReputation/ReputationDimensions_Dictionary_v1.xlsx") %>% 
  filter(!is.na(palabra))

dictionary.reputation <- quanteda::dictionary(list(
  funcional = dictionary.reputation %>% filter(dictionary=="funcional") %>% dplyr::select(palabra) %>% unlist() %>%  c(),
  legal = dictionary.reputation %>% filter(dictionary=="legal") %>% dplyr::select(palabra) %>%  unlist() %>%  c(),
  moral = dictionary.reputation %>% filter(dictionary=="moral") %>% dplyr::select(palabra) %>% unlist() %>%  c(),
  tecnico = dictionary.reputation %>% filter(dictionary=="tecnico") %>% dplyr::select(palabra) %>% unlist() %>%  c()
))


# Create corpus, tokens and DFM with dictionary ====

conferencias_corpus.interv  <- quanteda::corpus(data_interventions %>% select(-doc_id), 
                                                text_field= "intervention_text")

conferencias_tokens.interv <- quanteda::tokens(conferencias_corpus.interv, 
                                               remove_punct = FALSE,
                                               remove_symbols=FALSE, 
                                               remove_separators=FALSE,
                                               split_hyphens = TRUE, 
                                               remove_numbers = FALSE,
                                               remove_url=FALSE) %>% quanteda::tokens_tolower()

conferencias_dmf.interv <- quanteda::dfm(conferencias_tokens.interv,
                                         dictionary = c(dictionary.reputation))

data_interventions <- data_interventions %>% bind_cols(conferencias_dmf.interv %>% as.data.frame())
data_interventions <- data_interventions %>% select(-doc_id...9) %>% rename(doc_id = doc_id...6)

# Clean environ.
rm(conferencias_dmf.interv, conferencias_tokens.interv, conferencias_corpus.interv)

# Generate gatell specific data for validation ====
gatell <- data_interventions %>% filter(actor_name=="HUGO LÓPEZ-GATELL RAMÍREZ") %>% 
  mutate(across(c(funcional:tecnico), ~ ./words*100, .names = "{.col}_share"))

# Generate random data for validation
set.seed(20313)
gatell_sh.random <- gatell %>% filter(words >= 100 & words <=900) %>% sample_n(200)
gatell_sh.random <- gatell_sh.random %>% mutate(share_sum = tecnico_share+funcional_share+moral_share+legal_share) %>% 
  sample_n(nrow(.))

summary(gatell_sh.random$share_sum)

# save(gatell_sh.random, file = "data/datos_valición.medios_gatell.share.RData")

# Data for validation ====

gatell_sh.random <- gatell_sh.random %>% select(-c(funcional:share_sum))
gatell_sh.random <- gatell_sh.random %>% relocate(intervention_text, .before = NULL, .after = id)

gatell_sh.random$samanta <- c(rep(1,130),rep(0,70))
gatell_sh.random$daniel  <- c(rep(0,100),rep(1,70),rep(0,30))
gatell_sh.random$edgar   <- c(rep(0,80),rep(1,30),rep(0,50),rep(1,40))

gatell_sh.random <- gatell_sh.random %>% 
  mutate(legal_quali = NA,
         moral_quali = NA,
         funcional_quali = NA, 
         tecnico_quali = NA)

# openxlsx::write.xlsx(gatell_sh.random, file = "../validación_quali.xlsx")

