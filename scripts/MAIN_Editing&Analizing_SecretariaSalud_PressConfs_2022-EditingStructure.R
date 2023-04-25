### BDD Gatell

# Paqueterías
require("pacman")
p_load(ggplot2, ggcorrplot, quanteda, quanteda.textmodels, quanteda.textstats, 
       quanteda.textplots, stringi, tibble, purrr, tidyr, stm,
       readtext, openxlsx, ggcorrplot, devtools, newsmap, seededlda, stopwords, 
       ggpubr, lubridate,stringr, car,
       rmarkdown, dplyr, tidyverse, igraph, RcppRoll, patchwork, dynlm, lmtest, sandwich, vars)
setwd("../D_Data/Vespertinas")

# Datos conferencias ====

data <- readtext("*.docx", cache = TRUE,
                  encoding = "UTF-8",
                  docvarsfrom = "filenames", dvsep="-",
                  docvarnames = c("doc"))                

data <- data %>% mutate(date = as.Date(paste0("20",stringr::str_extract(doc,"^2\\d"),"-", # year
                                              gsub("^2\\d","",stringr::str_extract(doc,"^2\\d{3}")),"-", # month
                                              gsub("^2\\d{3}","",stringr::str_extract(doc,"^2\\d{5}"))))) # day

# save(data, file = "../data-raw_joint.RData")
load("../data-raw_joint.RData")
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

data_interventions <- data.frame()

for(i in 1:nrow(data)){
  
  interventions <- strsplit2(x = data$text[i],
                             "\\\n[A-Z]{4}.+?(?=[:])|\\\nDOCTOR\\s+VÍCTOR\\s+HUGO|\\\nINTERLOCUTOR(|A)|\\\nPREGUNTA",
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

data_interventions <- data_interventions %>% 
  mutate(actor = trimws(actor),
         intervention_text = trimws(intervention_text),
         actor_name = gsub("[,].*","",actor),
         actor_name = ifelse(grepl("rsión\\s+estenográ|Conferencia",actor_name),NA,actor_name))

data_interventions <- data_interventions %>% 
  mutate(actor_name = ifelse(grepl("GATELL",actor_name),"HUGO LÓPEZ-GATELL RAMÍREZ", actor_name))

actors <- table(data_interventions$actor_name) %>% as.data.frame() %>% 
  arrange(-Freq) %>% mutate(total = sum(Freq),
                            share = Freq/total*100) %>% 
  mutate(cumulate_sum = cumsum(share)) %>% 
  mutate(Freq_cl = ifelse(grepl("PREGUNTA|INTERLOCUTOR",Var1),NA,Freq)) %>% 
  mutate(total_cl = sum(Freq_cl, na.rm = TRUE), share_cl = Freq_cl/total_cl*100) %>% 
  mutate(share_cl = ifelse(is.na(share_cl),0,share_cl)) %>% 
  mutate(cumulate_sum_cl = cumsum(share_cl)) 

# Possible actors:

# JOSÉ LUIS ALOMÍA ZEGARRA, DIRECTOR GENERAL DE EPIDEMIOLOGÍA
# HUGO LÓPEZ-GATELL RAMÍREZ, SUBSECRETARIO DE PREVENCIÓN Y PROMOCIÓN DE LA SALUD
# RICARDO CORTÉS ALCALÁ, DIRECTOR GENERAL DE PROMOCIÓN DE LA SALUD
# RUY LÓPEZ RIDAURA, DIRECTOR GENERAL DEL CENTRO NACIONAL DE PROGRAMAS PREVENTIVOS Y CONTROL DE ENFERMEDADES (CENAPRECE)
# ANA LUCÍA DE LA GARZA BARROSO, DIRECTORA DE INVESTIGACIÓN OPERATIVA EPIDEMIOLÓGICA
# SANTA ELIZABETH CEBALLOS LICEAGA, DIRECTORA DE VIGILANCIA EPIDEMIOLÓGICA DE ENFERMEDADES TRANSMISIBLES
# ALETHSE DE LA TORRE ROSAS, DIRECTORA GENERAL DEL CENTRO NACIONAL DE PREVENCIÓN Y EL CONTROL DEL VIH Y EL SIDA (CENSIDA)
# KARLA BERDICHEVSKY FELDMAN, DIRECTORA GENERAL DEL CENTRO NACIONAL DE EQUIDAD DE GÉNERO Y SALUD REPRODUCTIVA
# ARLETTE SAAVEDRA ROMERO, ENCARGADA DE DESPACHO LA DIRECCIÓN DE ESTRATEGIAS Y DESARROLLO DE ENTORNOS SALUDABLES, DIRECCIÓN GENERAL DE PROMOCIÓN DE LA SALUD
# (INICIA VIDEO)
# ANTA ELIZABETH CEBALLOS LICEAGA, DIRECTORA DE VIGILANCIA EPIDEMIOLÓGICA DE ENFERMEDADES TRANSMISIBLES
# CHRISTIAN ARTURO ZARAGOZA JIMÉNEZ, DIRECTOR DE INFORMACIÓN EPIDEMIOLÓGICA DE LA DIRECCIÓN GENERAL DE EPIDEMIOLOGÍA
# GADY ZABICKY SIROT, TITULAR DE LA COMISIÓN NACIONAL CONTRA LAS ADICCIONES (CONADIC):
# JORGE GONZÁLEZ OLVERA, DIRECTOR GENERAL DE CONADIC
# NADIA ROBLES SOTO, DIRECTORA DE LA COORDINACIÓN DE PROGRAMAS NACIONALES CONTRA LAS ADICCIONES; RESPONSABLE DEL OBSERVATORIO MEXICANO DE DROGAS (OMD), CONADIC  
# ALEJANDRA FRAUSTO GUERRERO, SECRETARIA DE CULTURA
# VÍCTOR HUGO BORJA ABURTO, DIRECTOR DE PRESTACIONES MÉDICAS DEL INSTITUTO MEXICANO DEL SEGURO SOCIAL (IMSS)

 
data_interventions <- data_interventions %>% 
  mutate(words = ntoken(intervention_text))

summary(data_interventions$words)

data_interventions %>% filter(actor_name=="HUGO LÓPEZ-GATELL RAMÍREZ") %>% 
  summarise(mean_words = mean(words))

data_interventions <- data_interventions %>% 
  left_join(data %>% select(-text), by = c("session" = "doc"))

data_interventions <- data_interventions %>% 
  group_by(doc_id) %>% mutate(id = row_number()) %>% ungroup()


# Definiendo diccionario

dictionary.reputation <- openxlsx::read.xlsx("../../D_DictionaryReputation/ReputationDimensions_Dictionary_v1.xlsx") %>% 
  filter(!is.na(palabra))
dictionary.reputation <- quanteda::dictionary(list(
  funcional = dictionary.reputation %>% filter(dictionary=="funcional") %>% dplyr::select(palabra) %>% unlist() %>%  c(),
  legal = dictionary.reputation %>% filter(dictionary=="legal") %>% dplyr::select(palabra) %>%  unlist() %>%  c(),
  moral = dictionary.reputation %>% filter(dictionary=="moral") %>% dplyr::select(palabra) %>% unlist() %>%  c(),
  tecnico = dictionary.reputation %>% filter(dictionary=="tecnico") %>% dplyr::select(palabra) %>% unlist() %>%  c()
))


# Crear corpus, tokens y DFM con diccionario
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

rm(conferencias_dmf.interv, conferencias_tokens.interv, conferencias_corpus.interv)

gatell <- data_interventions %>% filter(actor_name=="HUGO LÓPEZ-GATELL RAMÍREZ") %>% 
  mutate(across(c(funcional:tecnico), ~ ./words*100, .names = "{.col}_share"))

set.seed(20313)
gatell_sh.random <- gatell %>% filter(words >= 100 & words <=900) %>% sample_n(200)
gatell_sh.random <- gatell_sh.random %>% mutate(share_sum = tecnico_share+funcional_share+moral_share+legal_share) %>% 
  sample_n(nrow(.))

summary(gatell_sh.random$share_sum)
 
#save.image(file = "../datos_valición.medios_gatell.share.RData")
load("../../D_Data/datos_valición.medios_gatell.share.RData")


gatell_sh.random <- gatell_sh.random %>% select(-c(funcional:share_sum))
gatell_sh.random <- gatell_sh.random %>% relocate(intervention_text, .before = NULL, .after = id)
gatell_sh.random$samanta <- c(rep(1,130),rep(0,70))
gatell_sh.random$daniel <- c(rep(0,100),rep(1,70),rep(0
                                                      ,30))
gatell_sh.random$edgar <- c(rep(0,80),rep(1,30),rep(0,50),rep(1,40))
gatell_sh.random <- gatell_sh.random %>% 
  mutate(legal_quali = NA,
         moral_quali = NA,
         funcional_quali = NA, 
         tecnico_quali = NA)

#openxlsx::write.xlsx(gatell_sh.random, file = "../D_Data/validación_quali.xlsx")
rm(gatell_sh.random)



# Twitter data ====

twitter_data <- readRDS("../../D_Data_Twitter/all_tweets_SSalud_mx.022020-022021.rds")
twitter_data <- twitter_data %>% filter(!author_id=="132225222")
twitter_data <- twitter_data %>% mutate(date = as.Date(created_at))
twitter_data <- twitter_data %>% 
  mutate(month = as.Date(paste0(lubridate::year(date),"-",lubridate::month(date),"-01")))

table(twitter_data$month >= as.Date("2020-02-01") & twitter_data$month <= as.Date("2021-06-01"))

tweet_count <- twitter_data %>% group_by(date) %>% summarise(count = n())


ggplot(tweet_count, aes(x = month, y = count)) + geom_line() + theme_minimal() + ylab("# Tweets") +
  xlab("Date")

tweets_corpus  <- quanteda::corpus(twitter_data, text_field= "text")


tweets_tokens <- quanteda::tokens(tweets_corpus, 
                                        remove_punct = FALSE,
                                        remove_symbols=FALSE, 
                                        remove_separators=FALSE,
                                        split_hyphens = TRUE, 
                                        remove_numbers = FALSE,
                                        remove_url=FALSE) %>% quanteda::tokens_tolower()

list <- ls()
load("C:\\Users\\saldivia-gonzatti\\Nextcloud\\Shared\\Análisis texto - LNPP\\Sesión_2\\auto_dictionaries_lsd.RData")

# Excluyamos todos los demás diccionarios en idiomas desde el francés hasta el
# húngaro.

rm(list=setdiff(ls(), c("extendeddict_es", list)))
tweets_dmf <- quanteda::dfm(tweets_tokens, dictionary = c(extendeddict_es))

twitter_data <- twitter_data %>% bind_cols(tweets_dmf %>%
                             as.data.frame()) 

tw.daily_sentiment.legit <- twitter_data %>% group_by(date) %>% 
  summarise(pos = sum(pos, na.rm = TRUE), neg = sum(neg, na.rm = TRUE))

tw.daily_sentiment.legit <- tw.daily_sentiment.legit %>% mutate(sentiment = base::log((pos+0.5)/(neg+0.5)))


ggplot(tw.daily_sentiment.legit, aes(x = date, y = sentiment)) + geom_line() + theme_minimal() + ylab("Tweets sentiment") +
  xlab("Date")

# save(tw.daily_sentiment.legit, tweet_count, file = "../twitter_aggregated.RData")


# S: Data preparada ====

# Cargar data conferencia
load("../../D_Data/datos_valición.medios_gatell.share.RData")
rm(gatell_sh.random, strsplit2)

actor_number <- data_interventions %>% filter(!grepl("^PREGUNTA$|^INTERLOCUTOR(|A)", actor_name)) %>%
  dplyr::select(actor, date) %>% 
  filter(!duplicated(.)) %>% 
  group_by(date) %>% summarise(actors_number = n())


ggplot(actor_number, aes(x = date, y = actors_number)) + geom_line() + theme_minimal() + ylab("Actors / conference") +
  xlab("Date")

summary(actor_number$actors_number) # summary


# Data at the day level - paper analysis

data_interventions <- data_interventions %>%
  mutate(across(c(funcional:tecnico), ~ ./words*100, .names = "{.col}_share"))

data_interventions.aggr <- data_interventions %>% group_by(date) %>% 
  summarise(intervention_text.day = paste0(intervention_text, collapse = " "),
            words_total = sum(words, na.rm = TRUE),
            funcional = sum(funcional, na.rm = TRUE),
            legal = sum(legal, na.rm = TRUE),
            moral = sum(moral, na.rm =  TRUE),
            tecnico = sum(tecnico, na.rm = TRUE)) %>% ungroup()

data_interventions.aggr <- data_interventions.aggr %>% left_join(actor_number)


# summary(data_interventions.aggr$funcional)

# Load Twitter data

load("../twitter_aggregated.RData")
tw.daily_sentiment.legit <- tw.daily_sentiment.legit %>% arrange(date) %>% 
  mutate(sentiment_week.ma = zoo::rollapply(sentiment, 7, mean, align='right', fill=NA))
data_interventions.aggr <- data_interventions.aggr %>% left_join(tw.daily_sentiment.legit)

# Conferencia transformation ====

# Tranformar data y crear indicadores
#data_indicadores <- data %>% group_by(date) %>% 
#  summarise_at(vars(funcional:Flesch), mean, na.rm = TRUE) %>% 
#  mutate(senti.ton = base::log((pos+0.5)/(neg+0.5))) %>% 
#  dplyr::select(-pos, -neg) %>% 
#  pivot_longer(!date, names_to = "dimension", values_to = "count")
#
#data_indicadores <- data_indicadores %>% group_by(dimension) %>% 
#  mutate(mean_dimension = mean(count, na.rm = TRUE)) %>% 
#  ungroup() %>% 
#  mutate(indicador = count-mean_dimension+0)
#
## Visualizar
#data_indicadores <- data_indicadores %>% ungroup() %>% arrange(dimension,date) %>% group_by(dimension) %>% 
#  mutate(indicador_week.ma = zoo::rollapply(indicador,7, mean, align='right', fill=NA)) %>% ungroup() %>% 
#  mutate(`Reputational dimension` = case_when(dimension == "funcional" ~ "Performative",
#                                              dimension == "legal" ~ "Legal-procedural",
#                                              dimension == "tecnico" ~ "Technical",
#                                              dimension == "moral" ~ "Moral"))
#
#covid.deaths <- read.csv(file = "../Casos_Diarios_Estado_Nacional_Defunciones_20220121.csv") %>% 
#  filter(nombre=="Nacional") %>% t() %>% as.data.frame() %>% 
#  tibble::rownames_to_column("date") %>% 
#  mutate(population = 127792286) %>% 
#  filter(grepl("X",date)) %>% mutate(date = lubridate::dmy(gsub("X","",date))) %>% 
#  rename(deaths = `V1`) %>% 
#  mutate(deaths = as.numeric(deaths),
#         incidende = deaths/population*100000)
#
#
#data_indicadores <- data_indicadores %>% left_join(covid.deaths)
#data_indicadores <- data_indicadores %>% mutate(deaths = ifelse(is.na(deaths),0,deaths),
#                                                population = ifelse(is.na(population),127792286,population),
#                                                incidende = ifelse(is.na(incidende),0,incidende))
#

# Covid deaths ====


covid.deaths <- read.csv(file = "../Casos_Diarios_Estado_Nacional_Defunciones_20220121.csv") %>% 
  filter(nombre=="Nacional") %>% t() %>% as.data.frame() %>% 
  tibble::rownames_to_column("date") %>% 
  mutate(population = 127792286) %>% 
  filter(grepl("X",date)) %>% mutate(date = lubridate::dmy(gsub("X","",date))) %>% 
  dplyr::rename(deaths = `V1`) %>% 
  mutate(deaths = as.numeric(deaths),
         incidende = deaths/population*100000)

covid.deaths <- covid.deaths %>% arrange(date) %>% 
  mutate(deaths_week.ma = zoo::rollapply(deaths,7, mean, align='right', fill=NA))


data_interventions.aggr <- data_interventions.aggr %>% left_join(covid.deaths)

data_interventions.aggr <- data_interventions.aggr %>% 
  mutate(across(c(funcional:tecnico), ~ ./words_total*100, .names = "{.col}_share")) %>% 
  mutate(across(c(funcional_share:tecnico_share), ~ .-mean(.), .names = "{.col}.cent"))

stargazer::stargazer(
  lm(funcional_share ~ lag(incidende), data = data_interventions.aggr),
  lm(legal_share ~ lag(incidende) , data = data_interventions.aggr),
  lm(moral_share ~ lag(incidende) , data = data_interventions.aggr),
  lm(tecnico_share ~ lag(incidende), data = data_interventions.aggr),
  type = "html", style = "apsr", report = "vcsp", out = "../../A_Análisis/OLS_explore.Gatell_10032022.html")


data_indicadores <- data_interventions.aggr %>%
  dplyr::select(date, actors_number, deaths, deaths_week.ma, sentiment, sentiment_week.ma,funcional_share.cent:tecnico_share.cent) %>% 
  pivot_longer(!date, names_to = "dimension", values_to = "value")

data_indicadores <- data_indicadores %>% group_by(dimension) %>% 
  mutate(mean_dimension = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(indicador = value-mean_dimension+0)

# Visualizar
data_indicadores <- data_indicadores %>% ungroup() %>% arrange(dimension,date) %>% group_by(dimension) %>% 
  mutate(indicador_week.ma = zoo::rollapply(indicador,7, mean, align='right', fill=NA)) %>% ungroup() %>% 
  mutate(`Reputational dimension` = case_when(dimension == "funcional_share.cent" ~ "Performative",
                                              dimension == "legal_share.cent" ~ "Legal-procedural",
                                              dimension == "tecnico_share.cent" ~ "Technical",
                                              dimension == "moral_share.cent" ~ "Moral"))

reputation_gr <- ggplot(data_indicadores %>% dplyr::rename(`Reputational\nDimension` = `Reputational dimension`) %>% 
                          filter(!grepl("Flesch|senti[.]ton|senti|actor", `Reputational\nDimension`)) %>% 
                          filter(!is.na(`Reputational\nDimension`)),
       aes(x = date, y = indicador_week.ma,  colour = `Reputational\nDimension`)) +
  geom_smooth(span = 0.5) + theme_minimal() + xlab("Weekly Trend Daily") + ylab("Indicator") #+

deaths_gr <- ggplot(data_indicadores %>% filter(dimension=="deaths"))+ ylab("Daily Deaths") + xlab("Date") +
  geom_area(aes(x = date, y = value), fill="grey", colour="black", size = 1) + theme_minimal()

reputation_gr/deaths_gr

#save(data_indicadores, file = "../indicadores_conferences_26032022.RData")

#ggsave(filename="../../V_Visualizations/ReputationalDimension_Deaths_05-02-2022.png", 
#       plot=last_plot(), device="png", units="cm", width=20, height=12.5, dpi=300)


data_meta <- data_interventions.aggr %>% dplyr::select(date, deaths, deaths_week.ma, sentiment, sentiment_week.ma, actors_number) %>% 
  filter(!duplicated(.))

data_indicadores <- data_indicadores %>% left_join(data_meta)
gatell_dates <- gatell %>% dplyr::select(date) %>% mutate(gatell = 1) %>% filter(!duplicated(.))
data_indicadores <- data_indicadores %>% left_join(gatell_dates)
data_indicadores <- data_indicadores %>% mutate(gatell = ifelse(is.na(gatell),0,gatell))

data_indicadores %>% filter(dimension=="legal_share.cent") %>% 
  summarise(mean(gatell))

data_indicadores <- data_indicadores %>% arrange(dimension, date) %>% 
  group_by(dimension) %>%
  mutate(lead.indicador = dplyr::lead(indicador, n = 1, default = NA),
         lead.deaths = dplyr::lead(deaths, n = 1, default = NA),
         lead.value = dplyr::lead(value, n = 1, default = NA),
         lead.sentiment = dplyr::lead(sentiment , n = 1, default = NA),
         lead.actors_number = dplyr::lead(actors_number , n = 1, default = NA),
         id = row_number()) %>% ungroup() %>% 
  mutate(indicador_change = lead.indicador-indicador,
         deaths_change = lead.deaths-deaths)


# Complex VAR models ====


options(scipen = 999)

granger_tests <- data.frame()
issue_list <- c("Performative","Legal-procedural","Moral","Technical")

for(p in 1:7){
for(is in 1:4){
  issue_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                 dplyr::select(indicador),
               start = c(1, 1), 
               end = c(444, 1), 
               frequency = 1)
  
  deaths_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                    dplyr::select(deaths),
                  start = c(1, 1), 
                  end = c(444, 1), 
                  frequency = 1)
  
  deaths.week_ma_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                    dplyr::select(deaths_week.ma),
                  start = c(1, 1), 
                  end = c(444, 1), 
                  frequency = 1)
  
  actor.nr_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                      dplyr::select(actors_number),
                 start = c(1, 1), 
                 end = c(444, 1), 
                 frequency = 1)
  sentiment_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                       dplyr::select(sentiment),
                start = c(1, 1), 
                end = c(444, 1), 
                frequency = 1)
  
  sentiment.week_ma_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                       dplyr::select(sentiment_week.ma),
                     start = c(1, 1), 
                     end = c(444, 1), 
                     frequency = 1)
  
  gatell_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                    dplyr::select(gatell),
                     start = c(1, 1), 
                     end = c(444, 1), 
                     frequency = 1)
  
  
  # Models
  
  VAR_EQ1 <- dynlm(issue_ts ~ L(issue_ts, 1) + L(deaths_ts, 1:p), 
                   start = c(1, 25), 
                   end = c(444, 1))
  
 
  
  VAR_EQ2 <- dynlm(issue_ts ~ L(issue_ts, 1) +  L(deaths_ts, 1:p) + L(actor.nr_ts, 0) + L(gatell_ts, 0) + L(sentiment.week_ma_ts, 1), 
                   start = c(1, 25), 
                   end = c(444, 1))
  
  if(p == 1){
    VAR_EQ1.ma <- dynlm(issue_ts ~ L(issue_ts, 1) + L(deaths.week_ma_ts, 1), 
                        start = c(1, 25), 
                        end = c(444, 1))
    VAR_EQ2.ma <- dynlm(issue_ts ~ L(issue_ts, 1) +  L(deaths.week_ma_ts, 1) + L(actor.nr_ts, 0) + L(gatell_ts, 0) + L(sentiment.week_ma_ts, 1), 
                        start = c(1, 25), 
                        end = c(444, 1))
    
    VAR_EQ3.ma <- dynlm(issue_ts ~ L(issue_ts, 1) + L(sentiment.week_ma_ts, 1), 
                        start = c(1, 25), 
                        end = c(444, 1))
    
    
    
    granger_tests <- granger_tests %>% 
      bind_rows(car::linearHypothesis(VAR_EQ1.ma, hypothesis.matrix = c("L(deaths.week_ma_ts, 1)"), vcov. = sandwich) %>% 
                  mutate(type = "SimpleDeath", dimension = issue_list[is]), 
                car::linearHypothesis(VAR_EQ2.ma, hypothesis.matrix = c("L(deaths.week_ma_ts, 1)"), vcov. = sandwich) %>% 
                  mutate(type = "All", dimension = issue_list[is]),
                car::linearHypothesis(VAR_EQ3.ma, hypothesis.matrix = c("L(sentiment.week_ma_ts, 1)"), vcov. = sandwich) %>% 
                  mutate(type = "Sentiment", dimension = issue_list[is]),)
    
    assign(paste0(issue_list[is],"-","-DimensionByDeathsWEEK-MA"), VAR_EQ1.ma)
    assign(paste0(issue_list[is],"-","-DimensionByAllWEEK-MA"), VAR_EQ2.ma)
    assign(paste0(issue_list[is],"-","-DimensionTwitterWEEK-MA"), VAR_EQ3.ma)
    
  }
  
  assign(paste0(issue_list[is],"-","-DimensionByDeaths",p), VAR_EQ1) # Explaining this phenomenon (y)
  assign(paste0(issue_list[is],"-","-DimensionByAll",p), VAR_EQ2) # Explaining this phenomenon (y)
  
}
}

rm(p, is)


  ## Main paper models ====
stargazer::stargazer(`Performative--DimensionByDeathsWEEK-MA`, `Performative--DimensionByAllWEEK-MA`,
                     `Technical--DimensionByDeathsWEEK-MA`, `Technical--DimensionByAllWEEK-MA`,
                     `Legal-procedural--DimensionByDeathsWEEK-MA`, `Legal-procedural--DimensionByAllWEEK-MA`,
                     `Moral--DimensionByDeathsWEEK-MA`, `Moral--DimensionByAllWEEK-MA`, type = "text", style = "apsr", report = "vc*sp",
                     column.labels = c(rep("Performative",2), rep("Technical",2), rep("Legal",2), rep("Moral",2)),
                     covariate.labels = c("Reputation dimension, t-1", "Covid-19 deaths t-1 - t-7, average",
                                          "# Actors in Conference","Gatell in Conference (0|1)",
                                          "Public Sentiment -> Organization t-1 - t-7, average"),
                     out = "../../A_Análisis/R_Results_December2022/var.moving-average_models_28122022.html")

granger_tests <- granger_tests %>% filter(!is.na(Df)) %>% 
  mutate(Significance = case_when(`Pr(>F)` < 0.10 & `Pr(>F)` >= 0.05 ~ "*",
                                  `Pr(>F)` < 0.05 & `Pr(>F)` >= 0.01 ~ "**",
                                  `Pr(>F)` < 0.01 ~ "***",
                                  TRUE ~ NA_character_))

openxlsx::write.xlsx(granger_tests, file = "../../A_Análisis/R_Results_December2022/granger_tests.results.xlsx")


  ## Modelos p = 3 para el apéndice ====
stargazer::stargazer(`Performative--DimensionByDeaths3`, `Performative--DimensionByAll3`,
                     `Technical--DimensionByDeaths3`, `Technical--DimensionByAll3`,
                     `Legal-procedural--DimensionByDeaths3`, `Legal-procedural--DimensionByAll3`,
                     `Moral--DimensionByDeaths3`, `Moral--DimensionByAll3`, type = "text", style = "apsr", report = "vc*sp",
                     column.labels = c(rep("Performative",2), rep("Technical",2), rep("Legal",2), rep("Moral",2)),
                     covariate.labels = c("Reputation dimension, t-1", "Covid-19 deaths t-1", "Covid-19 deaths t-2", "Covid-19 deaths t-3",
                                          "# Actors in Conference","Gatell in Conference (0|1)",
                                          "Public Sentiment -> Organization t-1 - t-7, average"),
                     out = "../../A_Análisis/R_Results_December2022/var.p3_models_28122022.html")

  ## Modelos sentimiento público (Twitter) - apéndice ====
stargazer::stargazer(`Performative--DimensionTwitterWEEK-MA`, 
                     `Technical--DimensionTwitterWEEK-MA`,
                     `Legal-procedural--DimensionTwitterWEEK-MA`,
                     `Moral--DimensionTwitterWEEK-MA`, type = "text", style = "apsr", report = "vc*sp",
                     column.labels = c(rep("Performative",1), rep("Technical",1), rep("Legal",1), rep("Moral",1)),
                     covariate.labels = c("Reputation dimension, t-1",
                                          "Public Sentiment -> Organization t-1 - t-7, average"),
                     out = "../../A_Análisis/R_Results_December2022/var.twitter-models_29122022.html")

  ## Anova tests ====
anova_performative <- anova(`Performative--DimensionByDeaths1`,`Performative--DimensionByDeaths2`,
                            `Performative--DimensionByDeaths3`,`Performative--DimensionByDeaths4`,
                            `Performative--DimensionByDeaths5`,`Performative--DimensionByDeaths6`,
                            `Performative--DimensionByDeaths7`) %>% as.data.frame() %>% 
  mutate(Significance = case_when(`Pr(>F)` < 0.10 ~ "*",
                                  `Pr(>F)` < 0.05 ~ "**",
                                  `Pr(>F)` < 0.01 ~ "***",
                                  TRUE ~ NA_character_),
         `Reputational Dimension` = "Performative")

anova_technical <- anova(`Technical--DimensionByDeaths1`,`Technical--DimensionByDeaths2`,
                         `Technical--DimensionByDeaths3`,`Technical--DimensionByDeaths4`,
                         `Technical--DimensionByDeaths5`,`Technical--DimensionByDeaths6`,
                         `Technical--DimensionByDeaths7`)  %>% as.data.frame() %>% 
  mutate(Significance = case_when(`Pr(>F)` < 0.10 ~ "*",
                                  `Pr(>F)` < 0.05 ~ "**",
                                  `Pr(>F)` < 0.01 ~ "***",
                                  TRUE ~ NA_character_),
         `Reputational Dimension` = "Technical")

anova_legal <- anova(`Legal-procedural--DimensionByDeaths1`,`Legal-procedural--DimensionByDeaths2`,
                     `Legal-procedural--DimensionByDeaths3`,`Legal-procedural--DimensionByDeaths4`,
                     `Legal-procedural--DimensionByDeaths5`,`Legal-procedural--DimensionByDeaths6`,
                     `Legal-procedural--DimensionByDeaths7`) %>% as.data.frame() %>% 
  mutate(Significance = case_when(`Pr(>F)` < 0.10 ~ "*",
                                  `Pr(>F)` < 0.05 ~ "**",
                                  `Pr(>F)` < 0.01 ~ "***",
                                  TRUE ~ NA_character_),
         `Reputational Dimension` = "Legal")

anova_moral <- anova(`Moral--DimensionByDeaths1`,`Moral--DimensionByDeaths2`,
                     `Moral--DimensionByDeaths3`,`Moral--DimensionByDeaths4`,
                     `Moral--DimensionByDeaths5`,`Moral--DimensionByDeaths6`,
                     `Moral--DimensionByDeaths7`) %>% as.data.frame() %>% 
  mutate(Significance = case_when(`Pr(>F)` < 0.10 ~ "*",
                                  `Pr(>F)` < 0.05 ~ "**",
                                  `Pr(>F)` < 0.01 ~ "***",
                                  TRUE ~ NA_character_),
         `Reputational Dimension` = "Moral")

anovas <- bind_rows(anova_performative, anova_technical, anova_legal, anova_moral)
openxlsx::write.xlsx(anovas, file = "../../A_Análisis/R_Results_December2022/anovas_comp.xlsx" )

  ## Anova tests ALL ====
anova_performative.all <- anova(`Performative--DimensionByAll1`,`Performative--DimensionByAll2`,
                            `Performative--DimensionByAll3`,`Performative--DimensionByAll4`,
                            `Performative--DimensionByAll5`,`Performative--DimensionByAll6`,
                            `Performative--DimensionByAll7`) %>% as.data.frame() %>% 
  mutate(Significance = case_when(`Pr(>F)` < 0.10 ~ "*",
                                  `Pr(>F)` < 0.05 ~ "**",
                                  `Pr(>F)` < 0.01 ~ "***",
                                  TRUE ~ NA_character_),
         `Reputational Dimension` = "Performative")

anova_technical.all <- anova(`Technical--DimensionByAll1`,`Technical--DimensionByAll2`,
                         `Technical--DimensionByAll3`,`Technical--DimensionByAll4`,
                         `Technical--DimensionByAll5`,`Technical--DimensionByAll6`,
                         `Technical--DimensionByAll7`)  %>% as.data.frame() %>% 
  mutate(Significance = case_when(`Pr(>F)` < 0.10 ~ "*",
                                  `Pr(>F)` < 0.05 ~ "**",
                                  `Pr(>F)` < 0.01 ~ "***",
                                  TRUE ~ NA_character_),
         `Reputational Dimension` = "Technical")

anova_legal.all <- anova(`Legal-procedural--DimensionByAll1`,`Legal-procedural--DimensionByAll2`,
                     `Legal-procedural--DimensionByAll3`,`Legal-procedural--DimensionByAll4`,
                     `Legal-procedural--DimensionByAll5`,`Legal-procedural--DimensionByAll6`,
                     `Legal-procedural--DimensionByAll7`) %>% as.data.frame() %>% 
  mutate(Significance = case_when(`Pr(>F)` < 0.10 ~ "*",
                                  `Pr(>F)` < 0.05 ~ "**",
                                  `Pr(>F)` < 0.01 ~ "***",
                                  TRUE ~ NA_character_),
         `Reputational Dimension` = "Legal")

anova_moral.all <- anova(`Moral--DimensionByAll1`,`Moral--DimensionByAll2`,
                     `Moral--DimensionByAll3`,`Moral--DimensionByAll4`,
                     `Moral--DimensionByAll5`,`Moral--DimensionByAll6`,
                     `Moral--DimensionByAll7`) %>% as.data.frame() %>% 
  mutate(Significance = case_when(`Pr(>F)` < 0.10 ~ "*",
                                  `Pr(>F)` < 0.05 ~ "**",
                                  `Pr(>F)` < 0.01 ~ "***",
                                  TRUE ~ NA_character_),
         `Reputational Dimension` = "Moral")

anovas.all <- bind_rows(anova_performative.all, anova_technical.all, anova_legal.all, anova_moral.all)
openxlsx::write.xlsx(anovas.all, file = "../../A_Análisis/R_Results_December2022/anovas_comp_ALL.xlsx" )

# Formal Granger causality tests ====

# Define predetermined p = 3

rm(list=setdiff(ls(), c("data_indicadores")))

p = 3


# Performative

issue_ts.performative <- ts(data_indicadores %>% filter(`Reputational dimension`=="Performative") %>%
                 dplyr::select(indicador),
               start = c(1, 1), 
               end = c(444, 1), 
               frequency = 1)

deaths_ts.performative  <- ts(data_indicadores %>% filter(`Reputational dimension`=="Performative") %>% 
                  dplyr::select(deaths),
                start = c(1, 1), 
                end = c(444, 1), 
                frequency = 1)


VAR_EQ.performative  <- dynlm(issue_ts.performative ~ L(issue_ts.performative, 1) + L(deaths_ts.performative, 1:p), 
                 start = c(1, 25), 
                 end = c(444, 1))

names(VAR_EQ.performative$coefficients) <- c("Constant","Issue t-1","Deaths t-1","Deaths t-2","Deaths t-3")
summary(VAR_EQ.performative)

car::linearHypothesis(VAR_EQ.performative, 
                 hypothesis.matrix = c("Deaths t-1","Deaths t-2","Deaths t-3"),
                 vcov. = sandwich)


# Legal
issue_ts.legal <- ts(data_indicadores %>% filter(`Reputational dimension`=="Legal-procedural") %>%
                              dplyr::select(indicador),
                            start = c(1, 1), 
                            end = c(444, 1), 
                            frequency = 1)

deaths_ts.legal  <- ts(data_indicadores %>% filter(`Reputational dimension`=="Legal-procedural") %>% 
                                dplyr::select(deaths),
                              start = c(1, 1), 
                              end = c(444, 1), 
                              frequency = 1)


VAR_EQ.legal  <- dynlm(issue_ts.legal ~ L(issue_ts.legal, 1) + L(deaths_ts.legal, 1:p), 
                              start = c(1, 25), 
                              end = c(444, 1))

names(VAR_EQ.legal$coefficients) <- c("Constant","Issue t-1","Deaths t-1","Deaths t-2","Deaths t-3")
summary(VAR_EQ.legal)

car::linearHypothesis(VAR_EQ.legal, 
                      hypothesis.matrix = c("Deaths t-1","Deaths t-2","Deaths t-3"),
                      vcov. = sandwich)


# Data comunicados de salud ====

library(ggplot2)
library(dplyr)

load("../data_comunicados_salud_21022022.RData")

data_comunicados <- data_comunicados %>% filter(date>="2013-01-01")
data_comunicados <- data_comunicados %>% mutate(date_month = as.Date(paste0(gsub("\\d{2}$","",date),"01")))
data_comunicados <- data_comunicados %>% mutate(text_full = paste0(titulo," ",entrada," ", texto_cuerpo))

time_aggr <- data_comunicados %>% 
  group_by(date_month) %>% 
  tally()

ggplot(time_aggr, aes(date_month, n)) + geom_smooth(span = 0.4, colour = "black") + ylab("# Press Releases Health Secretary") +
  xlab("Time (Monthly)") + theme_minimal()

ggsave(filename="../../A_Análisis/R_Results_December2022/press_releases_number.png", 
      plot=last_plot(), device="png", units="cm", width=20, height=8, dpi=300)


daily_texts <- data_comunicados %>% 
  group_by(date) %>% summarise(text = paste0(text_full, collapse = " "))

# Definiendo diccionario

dictionary.reputation <- openxlsx::read.xlsx("../../D_DictionaryReputation/ReputationDimensions_Dictionary_v1.xlsx") %>% 
  filter(!is.na(palabra))
dictionary.reputation <- quanteda::dictionary(list(
  funcional = dictionary.reputation %>% filter(dictionary=="funcional") %>% dplyr::select(palabra) %>% unlist() %>%  c(),
  legal = dictionary.reputation %>% filter(dictionary=="legal") %>% dplyr::select(palabra) %>%  unlist() %>%  c(),
  moral = dictionary.reputation %>% filter(dictionary=="moral") %>% dplyr::select(palabra) %>% unlist() %>%  c(),
  tecnico = dictionary.reputation %>% filter(dictionary=="tecnico") %>% dplyr::select(palabra) %>% unlist() %>%  c()
))


# Crear corpus, tokens y DFM con diccionario
press_corpus  <- quanteda::corpus(daily_texts, text_field= "text")

press_tokens <- quanteda::tokens(press_corpus, 
                                 remove_punct = FALSE,
                                 remove_symbols=FALSE, 
                                 remove_separators=FALSE,
                                 split_hyphens = TRUE, 
                                 remove_numbers = FALSE,
                                 remove_url=FALSE) %>% quanteda::tokens_tolower()

press_dfm <- quanteda::dfm(press_tokens,
                           dictionary = c(dictionary.reputation))

daily_texts <- daily_texts %>% bind_cols(press_dfm %>% as.data.frame())

rm(press_corpus, press_tokens, press_dfm)


# Load Twitter data
load("../twitter_aggregated.RData")
tw.daily_sentiment.legit <- tw.daily_sentiment.legit %>% arrange(date) %>% 
  mutate(sentiment_week.ma = zoo::rollapply(sentiment, 7, mean, align='right', fill=NA))

daily_texts <- daily_texts %>% left_join(tw.daily_sentiment.legit)

daily_texts <- daily_texts %>% mutate(words = ntoken(text))

# S: Análisis comunicados (apéndice) ====

covid.deaths <- read.csv(file = "../Casos_Diarios_Estado_Nacional_Defunciones_20220121.csv") %>% 
  filter(nombre=="Nacional") %>% t() %>% as.data.frame() %>% 
  tibble::rownames_to_column("date") %>% 
  mutate(population = 127792286) %>% 
  filter(grepl("X",date)) %>% mutate(date = lubridate::dmy(gsub("X","",date))) %>% 
  dplyr::rename(deaths = `V1`) %>% 
  mutate(deaths = as.numeric(deaths),
         incidende = deaths/population*100000)

covid.deaths <- covid.deaths %>% arrange(date) %>% 
  mutate(deaths_week.ma = zoo::rollapply(deaths,7, mean, align='right', fill=NA))

daily_texts <- daily_texts %>% left_join(covid.deaths)

daily_texts <- daily_texts %>% 
  mutate(across(c(funcional:tecnico), ~ ./words*100, .names = "{.col}_share")) %>% 
  mutate(across(c(funcional_share:tecnico_share), ~ .-mean(.), .names = "{.col}.cent"))

data_indicadores <- daily_texts %>%
  dplyr::select(date, deaths, deaths_week.ma, sentiment, sentiment_week.ma, funcional_share.cent:tecnico_share.cent) %>% 
  pivot_longer(!date, names_to = "dimension", values_to = "value")

data_indicadores <- data_indicadores %>% group_by(dimension) %>% 
  mutate(mean_dimension = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(indicador = value-mean_dimension+0)

# Visualizar
data_indicadores <- data_indicadores %>% ungroup() %>% arrange(dimension,date) %>% group_by(dimension) %>% 
  mutate(indicador_week.ma = zoo::rollapply(indicador,7, mean, align='right', fill=NA)) %>% ungroup() %>% 
  mutate(`Reputational dimension` = case_when(dimension == "funcional_share.cent" ~ "Performative",
                                              dimension == "legal_share.cent" ~ "Legal-procedural",
                                              dimension == "tecnico_share.cent" ~ "Technical",
                                              dimension == "moral_share.cent" ~ "Moral"))

reputation_gr <- ggplot(data_indicadores %>% dplyr::rename(`Reputational\nDimension` = `Reputational dimension`) %>% 
                          filter(!grepl("Flesch|senti[.]ton|senti|actor", `Reputational\nDimension`)) %>% 
                          filter(!is.na(`Reputational\nDimension`)),
                        aes(x = date, y = indicador_week.ma,  colour = `Reputational\nDimension`)) +
  geom_smooth(span = 0.5) + theme_minimal() + xlab("Weekly Trend Daily") + ylab("Indicator")

deaths_gr <- ggplot(data_indicadores %>% filter(dimension=="deaths"))+ ylab("Daily Deaths") + xlab("Date") +
  geom_area(aes(x = date, y = value), fill="grey", colour="black", size = 1) + theme_minimal()

reputation_gr/deaths_gr

#ggsave(filename="../../V_Visualizations/ReputationalDimension_PressReleases_Deaths_26-03-2022.png", 
#       plot=last_plot(), device="png", units="cm", width=20, height=6.5, dpi=300)


data_meta <- daily_texts %>% dplyr::select(date, deaths, deaths_week.ma, sentiment, sentiment_week.ma) %>% 
  filter(!duplicated(.))

data_indicadores <- data_indicadores %>% left_join(data_meta)

data_indicadores <- data_indicadores %>% arrange(dimension, date) %>% 
  group_by(dimension) %>%
  mutate(lead.indicador = dplyr::lead(indicador, n = 1, default = NA),
         lead.deaths = dplyr::lead(deaths, n = 1, default = NA),
         lead.value = dplyr::lead(value, n = 1, default = NA),
         lead.sentiment = dplyr::lead(sentiment , n = 1, default = NA),
         id = row_number()) %>% ungroup() %>% 
  mutate(indicador_change = lead.indicador-indicador,
         deaths_change = lead.deaths-deaths)

# Complex VAR models
issue_list <- c("Performative","Legal-procedural","Moral","Technical")

data_indicadores <- data_indicadores %>% filter(!is.na(deaths))

#save(data_indicadores, file = "../indicadores_press.releases_29122022.RData")
load("../indicadores_press.releases_29122022.RData")
# We do not change the code but this is with moving average (see)
for(is in 1:4){
  issue_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                   dplyr::select(indicador),
                 start = c(1, 1), 
                 end = c(444, 1), 
                 frequency = 1)
  
  deaths_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                    dplyr::select(deaths_week.ma),
                  start = c(1, 1), 
                  end = c(444, 1), 
                  frequency = 1)
  
  sentiment_ts <- ts(data_indicadores %>% filter(`Reputational dimension`==issue_list[is]) %>% 
                       dplyr::select(sentiment_week.ma),
                     start = c(1, 1), 
                     end = c(444, 1), 
                     frequency = 1)
  
  VAR_EQ1 <- dynlm(issue_ts ~ L(issue_ts, 1) + L(deaths_ts, 1), 
                   start = c(1, 1), 
                   end = c(444, 1))
  
  VAR_EQ2 <- dynlm(issue_ts ~ L(issue_ts, 1) +  L(deaths_ts, 1) + L(sentiment_ts, 1), 
                   start = c(1, 1), 
                   end = c(444, 1))
  
  assign(paste0(issue_list[is],"-","-DimensionByDeaths"), VAR_EQ1) # Explaining this phenomenon (y)
  assign(paste0(issue_list[is],"-","-DimensionByAll"), VAR_EQ2) # Explaining this phenomenon (y)
  
}

stargazer::stargazer(`Performative--DimensionByDeaths`,
                     `Performative--DimensionByAll`,
                     `Technical--DimensionByDeaths`,
                     `Technical--DimensionByAll`,
                     `Legal-procedural--DimensionByDeaths`,
                     `Legal-procedural--DimensionByAll`,
                     `Moral--DimensionByDeaths`,
                     `Moral--DimensionByAll`,
                     type = "text", style = "apsr", report = "vc*sp",
                     column.labels = c(rep("Performative",2), rep("Technical",2), rep("Legal",2), rep("Moral",2)),
                     covariate.labels = c("Reputation dimension, t-1", "Covid-19 deaths t-1 - t-7, average",
                                          "Public Sentiment -> Organization t-1 - t-7, average"),
                     out = "../../A_Análisis/R_Results_December2022/var.moving-average_PRESS.releases-models_29122022.html")



# Análisis extra de correlación ====

# Comparing indicators
load("C:/Users/saldivia-gonzatti/Documents/P_PAR_Mexico_Article/D_Data/indicadores_conferences_26032022.RData")
indicadores_conferencia <- data_indicadores %>% dplyr::select(date, dimension, indicador) %>% 
  filter(grepl("cent", dimension))
load("C:/Users/saldivia-gonzatti/Documents/P_PAR_Mexico_Article/D_Data/indicadores_press.releases_26032022.RData")
indicadores_press.releases <- data_indicadores %>% dplyr::select(date, dimension, indicador) %>% ungroup() %>% 
  filter(grepl("cent", dimension)) %>% dplyr::rename(indicador_press.releases = indicador)

indicadores <- left_join(indicadores_conferencia, indicadores_press.releases)
rm(data_indicadores, indicadores_conferencia, indicadores_press.releases)


cor.test(indicadores$indicador[indicadores$dimension=="funcional_share.cent"],
         indicadores$indicador_press.releases[indicadores$dimension=="funcional_share.cent"])

cor.test(indicadores$indicador[indicadores$dimension=="legal_share.cent"],
         indicadores$indicador_press.releases[indicadores$dimension=="legal_share.cent"])

cor.test(indicadores$indicador[indicadores$dimension=="tecnico_share.cent"],
         indicadores$indicador_press.releases[indicadores$dimension=="tecnico_share.cent"])

cor.test(indicadores$indicador[indicadores$dimension=="moral_share.cent"],
         indicadores$indicador_press.releases[indicadores$dimension=="moral_share.cent"])


# Print dictionary

dictionary.reputation <- openxlsx::read.xlsx("../../D_DictionaryReputation/ReputationDimensions_Dictionary_v1.xlsx") %>% 
  filter(!is.na(palabra))

fct_example <-as.character(dictionary.reputation %>% filter(dictionary=="funcional") %>% dplyr::select(palabra) %>% 
  summarise(paste0(., collapse = ", "))) %>% gsub('["]',"",.)
cat(fct_example)


library(tidyverse)
load("C:/Users/saldivia-gonzatti/Documents/P_PAR_Mexico_Article/D_Data/indicadores_conferences_26032022.RData")
data <- data_indicadores %>% dplyr::select(date, dimension, indicador) %>% 
  filter(grepl("senti|deat", dimension))
data <- data %>% pivot_wider(names_from = dimension, values_from = indicador)

cor.test(data$deaths, data$sentiment)

