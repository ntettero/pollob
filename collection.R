library(lubridate)
library(tibble)
library(dplyr)
library(data.table)
library(flempar)
library(foreach)
library(tidyr)
library(stringr)

data.frame(datum_start  = seq(ymd("2018-01-01"),
                              ymd("2022-11-01"),
                              by = 365)) %>%
  mutate(datum_end = lead(datum_start)-1) %>%
  mutate(datum_end = if_else(is.na(datum_end),ymd("2022-11-01"),datum_end))-> date

#script for data collection training BERT
for(i in 1:nrow(date)){
  
  output <- try({
    
    get_work(date_range_from=date$datum_start[[i]]
             ,date_range_to=date$datum_end[[i]]
             ,type="speech"
             ,fact="oral_questions_and_interpellations"
             ,plen_comm="plen")
  })
  
  if(any(stringr::str_detect(tolower(output[1]),"no sessions found"))){
    
      next()
    
    
  }
  
  
  if(any(class(output) == "try-error")){
    
    message("error, will sleep for 1 minute")
    Sys.sleep(60)
    message("slept for 1 minute, let's retry")
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="speech"
               ,fact="oral_questions_and_interpellations"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    message("another error, will sleep for 2 minutes")
    Sys.sleep(2*60)
    message("slept for 2 minutes, let's retry")
    
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="speech"
               ,fact="oral_questions_and_interpellations"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    message("another error, powernap of 15 minutes")
    Sys.sleep(15*60)
    message("slept for 15 minutes, let's try one more time")
    
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="speech"
               ,fact="oral_questions_and_interpellations"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    stop()
    message("function stopped")
    
    
  }
  
  
  saveRDS(output, paste0("~/pollob/data/speech/",date$datum_start[[i]],".rds"))
  
  message(i,"/",nrow(date))
  
}

oral_questions_and_interpellations <- list.files(path = "~/pollob/data/speech/"
                                                 ,pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS)

for(i in 1:nrow(date)){
  
  output <- try({
    
    get_work(date_range_from=date$datum_start[[i]]
             ,date_range_to=date$datum_end[[i]]
             ,type="details"
             ,fact="oral_questions_and_interpellations"
             ,plen_comm="plen")
  })
  
  if(any(stringr::str_detect(tolower(output[1]),"no sessions found"))){
    
    next()
    
    
  }
  
  
  if(any(class(output) == "try-error")){
    
    message("error, will sleep for 1 minute")
    Sys.sleep(60)
    message("slept for 1 minute, let's retry")
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="details"
               ,fact="oral_questions_and_interpellations"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    message("another error, will sleep for 2 minutes")
    Sys.sleep(2*60)
    message("slept for 2 minutes, let's retry")
    
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="details"
               ,fact="oral_questions_and_interpellations"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    message("another error, powernap of 15 minutes")
    Sys.sleep(15*60)
    message("slept for 15 minutes, let's try one more time")
    
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="details"
               ,fact="oral_questions_and_interpellations"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    stop()
    message("function stopped")
    
    
  }
  
  
  saveRDS(output, paste0("~/pollob/data/details/det_",date$datum_start[[i]],".rds"))
  
  message(i,"/",nrow(date))
  
}

details <- list.files(path = "~/pollob/data/details/", pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS)

#cleaning details 
details %>%
  select(verg_id, id_fact, journaallijn_id, titel, onderwerp, result_thema_1, result_thema_2
         , result_thema_3, result_thema_4, result_thema_5, zittingsjaar, datumbegin) %>%
  mutate(datum = as.Date(datumbegin, format = "%Y-%m-%d")) %>%
  select(verg_id, id_fact, journaallijn_id, titel, onderwerp, result_thema_1, result_thema_2
         , result_thema_3, result_thema_4, result_thema_5, zittingsjaar, datum) -> details_clean




current <- get_mp(selection="current"
                  , fact="raw")

current %>%
  tibble::tibble(vv = .) %>%
  tidyr::unnest_wider(vv) %>%
  dplyr::select(id_mp=id,voornaam, achternaam=naam,geslacht,geboortedatum,geboorteplaats,huidigefractie,lidmaatschap) %>%
  tidyr::unnest_wider(huidigefractie, names_sep="_") %>%
  tidyr::unnest_wider(huidigefractie_1, names_sep="_") %>%
  tidyr::unnest_wider(lidmaatschap, names_sep="_") %>%
  tidyr::unnest(lidmaatschap_1) %>%
  tidyr::unnest(c( fractie)) %>%
  dplyr::select(id_mp,voornaam, achternaam,geslacht,geboortedatum,geboorteplaats,party_id_current=huidigefractie_1_id,party_naam_current = huidigefractie_1_naam,naam,datumVan,datumTot,zetel_aantal = `zetel-aantal`) %>%
  dplyr::mutate(geboortedatum = lubridate::date(lubridate::ymd_hms(geboortedatum)))  -> current_clean


current_clean %>%
  mutate(datumTot = as.Date(datumTot)) %>%
  mutate(datumVan = as.Date(datumVan)) %>%
  mutate(datumTot = if_else(is.na(datumTot), Sys.Date(), datumTot)) %>%
  # sequence of monthly dates for each corresponding start, end elements
  transmute(id_mp, voornaam, achternaam, geslacht, geboortedatum
            , geboorteplaats, naam, zetel_aantal, 
            day = map2(datumVan, datumTot, seq, by = "1 day")) %>%
  # unnest the list column
  tidyr::unnest(cols = c(day)) %>%
  # remove any duplicate rows
  distinct -> id_mp_day


former <- get_mp(selection="former"
                     ,fact="raw"
                     , use_parallel=TRUE)

former %>%
  tibble::tibble(vv = .) %>%
  tidyr::unnest_wider(vv) %>%
  dplyr::select(id_mp=id,voornaam, achternaam=naam,geslacht,geboortedatum,geboorteplaats,huidigefractie,lidmaatschap) %>%
  tidyr::unnest_wider(huidigefractie, names_sep="_") %>%
  tidyr::unnest_wider(huidigefractie_1, names_sep="_") %>%
  tidyr::unnest_wider(lidmaatschap, names_sep="_") %>%
  tidyr::unnest(lidmaatschap_1) %>%
  tidyr::unnest(c( fractie)) %>%
  dplyr::select(id_mp,voornaam, achternaam,geslacht,geboortedatum,geboorteplaats,party_id_current=huidigefractie_1_id,party_naam_current = huidigefractie_1_naam,naam,datumVan,datumTot,`zetel-aantal`) %>%
  dplyr::mutate(geboortedatum = lubridate::date(lubridate::ymd_hms(geboortedatum)))  -> former_clean
  
mp <- rbind(current_clean,former_clean)

write.csv(id_mp_day, file = "~/pollob/data/df_mp.csv")

#cleaning the text in oqai
oral_questions_and_interpellations %>%
  mutate(text = str_to_lower(text)) %>%
  #mutate(text = str_replace_all(text, "<p>", "")) %>%
  #mutate(text = str_replace_all(text, "</p>", "")) %>%
  mutate(text = str_replace_all(text, "<em>", "")) %>%
  mutate(text = str_replace_all(text, "/em>", "")) %>%
  mutate(text = str_replace_all(text, "<strong>", "")) %>%
  mutate(text = str_replace_all(text, "</strong>", "")) %>%
  #mutate(text = str_replace_all(text, "</span>", "")) %>%
  #mutate(text = str_replace_all(text, "<span>", "")) %>%
  mutate(text = str_replace_all(text, "<html>", "")) %>%
  mutate(text = str_replace_all(text, "<head>", "")) %>%
  mutate(text = str_replace_all(text, "</head>", "")) %>%
  mutate(text = str_replace_all(text, "<title>", "")) %>%
  mutate(text = str_replace_all(text, "</title>", "")) %>%
  mutate(text = str_replace_all(text, "<body>", "")) %>%
  mutate(text = str_replace_all(text, "</body>", "")) %>%
  mutate(text = str_replace_all(text, "<sub>", "")) %>%
  mutate(text = str_replace_all(text, "</sub>", "")) %>%
  mutate(text = str_replace_all(text, "&#8217;", "")) %>%
  mutate(text = str_replace_all(text, "&#8216;", "")) %>%
  mutate(text = str_replace_all(text, "&#8211;", "")) %>%
  mutate(text = gsub("\r", "", text)) %>%
  mutate(text = gsub("\n", "", text)) %>%
  mutate(text = str_replace_all(text, "&#235;", "ë")) %>%
  mutate(text = str_replace_all(text, "&#246;", "ö")) %>%
  mutate(text = str_replace_all(text, "&#233;", "é")) %>%
  mutate(text = str_replace_all(text, "&eacute", "é")) %>%
  mutate(text = str_replace_all(text, "&#239;", "ï")) %>%
  mutate(text = str_replace_all(text, "&iuml;", "ï")) %>% 
  mutate(text = str_replace_all(text, "&#146;", "'")) %>%
  mutate(text = str_replace_all(text, "&#147;", "'")) %>%
  mutate(text = str_replace_all(text, "&#150;", "-")) %>%
  mutate(text = str_replace_all(text, "&euml;", "ë")) %>%
  mutate(text = str_replace_all(text, "&eacute;", "é")) %>%
  mutate(text = str_replace_all(text, "&#39;", "'")) %>%
  mutate(text = str_replace_all(text, '&#8220;', '"')) %>%
  mutate(text = str_replace_all(text, '&#8221;', '"')) %>%
  mutate(text = str_replace_all(text, '<span style="font-style:italic;">', "")) %>%
  mutate(text = str_replace_all(text, '<span style="vertical-align:sub;">', "")) %>%
  mutate(text = str_replace(text, "&#39;", "'")) %>%
  mutate(text = str_replace(text, "&rsquo;", "'"))-> df_speech

write.csv(df_speech, file = "~/pollob/data/df_speech.csv")

df_speech %>%
  select(text) -> training_text

write.csv(training_text, file = "~/pollob/data/training_text.csv")

df_speech %>%
  
  left_join(details_clean, by = c("journaallijn_id" = "journaallijn_id")) %>% 
  mutate(jaar = as.integer(format(datum, "%Y"))) %>%
  
  left_join(id_mp_day, by = c("persoon_id" = "id_mp", "datum" = "day")) -> df_final

