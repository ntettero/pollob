id_mp_day <- read.csv("")

dataset_20230518 <- readRDS("~/Downloads/dataset_20230518.rds")

#cleaning the text
dataset_20230518 %>%
  mutate(text = str_to_lower(text)) %>%
  mutate(text = str_replace_all(text, "<p>", "")) %>%
  #mutate(text = str_replace_all(text, "</p>", "")) %>%
  mutate(text = str_replace_all(text, "<em>", "")) %>%
  mutate(text = str_replace_all(text, "/em>", "")) %>%
  mutate(text = str_replace_all(text, "<strong>", "")) %>%
  mutate(text = str_replace_all(text, "</strong>", "")) %>%
  mutate(text = str_replace_all(text, "</span>", "")) %>%
  mutate(text = str_replace_all(text, "<span>", "")) %>%
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
  mutate(text = str_replace(text, "&rsquo;", "'"))-> dataset_20230518

#cleaning details 
dataset_20230518 %>%
  select(verg_id, id_fact, journaallijn_id, titel, onderwerp, result_thema_1, result_thema_2
         , result_thema_3, result_thema_4, result_thema_5, zittingsjaar, datumbegin, text
         , sprekertitel, persoon_id) %>%
  mutate(datum = as.Date(datumbegin, format = "%Y-%m-%d")) %>%
  select(verg_id, id_fact, journaallijn_id, titel, onderwerp, result_thema_1, result_thema_2
         , result_thema_3, result_thema_4, result_thema_5, zittingsjaar, datum, text
         , sprekertitel, persoon_id) -> dataset_20230518

# Function to explode dataframe based on </p> tag
explode_dataframe <- function(df) {
  df %>%
    mutate(text = str_replace(text, "</p>$", "")) %>%  # Remove </p> at the end of text block
    separate_rows(text, sep = "</p>") %>%  # Explode dataframe based on </p> tag
    filter(text != "")  # Filter out empty text values
}

# Call the explode_dataframe function with your dataframe
exploded_df <- explode_dataframe(dataset_20230518)

exploded_df %>%
  left_join(id_mp_day, by = c("persoon_id" = "id_mp", "datum" = "day")) -> exploded_mp_df

write.csv(exploded_mp_df, file = "~/pollob/data/exploded_mp_df.csv")