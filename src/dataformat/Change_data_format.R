################################################################################
#
# Change formatting data
#
################################################################################


 Change_data_format <- function(df , tz = "Europe/Brussels") {
  
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(stringr)
  })
  
  
  ### Dates are dmy ("19-12-2025"), times as hms ("00:15:00")
  #----------------------------------------------------------
  df1 <- df %>%
    mutate(
      start_ts = force_tz(dmy(Van..datum.) + hms(Van..tijdstip.), tz = tz),
      end_ts   = force_tz(dmy(Tot..datum.) + hms(Tot..tijdstip.), tz = tz)
    )
  
  
  ### Normalize register into type + period
  #----------------------------------------
  df1 <- df1 %>%
    mutate(
      register_clean = str_squish(Register),
      type = case_when(
        str_detect(register_clean, regex("^Afname", ignore_case = TRUE)) ~ "Verbruik",
        str_detect(register_clean, regex("^Injectie", ignore_case = TRUE)) ~ "Injectie",
        TRUE ~ "other"
      ),
      period = case_when(
        str_detect(register_clean, regex("Nacht", ignore_case = TRUE)) ~ "Nacht",
        str_detect(register_clean, regex("Dag",   ignore_case = TRUE)) ~ "Dag",
        TRUE ~ "Onbekend"
      )
    ) %>%
    filter(type %in% c("Verbruik", "Injectie"))
  
  
  ### Wide format + derived series
  #-------------------------------
  df_wide <- df1 %>% select(start_ts, end_ts, Van..datum., Van..tijdstip., 
                            Tot..datum.,Tot..tijdstip., type, Volume) %>%
    pivot_wider(id_cols    = c(start_ts, end_ts, `Van..datum.`, `Van..tijdstip.`, `Tot..datum.`, `Tot..tijdstip.`),
                names_from = type, 
                values_from = Volume, 
                values_fill = 0,
                values_fn   = list(Volume = sum)) %>%
    arrange(start_ts) %>%  
  distinct() %>% 
  dplyr::rename(
    start_time    = start_ts,
    end_time      = end_ts,
    start_date    = `Van..datum.`,
    start_clock   = `Van..tijdstip.`,
    end_date      = `Tot..datum.`,
    end_clock     = `Tot..tijdstip.`, 
    Verbruik = Verbruik,
    Injectie = Injectie
  )
  
  
  return(df_wide)
}
