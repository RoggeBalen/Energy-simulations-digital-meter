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
  
  
  ### Keep only interval kWh rows and non-NA volume
  #------------------------------------------------
  df1 <- df %>%
    filter(Eenheid %in% c("kWh", "KWH"),
           !is.na(Volume))
  
  
  ### Dates are dmy ("19-12-2025"), times as hms ("00:15:00")
  #----------------------------------------------------------
  df1 <- df1 %>%
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
        str_detect(register_clean, regex("^Afname", ignore_case = TRUE)) ~ "consumption",
        str_detect(register_clean, regex("^Injectie", ignore_case = TRUE)) ~ "production",
        TRUE ~ "other"
      ),
      period = case_when(
        str_detect(register_clean, regex("Nacht", ignore_case = TRUE)) ~ "Nacht",
        str_detect(register_clean, regex("Dag",   ignore_case = TRUE)) ~ "Dag",
        TRUE ~ "Onbekend"
      )
    ) %>%
    filter(type %in% c("consumption", "production"))
  
  
  ### Aggregate to single row per interval & type (safety if duplicates exist)
  #---------------------------------------------------------------------------
  df_aggr <- df1 %>%
    group_by(start_ts, type) %>%
    summarise(energy_kWh = sum(Volume, na.rm = TRUE), .groups = "drop")
  
  
  ### Wide format + derived series
  #-------------------------------
  df_wide <- df_aggr %>%
    pivot_wider(names_from = type, values_from = energy_kWh, values_fill = 0) %>%
    arrange(start_ts) 
  
  return(df_wide)
}
