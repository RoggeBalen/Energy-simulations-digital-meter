################################################################################
#
# Create visual to show consumption and production over time
#
################################################################################


plot_prod_vs_cons <- function(df,
                              tz = "Europe/Brussels",
                              show_pv_used = TRUE,
                              smooth = TRUE) {
  
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(stringr)
    library(ggplot2)
    library(scales)
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
    rename(consumption_kWh = consumption,
           production_kWh  = production) %>%
    arrange(start_ts) %>%
    mutate(
      pv_used_kWh = pmin(consumption_kWh, production_kWh),
      import_kWh  = pmax(consumption_kWh - production_kWh, 0),
      export_kWh  = pmax(production_kWh - consumption_kWh, 0)
    )
  
  ### Plot
  #-------
  p <- ggplot(df_wide, aes(x = start_ts)) +
    geom_line(aes(y = consumption_kWh, color = "Afname (consumption)"), linewidth = 0.8) +
    geom_line(aes(y = production_kWh,  color = "Injectie (production)"),  linewidth = 0.8) +
    scale_color_manual(values = c("Afname (consumption)" = "#1f77b4",   # blue
                                  "Injectie (production)"  = "#2ca02c"))  # green
  
  
  if (show_pv_used) {
    p <- p +
      geom_area(aes(y = pv_used_kWh, fill = "PV gebruikt op site"), alpha = 0.25) +
      scale_fill_manual(values = c("PV gebruikt op site" = "#9edae5"))
  }
  
  
  if (smooth) {
    p <- p +
      geom_smooth(aes(y = consumption_kWh, color = "Afname (consumption)"),
                  method = "loess", se = FALSE, linewidth = 0.6, linetype = "dashed") +
      geom_smooth(aes(y = production_kWh,  color = "Injectie (production)"),
                  method = "loess", se = FALSE, linewidth = 0.6, linetype = "dashed")
  }
  
  
  p <- p +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
    scale_y_continuous(labels = label_number(suffix = " kWh", accuracy = 0.01)) +
    labs(
      title = "Productie vs. Afname per kwartier",
      x = "Tijd",
      y = "Energie (kWh per interval)",
      color = "",
      fill  = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top",
          panel.grid.minor = element_blank())
  
  return(p)
}

