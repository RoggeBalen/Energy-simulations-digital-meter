################################################################################
#
# Create visual to show battery capacity overtime
#
################################################################################

plot_week_switch_batt_cap <- function(df, tz_display = "Europe/Brussels") {
  
  
  ### Zorg dat start_time POSIXct is
  #---------------------------------
  if (!inherits(df$start_time, "POSIXct")) {
    df$start_time <- as.POSIXct(df$start_time, tz = tz_display)
  }
  
  
  ### Weeklabels en ranges
  #-----------------------
  df_weeks <- df %>%
    mutate(
      iso_year   = isoyear(start_time),
      iso_week   = isoweek(start_time),
      week_label = sprintf("%d-W%02d", iso_year, iso_week)
    ) %>%
    group_by(week_label) %>%
    summarise(
      week_start = min(start_time, na.rm = TRUE),
      week_end   = max(start_time, na.rm = TRUE),
      .groups    = "drop"
    )
  
  ### Definieer kleurenreeks voor Kostprijs per Type 
  #-------------------------------------------------
  kost_colors <- c(
    "Geen batterij"      = "#2ca02c",
    "Batterij (1kWh)"    = "#d62728",
    "Batterij (3kWh)"    = "#b12168",
    "Batterij (5kWh)"    = "#9467bd",
    "Batterij (10kWh)"   = "#7f7f7f"
  )
  type_map <- c(
    "0"  = "Geen batterij",
    "1"  = "Batterij (1kWh)",
    "3"  = "Batterij (3kWh)",
    "5"  = "Batterij (5kWh)",
    "10" = "Batterij (10kWh)"
  )
  
  
  df <- df %>%
    mutate(
      Type_orig = as.character(Type),
      Type_lbl  = dplyr::recode(Type_orig, !!!type_map, .default = "Geen batterij"),
      Tarief    = ifelse(is.na(Tarief), "Onbekend", Tarief),
      Type_lbl  = factor(Type_lbl, levels = names(kost_colors)),
      Tarief    = factor(Tarief, levels = unique(Tarief))
    )
  
  # 4) Plot basis
  p <- plot_ly(df, x = ~start_time)
  
  # 4a) Batterij-capaciteit: per Type én Tarief (als je Tarief wil onderscheiden met streepjes)
  p <- p %>%
    add_lines(
      y          = ~Batt_kWh,
      split      = ~interaction(Type_lbl, Tarief, drop = TRUE), # maakt meerdere traces
      color      = ~Type_lbl,
      colors     = kost_colors,
      linetype   = ~Tarief,
      legendgroup= ~Type_lbl,                                   # koppel aan hetzelfde legend group
      name       = ~Type_lbl,                                   # gebruik Type als legendlabel
      hovertemplate = "Tijd: %{x}<br>Capaciteit: %{y:.1f} kWh<extra></extra>"
    )
  
  # 4b) Verbruik: elke batterij zijn eigen verbruikslijn (op y2)
  p <- p %>%
    add_lines(
      y            = ~Verbruik,
      split        = ~Type_lbl,              # <-- cruciaal
      legendgroup  = ~Type_lbl,
      name         = ~paste(Type_lbl, "– Verbruik"),
      yaxis        = "y2",
      line         = list(color = "#1f77b4", width = 1.5, dash = "dot"),
      hovertemplate= "Tijd: %{x}<br>Verbruik: %{y:.3f} kWh<extra></extra>"
    )
  
  # 4c) Injectie: idem per batterij (op y2)
  p <- p %>%
    add_lines(
      y            = ~Injectie,
      split        = ~Type_lbl,              # <-- cruciaal
      legendgroup  = ~Type_lbl,
      name         = ~paste(Type_lbl, "– Injectie"),
      yaxis        = "y2",
      line         = list(color = "#ff7f0e", width = 1.5, dash = "dot"),
      hovertemplate= "Tijd: %{x}<br>Injectie: %{y:.3f} kWh<extra></extra>"
    )
  
  # 5) Layout
  p <- p %>%
    layout(
      xaxis = list(
        title = "Tijd",
        type  = "date",
        rangeselector = list(
          buttons = list(
            list(step = "all", label = "Alles"),
            list(count = 3, label = "Laatste 3 maanden", step = "month", stepmode = "backward"),
            list(count = 1, label = "Laatste maand",     step = "month", stepmode = "backward"),
            list(count = 7, label = "Laatste week",       step = "day",   stepmode = "backward"),
            list(count = 1, label = "Laatste dag",        step = "day",   stepmode = "backward")
          )
        ),
        rangeslider = list(visible = TRUE)
      ),
      yaxis = list(title = "Batterij capaciteit (kWh)"),
      yaxis2 = list(
        title      = "Verbruik/Injectie (kWh)",
        overlaying = "y",
        side       = "right",
        showgrid   = FALSE
      ),
      legend = list(orientation = "h", x = 0, y = 1.15),
      margin = list(l = 60, r = 60, t = 70, b = 90)
    )
  
  # 6) Dropdown met weken (zoom x-as)
  week_buttons <- lapply(seq_len(nrow(df_weeks)), function(i) {
    list(
      method = "relayout",
      label  = df_weeks$week_label[i],
      args   = list(list(xaxis = list(range = c(df_weeks$week_start[i], df_weeks$week_end[i]))))
    )
  })
  
  
  return(p)
}
