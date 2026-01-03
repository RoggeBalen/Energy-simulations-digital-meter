################################################################################
#
# Create visual to show costprice over time
#
################################################################################



plot_week_switch_batt <- function(df, tz_display = "Europe/Brussels") {

  
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
      "Batterij (5kWh)"    = "#d62728",
      "Batterij (7.5kWh)"  = "#9467bd",
      "Batterij (10kWh)"   = "#7f7f7f"
    )
    type_map <- c(
      "0"  = "Geen batterij",
      "5"  = "Batterij (5kWh)",
      "7.5" = "Batterij (7.5kWh)",
      "10" = "Batterij (10kWh)"
    )
    
    df <- df %>%
      mutate(
        Type_orig = as.character(Type),
        Type_lbl  = dplyr::recode(Type_orig, !!!type_map, .default = "Geen batterij"),
        Tarief    = ifelse(is.na(Tarief), "Onbekend", Tarief)
      )
    
    # Zorg voor nette factorvolgorde in de legende
    df <- df %>%
      mutate(
        Type_lbl = factor(Type_lbl, levels = names(kost_colors)),
        Tarief   = factor(Tarief, levels = unique(Tarief))
      )
  
  
  
  
  ### Plot basis
  #-------------
  p <- plot_ly(df, x = ~start_time) %>%
    add_lines(
      y = ~Kostprijs,
      color = ~Type_lbl,
      colors = kost_colors,          
      linetype = ~Tarief,
      hovertemplate = "Tijd: %{x}<br>Kost: €%{y:.2f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title = "Tijd",
          type  = "date",
          rangeselector = list(
            buttons = list(
              list(step = "all", label = "Alles"),
              list(count = 3, label = "Laatste 3 maanden", step = "month",  stepmode = "backward"),
              list(count = 1, label = "Laaste maand", step = "month",  stepmode = "backward"),
              list(count = 7, label = "Laatste week", step = "day", stepmode = "backward"),
              list(count = 1, label = "Laatste dag", step = "day", stepmode = "backward")
            )
          ),
          rangeslider = list(visible = TRUE)
        ),
        yaxis = list(title = "Kost (€)"),
        legend = list(orientation = "h", x = 0, y = 1.15),
        margin = list(l = 60, r = 20, t = 70, b = 90)
      )
    
    # Dropdown met weken (zoom x-as)
    week_buttons <- lapply(seq_len(nrow(df_weeks)), function(i) {
      list(
        method = "relayout",
        label  = df_weeks$week_label[i],
        args   = list(list(xaxis = list(range = c(df_weeks$week_start[i], df_weeks$week_end[i]))))
      )
    })
    
    
    # Bepaal aantal traces en verberg vanaf trace 3
    n_tr <- length(plotly_build(p)$x$data)
    visible_traces <- c(2,4)                  # eerste 2 zichtbaar
    hidden_traces  <- setdiff(seq_len(n_tr), visible_traces)
    
    p <- p %>% style(visible = "legendonly", traces = hidden_traces)
    
    
    p %>% layout(
      updatemenus = list(
        list(
          type = "dropdown",
          x = 5.01, y = 1.12, xanchor = "left", yanchor = "top",
          showactive = TRUE,
          buttons = week_buttons
        )
      )
    )
  
  return(p)
}
