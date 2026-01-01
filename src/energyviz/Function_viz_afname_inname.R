################################################################################
#
# Create visual to show consumption and production over time
#
################################################################################


plot_week_switch <- function(df , tz_display = "Europe/Brussels") {
  
  
  # Zorg dat start_time POSIXct is
  if (!inherits(df$start_time, "POSIXct")) {
    df$start_time <- as.POSIXct(df$start_time, tz = tz_display)
  }
  
  
  # Weeklabels en ranges
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
  
  # Plot
  p <- plot_ly(df, x = ~ start_time) %>%
    add_lines(y = ~ Verbruik,
              name = "Verbruik (kWh)",
              line = list(color = "#1f77b4", width = 2),
              hovertemplate = "Tijd: %{x}<br>Productie: %{y:.3f} kWh<extra></extra>") %>%
    add_lines(y = ~ Injectie,
              name = "Injectie (kWh)",
              line = list(color = "#ff7f0e", width = 2, dash = "dot"),
              hovertemplate = "Tijd: %{x}<br>Injectie: %{y:.3f} kWh<extra></extra>") %>%
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
      yaxis = list(title = "Energie (kWh)"),
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
