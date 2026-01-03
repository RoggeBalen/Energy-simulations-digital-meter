################################################################################
#
# Battery simulation
#
################################################################################


simulate_battery <- function(data, batt_cap = 0, tarief = "vast", energie_kost = energy_cost_ecopower) {
  # Data: een dataframe bestaande uit injectie, verbuik en tijd gegevens
  # Batt_cap: capaciteit van de batterij
  # Tarief: "vast" of "variabel" of "dynamisch"
  # Energie_kost: functie om de kostprijs van het desbetreffende tarief te berekenen
  
  
  n <- nrow(data)
  Batt <- numeric(n)
  Batt_diff <- numeric(n)
  kost <- numeric(n)
  
  for (i in 1:(n - 1)) {
    
    
    ### Bekijk huidige batterij
    #--------------------------
    batt_now <- Batt[i]
    
    
    ### Kostprijs
    #------------
    delta_cost <- 0
    
    
    ### Batterij einde interval
    #--------------------------
    batt_next <- batt_now
    
    
    ### Energie verbruikt
    #--------------------
    if (data$Verbruik[i] > 0) {
      
      
      ### Energie gebruikt van de batterij
      #-----------------------------------
      used_from_batt <- min(data$Verbruik[i], Batt[i])
      
      
      ### Update batterij capaciteit
      #-----------------------------
      batt_next <- batt_next - used_from_batt
      
      
      ### Overige energie die van het net komt
      #---------------------------------------
      remaining <- data$Verbruik[i] - used_from_batt    # 0 of >0
      
      
      ### Definieer de kost voor net energie
      #-------------------------------------
      if (remaining > 0) {
        delta_cost <- delta_cost + energie_kost(remaining, data$start_time[i], tarief)
      }
    }
    
    
    ### Meer geproduceerd dan vebruikt dus injectie
    #----------------------------------------------
    if (data$Injectie[i] > 0) {
      
      
      ### Capaciteit beschikbaar om op te slagen
      #-----------------------------------------
      space <- batt_cap - Batt[i]             # Overige capaciteit om op te vullen
      stored <- min(data$Injectie[i], space)  # Slaag zoveel mogelijk op
      
      
      ### Update nieuwe batterij capaciteit
      #------------------------------------
      batt_next <- batt_next + stored
      
      
      ### Overge energie
      #-----------------
      remaining <- data$Injectie[i] - stored
      
      
      ### Optie verkoop overige energie aan het net (negatieve kost)
      #-------------------------------------------------------------
      if (remaining > 0) {
        delta_cost <- delta_cost + energie_kost(-remaining, data$start_time[i], tarief)
      }
    }
    
    Batt[i + 1] <- batt_next
    kost[i + 1] <- kost[i] + delta_cost
  }
  
  
  return(
    tibble(
    start_time = data$start_time,
    Verbruik = data$Verbruik,
    Injectie = data$Injectie,
    Batt_kWh = Batt,
    Batt_diff_kWh = c(NA, diff(Batt)),
    Kostprijs = kost,
    Type = factor(batt_cap),
    Tarief = tarief
  ))
}
