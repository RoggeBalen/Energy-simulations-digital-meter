################################################################################
#
# Battery simulation
#
################################################################################


simulate_battery <- function(data, batt_cap = 0, tarief = "vast", energie_kost = energy_cost_ecopower) {
  # data: dataframe 
  # batt_cap: 0,5,10 kWh
  # tarief: "vast" of "variabel" of "dynamisch"
  # energie_kost: 
  
  
  n <- nrow(data)
  Batt <- numeric(n)
  Batt_diff <- numeric(n)
  kost <- numeric(n)
  
  for (i in 1:(n - 1)) {
    
    ### Define the difference
    #------------------------
    diff <- data$Verbruik[i] - data$Injectie[i]
    
    
    ### Meer verbuik dan productie
    #-----------------------------
    if (diff > 0) {
      
      
      ### Energie gebruikt van de batterij
      #-----------------------------------
      used_from_batt <- min(diff, Batt[i])
      
      
      ### Update batterij capaciteit
      #-----------------------------
      Batt[i + 1] <- Batt[i] - used_from_batt
      
      
      ### Overige energie die van het net komt
      #---------------------------------------
      remaining <- diff - used_from_batt    # 0 of >0
      
      
      ### Definieer de kost voor net energie
      #-------------------------------------
      if (remaining > 0) {
        kost[i + 1] <- kost[i] + energie_kost(remaining, data$start_time[i], tarief)
      }
      else{
        kost[i+1] <- kost[i]
      }
    }
    
    
    ### Meer geproduceerd dan vebruikt
    #---------------------------------
    if (diff <= 0) {
      
      
      ### Verschil dat kan worden opgeslagen
      #-------------------------------------
      surplus <- abs(diff)
      
      
      ### Capaciteit beschikbaar om op te slagen
      #-----------------------------------------
      space <- batt_cap - Batt[i]  # remaining battery capacity
      stored <- min(surplus, space)  # store only what fits
      
      
      ### Update nieuwe batterij capaciteit
      #------------------------------------
      Batt[i + 1] <- Batt[i] + stored
      
      
      ### Overge energie
      #-----------------
      remaining <- surplus - stored
      
      
      ### Optional sell surplus back to net
      #------------------------------------
      if (remaining > 0) {
        kost[i+1] <- kost[i] + energie_kost(-remaining, data$start_time[i], tarief) # Klopt deze functie???
      }
      else{
        kost[i+1] <- kost[i]
      }
    }
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
