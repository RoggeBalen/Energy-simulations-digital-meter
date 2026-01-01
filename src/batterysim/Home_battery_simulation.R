################################################################################
#
# Battery simulation
#
################################################################################


simulate_battery <- function(data, batt_cap, tarief, energie_kost) {
  # data: dataframe 
  # batt_cap:
  # tarief: "vast" of "variabel"
  # energie_kost: functie ....
  
  
  n <- nrow(data)
  Batt <- numeric(n)
  Batt_diff <- numeric(n)
  kost <- numeric(n)
  
  for (i in 1:(n - 1)) {
    
    ### Define the difference
    #------------------------
    diff <- data$consumption[i] - data$production[i]
    
    
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
        kost[i + 1] <- kost[i] + energie_kost(remaining, data$start_ts[i], tarief)
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
        kost[i+1] <- kost[i] + energie_kost(-remaining, data$start_ts[i], tarief) # Klopt deze functie???
      }
      else{
        kost[i+1] <- kost[i]
      }
    }
  }
  
  
  return(
    tibble(
    Timestamp = data$start_ts,
    Productie = data$production,
    Verbuik = data$consumption,
    Batt_kWh = Batt,
    Batt_diff_kWh = c(NA, diff(Batt)),
    Kost_Euro = kost,
    Cap = factor(batt_cap)
  ))
}
