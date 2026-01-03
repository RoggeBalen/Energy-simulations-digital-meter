################################################################################
#
# Energycost Ecopower
#
################################################################################


energy_cost_ecopower <- function(kWH , tijd , tarief = "Vast tarief ecopower Kempen"){
  # kWH: verbruik of injectie
  # Tijd: tijdstip van verbruik of injectie
  # Tarief: type tarief voor kostenberekening
  
  
  ### Vast tarief ecopower
  #-----------------------
  if(tarief == "Vast tarief ecopower Kempen"){
    
    
    ### Verbruik
    #-----------
    if(kWH > 0){
      output = (0.1286 + 0.011 + 0.00392 + 17.51/365/24/4 + 0.0607412 
                + 0.0019261 + 0.04748) * kWH
    }
    
    
    ### Injectie
    #-----------
    if(kWH <= 0){
      output = 0.02 * kWH
    }
  }
  
  
  ### Variabel tarief ecopower
  #---------------------------
  if(tarief == "variabel"){
    output <- -80   # nog onzin
  }
  
  
  ### Dynamisch tarief ecopower
  #----------------------------
  if(tarief == "dynamisch"){
    
  }
  
  
  return(output)
}
