################################################################################
#
# Energycost Ecopower
#
################################################################################


energy_cost_ecopower <- function(verbruik , tijd , tarief){
  
  
  ### Vast tarief ecopower
  #-----------------------
  if(tarief == "vast"){
    
    
    ### Verbruik
    #-----------
    if(verbruik > 0){
      output = 0.1187 * verbruik
    }
    
    
    ### Injectie
    #-----------
    if(verbruik <= 0){
      output = 0.02 * verbruik
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
