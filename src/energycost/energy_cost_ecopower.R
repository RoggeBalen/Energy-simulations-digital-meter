################################################################################
#
# Energycost Ecopower
#
################################################################################


energy_cost_ecopower <- function(verbruik , tijd , tarief){
  
  
  if(tarief == "vast"){
    
    
    ### Verbruik
    #-----------
    if(verbruik > 0){
      output = 0.1187 * verbruik
    }
    
    
    ### Injectie
    #-----------
    if(verbruik < 0){
      output = 0.02 * verbruik
    }
  }
  
  
  
  if(tarief == "variabel"){
    
    
    output <- -80   # nog onzin
    
  }
  
  return(output)
}
