################################################################################
#
# Energycost Ecopower
#
################################################################################


EPEXDAM_all <- data.frame(waarde = c(65.33, 83.07, 68.98, 63.60, 75.15, 86.19, 85.35 , 108.52, 108.52),
                      maand = c(6, 7, 8, 9, 10, 11, 12, 1, 2),
                      jaar = c(2025, 2025, 2025, 2025, 2025, 2025, 2025, 2026, 2026))


energy_cost_ecopower <- function(kWH , 
                                 tijd, 
                                 tarief = "Vast tarief ecopower Kempen",
                                 date){
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
  if(tarief == "Variabel tarief ecopower Kempen"){
    
    
    ### Selecteer de juiste epexdam voor de juiste maand
    #---------------------------------------------------
    EPEXDAM <- EPEXDAM_all %>% filter(maand == month(date) & jaar == year(date))%>% select(waarde)
    
    
    ### Verbruik
    #-----------
    if(kWH > 0){
      
      
      ### Weektarief
      #-------------
      if (weekdays(as.Date(date, tryFormats = c("%Y-%m-%d"))) %in% 
                   c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ){
        
        ### Check time
        #-------------
        if (as.ITime("00:00:00") < as.ITime(tijd) & as.ITime(tijd) < as.ITime("01:00:00")){ 
          output = (2.1872+(0.1056 * EPEXDAM)) * kWH / 100       
        } else if (as.ITime("01:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("07:00:00")){ 
          output = (1.2572+(0.0826*EPEXDAM)) * kWH / 100
        } else if (as.ITime("07:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("11:00:00")){ 
          output = (2.9372+(0.1429*EPEXDAM)) * kWH / 100
        } else if (as.ITime("11:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("17:00:00")){ 
          output = (2.1872+(0.1056*EPEXDAM)) * kWH / 100
        } else if (as.ITime("17:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("22:00:00")){ 
          output = (2.9372+(0.1429*EPEXDAM)) * kWH / 100
        }else { output = (2.1872+(0.1056*EPEXDAM)) * kWH / 100
        } 
      }else {
        if (as.ITime("00:00:00") <= as.ITime(tijd) & as.ITime(tijd)<as.ITime("01:00:00")){ 
          output = (2.1872+(0.1056 * EPEXDAM)) * kWH / 100       
        } else if (as.ITime("01:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("07:00:00")){ 
          output = (1.2572+(0.0826 * EPEXDAM)) * kWH / 100
        } else if (as.ITime("07:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("11:00:00")){ 
          output = (2.1872+(0.1056 * EPEXDAM)) * kWH / 100
        } else if (as.ITime("11:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("17:00:00")){ 
          output = (1.2572+(0.0826 * EPEXDAM)) * kWH / 100
        } else { output = (1.2572+(0.0826 * EPEXDAM)) * kWH / 100      
        }
      }
    } 
    
    
    ### Injectie
    #-----------
    if(kWH <= 0){
      
      ### Check time
      #-------------
      if (weekdays(as.Date(date, tryFormats = c("%Y-%m-%d"))) %in% 
                   c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ){
        
        if (as.ITime("00:00:00") < as.ITime(tijd) & as.ITime(tijd) < as.ITime("01:00:00")){ 
          output = (0.03+(0.0481*EPEXDAM)) * kWH / 100
        } else if (as.ITime("01:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("07:00:00")){ 
          output = (0.03+(0.0139*EPEXDAM)) * kWH / 100
        } else if (as.ITime("07:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("11:00:00")){ 
          output = (0.03+(0.0927*EPEXDAM)) * kWH / 100
        } else if (as.ITime("11:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("17:00:00")){ 
          output = (0.03+(0.0481*EPEXDAM)) * kWH / 100
        } else if (as.ITime("17:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("22:00:00")){ 
          output = (0.03+(0.0927*EPEXDAM)) * kWH / 100
        }else { output = (0.03+(0.0481*EPEXDAM)) * kWH / 100
        } 
      } else {
        if (as.ITime("00:00:00") <= as.ITime(tijd) & as.ITime(tijd)<as.ITime("01:00:00")){ 
          output = (0.03+(0.0481*EPEXDAM)) * kWH / 100
        } else if (as.ITime("01:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("07:00:00")){ 
          output = (0.03+(0.0139*EPEXDAM)) * kWH / 100
        } else if (as.ITime("07:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("11:00:00")){ 
          output = (0.03+(0.0481*EPEXDAM)) * kWH / 100
        } else if (as.ITime("11:00:00") <= as.ITime(tijd) & as.ITime(tijd) < as.ITime("17:00:00")){ 
          output = (0.03+(0.0139*EPEXDAM)) * kWH / 100
        } else { output = (0.03+(0.0481*EPEXDAM)) * kWH / 100
        }
      }
    }
  }
  
  
  ### Dynamisch tarief ecopower
  #----------------------------
  if(tarief == "dynamisch"){
    
  }
  
  
  return(as.numeric(output))
}
