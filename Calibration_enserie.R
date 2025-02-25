#Calibation
#Library
library(tidyverse)
library(ggpmisc)
library(ggrepel)


#paths
#Root
folder_root <- "/home/jorge/Documentos/Postdoctoral/Onedrive_UB/UB/FLURB/N2O/Discrete_sample/Licors_en_serie/Calibration_GitHub" # You have to make sure this is pointing to the write folder on your local machine


#Import files
##=====N2O========
  data <- read_csv(paste0(folder_root, "/Results_ppm/integrated_injections_N2O_TG20-01377-2024-12-12T000000.csv"))
  
  
  #Add concentration and volume of inyeccion and ppmml variable
  data <- data %>% separate(label, into = c("Concentration", "Volume"), sep = "_")
  data <- data %>% mutate(Concentration = case_when(Concentration == "CalBottle" ~ 6,
                                                    Concentration == "CalBottle2" ~ 6,
                                                    Concentration == "Calmig" ~ 3,
                                                    Concentration == "Calbaja" ~ 0.3),
                          Volume = as.numeric(Volume))%>% 
    mutate(mlppm=Concentration*Volume) %>% 
    mutate(label = paste(Concentration, Volume, sep = "-"))
  
  #Let's have a look
  data %>% 
    ggplot(aes(x = mlppm, y = peaksum, color = label))+
    geom_point()
  #Something was wrong with 6-0.8 mL so I remove it
  data <- data %>% filter(label != "6-0.8")
  data <- data %>% filter(label != "NA-NA")
  
  #Fit a lineal rect
  data %>% 
    ggplot(aes(x = mlppm, y = peaksum))+
    geom_point()+
    geom_smooth(method = "lm")+
    stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4)
  #Average
  ##Select 3 samples with the lower CV for each volume-dilution treatment
  # Function to find the best combination for a group
  select_lowest_cv <- function(measurements) {
    # Generate all combinations of 3 measurements
    combos <- combn(measurements, 3, simplify = FALSE)
    
    # Calculate the CV for each combination
    cvs <- sapply(combos, function(x) sd(x) / mean(x) * 100)
    
    # Select the combination with the lowest CV
    best_combo <- combos[[which.min(cvs)]]
    return(best_combo)
  }
  
  #Create a df with the combination selected
  result <- data %>%
    group_by(label) %>%
    reframe(
      peaksum = select_lowest_cv(peaksum)
    ) %>% 
    mutate(Selected = "Yes") #This is to track the best combination after join
  
  #Join with data
  data <- data %>% left_join(result)
  data %>% 
    #filter(Selected == "Yes")%>% #If you want to keep just the three best inyections
    group_by(label) %>% 
    summarise(mlppm = mean(mlppm),
              peaksum = mean(peaksum),
              Concentration = unique(Concentration)) %>% 
    ggplot(aes(x = mlppm, y = peaksum))+
    geom_point(aes(color = as.factor(Concentration)))+
    labs(x = "ml·ppm N2O", y = "Peak-area", color = "Dilution")+
    geom_smooth(method = "lm")+
    stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4)+
    theme_bw()
  ggsave(paste0(folder_root, "/Calibration/Calibration_N2O_", unique(data$dayofanalysis), ".pdf"), width = 15, height = 15, units = "cm")
  
  #Save values of fit
  fit <- data %>% 
    #filter(Selected == "Yes") %>%
    group_by(label) %>% 
    summarise(mlppm = mean(mlppm),
              peaksum = mean(peaksum),
              Concentration = unique(Concentration)) %>% 
    ungroup() %>% 
    summarise(Slope = lm(peaksum ~ mlppm)$coefficients[2],
              Intercept = lm(peaksum ~ mlppm)$coefficients[1],
              R2 = cor(mlppm, peaksum)^2)
  
    # Check limit of detection
    #Import files
    #N2O
    data_bs <- read_csv(paste0(folder_root, "/Results_ppm/baselines_N2O_TG20-01377-2024-12-12T000000.csv"))
    
    data_bs <- data_bs %>% mutate(LD = base_sd*3, LQ = base_sd*10)
    data_bs <- data_bs %>% bind_cols(fit)
    #Injecting 1 mL
    data_bs <- data_bs %>% mutate(LD_1mL = (LD+Intercept)/Slope,
                                  LQ_1mL = (LQ+Intercept)/Slope)
    #Join to export
    Calibration_N2O <- data_bs %>% filter(label == "loop_baseline") %>% select(Slope, Intercept, R2, LD, LQ, LD_1mL, LQ_1mL) %>% mutate(Species = "N2O")

##====CO2 and CH4==============
    #Import files
    #CH4
    CH4 <- read_csv(paste0(folder_root, "/Results_ppm/integrated_injections_CH4_TG10-01932-2024-12-12T000000.csv"))
    CH4 <- CH4 %>% mutate(Species = "CH4")
    CO2 <- read_csv(paste0(folder_root, "/Results_ppm/integrated_injections_CO2_TG10-01932-2024-12-12T000000.csv"))
    CO2 <- CO2 %>% mutate(Species = "CO2")
    data <- CH4 %>% bind_rows(CO2)
    
    #Add concentration and volume of inyeccion and ppmml variable
    data <- data %>% separate(label, into = c("Concentration", "Volume"), sep = "_")
    data <- data %>% mutate(Concentration = case_when(Concentration == "CalBottle" & Species == "CH4" ~ 15,
                                                      Concentration == "CalBottle2" & Species == "CH4" ~ 15,
                                                      Concentration == "Calmig" & Species == "CH4" ~ 7.5,
                                                      Concentration == "Calbaja" & Species == "CH4" ~ 0.75,
                                                      Concentration == "CalBottle" & Species == "CO2" ~ 3000,
                                                      Concentration == "CalBottle2" & Species == "CO2" ~ 3000,
                                                      Concentration == "Calmig" & Species == "CO2" ~ 1500,
                                                      Concentration == "Calbaja" & Species == "CO2" ~ 150),
                            Volume = as.numeric(Volume))%>% 
      mutate(mlppm=Concentration*Volume) %>% 
      mutate(label = paste(Concentration, Volume, sep = "-"))
    
    #Let's have a look
    data %>% 
      ggplot(aes(x = mlppm, y = peaksum, color = label))+
      geom_point()+
      facet_wrap(~Species, scales = "free")
    #Something was wrong with 15-0.8 mL and the same 3000-0.8 mL so I remove it
    data <- data %>% filter(label != "15-0.8") %>% filter(label != "3000-0.8")
    
    #Fit a lineal rect
    data %>% 
      ggplot(aes(x = mlppm, y = peaksum))+
      geom_point()+
      facet_wrap(~Species, scales = "free")+
      geom_smooth(method = "lm")+
      stat_poly_eq(use_label(c("eq", "R2")))
    
    
    ##Select 3 samples with the lower CV for each volume-dilution treatment
    # Function to find the best combination for a group
    select_lowest_cv <- function(measurements) {
      # Generate all combinations of 3 measurements
      combos <- combn(measurements, 3, simplify = FALSE)
      
      # Calculate the CV for each combination
      cvs <- sapply(combos, function(x) sd(x) / mean(x) * 100)
      
      # Select the combination with the lowest CV
      best_combo <- combos[[which.min(cvs)]]
      return(best_combo)
    }
    
    #Create a df with the combination selected
    result <- data %>%
      group_by(Species, label) %>%
      reframe(
        peaksum = select_lowest_cv(peaksum)
      ) %>% 
      mutate(Selected = "Yes") #This is to track the best combination after join
    
    #Join with data
    data <- data %>% left_join(result)
    
    #Fit a lineal rect
    data %>% 
      filter(Selected == "Yes") %>% 
      filter(label != "150-1") %>% 
      ggplot(aes(x = mlppm, y = peaksum, color = label))+
      geom_point()+
      facet_wrap(~Species, scales = "free")+
      geom_smooth(method = "lm")+
      stat_poly_eq(use_label(c("eq", "R2")))
    
    #Average
    data %>% 
      #filter(Selected == "Yes") %>%
      #filter(label != "150-1") %>% 
      group_by(Species, label) %>% 
      summarise(mlppm = mean(mlppm),
                peaksum = mean(peaksum),
                Concentration = unique(Concentration)) %>% 
      ggplot(aes(x = mlppm, y = peaksum))+
      geom_point(aes(color = as.factor(Concentration)))+
      labs(x = "ml·ppm", y = "Peak-area", color = "Dilution")+
      facet_wrap(~Species, scales = "free")+
      geom_smooth(method = "lm")+
      stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4)+
      theme_bw()
    ggsave(paste0(folder_root, "/Calibration/Calibration_CO2_CH4_", unique(data$dayofanalysis), ".pdf"), width = 20, height = 15, units = "cm")
    
    #Save values of fit
    fit <- data %>% 
      #filter(Selected == "Yes") %>%
      #filter(label != "150-1") %>% 
      group_by(Species, label) %>% 
      summarise(mlppm = mean(mlppm),
                peaksum = mean(peaksum),
                Concentration = unique(Concentration)) %>% 
      ungroup() %>% 
      group_by(Species) %>% 
      summarise(Slope = lm(peaksum ~ mlppm)$coefficients[2],
                Intercept = lm(peaksum ~ mlppm)$coefficients[1],
                R2 = cor(mlppm, peaksum)^2)
    
    # Check limit of detection
    #Import files
    #CH4
    CH4_bs <- read_csv(paste0(folder_root, "/Results_ppm/baselines_CH4_TG10-01932-2024-12-12T000000.csv"))
    CH4_bs <- CH4_bs %>% mutate(Species = "CH4") %>% mutate(across(c(base_avg, base_sd), ~./1000)) #Convert to ppm
    CO2_bs <- read_csv(paste0(folder_root, "/Results_ppm/baselines_CO2_TG10-01932-2024-12-12T000000.csv"))
    CO2_bs <- CO2_bs %>% mutate(Species = "CO2") %>% mutate(across(c(base_avg, base_sd), ~./1000)) #Convert to ppm
    data_bs <- CH4_bs %>% bind_rows(CO2_bs) %>% left_join(fit)
    
    data_bs <- data_bs %>% mutate(LD = base_sd*3, LQ = base_sd*10)
    #Injecting 1 mL
    data_bs <- data_bs %>% mutate(LD_1mL = (LD+Intercept)/Slope,
                                  LQ_1mL = (LQ+Intercept)/Slope)
    #Join to export
    Calibration_CO2_CH4 <- data_bs %>% filter(label == "loop_baseline") %>% select(Species, Slope, Intercept, R2, LD, LQ, LD_1mL, LQ_1mL)
    

##=====Join and export======
    Calibration <- Calibration_N2O %>% bind_rows(Calibration_CO2_CH4)
    
    write_csv(Calibration, paste0(folder_root, "/Calibration/Calibration_and_limit_of_detection_", unique(data$dayofanalysis), ".csv"))  
    