library(tidyverse)

# function: Sampling the exploration time from a normal distribution, ensuring that the outcome is positive
explengthsample <- function(expN, expVar) {
  out <- rnorm(1, expN, expVar)
  while (out <= 0) {
    out <- rnorm(1, expN, expVar)
  }
  out
}

# Function: Calculate the inverse log function used to extrapolating learning progress from confidence
log_inve <- function(x, a, h = 0) {
  if ((1/x) - 1 > 0) {
    out <- ((log((1/x) - 1) / -a) + h)
  } else if ((1/x) - 1 < 0) {
    out <- ((-log(-((1/x) - 1)) / -a) + h)
  } else if ((1/x) - 1 == 0) {
    out <- ((0 / -a) + h)
  }
  out
}

# Function: Calculate bonus multiples based on confidence
Bonus <- function(Con) {
  if (Con >= 0.75) {
    out <- 3 * Con
  } else if (Con >= 0.6) {
    out <- 2 * Con
  } else {
    out <- Con
  }
  out
}

# Function: Calculate accuracy and performance feedback
computeAC_performance <- function(risk, Schema_res, Item_EI, Outputs_cho, ThisPhase, schemainfo) {
  if (risk == "high") {
    if (length(unique(Schema_res)) == 1 && unique(Schema_res) != "give_up") {
      AC <- 1
      performance <- sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase, c("Cho_1", "Cho_2", "Cho_3", "Cho_4")]]) * 3
    } else if (length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2) {
      AC <- 0.5
      performance <- sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase, c("Cho_1", "Cho_2", "Cho_3", "Cho_4")]])
    } else {
      AC <- 0
      performance <- sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase, c("Cho_1", "Cho_2", "Cho_3", "Cho_4")]])
    }
  } else {
    if (length(unique(Schema_res)) == 1 && unique(Schema_res) != "give_up") {
      AC <- 1
      performance <- sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase, c("Cho_1", "Cho_2", "Cho_3", "Cho_4")]]) * 3
    } else if (length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2) {
      AC <- 0.5
      right_schema <- as.data.frame(sort(table(Schema_res), decreasing = TRUE))[1, 1]
      performance <- sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase, c("Cho_1", "Cho_2", "Cho_3", "Cho_4")]]) + 
        schemainfo$payoff[schemainfo$schemaID == right_schema] * 3
    } else {
      AC <- 0
      performance <- sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase, c("Cho_1", "Cho_2", "Cho_3", "Cho_4")]])
    }
  }
  mean.payoff <- sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase, c("Cho_1", "Cho_2", "Cho_3", "Cho_4")]]) / 4
  return(list(AC, performance, mean.payoff))
}

# Main function: simulation experiment for running mode learning
simulation <- function(Param.df, type, exp_type, save = FALSE, savepath = "",
                       sim.mode = c("before", "after", "whole")[3], 
                       before.path = NA,
                       save.confi = FALSE,
                       after.read.best.only = FALSE,
                       model.version = c(1, 2, 3)[1],
                       scale.confi.init) {
  subjectnum <- nrow(Param.df)
  simupath <- file.path(savepath, type)
  expschema <- read.csv(paste0("data/", exp_type, "_schemainfo2.csv"))
  
  if (scale.confi.init) {
    for (row in 1:nrow(expschema)) {
      curr.payoff <- expschema$payoff[row]
      if (curr.payoff == 2) expschema$familirarity[row] <- 0.9
      else if (curr.payoff == 3) expschema$familirarity[row] <- 0.6
      else if (curr.payoff == 4) expschema$familirarity[row] <- 0.4
      else if (curr.payoff == 5) expschema$familirarity[row] <- 0.3
      else if (curr.payoff == 6) expschema$familirarity[row] <- 0.2
    }
  }
  print(expschema)
  
  max_Round <- 200
  ALL.df <- data.frame()
  confidence.df <- data.frame()
  gconfidence.df <- data.frame()
  dwelltime.df <- data.frame()
  
  for (Subject in 1:subjectnum) {
    error <- 0
    start.time <- Sys.time()
    
    # Assign parameters from the parameter table
    a_schema <- Param.df$a_schema[Subject]
    h_schema <- Param.df$h_schema[Subject]
    Beta_N <- Param.df$Beta_N[Subject]
    Beta_Var <- Param.df$Beta_Var[Subject]
    a_generic <- Param.df$a_generic[Subject]
    h_generic <- Param.df$h_generic[Subject]
    Beta_gN <- Param.df$Beta_gN[Subject]
    Beta_gVar <- Param.df$Beta_gVar[Subject]
    w <- Param.df$w[Subject]
    Phi <- Param.df$Phi[Subject]
    decay_speed <- Param.df$decay_speed[Subject]
    decay_speed_thres <- Param.df$decay_speed_thres[Subject]
    thres_item_inter <- Param.df$thres_item_inter[Subject]
    thres_item_final <- Param.df$thres_item_final[Subject]
    theta_shift <- Param.df$theta_shift[Subject]
    timevar <- Param.df$timevar[Subject]
    modeltimestep <- Param.df$modeltimestep[Subject]
    # new parameters
    gamma_incentive <- Param.df$gamma_incentive[Subject] # Intensity of incentive effect
    gamma_inhibition <- Param.df$gamma_inhibition[Subject] # Intensity of inhibition effect
    WSLS_boost_factor <- Param.df$WSLS_boost_factor[Subject] # WSLS forward bias
    WSLS_penalty_factor <- Param.df$WSLS_penalty_factor[Subject] # WSLS backward bias
    thres_con <- Param.df$thres_con[Subject]
    AC_threshold <- Param.df$AC_threshold[Subject]
    give_up_schema_thres <- Param.df$give_up_schema_thres[Subject]
    give_up_item_final_thres <- Param.df$give_up_item_final_thres[Subject]
    payoff_attention_weight <- Param.df$payoff_attention_weight[Subject]
    before_time <- Param.df$before_time[Subject]
    
    Outputs_cho <- data.frame(
      Subject = rep(Subject, max_Round * 2),
      Round = rep(1:(max_Round * 2), each = 2),
      Phase = 1:(max_Round * 2),
      Schema = rep("0", max_Round * 2),  # Changed to character type to be compatible with "give_up"
      Schema_RT = rep(0, max_Round * 2),
      Schema_OB = rep(0, max_Round * 2),
      Schema_AS = rep(0, max_Round * 2),
      Cho_1 = rep(0, max_Round * 2), Cho_2 = rep(0, max_Round * 2), 
      Cho_3 = rep(0, max_Round * 2), Cho_4 = rep(0, max_Round * 2),
      RT_1 = rep(0, max_Round * 2), RT_2 = rep(0, max_Round * 2), 
      RT_3 = rep(0, max_Round * 2), RT_4 = rep(0, max_Round * 2),
      OB_1 = rep(0, max_Round * 2), OB_2 = rep(0, max_Round * 2), 
      OB_3 = rep(0, max_Round * 2), OB_4 = rep(0, max_Round * 2),
      AS_1 = rep(0, max_Round * 2), AS_2 = rep(0, max_Round * 2), 
      AS_3 = rep(0, max_Round * 2), AS_4 = rep(0, max_Round * 2),
      schema_payoff = rep("0", max_Round * 2),  # Changed to character type to be compatible with "give_up"
      AC = rep(0, max_Round * 2),
      performance = rep(0, max_Round * 2),
      afterbreak = rep(0, max_Round * 2),
      emotion_factor = rep(0, max_Round * 2) # Add emotional factor
    )
    Outputs_learn <- data.frame()
    Outputs_glearn <- data.frame()
    Outputs_dwell <- data.frame()
    
    schemainfo <- expschema %>% filter(new == 0) %>% select(-c(author, new))
    colnames(schemainfo)[c(3, 5, 6)] <- c('conN', 'expN', 'expVar')
    schemainfo <- arrange(schemainfo, desc(payoff))
    if (!scale.confi.init) schemainfo$conN <- 0.2 + 0.8 * schemainfo$conN
    schemainfo$conVar <- rep(var(schemainfo$conN), 10)
    
    Expitem <- data.frame(itemID = 1:240)
    Expitem$SchemaID <- rep(schemainfo$schemaID, each = 24)
    Decitem <- data.frame(ItemID = 1:120)
    Decitem$SchemaID <- rep(schemainfo$schemaID, each = 12)
    Decitem$payoff <- rep(schemainfo$payoff, each = 12)
    
    Gcon <- data.frame(conN = mean(schemainfo$conN), conVar = mean(schemainfo$conVar))
    
    ThisRound <- 1
    clock <- 0
    break_happens <- 0
    explorationtime <- 600
    last_performance <- 0
    previous_decision_outcome <- "None" # Initializes the WSLS state
    previous_dominant_schemaID <- NA # Initializes the dominant mode ID
    
    if (type %in% c("L", "Lc", "LH")) risk <- "low" else risk <- "high"
    not.updated <- TRUE
    give_up_mode <- FALSE  # New flag bit
    
    while (clock < 4500) {
      print(paste("Subjects:", Subject, "Round:", ThisRound, "Time:", clock))
      print(paste("Break State:", break_happens, "Risk:", risk, "Type:", type, "Schema Abandonment:", give_up_mode))
      
      if (sim.mode == "after" & not.updated) {
        clock <- before_time
        not.updated <- FALSE
      }
      
      if (break_happens == 0 & clock >= before_time) {
        if (sim.mode == "after") {
          ref.schemainfo <- read.csv(file.path(before.path, "confidence.csv"))
          ref.gconfidence.df <- read.csv(file.path(before.path, "gconfidence.csv"))
          if (after.read.best.only) {
            target.schemainfo <- ref.schemainfo
            target.Gcon <- ref.gconfidence.df
          } else {
            random.subj <- Subject
            if (!(random.subj %in% ref.schemainfo$Subject)) {
              print('The previous simulation data does not exist. The execution continues')
              error <- 1
              break
            }
            target.schemainfo <- ref.schemainfo %>% filter(Subject == random.subj)
            target.Gcon <- ref.gconfidence.df %>% filter(Subject == random.subj) %>% select(-Subject)
          }
          schemainfo <- target.schemainfo %>% filter(Time == max(target.schemainfo$Time)) %>% select(-c(Time, Subject))
          Gcon <- target.Gcon[nrow(target.Gcon), ]
        }
        
        if (type %in% c("L", "H")) {
          schemainfo2 <- expschema %>% filter(type == 'new') %>% select(-c(author, new))
          colnames(schemainfo2)[c(3, 5, 6)] <- c('conN', 'expN', 'expVar')
          schemainfo2 <- arrange(schemainfo2, desc(payoff))
          schemainfo2$conN <- 0.2 + 0.8 * schemainfo2$conN
          schemainfo2$conVar <- rep(var(schemainfo2$conN), 5)
          if ("X" %in% colnames(schemainfo)) {
            schemainfo <- schemainfo %>% filter(type == 'same') %>% select(-c("X"))
          } else {
            schemainfo <- schemainfo %>% filter(type == 'same')
          }
          schemainfo <- rbind(schemainfo, schemainfo2)
          schemainfo <- arrange(schemainfo, desc(payoff))
          Expitem$SchemaID <- rep(schemainfo$schemaID, each = 24)
          Decitem$SchemaID <- rep(schemainfo$schemaID, each = 12)
          Decitem$payoff <- rep(schemainfo$payoff, each = 12)
        }
        
        if (type %in% c("L", "Lc", "HL")) risk <- "low" else risk <- "high"
        print(paste0("break at ", clock, " seconds"))
        break_happens <- 1
        breakR <- ThisRound
        explorationtime <- 300
        if (sim.mode == "before") break
      }
      
      # Calculating the Emotional Factor
      if (ThisRound > 1) {
        max_possible_perf <- max(schemainfo$payoff) * 3 * 4 * 2
        perf_ratio <- last_performance / max_possible_perf
        if (perf_ratio > thres_con) {
          emotion_factor <- emotion_factor + gamma_incentive * (perf_ratio - thres_con)
        } else {
          emotion_factor <- emotion_factor - gamma_inhibition * (thres_con - perf_ratio)
        }
      } else {
        emotion_factor <- 1
      }
      print(paste('Emotional Factor：', emotion_factor))
      
      # Check if the model is in a pattern of abandoning schema matching
      if (ThisRound > 1) {
        avg_AC <- mean(Outputs_cho$AC[Outputs_cho$Round < ThisRound])
        give_up_mode <- (avg_AC < AC_threshold)
      }
      
      # Phase of exploration
      Con_PriorExp <- mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
      Exptime <- mapply(explengthsample, expN = schemainfo$expN, expVar = schemainfo$expVar)
      Exptime <- Exptime * explorationtime / sum(Exptime) * 0.6
      clock <- clock + explorationtime
      explorationtime <- 180
      
      Outputs_learn <- rbind(Outputs_learn, schemainfo)
      Outputs_glearn <- rbind(Outputs_glearn, Gcon)
      
      learning <- mapply(log_inve, Con_PriorExp, a_schema)
      learning <- learning + Exptime
      Con_afterExp <- 1 / (1 + exp(-a_schema * learning))
      schemainfo$conN <- schemainfo$conN + emotion_factor * Beta_N * (Con_afterExp - Con_PriorExp)
      schemainfo$conVar <- schemainfo$conVar + emotion_factor * Beta_Var * (abs(Con_afterExp - Con_PriorExp) - schemainfo$conVar)
      
      Con_PriorExp <- rnorm(1, mean = Gcon$conN, sd = Gcon$conVar)
      learning <- mapply(log_inve, Con_PriorExp, a_generic)
      learning <- learning + mean(Exptime)
      Con_afterExp <- 1 / (1 + exp(-a_generic * learning))
      Gcon$conN <- Gcon$conN + Beta_gN * (Con_afterExp - Con_PriorExp)
      Gcon$conVar <- Gcon$conVar + Beta_gVar * (abs(Con_afterExp - Con_PriorExp) - Gcon$conVar)
      
      Thischosen <- schemainfo %>% 
        group_by(payoff) %>% 
        summarise(chosen = sample(schemaID, size = 1, prob = NULL))
      
      # Decision making phase
      decisiontime <- 1
      while (decisiontime <= 2) {
        if (clock >= 5500 | Sys.time() - start.time > 240) {
          print('The decision phase exceeds the time limit！')
          error <- 1
          break
        }
        
        ThisPhase <- ifelse(decisiontime == 1, (ThisRound * 2) - 1, ThisRound * 2)
        
        Schema_EI <- data.frame(
          schemaID = schemainfo$schemaID,
          payoff = schemainfo$payoff
        )
        Schema_EI$Scon <- mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
        Schema_EI$Gcon <- rnorm(1, mean = Gcon$conN, sd = Gcon$conVar)
        if (give_up_mode) {
          # Simulate the abandonment of schema matching
          Schema_EI$weightCon <- 0.8 * w * Schema_EI$Scon + (1 - 0.8 * w) * Schema_EI$Gcon
          Schema_EI$DM <- Schema_EI$weightCon * Schema_EI$payoff
        } else {
          Schema_EI$weightCon <- w * Schema_EI$Scon + (1 - w) * Schema_EI$Gcon
          Schema_EI$DM <- mapply(Bonus, Schema_EI$weightCon) * Schema_EI$payoff
        }
        
        if (decisiontime == 1) {
          ChoSchema <- Schema_EI[Schema_EI$schemaID %in% Thischosen$chosen, ]
        } else {
          ChoSchema <- Schema_EI[!Schema_EI$schemaID %in% Thischosen$chosen, ]
        }
        
        ChoSchema$evidence <- 0
        ChoItem <- Decitem[Decitem$SchemaID %in% ChoSchema$schemaID, ] %>% 
          group_by(SchemaID) %>%
          summarise(chosen = sample(ItemID, size = 4, prob = NULL))
        
        Item_EI <- data.frame(
          ID = Decitem[Decitem$ItemID %in% ChoItem$chosen, 1],
          Schema = Decitem[Decitem$ItemID %in% ChoItem$chosen, 2],
          payoff = Decitem[Decitem$ItemID %in% ChoItem$chosen, 3]
        )
        Item_EI <- Item_EI %>% 
          group_by(Schema) %>% 
          mutate(
            N = schemainfo$conN[schemainfo$schemaID == Schema],
            Var = schemainfo$conVar[schemainfo$schemaID == Schema],
            DM = ChoSchema$DM[Schema == ChoSchema$schemaID]
          )
        
        Item_EI$evidence <- 0
        Item_EI$recovery <- 0
        Item_EI$timevar <- 1
        Item_EI$decision <- 0
        Item_EI$OB <- 0
        Item_EI$AS <- 0
        Item_EI$status <- 0
        Item_EI$thres <- thres_item_inter
        
        schema_decision <- 0
        Finish <- 0
        dwell_choice_flag <- 0
        attention <- 0
        shift <- 0
        CTime <- 0
        dwelltime <- 0
        thres_schema <- ifelse(give_up_mode, give_up_schema_thres, Param.df$thres_schema[Subject])
        
        if (give_up_mode) {
          Outputs_cho$Schema[ThisPhase] <- "give_up"
          Outputs_cho$schema_payoff[ThisPhase] <- "give_up"
          Outputs_cho$Schema_RT[ThisPhase] <- 0
          Outputs_cho$Schema_OB[ThisPhase] <- 0
          Outputs_cho$Schema_AS[ThisPhase] <- 0
          schema_decision <- "give_up"
          Item_EI$thres <- give_up_item_final_thres
          Item_EI$status <- 1
          Finish <- 1
        }
        
        while (Finish <= 4) {
          if (clock >= 5500) {
            print('Item selection exceeded the time limit！')
            error <- 1
            break
          }
          
          if (attention == 0) {
            Item_EI$timevar <- 1 - (1 / (exp(timevar * Item_EI$recovery)))
            if (sum(Item_EI$decision == 0) == 1) {
              attention <- Item_EI$ID[Item_EI$decision == 0]
            } else {
              if (schema_decision == 0 && !give_up_mode) {
                if (mean(Item_EI$decision) == 1) {
                  print("Modes are still not selected after all items are selected")
                  error <- 1
                  break
                }
                p.list <- (exp(Phi * Item_EI$evidence[Item_EI$decision == 0] * 
                                 Item_EI$DM[Item_EI$decision == 0] * 
                                 Item_EI$timevar[Item_EI$decision == 0])) /
                  sum(exp(Phi * Item_EI$evidence[Item_EI$decision == 0] * 
                            Item_EI$DM[Item_EI$decision == 0] * 
                            Item_EI$timevar[Item_EI$decision == 0]))
                p.list[is.na(p.list)] <- 1
                
                # WSLS adjustment
                if (ThisRound > 1) {
                  if (previous_decision_outcome == "Win" & !is.na(previous_dominant_schemaID)) {
                    boost_idx <- which(Item_EI$Schema[Item_EI$decision == 0] == previous_dominant_schemaID)
                    if (length(boost_idx) > 0) {
                      p.list[boost_idx] <- p.list[boost_idx] * WSLS_boost_factor
                    }
                  } else if (previous_decision_outcome == "Lose" & !is.na(previous_dominant_schemaID)) {
                    penalty_idx <- which(Item_EI$Schema[Item_EI$decision == 0] == previous_dominant_schemaID)
                    if (length(penalty_idx) > 0) {
                      p.list[penalty_idx] <- p.list[penalty_idx] * WSLS_penalty_factor
                    }
                  }
                  p.list <- p.list / sum(p.list) # Renormalize
                }
                
                attention <- sample(Item_EI$ID[Item_EI$decision == 0], 1, prob = p.list)
              } else {
                p.list <- (exp(Phi * Item_EI$evidence[Item_EI$decision == 0] * 
                                 Item_EI$timevar[Item_EI$decision == 0] + 
                                 payoff_attention_weight * Item_EI$payoff[Item_EI$decision == 0])) /
                  sum(exp(Phi * Item_EI$evidence[Item_EI$decision == 0] * 
                            Item_EI$timevar[Item_EI$decision == 0] + 
                            payoff_attention_weight * Item_EI$payoff[Item_EI$decision == 0]))
                p.list[is.na(p.list)] <- 1
                attention <- sample(Item_EI$ID[Item_EI$decision == 0], 1, prob = p.list)
              }
            }
            Item_EI$OB[Item_EI$ID == attention] <- 1
            Item_EI$AS[Item_EI$ID == attention] <- Item_EI$AS[Item_EI$ID == attention] + 1
            shift <- 0
          }
          
          while (shift == 0) {
            if (clock >= 5500) {
              print('The attention shift cycle exceeds the time limit！')
              error <- 1
              break
            }
            
            CTime <- CTime + modeltimestep
            clock <- clock + modeltimestep
            dwelltime <- dwelltime + modeltimestep
            
            Item_EI$evidence[Item_EI$ID == attention] <- Item_EI$evidence[Item_EI$ID == attention] +
              emotion_factor * rnorm(1, Item_EI$N[Item_EI$ID == attention], Item_EI$Var[Item_EI$ID == attention])
            
            Item_EI$recovery[Item_EI$ID != attention] <- Item_EI$recovery[Item_EI$ID != attention] + 1
            Item_EI$evidence[Item_EI$ID != attention & Item_EI$decision == 0] <- 
              Item_EI$evidence[Item_EI$ID != attention & Item_EI$decision == 0] * decay_speed
            Item_EI$thres <- Item_EI$thres * decay_speed_thres
            thres_schema <- thres_schema * decay_speed_thres
            Item_EI$evidence[Item_EI$evidence > Item_EI$thres] <- Item_EI$thres
            
            if (Item_EI$evidence[Item_EI$decision == 0 & Item_EI$ID == attention] >= 
                Item_EI$thres[Item_EI$decision == 0 & Item_EI$ID == attention]) {
              shift <- 1
              if (schema_decision != 0 || give_up_mode) {
                Outputs_cho[ThisPhase, paste0("Cho_", Finish)] <- Item_EI$ID[Item_EI$evidence >= Item_EI$thres & 
                                                                               Item_EI$decision == 0 & 
                                                                               Item_EI$ID == attention]
                Outputs_cho[ThisPhase, paste0("RT_", Finish)] <- CTime
                Outputs_cho[ThisPhase, paste0("OB_", Finish)] <- sum(Item_EI$OB == 1)
                Outputs_cho[ThisPhase, paste0("AS_", Finish)] <- sum(Item_EI$AS)
                Item_EI$OB <- 0
                Item_EI$AS <- 0
                CTime <- 0
                Finish <- Finish + 1
                dwell_choice_flag <- 1
              }
              Item_EI$decision[Item_EI$evidence >= Item_EI$thres & Item_EI$decision == 0] <- 1
              Item_EI$thres[Item_EI$decision == 0] <- ifelse(give_up_mode, give_up_item_final_thres, thres_item_final)
              Item_EI$status[Item_EI$thres == ifelse(give_up_mode, give_up_item_final_thres, thres_item_final) & Item_EI$decision == 0] <- 1
            } else {
              shift <- sample(c(1, rep(0, theta_shift)), 1, prob = NULL)
            }
            
            if (schema_decision == 0 && !give_up_mode) {
              ChoSchema <- ChoSchema %>% group_by(schemaID) %>% 
                mutate(evidence = sum(Item_EI$evidence[Item_EI$Schema == schemaID]))
              if (any(ChoSchema$evidence >= thres_schema)) {
                if (length(ChoSchema$schemaID[ChoSchema$evidence >= thres_schema]) != 1) {
                  schema_decision <- ChoSchema$schemaID[ChoSchema$evidence == max(ChoSchema$evidence)][1]
                } else {
                  schema_decision <- ChoSchema$schemaID[ChoSchema$evidence >= thres_schema]
                }
                Outputs_cho$emotion_factor[ThisPhase] <- emotion_factor
                Outputs_cho$Schema[ThisPhase] <- schema_decision
                Outputs_cho$Schema_RT[ThisPhase] <- CTime
                Outputs_cho$Schema_OB[ThisPhase] <- sum(Item_EI$OB == 1)
                Outputs_cho$Schema_AS[ThisPhase] <- sum(Item_EI$AS)
                Outputs_cho$schema_payoff[ThisPhase] <- ChoSchema$payoff[ChoSchema$schemaID == schema_decision]
                Item_EI$OB <- 0
                Item_EI$AS <- 0
                CTime <- 0
                shift <- 1
                Item_EI$decision <- 0
                Finish <- 1
                dwell_choice_flag <- 1
                Item_EI$thres[Item_EI$status == 0] <- thres_item_final
                Item_EI$status <- 1
              }
            }
            
            if (shift == 1) {
              onerecord <- data.frame(
                Subject = Subject,
                Round = ThisRound,
                Phase = ThisPhase,
                item_ID = attention,
                item_Schema = Item_EI$Schema[Item_EI$ID == attention],
                which_choice = Finish - dwell_choice_flag,
                dwelltime = dwelltime,
                afterbreak = break_happens
              )
              Outputs_dwell <- rbind(Outputs_dwell, onerecord)
              dwelltime <- 0
              dwell_choice_flag <- 0
              Item_EI$recovery[Item_EI$ID == attention] <- 0
              Item_EI$timevar[Item_EI$ID == attention] <- 0
              attention <- 0
            }
          }
        }
        
        Schema_res <- Item_EI$Schema[Item_EI$ID %in% Outputs_cho[ThisPhase, c("Cho_1", "Cho_2", "Cho_3", "Cho_4")]]
        feedback <- computeAC_performance(risk, Schema_res, Item_EI, Outputs_cho, ThisPhase, schemainfo)
        Outputs_cho$AC[ThisPhase] <- feedback[[1]]
        Outputs_cho$performance[ThisPhase] <- feedback[[2]]
        Outputs_cho$payoff[ThisPhase] <- feedback[[3]]
        Outputs_cho$afterbreak[ThisPhase] <- break_happens
        Outputs_cho$emotion_factor[ThisPhase] <- emotion_factor
        
        Outputs_learn <- rbind(Outputs_learn, schemainfo)
        Outputs_glearn <- rbind(Outputs_glearn, Gcon)
        
        schemainfo$conN[schemainfo$schemaID %in% unique(Schema_res)] <- 
          schemainfo$conN[schemainfo$schemaID %in% unique(Schema_res)] + 
          emotion_factor * Beta_N * (Outputs_cho$AC[ThisPhase] - Schema_EI$Scon[Schema_EI$schemaID %in% unique(Schema_res)])
        schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)] <- 
          schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)] + 
          emotion_factor * Beta_Var * (abs(Schema_EI$Scon[Schema_EI$schemaID %in% unique(Schema_res)] - Outputs_cho$AC[ThisPhase]) - 
                                         schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)])
        
        Gcon$conN <- Gcon$conN + Beta_gN * (Outputs_cho$AC[ThisPhase] - unique(Schema_EI$Gcon))
        Gcon$conVar <- Gcon$conVar + Beta_gVar * (abs(Outputs_cho$AC[ThisPhase] - unique(Schema_EI$Gcon)) - Gcon$conVar)
        
        # Update WSLS status at the end of Phase 2
        if (decisiontime == 2) {
          if (Outputs_cho$AC[ThisPhase] == 1) {
            previous_decision_outcome <- "Win"
            previous_dominant_schemaID <- unique(Schema_res)[1]
          } else {
            previous_decision_outcome <- "Lose"
            schema_counts <- table(Schema_res)
            max_count <- max(schema_counts)
            dominant_schemas <- names(schema_counts)[schema_counts == max_count]
            if (length(dominant_schemas) == 1 && dominant_schemas != "give_up") {
              previous_dominant_schemaID <- as.numeric(dominant_schemas)
            } else {
              previous_dominant_schemaID <- NA # Set to NA when tied
            }
          }
        }
        
        decisiontime <- decisiontime + 1
      }
      
      last_performance <- sum(Outputs_cho$performance[Outputs_cho$Round == ThisRound])
      ThisRound <- ThisRound + 1
    }
    
    if (error != 1) {
      Outputs_cho$breakR <- breakR
      Outputs_cho <- Outputs_cho[Outputs_cho$RT_4 != 0, ]
      Outputs_dwell <- Outputs_dwell[Outputs_dwell$Phase <= max(Outputs_cho$Phase), ]
      ALL.df <- rbind(ALL.df, Outputs_cho)
      dwelltime.df <- rbind(dwelltime.df, Outputs_dwell)
      
      Time <- rep(1:((ThisRound - 1) * 3), each = 10)
      Outputs_learn <- cbind(Outputs_learn, Time)
      Outputs_learn$Subject <- Subject
      Outputs_glearn$Subject <- Subject
      if (save.confi == TRUE) {
        confidence.df <- rbind(confidence.df, Outputs_learn)
      }
      gconfidence.df <- rbind(gconfidence.df, Outputs_glearn)
    }
    
    gc()
  }
  
  ALL.df$type <- type
  dwell_mean <- dwelltime.df %>%
    mutate(choice = replace(which_choice, which_choice == 0, 1)) %>%
    group_by(Subject, Round, Phase, choice, afterbreak) %>%
    summarise(dwell_mean = mean(dwelltime)) %>%
    pivot_wider(names_from = choice, names_glue = 'dwellmean_{choice}', values_from = dwell_mean)
  
  allresult <- merge(ALL.df, dwell_mean, by = c("Subject", "Round", "Phase", "afterbreak"))
  allresult <- allresult %>% arrange(Subject, Phase)
  allresult <- allresult %>%
    mutate(
      RT_1 = Schema_RT + RT_1,
      OB_1 = Schema_OB + OB_1,
      AS_1 = Schema_AS + AS_1
    )
  allresult$OB_1[allresult$OB_1 > 20] <- 20
  
  if (save) {
    if (!dir.exists(simupath)) dir.create(simupath, recursive = TRUE)
    write_csv(allresult, file.path(simupath, 'allresult_processed.csv'))
    write_csv(Param.df, file.path(simupath, 'Paras.csv'))
    if (save.confi == TRUE) {
      write_csv(confidence.df, file.path(simupath, 'confidence.csv'))
      write_csv(gconfidence.df, file.path(simupath, 'gconfidence.csv'))
    }
  }
  
  return(list(param = Param.df, allresult_processed = allresult))
}

