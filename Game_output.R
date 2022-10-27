
###         Functions       ###
# By: Dayanara M. Diaz Vargas #
###                         ###
#---------------------------------------------------------------------------------------------
game_payoffs <- function(num_of_firms, p=0.5, K="none", S0=0, S1=1, S2=2, best_resp="Yes"){
  
  # set K
  if(K == "none"){
  K <- num_of_firms +1 # resource
  }
  
  
  # Probabilities 
  pr_e <- floor(num_of_firms/2)/num_of_firms
  pr_n <- 1-pr_e
  
  # Payoffs
  p0 <- S0
  p1 <- S1-p
  p2 <- S2-p
  p1_1 <- (S1-p)*pr_e+(-p)*pr_n
  p2_2 <- (S2-p)*pr_e+(-p)*pr_n
  
  # recurrent words
  vi <- "You get = "
  Si <- ", if you choose = "
  S_i <- " & the rest of the firms choose = "
  
  # return
  
  if(best_resp == "Yes"){
      # also return payoffs
      payoffs <- data.frame( 
        Scenario = c("Sum_Si<K", "Sum_Si<K", "Sum_Si<K", "Sum_Si=>K", "Sum_Si=>K", "Sum_Si=>K"),
        Si = c(0, 1, 2, 1, 2, 0),
        Sj_left = c(S0, S0, S0, 
                    ifelse(((num_of_firms-2)*S2+S1*2)>=K, S1, S2), 
                    ifelse(((num_of_firms-2)*S2+S1*2)>=K, S1, S2), 
                    ifelse(((num_of_firms-2)*S2+S1*2)>=K, S1, S2)),
        Sj_right = c(S2, 
                     if((num_of_firms-2)*S0+S2+S1<K){
                       S2
                     } else if((num_of_firms-2)*S0+S1+S1<K){
                       S1 
                     } else {
                       S0
                     }, 
                     if((num_of_firms-2)*S0+S2+S2<K){
                       S2
                     } else if((num_of_firms-2)*S0+S1+S2<K){
                       S1
                     } else {
                       S0
                     }, 
                     S2, S2, S2),
        Si_Payoff = c(p0, p1, p2, p1_1, p2_2, p0),
        Sj_Payoff_left = c(p0, p0, p0, 
                           ifelse(((num_of_firms-2)*S2+S1*2)>=K, p1_1, p2_2),
                           ifelse(((num_of_firms-2)*S2+S1*2)>=K, p1_1, p2_2),
                           ifelse(((num_of_firms-2)*S2+S1*2)>=K, p1_1, p2_2)),
        Sj_Payoff_right = c(p2, 
                     if((num_of_firms-2)*S0+S2+S1<K){
                       p2
                     } else if((num_of_firms-2)*S0+S1+S1<K){
                       p1 
                     } else {
                       p0
                     }, 
                     if((num_of_firms-2)*S0+S2+S2<K){
                       p2
                     } else if((num_of_firms-2)*S0+S1+S2<K){
                       p1
                     } else {
                       p0
                     }, 
                     p2_2, p2_2, p2_2)
        )
      return(payoffs)
  } else {
    cat("Your payoff in each scenario, with ",  num_of_firms-1," other firms:\n") 
      cat(vi , p0, Si, S0 , S_i, "[0, infinity]\n")
        cat(vi, p1, Si, S1, S_i, "[0, ", K-S1-1, "]\n")
          cat(vi, p2, Si, S2, S_i, "[0, ", K-S2-1, "]\n")
            cat(vi, p1_1, Si, S1, S_i, " [", K-S1, " , infinity]\n")
              cat(vi, p2_2, Si, S2, S_i,"[", K-S2, " , infinity]\n")
                cat("\n")
                cat("\n")
    }
}


#----------------------------------------------------------------------------------------------

game_pareto <- function(num_of_firms, K="none"){
  
  # set K
  if(K == "none"){
  K <- num_of_firms +1 # resource
  }
  
  # payoffs
    pays <- game_payoffs(num_of_firms=num_of_firms, best_resp="Yes")
    
    # pareto optimal strategy
    pareto <- pays[which((pays$Si_Payoff) == max(pays$Si_Payoff)), ]
      
      # number of other firms that choose each strategy 
                    nl <- 1 # num. of firms that choose the minimum strategy
                    nr <- 0 # num. of firms that choose the maximum strategy
                    nm <- 0 # num. of firms that choose the median strategy
                    m <- if(pareto$Sj_right==2 & pareto$Sj_left==0){
                            m <- 1
                            } else {
                             m <- "None"
                            }
                      
                    
                    if(m == "None"){
                                   nr <- ifelse(pareto$Sj_right==0, 
                                          floor((K - pareto$Si - nl*pareto$Sj_left)/1)-1, 
                                          floor((K - pareto$Si - nl*pareto$Sj_left)/pareto$Sj_right)-1)
                                   nl <- num_of_firms-nr-1
                                   
                      } else {while((nr*pareto$Sj_right+pareto$Si+nl*pareto$Sj_left+nm*m< K) & (nr+nl+nm+1<num_of_firms)){    
                            nm <- ifelse((nr*pareto$Sj_right+pareto$Si+nl*pareto$Sj_left+(nm+1)*m<K) & 
                                     (nr+nl+nm+1<num_of_firms) &
                                     (((ifelse(pareto$Sj_right==0, 
                                             floor((K - pareto$Si - nl*pareto$Sj_left - nm*m)/1), 
                                            floor((K - pareto$Si - nl*pareto$Sj_left - nm*m)/pareto$Sj_right)))*pareto$Sj_right+pareto$Si+nl*pareto$Sj_left+(nm)*m)<K),
                                   0, nm+1)
                                if((nr*pareto$Sj_right+pareto$Si+nl*pareto$Sj_left+nm*m<K) & (nr+nl+nm+1<num_of_firms)){
                                  nr <- ifelse(pareto$Sj_right==0, 
                                             floor((K - pareto$Si - nl*pareto$Sj_left - nm*m)/1), 
                                            floor((K - pareto$Si - nl*pareto$Sj_left - nm*m)/pareto$Sj_right))
                                    if((nr*pareto$Sj_right+pareto$Si+nl*pareto$Sj_left+nm*m< K) & (nr+nl+nm+1<num_of_firms)){
                                      nl <- num_of_firms-nr-nm-1
                                    }
                                }}
                        }
      
          # print result
            if(m == "None" & pareto$Sj_left==pareto$Sj_right){
                       cat("Pareto Optimality is reached when: Si = ", 
                          pareto$Si, " with vi(Si, S_i) = ", 
                          pareto$Si_Payoff, ", if ", pareto$Scenario,
                          " & ", "Sj = ", pareto$Sj_left,
                          " , with payoff vj(Sj, Si) = ",
                          pareto$Sj_Payoff_left, ". With ",
                          nl, ifelse(nl==1, "other firm ", "other firms "), "choosing ", 
                          pareto$Sj_left," and viceversa.\n")
                       cat("\n")
                       cat("That is: \n")
                       if(pareto$Sj_left==pareto$Si){
                              cat(nl+1, " firms choose ", pareto$Sj_left, " units", ", each with a payoff of ", pareto$Sj_Payoff_left,".\n")
                       } else {
                              cat(nl, "firms choose ", pareto$Sj_left, " units, with a payoff of ", pareto$Sj_Payoff_left, ". \n", "1 firm chooses ", pareto$Si, " units, with a payoff of ", pareto$Si_Payoff, ". \n")
                       }
            } else if (m == "None" & pareto$Sj_left!=pareto$Sj_right){
                cat("Pareto Optimality is reached when: Si = ", 
                          pareto$Si, " with vi(Si, S_i) = ", 
                          pareto$Si_Payoff, ", if ", pareto$Scenario,
                          " & ", "Sj = [", pareto$Sj_left, " , ", pareto$Sj_right,
                          " ], with payoff vj(Sj, Si) = [ ",
                          pareto$Sj_Payoff_left, " , ",
                          pareto$Sj_Payoff_right," ]. With ",
                          nl, ifelse(nl==1, "other firm choosing", "other firms choosing"), 
                          pareto$Sj_left, " and ", nr , 
                          ifelse(nr==1, "other firm choosing", "other firms choosing"), 
                          pareto$Sj_right," and viceversa.\n")
                       cat("\n")
                       cat("That is: \n")
                         if((pareto$Sj_left==pareto$Si) | (pareto$Sj_left==pareto$Si)){
                                cat(ifelse(pareto$Sj_left==pareto$Si, nl+1, nl), " firms choose ", pareto$Sj_left, " units", ", with a payoff of ", pareto$Sj_Payoff_left,
                                    ". \n", ifelse(pareto$Sj_left==pareto$Si, nr, nr+1), "firms choose ", pareto$Sj_right, " units, with a payoff of ", pareto$Sj_Payoff_right,
                                    ". \n")
                         } else {
                                cat(nl, "firms choose ", pareto$Sj_left, " units, with a payoff of ", pareto$Sj_Payoff_left, 
                                    ". \n", "1 firm chooses ", pareto$Si, " units, with a payoff of ", pareto$Si_Payoff, 
                                    ". \n", nr, "firms choose", pareto$Sj_right, "units, with a payoff of ", pareto$Sj_Payoff_right, 
                                    ". \n")
                         }    
            } else {
                        cat("Pareto Optimality is reached when: Si = ", 
                          pareto$Si, " with vi(Si, S_i) = ", 
                          pareto$Si_Payoff, ", if ", pareto$Scenario,
                          " & ", "Sj = [", pareto$Sj_left, " , ",
                          pareto$Sj_right, " ], with payoff vj(Sj, Si) = [",
                          pareto$Sj_Payoff_left, " , ",
                          pareto$Sj_Payoff_right, "]. With ",
                          nr, ifelse(nr==1, "other firm ", "other firms "), "choosing ",  pareto$Sj_right,
                          "; ", nm, ifelse(nm==1, "other firm choosing", "other firms choosing"), m,
                          ", and ", nl, ifelse(nl==1, "other firm", "other firms"),
                          " choosing", pareto$Sj_Payoff_left,", and viceversa.\n")
                          cat("\n")
                          cat("That is: \n")
                          if((pareto$Sj_left==pareto$Si) | (pareto$Sj_left==pareto$Si)){
                                cat(ifelse(pareto$Sj_left==pareto$Si, nl+1, nl), " firms choose ", pareto$Sj_left, " units", ", with a payoff of ", pareto$Sj_Payoff_left,
                                    ". \n", nm, " firms choose ", m, " units,  with a payoff of ", 0,
                                    ". \n", ifelse(pareto$Sj_left==pareto$Si, nr, nr+1), "firms choose ", pareto$Sj_right, " units, with a payoff of ", pareto$Sj_Payoff_right,
                                    ". \n")
                         } else {
                                cat(ifelse(pareto$Sj_left==pareto$Si, nl+1, nl), "firms choose ", pareto$Sj_left, " units, with a payoff of ", pareto$Sj_Payoff_left, 
                                    ". \n", nm, " firms choose ", m, " units,  with a payoff of ", 0,
                                    ". \n", ifelse(pareto$Sj_left==pareto$Si, nr, nr+1), "firms choose", pareto$Sj_right, "units, with a payoff of ", pareto$Sj_Payoff_right, 
                                    ". \n")
                         } 
            }
}

#-----------------------------------------------------------------------------------------------------

game_NE <- function(num_of_firms, K="none") {
  num_of_firms=3
  K="none"
  # set K
  if(K == "none"){
  K <- num_of_firms +1 # resource
  }
  
  # payoffs
    pays <- game_payoffs(num_of_firms=num_of_firms, best_resp="Yes")
    
  # Nash Equilibriums
    NEs <- pays[(pays$Si == pays$Sj_right | pays$Si == pays$Sj_right | (pays$Si == pays$Sj_right-pays$Sj_left-1 & pays$Si == 1)) & (pays$Scenario == "Sum_Si<K" |  pays$Scenario == "Sum_Si=>K"),]
  
    # results
    cat("There ", ifelse(nrow(NEs)==1, " is ", " are "), nrow(NEs), ifelse(nrow(NEs)==1, " Nash Equilibrium ", " Nash Equilibriums "), ".\n")
    cat("\n")
    for(i in 1:nrow(NEs)){
    cat(" ", "(", i, ")", "Nash Equilibrium ", i, ", for ", NEs$Scenario[i], ": \n")
    if(NEs$Sj_left[i]==NEs$Sj_right[i]){
      cat("  ", num_of_firms, " firms choose ", NEs$Sj_left[i], " units, each with a payoff of ", NEs$Sj_Payoff_left[i],". \n")
      cat("\n")
    } else if (NEs$Sj_left[i]!=NEs$Sj_right[i] & NEs$Sj_left[i]==NEs$Si[i]){
      cat("  ", num_of_firms, " firms choose ", NEs$Sj_left[i], " units, each with a payoff of ", NEs$Sj_Payoff_left[i],". \n")
      cat("\n")
    } else if (NEs$Sj_left[i]!=NEs$Sj_right[i] & NEs$Sj_right[i]==NEs$Si[i]){
      cat("  ", num_of_firms, " firms choose ", NEs$Sj_right[i], " units, each with a payoff of ", NEs$Sj_Payoff_right[i],". \n")
      cat("\n")
    } else {
      cat("  ", num_of_firms, " firms choose ", NEs$Si[i], " units, each with a payoff of ", NEs$Si_Payoff[i],". \n")
    }
    }
}
#-----------------------------------------------------------------------------------------------------


game_simulations <- function(num_simulations, best_resp="Yes", pareto="Yes", NE="Yes"){
  for (i in 2:num_simulations) {
    if(best_resp=="Yes" & pareto == "No"){
      cat("Game of ", i, " firms:")
      cat("\n")
      print(game_payoffs(num_of_firms=i, best_resp=best_resp))
      cat("\n")
    } else if (best_resp=="Yes" & pareto == "Yes" & NE=="Yes") {
      cat("Game of ", i, " firms:")
      cat("\n")
      print(game_payoffs(num_of_firms=i, best_resp=best_resp))
      cat("\n")
      cat("---- Pareto Equilibrium --- ")
      cat("\n")
      game_pareto(num_of_firms=i)
      cat("\n")
      cat("---- Nash Equilibrium --- ")
      cat("\n")
      game_NE(num_of_firms=i)
      cat("\n")
      cat("-------------------------------------------------------\n")
      cat("\n")
      cat("\n")
    } else {
    game_payoffs(num_of_firms=i, best_resp=best_resp)
    }
  }
}




#-----------------------------------------------------------------------------------------------------

#### run all
# game_simulations(9, best_resp="Yes")
# game_simulations(9, best_resp="No")



