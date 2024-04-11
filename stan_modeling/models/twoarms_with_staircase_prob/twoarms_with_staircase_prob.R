#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  beta  = parameters['beta']

  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  Qval               = as.matrix(t(rep(0.5,Narms)))
  colnames(Qval)     =sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})

  expvalues          = c(0.5,0.5) #set initial probability of each choice
  correct_responses = 0
  incorrect_responses = 0
  step_size = 0.2
  df                 =data.frame()
  
  
for (block in 1:Nblocks){
  
  Qval      = as.matrix(t(rep(0.5,Narms)))
  colnames(Qval)     =sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
  
  for (trial in 1:Ntrials_perblock){
    
    # Update staircase
    if (correct_responses == 3) {
      expvalues[2] = expvalues[2] - step_size/2
      expvalues[1] = expvalues[1] + step_size/2
      correct_responses = 0
      if (expvalues[2]<0.05) {
        expvalues[2] = 0
        expvalues[1] = 1
      }
    } else if (incorrect_responses == 1) {
      expvalues[2] = expvalues[2] + step_size/2
      expvalues[1] = expvalues[1] - step_size/2
      if (expvalues[1]<0.05) {
        expvalues[1] = 0
        expvalues[2] = 1
      }
      incorrect_responses = 0
    }

    #computer offer
    raffle    = sample(1:Narms,Nraffle,prob=rep(1/Narms,Narms)) 
    raffle    = sort(raffle)
    
    #players choice
    p         = exp(beta*Qval[raffle]) / sum(exp(beta*Qval[raffle]))
    choice    = sample(raffle,1,prob=p)
    unchosen  = raffle[choice!=raffle]
    
    #outcome 
    reward = sample(0:1,1,prob=c(1-expvalues[choice],expvalues[choice]))

    if (choice == raffle[2]) {
      correct_responses = correct_responses + 1
      incorrect_responses = 0
    }
    else{
      correct_responses = 0
      incorrect_responses = incorrect_responses + 1
    }
    
    #save trial's data
    
      #create data for current trials
      dfnew=data.frame(
            subject              = subject,
            block                = block,
            trial                = trial,
            first_trial_in_block = (trial==1)*1,
            choice               = choice,
            selected_offer       = (choice==raffle[2])*1,
            unchosen             = unchosen,
            offer1               = raffle[1],
            offer2               = raffle[2],
            expval_ch            = expvalues[choice],
            expval_unch          = expvalues[raffle[choice!=raffle]],
            reward               = reward,
            correct_responses = correct_responses,
            incorrect_responses = incorrect_responses,
            beta = beta
            )
      
      dfnew=cbind(dfnew,Qval)
      
      
      #bind to the overall df
      df=rbind(df,dfnew)
       
    
    
    #updating Qvalues
    Qval[choice] = Qval[choice] + alpha*(reward - Qval[choice])
  }
}     
  return (df)
}