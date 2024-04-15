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

    correct_responses = 0
  incorrect_responses = 0
  step_size = 2
  df                 =data.frame()
  
for (block in 1:Nblocks){
  
  Qval      = as.matrix(t(rep(0.5,Narms)))
  colnames(Qval)     =sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
  
  reward      = as.matrix(t(rep(0,Narms)))
  colnames(reward)     =sapply(1:Narms, function(n) {paste('reward',n,sep="")})
  
  
  for (trial in 1:Ntrials_perblock){

    #computer offer
    raffle    = sample(1:Narms,Nraffle,prob=rep(1/Narms,Narms)) 
    raffle    = sort(raffle)
    
    #players choice
    p         = exp(beta*Qval[raffle]) / sum(exp(beta*Qval[raffle]))
    choice    = sample(raffle,1,prob=p)
    unchosen  = raffle[choice!=raffle]
    
    #outcome
    if (choice == raffle[2]) {
      correct_responses = correct_responses + 1
      incorrect_responses = 0
    }
    else{
      correct_responses = 0
      incorrect_responses = incorrect_responses + 1
    }
    
    # Update staircase
    if (correct_responses == 3) {
      reward[2] = 0
      reward[1] = 1
      
      correct_responses = 0
      
      
    } else if (incorrect_responses == 1) {
      reward[1] = 0
      reward[2] = 1
      
      incorrect_responses = 0
      
    }
    else {
      reward[1] = Qval[1]
      reward[2] = Qval[2]
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
            reward               = reward,
            correct_responses = correct_responses,
            incorrect_responses = incorrect_responses,
            beta = beta
            )
      
      dfnew=cbind(dfnew,Qval)
      
      
      #bind to the overall df
      df=rbind(df,dfnew)
       
    
    
    #updating Qvalues
    Qval[choice] = Qval[choice] + alpha*(reward[choice] - Qval[choice])
    Qval[unchosen] = Qval[unchosen] + alpha*(reward[unchosen]- Qval[unchosen])
  }
}     
  return (df)
}