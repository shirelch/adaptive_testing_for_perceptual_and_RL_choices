#task similar to https://www.pnas.org/doi/pdf/10.1073/pnas.2002903117
sim.block = function(subject, parameters, cfg) {
  print(paste('subject', subject))
  
  #pre-allocation
  #set parameters
  beta = parameters['beta']
  mu0 = parameters['mu0']
  mu1 = parameters['mu1']
  sigma  = parameters['sigma']
  tau0 = parameters['tau0']
  tau1 = parameters['tau1']
  
  
  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  brightness               = as.matrix(t(rep(0, Narms)))
  colnames(brightness)     = sapply(1:Narms, function(n) {
    paste('brightness', n, sep = "")
  })
  df                 = data.frame()
  
  correct_responses = 0
  incorrect_responses = 0
  step_size = 0.1
  for (block in 1:Nblocks) {
    brightness[2]      = 0.6
    
    for (trial in 1:Ntrials_perblock) {
      # Update staircase
      if (correct_responses == 3) {
        brightness[2] = brightness[2] - step_size
        correct_responses = 0
        if (brightness[2]<0.1) {
          brightness[2] = 0
        }
      } else if (incorrect_responses == 1) {
        brightness[2] = brightness[2] + step_size
        incorrect_responses = 0
      }
      
      #computer offer
      raffle    = sample(1:Narms, Nraffle, prob = rep(1 / Narms, Narms))
      raffle    = sort(raffle)
      
      #players choice
      p         = exp(beta * brightness[raffle]) / sum(exp(beta * brightness[raffle]))
      choice    = sample(raffle, 1, prob = p)
      unchosen  = raffle[choice != raffle]
      
      #outcome
      if (choice == raffle[2]) {
        correct_responses = correct_responses + 1
        incorrecrt_responses = 0
      }
      else{
        correct_responses = 0
        incorrect_responses = incorrect_responses + 1
      }
      
      rt = rnorm(n = 1,mean = mu0 + mu1 * abs(brightness[raffle]), sd = sigma) + rexp(n = 1, rate = 1/(tau0 + tau1 * abs(brightness[raffle])))
      
      
      #save trial's data
      
      #create data for current trials
      dfnew = data.frame(
        subject              = subject,
        block                = block,
        trial                = trial,
        first_trial_in_block = (trial == 1) * 1,
        choice               = choice,
        selected_offer       = (choice == raffle[2]) * 1,
        unchosen             = unchosen,
        offer1               = raffle[1],
        offer2               = raffle[2],
        correct_responses = correct_responses,
        incorrect_responses = incorrect_responses,
        rt = rt
      )

      dfnew = cbind(dfnew, brightness)
      
      #bind to the overall df
      df = rbind(df, dfnew)
      
    }
  }
  return (df)
}