data {
  
  //General fixed parameters for the experiment/models
  
  int<lower=1> Nsubjects;
  
  int<lower=1> Nblocks;
  
  int<lower=1> Ntrials;
  
  array[Nsubjects] int<lower=1> Ntrials_per_subject;
  
  int<lower=2> Narms;
  
  int<lower=2> Nraffle;
  
  
  
  //Behavioral data:
  
  array[Nsubjects, Ntrials] int<lower=0> choice;
  
  array[Nsubjects, Ntrials] int<lower=0> correct_responses;
  
  array[Nsubjects, Ntrials] int<lower=0> incorrect_responses;
  
  array[Nsubjects, Ntrials] int<lower=0> offer1;
  
  array[Nsubjects, Ntrials] int<lower=0> offer2;
  
  array[Nsubjects, Ntrials] int<lower=0> selected_offer;
  
  array[Nsubjects, Ntrials] int<lower=0> first_trial_in_block;
  array[Nsubjects, Ntrials] real<lower=0> brightness1;
  array[Nsubjects, Ntrials] real<lower=0> brightness2;
  array[Nsubjects, Ntrials] real rt;
  
}

transformed data {
  
  int<lower=1> Nparameters = 6;
  
}

parameters {
  
  //population level parameters 
  
  vector[Nparameters] population_locations;
  
  vector<lower=0>[Nparameters] population_scales;
  
  
  
  //individuals level
  
  vector[Nsubjects] beta_random_effect;
  
  
  vector[Nsubjects] mu0_random_effect;
  
  
  
  vector[Nsubjects] mu1_random_effect;
  
  
  
  vector[Nsubjects] sigma_random_effect;
  
  
  
  vector[Nsubjects] tau0_random_effect;
  
  
  
  vector[Nsubjects] tau1_random_effect;
}

transformed parameters {
  
  vector[Nsubjects] beta;
  
  
  vector[Nsubjects] mu0;
  
  
  
  vector[Nsubjects] mu1;
  
  
  
  vector<lower=0>[Nsubjects] sigma;
  
  
  
  vector[Nsubjects] tau0;
  
  
  
  vector<lower=0>[Nsubjects] tau1;
  matrix[Ntrials, Nsubjects] p_ch_action;
  matrix[Ntrials, Nsubjects] tau_external = rep_matrix(0, Ntrials, Nsubjects);
  
  
  
  matrix[Ntrials, Nsubjects] mu_external;
  
  
  //RL
  
  for (subject in 1 : Nsubjects) {
    
    //set indvidual parameters
    
    beta[subject] = (population_locations[1]
    
    + population_scales[1] * beta_random_effect[subject]);
    
    
    
    
    mu0[subject] = (population_locations[2]
    
    + population_scales[2] * mu0_random_effect[subject]);
    
    
    
    mu1[subject] = (population_locations[3]
    
    + population_scales[3] * mu1_random_effect[subject]);
    
    
    
    sigma[subject] = (population_locations[4]
    
    + population_scales[4] * sigma_random_effect[subject]);
    
    
    
    tau0[subject] = (population_locations[5]
    
    + population_scales[5] * tau0_random_effect[subject]);
    
    
    
    tau1[subject] = (population_locations[6]
    
    + population_scales[6] * tau1_random_effect[subject]);
    
    //likelihood estimation
    
    for (trial in 1 : Ntrials_per_subject[subject]) {
      
      p_ch_action[trial, subject] = inv_logit(beta[subject] *(brightness2[subject,trial] - brightness1[subject,trial]));
      
      tau_external[trial, subject] = tau0[subject]
      
      + tau1[subject] * abs((brightness2[subject,trial] - brightness1[subject,trial]));
      
      
      
      mu_external[trial, subject] = mu0[subject] + mu1[subject] * abs((brightness2[subject,trial] - brightness1[subject,trial]));
    }
    
  }
  
}

model {
  
  // population level  
  
  population_locations ~ normal(0, 2);
  
  population_scales ~ cauchy(0, 2);
  
  
  
  // indvidual level  
  
  beta_random_effect ~ std_normal();
  mu0_random_effect ~ std_normal();
  
  
  
  mu1_random_effect ~ std_normal();
  
  
  
  sigma_random_effect ~ std_normal();
  
  
  
  tau0_random_effect ~ std_normal();
  
  
  
  tau1_random_effect ~ std_normal();
  
  
  
  for (subject in 1 : Nsubjects) {
    
    for (trial in 1 : Ntrials_per_subject[subject]) {
      
      target += bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject]
      
      * (brightness2[subject,trial] - brightness1[subject,trial]));
      
      target += exp_mod_normal_lpdf(rt[subject, trial] | mu_external[trial, subject], sigma[subject], inv(tau_external[trial, subject])
      
      );
      
    }
    
  }
  
}




