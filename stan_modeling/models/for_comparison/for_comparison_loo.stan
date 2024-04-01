data {
  
  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    
  int<lower = 4> Narms;                                             
  int<lower = 2> Nraffle; 
  int<lower = 0> fold[Nsubjects,Ntrials];
  real testfold;
  
  //Behavioral data:
  int<lower = 0> choice[Nsubjects,Ntrials];              
  int<lower = 0> reward[Nsubjects,Ntrials];              
  int<lower = 0> offer1[Nsubjects,Ntrials];              
  int<lower = 0> offer2[Nsubjects,Ntrials];              
  int<lower = 0> selected_offer[Nsubjects,Ntrials];      
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials];

  
}


transformed data{
  int<lower = 1> Nparameters=2; 
  vector[Narms] Qvalue_initial; 
  Qvalue_initial = rep_vector(0, Narms);
}




parameters {
  //population level parameters 
  vector         [Nparameters] population_locations;      
  vector<lower=0>[Nparameters] population_scales;         
  
  //individuals level
  vector[Nsubjects] alpha_random_effect;
  vector[Nsubjects] beta_random_effect;
}


transformed parameters {
  
  vector<lower=0, upper=1>[Nsubjects] alpha;
  vector                  [Nsubjects] beta;
  matrix                  [Ntrials,Nsubjects] p_ch_action;
  matrix                  [Ntrials,Nsubjects] Qdiff_external;
  matrix                  [Ntrials,Nsubjects] Qval1_external;
  matrix                  [Ntrials,Nsubjects] Qval2_external;
  matrix                  [Ntrials,Nsubjects] Qval3_external;
  matrix                  [Ntrials,Nsubjects] Qval4_external;
  matrix                  [Ntrials,Nsubjects] PE_external;
  
  
  
  //RL
  for (subject in 1:Nsubjects) {
    //internal variabels
    real   Qdiff;
    real   PE;
    real   Qval[Narms]; 
    
    //set indvidual parameters
    alpha[subject]   = inv_logit(population_locations[1]  + population_scales[1] * alpha_random_effect[subject]);
    beta[subject]    =          (population_locations[2]  + population_scales[2] * beta_random_effect [subject]);
    
    //likelihood estimation
    for (trial in 1:Ntrials_per_subject[subject]){
      if(fold[subject,trial]!=testfold){
      
      //reset Qvalues (first trial only)
      if (first_trial_in_block[subject,trial] == 1) {
        Qval = rep_array(0, Narms);
      }
      
      //calculate probability for each action
      Qdiff        = Qval[offer2[subject,trial]]- Qval[offer1[subject,trial]];
      
      p_ch_action[trial,subject] = inv_logit(beta[subject]*Qdiff);
      
      //update Qvalues
      PE  = reward[subject,trial]  - Qval[choice[subject,trial]];
      Qval[choice[subject,trial]] = Qval[choice[subject,trial]]-alpha[subject]*PE;
      
      #appened to external variabels
      Qdiff_external[trial,subject] = Qdiff;
      Qval1_external[trial,subject] = Qval[1];
      Qval2_external[trial,subject] = Qval[2];
      Qval3_external[trial,subject] = Qval[3];
      Qval4_external[trial,subject] = Qval[4];
      PE_external[trial,subject]    = PE;
    }  
      }
    
  }
  
}


model {
  
  // population level  
  population_locations  ~ normal(0,2);            
  population_scales     ~ cauchy(0,2);        
  
  // indvidual level  
  alpha_random_effect ~ std_normal();
  beta_random_effect  ~ std_normal();
  
  
  for (subject in 1:Nsubjects){
    for (trial in 1:Ntrials_per_subject[subject]){
      if(fold[subject,trial]!=testfold){ //fit parameters only for training set
      target+= bernoulli_logit_lpmf(selected_offer[subject,trial] | beta[subject] * Qdiff_external[trial,subject]);
      }
    }
  }
}

generated quantities {

  matrix[Ntrials,Nsubjects]  log_lik;
  vector[Narms] Qval;
  vector[Nraffle] Qoffer;
  real   PE;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial
log_lik=rep_matrix(0,Ntrials,Nsubjects);
    for (subject in 1:Nsubjects) {
 
        for (trial in 1:Ntrials_per_subject[subject]){
         
         if(fold[subject,trial] == testfold) {
 
      //reset Qvalues (first trial only)
      if (first_trial_in_block[subject,trial] == 1) {
        Qval =rep_vector(0, Narms);
      }
        //offer values
          Qoffer[1]=Qval[offer1[subject,trial]];
          Qoffer[2]=Qval[offer2[subject,trial]];
        log_lik[trial,subject] = bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject] * Qoffer);

      
      
      //update Qvalues
      PE  = reward[subject,trial]  - Qval[choice[subject,trial]];
      Qval[choice[subject,trial]] = Qval[choice[subject,trial]]+alpha[subject]*PE;
      
       
        }
        }
    }
}
