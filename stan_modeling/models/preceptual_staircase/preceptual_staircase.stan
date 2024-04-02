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

}

transformed data {

  int<lower=1> Nparameters = 1;

}

parameters {

  //population level parameters 

  vector[Nparameters] population_locations;

  vector<lower=0>[Nparameters] population_scales;

  

  //individuals level

  vector[Nsubjects] beta_random_effect;

}

transformed parameters {

  vector[Nsubjects] beta;

  matrix[Ntrials, Nsubjects] p_ch_action;
  

  //RL

  for (subject in 1 : Nsubjects) {
    
    //set indvidual parameters

    beta[subject] = (population_locations[1]

                     + population_scales[1] * beta_random_effect[subject]);

    

    //likelihood estimation

    for (trial in 1 : Ntrials_per_subject[subject]) {

      p_ch_action[trial, subject] = inv_logit(beta[subject] *(brightness2[subject,trial] - brightness1[subject,trial]));
      

    }

  }

}

model {

  // population level  

  population_locations ~ normal(0, 2);

  population_scales ~ cauchy(0, 2);

  

  // indvidual level  

  beta_random_effect ~ std_normal();

  

  for (subject in 1 : Nsubjects) {

    for (trial in 1 : Ntrials_per_subject[subject]) {

      target += bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject]

                                                                    * (brightness2[subject,trial] - brightness1[subject,trial]));

    }

  }

}




