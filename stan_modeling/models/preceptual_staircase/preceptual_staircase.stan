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

}

transformed data {

  int<lower=1> Nparameters = 1;

  vector[Narms] brightness_initial;

  brightness_initial = rep_vector(0, Narms);
  brightness_initial[Narms] = 0.6;

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

  matrix[Ntrials, Nsubjects] brightnessDiff_external;

  matrix[Ntrials, Nsubjects] brightness1_external;

  matrix[Ntrials, Nsubjects] brightness2_external;

  matrix[Ntrials, Nsubjects] PE_external;

  

  //RL

  for (subject in 1 : Nsubjects) {

    //internal variabels

    real brightnessDiff;

    real PE;

    array[Narms] real brightness;

    

    //set indvidual parameters

    beta[subject] = (population_locations[1]

                     + population_scales[1] * beta_random_effect[subject]);

    

    //likelihood estimation

    for (trial in 1 : Ntrials_per_subject[subject]) {

      //reset Qvalues (first trial only)

      if (first_trial_in_block[subject, trial] == 1) {

        brightness = rep_array(0, Narms);
        brightness[Narms] = 0.6;

      }

  //calculate probability for each action

      brightnessDiff = brightness[offer2[subject, trial]] - brightness[offer1[subject, trial]];

      

      p_ch_action[trial, subject] = inv_logit(beta[subject] * brightnessDiff);
      

      //appened to external variabels

      brightnessDiff_external[trial, subject] = brightnessDiff;

      brightness1_external[trial, subject] = brightness[1];

      brightness2_external[trial, subject] = brightness[2];

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

                                                                    * brightnessDiff_external[trial, subject]);

    }

  }

}




