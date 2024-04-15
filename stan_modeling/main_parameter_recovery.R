#####Setup--------------------
rm(list = ls())
source('./functions/my_starter.R')

path = set_workingmodel()

cfg = list(
  Nsubjects        = 100,
  Nblocks          = 1,
  Ntrials_perblock = 50,
  Narms            = 2, #number of arms in the task
  Nraffle          = 2, #number of arms offered for selection each trial
  rndwlk           = read.csv('./functions/rndwlk.csv', header = F)
)


#####Simulate data--------------------
generate_artificial_data(cfg = cfg)

df = get_df(mydatatype = set_datatype(),path, standata = F) 

# last_5_trials <- df %>%
#   group_by(subject, beta) %>%
#   arrange(desc(trial)) %>%
#   filter(row_number() <= 5) %>%
#   summarise(mean_brightness_diff = mean(brightness2-brightness1))

last_5_trials <- df %>%
  group_by(subject, beta) %>%
  arrange(desc(trial)) %>%
  filter(row_number() <= 5) %>%
  summarise(Qdiff = mean(Qbandit2-Qbandit1))

plot(last_5_trials$beta, last_5_trials$Qdiff,
     xlab = "Beta",
     ylab = "Mean Brightness Diffrences in Last 5 Trials",
     main = "Beta vs. Mean Brightness Diffrences in Last 5 Trials")


#rt = hist(df$rt)

accuracy = df%>%group_by(subject)%>%summarise(accuracy_rate = sum(selected_offer)/length(selected_offer))
#rt = df%>%group_by(subject)%>%summarise(mean_rt = mean(rt))

beta = df%>%
plot(accuracy)

# Assuming df is your dataframe
ggplot(df, aes(y = brightness2, x = rt)) +
  geom_point(size = 1) +  # Increase point size
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(x = "Response Time", y = "Brightness Differences", 
       title = "Response Time vs. Brightness Difficulty") +  # Add labels and title
  theme_minimal() # Set a minimalist theme

#####sample posterior--------------------

modelfit_compile(path, format = F)

modelfit_mcmc(
  
  path,
  data_path = paste0('data/stan_ready_data_files/artificial_standata_', path$name, '.Rdata'),
  
  mymcmc = list(
    datatype = 'artificial' ,
    samples  = 200,
    warmup  = 700,
    chains  = 4,
    cores   = 4
  )
)

#####examine results--------------------
mypars = c("population_scales[1]")

examine_mcmc(path, mypars, datatype = 'artificial')

examine_population_parameters_recovery(path, datatype = 'artificial')

examine_individual_parameters_recovery(path)


####examine model
#load parameters
fit   = readRDS(paste0(path$data, '/modelfit_recovery.rds'))

Qdiff = fit$draws(variables = 'Qdiff_external', format = 'draws_matrix')
Qval1 = fit$draws(variables = 'Qval1_external', format = 'draws_matrix')
Qval2 = fit$draws(variables = 'Qval2_external', format = 'draws_matrix')
Qval3 = fit$draws(variables = 'Qval3_external', format = 'draws_matrix')
Qval4 = fit$draws(variables = 'Qval4_external', format = 'draws_matrix')

PE    = fit$draws(variables = 'PE_external', format = 'draws_matrix')
