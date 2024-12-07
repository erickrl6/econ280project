library(foreign)
library(tidyverse)
library(dplyr)
library(broom)
library(sandwich)
library(lmtest)
library(purrr)

# read in data 
delay_reg <- read.dta('data/analysis/v0_v1_live_predicted_appended_regression_r.dta') %>% 
  filter(former3in1 == 1) # only road directly affected by HOV removal

df <- c()

# Define a function to run regressions and summarize results
run_regression <- function(data, hour_start, hour_end, time_label, cutoff_label) {
  
  # Filter data for the given time range and generate dummies for post_policy_lifting
  df <- data %>%
    filter(hff >= hour_start & hff <= hour_end) %>% 
    mutate(post_policy_lifting = (ymd(date)>cutoff_label) 
           %>% as.numeric()) # o.g. "2016-04-04"
  
  if(hour_start>=20){
    df <- data %>%
      filter(hff >= hour_start | hff <= hour_end)
  }
  
  # Get number of observations
  obs <- nrow(df) 
  obs_treated <- df %>% filter(post_policy_lifting==1) %>% nrow
  
  # Run regression
  model <- lm(delay ~ post_policy_lifting + south_to_north, data = df)
  
  # Calculate clustered standard errors
  vcov_cl <- vcovCL(model, cluster = ~date_direction)
  test <- coeftest(model, vcov = vcov_cl) #-> reg_result
   #  tidy() -> reg_result
  model -> reg_result
  
  # Calculate control mean
  control_mean <- df %>%
    filter(post_policy_lifting == 0) %>%
    summarise(mean_delay = mean(delay, na.rm = TRUE)) %>%
    pull(mean_delay) %>%
    round(2)
  
  mean_obs <- data.frame(time=time_label,
                         cutoff = cutoff_label,
                         ControlMeanstarg = control_mean,
                         Obs = obs,
                         Treated = obs_treated,
                         coef_mock = reg_result$coefficients[[2]],
                         se = test[2,2]
                         ) %>% 
    mutate(upper = coef_mock+2*se,
           lower=coef_mock-2*se,
           days_og_cutoff = (ymd(cutoff_label)-ymd("2016-04-04")) %>% as.numeric()
           )
  
  return(list(reg_result,mean_obs))
}

# 4. RUN REGRESSIONS FOR DIFFERENT TIME PERIODS AND SAVE TO A TABLE
time_periods <- list(
  "6 - 7 a.m." = c(6, 6.99),
  "7 - 10 a.m." = c(7, 9.99),
  "10 a.m. - 4:30 p.m." = c(10, 16.49),
  "4:30 - 7 p.m." = c(16.5, 18.99),
  "7 - 8 p.m." = c(19, 19.99),
  "8 p.m. - 6 a.m." = c(20, 5.99)
)

cutoff_periods <- delay_reg$date %>% unique %>% .[2:24]

# for each cutoff label - fuction to get coefplot varying the time of cutoff
cutoff_coefplot <- function(delay_reg,time_label,cutoff_periods) { 
  
  results_cutoff_7am <- lapply(cutoff_periods, function(cutoff_label) {
    hours <- time_periods[[time_label]]
    run_regression(delay_reg, hours[1], hours[2],time_label,cutoff_label)
  })
  
  # extract second element of each list to get observations and control mean
  obs_mean <- do.call(rbind.data.frame, map(results_cutoff_7am,2)) 
  
  plot <- (obs_mean %>% 
             ggplot(aes(x=days_og_cutoff,y=coef_mock))+ 
             geom_point()+ 
             geom_vline(xintercept = 0,color="red",linetype="dashed")+
             geom_hline(yintercept = 0,color="gray60",linetype="dashed")+
             geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
             theme_minimal()+
             scale_x_continuous(breaks=seq(-4,28,2))+
             ggtitle(paste('Average delay increase for',obs_mean$time))+
             xlab("Mock Cutoff date (days after original cutoff)")+
             ylab("")+
             theme(panel.grid.minor = element_blank())
           )
  name <- paste0("results/",obs_mean$time %>% 
                   unique %>% 
                   str_replace_all(" |\\.|:", ""),".png")
  ggsave(name)
  print(name)
  
  return(plot)
}

cutoff_coefplot(delay_reg,time_label="6 - 7 a.m.",cutoff_periods)
cutoff_coefplot(delay_reg,time_label="7 - 10 a.m.",cutoff_periods)
cutoff_coefplot(delay_reg,time_label="10 a.m. - 4:30 p.m.",cutoff_periods)
cutoff_coefplot(delay_reg,time_label="4:30 - 7 p.m.",cutoff_periods)
cutoff_coefplot(delay_reg,time_label="7 - 8 p.m.",cutoff_periods)
cutoff_coefplot(delay_reg,time_label= "8 p.m. - 6 a.m.",cutoff_periods)

 

#(coef_plot <- multiplot(
 #         map(results_cutoff_7am,1),
  #        intercept = FALSE,
   #       coefficients="post_policy_lifting",
    #      xlab = 'delay',
     #     ylab = '',
      #    #legend.reverse = TRUE,
       #   outerCI = 0,
        #  sort="alphabetical",
         # title = paste('Average Delay for',obs_mean$time)
          #) + theme_minimal() + coord_flip() #+ theme(legend.none)
  #)
