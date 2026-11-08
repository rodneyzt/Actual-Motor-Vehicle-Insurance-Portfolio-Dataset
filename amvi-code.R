library(here) # anchors relative file paths to .Rproj or .git folder/directory
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

options(scipen = 999)

data <- read_delim("Motor vehicle insurance data.csv",delim = ";", escape_double = FALSE,
                   trim_ws = TRUE, show_col_types = FALSE) %>%
  janitor::clean_names()
claims <- read_delim("sample type claim.csv",delim = ";", escape_double = FALSE,
                     trim_ws = TRUE, show_col_types = FALSE) %>%
  janitor::clean_names()

#'* ============================================================================================= *
#'* STRUCTURING DATA:  */ join the policy data with the claims data */
#'* ============================================================================================= *

claims_policy_level <- claims %>%
  group_by(id , cost_claims_year) %>%
  summarise(claim_count = n(),
            .groups = 'drop'
  ) #claim_count == n_claims_year
claims_claim_level <- claims %>%
  group_by(id , claims_type , cost_claims_year) %>%
  summarise(claim_type_count = n(), 
            amount = sum(cost_claims_by_type , na.rm = TRUE),
            .groups = "drop"
  )
claims_wide <- claims_policy_level %>% 
  left_join(claims_claim_level , by = c('id' , 'cost_claims_year')) %>%
  relocate(claims_type , .after = id)
claims_type <- unique(claims$claims_type)
data_wide <- data %>%
  left_join(claims_claim_level , by = c("id" , "cost_claims_year") ,
            relationship = "many-to-many") %>%
  mutate(claim_type_count = coalesce(claim_type_count , 0L),
         amount = coalesce(amount , 0L))

# use data from claims table where possible (to incorporate claims history info incl claim type)
data_wide <- data_wide %>%
  mutate(claims_type = case_when(cost_claims_year > 0 & n_claims_year > 0 & is.na(claims_type) ~ "anonymized",
                                 TRUE ~ claims_type) ,
         amount = case_when(claims_type == "anonymized" ~ cost_claims_year,
                            TRUE ~ amount) ,
         claim_type_count = case_when(claims_type == 'anonymized' ~ n_claims_year,
                                      TRUE ~ claim_type_count),
         avg_amount = case_when(claim_type_count > 0 ~ round(amount/claim_type_count,2) , 
                                claim_type_count == 0 ~ 0)                                       
  )
data_wide <- data_wide %>% mutate(policy_year = year(dmy(date_last_renewal)),
                                  policy_month = month(dmy(date_last_renewal), label = TRUE, abbr = TRUE))
#'* ============================================================================================= *
#'* DATA Partition: */ The data is split chronologically, not randomly */                           
#' The yrs 2015-2017 will serve as the training set */ & the yr 2018 as the test set /*    
#'* ============================================================================================= *

# claims level split
split_claims_level <- data_wide %>%
  mutate(policy_year = year(dmy(date_last_renewal)),
         policy_date = as.Date(paste0(policy_year,"-01-01"))) %>%
  arrange(policy_date) %>%
  sliding_period(index = policy_date, period = "year", lookback = 2,assess_stop = 1,complete = TRUE)
split_claims_level$splits[[1]]$data <- split_claims_level$splits[[1]]$data %>%
  select(-policy_date)

train <- analysis(split_claims_level$splits[[1]]) %>% arrange(id)

#'* ===============================================================================================*
#'* Exploratory Data Analysis (EDA) *                                                                
#'* ===============================================================================================*

#'* ============================================================================================= *
#'[ SUMMARY/DESCRIPTIVE STATS
#'* ============================================================================================= *

library(skimr) #probably, lightgbm, partykit, gt
library(e1071) # for calculating skewness
library(cli) # for using col_cyan
#options(cli.num_colors = 256)
library(fansi) # for using sgr_to_html
# defining custom histogram to use with skimr
custom_hist <- function(.x){
  inline_hist(.x) %>%
    col_cyan %>%
    sgr_to_html
}
my_skim <- skim_with(
  numeric = sfl( min.val = ~ min(., na.rm = TRUE),
                 median.val = ~ quantile(.,.50, na.rm = TRUE),
                 mean.val = ~ mean(., na.rm = TRUE),
                 p95.val = ~ quantile(., .95, na.rm = TRUE),
                 p99.val = ~ quantile(., .99, na.rm = TRUE),
                 max.val = ~ max(., na.rm = TRUE),
                 st.dev = ~ sd(., na.rm = TRUE),
                 skewness = ~ skewness(., na.rm = TRUE, type= 1), #pop/textbook formula
                 distribution = ~ custom_hist(.) #sgr-to_html: converts ANSI codes to HTML span styling strings. pair with esc=F in kable
  ), append = FALSE)

skim_numeric_summary <- train %>% select(where(is.numeric)) %>% 
  my_skim %>% mutate(across(where(is.numeric), ~round(.,4))) %>%
  yank("numeric")
skim_character_summary <-skim(train) %>% yank("character")

#'* ============================================================================================= * 
#'[ DATA VISUALIZATION  
#'* ============================================================================================= *
source("tidy-eda-toolkit.R")

# Target variables data viz: n_claims_year and avg_amount *                                             

tp1 <- train %>% group_by(id, policy_year) %>% slice(1) %>%
  filter(n_claims_year > 0 & n_claims_year < 10) %>%
  plot_categorical(. , n_claims_year, fill_var = policy_year,
                   title = "Distribution of number of claims from 2015 to 2017",
                   legend_title = "Policy Year")

tp2 <- train %>% group_by(id, policy_year) %>% slice(1) %>% 
  group_by(policy_year, policy_month) %>%
  summarise(`total claim count` = sum(n_claims_year, na.rm = TRUE),
            .groups = "drop") %>%
  plot_bivariate(. , policy_month, `total claim count`, color_var = policy_year,
                 xlab = "policy start month", legend_title = "policy start year",
                 title = "Total number of claims for each month from 2015 to 2017") +
  geom_line(aes(x = policy_month, y = `total claim count`, group = policy_year,
                colour = factor(policy_year)))

tp3 <- train %>%  #bimodal
  filter(avg_amount > 0 & avg_amount < 1500) %>%
  plot_numerical(. , avg_amount, type = "both", bins = 35,
                 title = "Distribution of the average claim amount in € for claims < €1,500",
                 subtitle = "excluding policies with €0 claim amounts",
                 ylab = "density", xlab = "average claim amount (€)",scico_palette = "oslo")

tp4 <- train %>%  #bimodal
  filter(avg_amount > 0 & avg_amount < 1500, avg_amount != 882) %>%
  plot_numerical(. , avg_amount, type = "both", bins = 35,
                 title = "Distribution of the average claim amount in € for claims < €1,500",
                 subtitle = "excluding policies with €0 and €882 claim amounts",
                 ylab = "density", xlab = "average claim amount (€)",scico_palette = "oslo")

tp5 <- train %>% filter(policy_year == 2017, premium < 1000) %>%
  plot_numerical(. , premium, type = "both", bins = 50, title = "Distribution of premiums for 2017",
                 subtitle = "The distribution for past years is similar to that of 2017")

# Univariate data viz: categorical variable                                                            

area_labels <- c('0' = 'rural', '1' = 'urban')
fuel_labels <- c('P' = 'petrol', 'D' = 'Diesel')
payment_labels <- c('0' = 'annual', '1' = 'bi-annual')
distr_channel_labs <- c('0' = 'Agent', '1' = 'Insurance broker')
risk_labels <- c('1' = 'motorbikes', '2' = 'vans', '3' = 'passenger cars', '4' = 'agricultural vehicles')
driver_labels <- c('1' = 'yes', '0' = 'no')

cp1 <- train %>%
  select(area, claims_type,n_claims_year,id) %>% distinct(.) %>%
  plot_boxplot(. , claims_type, n_claims_year, ylab = 'number of claims year', fill_var = area,
               fill_labels = area_labels, xlab = "claims type")

cp2 <- train %>% group_by(id, policy_year) %>% slice(1) %>% 
  select(id,type_fuel) %>% distinct %>%
  filter(!is.na(type_fuel)) %>%
  plot_categorical(. , type_fuel, fill_var = type_fuel, fill_labels = fuel_labels, legend_title = "Fuel type",
                   title = "Distribution of fuel types")
cp3 <- train %>%
  select(area, payment,id) %>% distinct(.) %>%
  plot_categorical(., payment, fill_var = area, fill_labels = area_labels,
                   title = "Distribution of policyholders", 
                   subtitle =  "by geographical area and payment timing") +
  scale_x_discrete(labels = payment_labels)

cp4 <- train %>%
  select(area, payment, distribution_channel,id) %>% distinct(.) %>%
  plot_categorical(.,distribution_channel,fill_var = area,fill_labels = area_labels,
                   facet_var = payment,facet_labels = payment_labels, 
                   title = "Count of policyholders by distribution channel for each each payment timing and area") +
  scale_x_discrete(labels = distr_channel_labs)

cp5 <- train %>%
  select(second_driver, type_risk,id) %>% distinct(.) %>%
  plot_categorical(.,type_risk,fill_var = second_driver,fill_labels = driver_labels,
                   title = "Count of policyholders for each insured risk type",
                   subtitle = "grouped by whethere a secondary driver exist or not") +
  scale_x_discrete(labels = risk_labels)

# Bivariate Dta viz: numerical and categorical variables                                              

bp1 <- train %>% 
  plot_boxplot(., claims_type, premium,fill_var = claims_type, xlab = "claims type", ylab = "averge claim amount", scico_palette = "lisbon")

bp2 <- train %>% 
  plot_boxplot(., claims_type, avg_amount, fill_var = type_risk, fill_labels = risk_labels,
               xlab = "claims type", ylab = "averge claim amount", scico_palette = "lisbon")

bp3 <- train %>% group_by(id, policy_year) %>% slice(1) %>%
  plot_bivariate(., power, value_vehicle, color_var = type_risk,
                 facet_var = type_risk, color_labels = risk_labels,
                 subtitle = "vehicle power vs vehicle value for each type of risk for the year 2017")

#'* ============================================================================================= ]*
#'*/ DATA CLEANING: */ Some columns have NA's */                                                         
#' claims_type: */ use anonymized for claims with incomplete info & Count>=1 */  
#'[ 2 separate claims are filed, and 1 has R0, i.e was probably rejected,bcos customer filed a... 
#'[ ...a claim that wasn't actually covered by their policy contract(e.g claiming 4 a complaint...
#'[ ...under a basic travel assistance policy) hence claim's declined,Claim was below excess...   
#'[ .../deductible hence no payout,Fraud (e.g breach of terms like unlisted driver operating ...  
#'[ ...vehicle at time of incident), etc                                                          
#'* ============================================================================================= *

# use data from claims table where possible (to incorporate claims history info incl claim type)
clean_data <- function(df) {
  df %>% 
    mutate(
      claims_type = case_when(cost_claims_year > 0 & n_claims_year > 0 & is.na(claims_type) ~
                                "anonymized",
                              TRUE ~ claims_type) ,
      amount = case_when(claims_type == "anonymized" ~ cost_claims_year, TRUE ~ amount) ,
      claim_type_count = case_when(claims_type == 'anonymized' ~ n_claims_year,
                                   TRUE ~ claim_type_count),
      avg_amount = case_when(claim_type_count > 0 ~ round(amount/claim_type_count,2) , 
                             claim_type_count == 0 ~ 0)                                       
    ) %>%
    # a var level/category needs a bare minimum of 30 rows per year to be statistically useful.
    # every factor must have more than 200 levels (naive approach) i.e approx 50 levels per year
    mutate( 
      # claims_type = case_when(claims_type %in% c("theft","injuries","other","fire") ~ "Rare",
      #                         TRUE ~ claims_type),
      claims_type = case_when(claims_type == "fire" ~ "other", TRUE ~ claims_type)
    ) 
}

train <- clean_data(train)

#'* ============================================================================================= *
#'*/ FEATURE ENGINEERING: */ Will construct new vars */                                   
#'[ some got insurance b4 obtaining a drivers licence & others with start date > last renewal??
#'* ============================================================================================= *

# feature eng
feature_eng <- function(df){
  df %>% 
    mutate(across(starts_with("date_") , dmy)) %>%
    mutate(
      is_882 = if_else(avg_amount == 882, 1, 0),
      policy_year = year(date_last_renewal) ,
      policy_month = month(date_last_renewal, label = TRUE, abbr = TRUE) ,
      driver_age = (policy_year - year(date_birth)) ,
      policy_quarter = paste0("Q",quarter(date_last_renewal)) ,
      date_month = month(date_last_renewal),
      policy_season = case_when(date_month<=8 & date_month>=6 ~ "summer" ,
                                date_month<=5 & date_month>=3 ~ "spring" ,
                                date_month<=2 | date_month>=12 ~ "winter" , 
                                date_month<=11 & date_month>=9 ~ "autumn") ,
      licence_duration = pmax(0,round(difftime(date_last_renewal , date_driving_licence ,
                                               units = "days") / 365 , 2)
      ) %>% as.numeric ,
      
      date_entry = pmax(date_start_contract , date_last_renewal , as.Date("2015-11-01") ,
                        na.rm = TRUE) , #the data ranges from 1nov2015 to dec2018.
      date_exit = pmin(date_next_renewal , date_lapse , as.Date("2018-12-31") ,
                       na.rm = TRUE) , #thus start and end date of investigation is 2015-11-01 and 2018-12-31, resp.
      policy_exposure = pmax(round(difftime(date_exit,date_entry,units = "days")/365,3) %>%
                               as.numeric,0),
      log_exposure = log(policy_exposure),
      policy_tenure = pmax(round(difftime(date_next_renewal,date_start_contract,
                                          units = "days")/365,0) %>%
                             as.numeric, 0)
    ) %>%
    select(-starts_with("date")) %>%
    #filter( policy_exposure > 0) %>% # policies with zero exposure are invalid
    mutate(policy_tenure = case_when(policy_exposure == 0 ~ dplyr::lag(policy_tenure),
                                     TRUE ~ policy_tenure) ) %>%
    group_by(id) %>% arrange(policy_year) %>%
    mutate(
      is_new_client = if_else(policy_tenure == 0,1,0),
      past_claims = dplyr::lag(cumsum(n_claims_year),default = 0)) %>%
    arrange(id) %>%
    ungroup()
}
# data cleaning 2.0
fix_data_types <- function(df) {
  character_vars = c("type_risk","type_fuel","claims_type","policy_quarter",
                     "policy_season","area","payment","second_driver","is_new_client")
  df %>%
    mutate( across(all_of(character_vars) , as_factor))
}

train <- feature_eng(train)
train <- fix_data_types(train)

preprocess_base <- recipe( is_882 + n_claims_year + avg_amount~ . ,
                           data = train) %>%
  step_filter(policy_exposure > 0, skip = TRUE) %>%
  step_impute_mode( all_nominal_predictors() ) %>%
  step_impute_median( all_numeric_predictors() )

#'[ ============================================================================================= ] 
#'[ DATA SPLITTING: The data is split chronologically, not randomly.                              
#'[ The yrs 2015-2017 will serve as the training/validation set & the yr 2018 as the test set.    
#'[ ============================================================================================= ]

dt <- data_wide %>% clean_data(.) %>% feature_eng(.) %>% fix_data_types(.) %>% filter(policy_exposure > 0)

train_test_split <- dt %>%
  mutate(policy_yr = as.Date(paste0(policy_year, "-01-01"))) %>%
  arrange(policy_yr) %>%
  sliding_period(index = policy_yr, period = "year", lookback = 2, assess_stop = 1, complete = TRUE)
train_test_split$splits[[1]]$data <- train_test_split$splits[[1]]$data %>%
  select(-policy_yr)

train_raw <- analysis(train_test_split$splits[[1]])

severity_resamples <- train_raw  %>%
  mutate(policy_yr = as.Date(paste0(policy_year, "-01-01"))) %>%
  arrange(policy_yr) %>%
  sliding_period(index = policy_yr, period = "year", lookback = Inf, assess_stop = 1, complete = TRUE)

severity_resamples$splits[[1]]$data <- severity_resamples$splits[[1]]$data %>% select(-policy_yr)

#'[ ================================================================================================]
#'[ Modelling                                                                                       
#'[ Use a negative binomial for frequency due to overdispersion                                     
#'[ portfolio has operational thresholds or flat-rate caps for severity data:                       
#'[ Classification: Train a Logistic Regression model to predict the prob that a claim will be = 882
#'[ Severity Regression: Train a standard severity regression model on rows where avg_amount != 882 
#'[ ================================================================================================]

#'*=================================================================================================*
#'*   Frequency: */*Negative Binomial*/
#'*=================================================================================================*

freq_preds <- c('id','type_risk', 'driver_age', 'licence_duration', 'power', 'value_vehicle', 'cylinder_capacity', 'length', 'weight', 'n_doors', 'policies_in_force', 'area', 'distribution_channel', 'policy_season', 'policy_month', 'policy_tenure', 'policy_year', 'policy_quarter', 'is_new_client', 'past_claims','log_exposure')

freq_train <- train_raw %>% group_by(id, policy_year) %>% slice(1) %>% ungroup()

freq_recipe <- preprocess_base %>%
  update_role(n_claims_year, new_role = 'outcome') %>% # declare the outcome variable
  step_rm(all_predictors(),-all_of(freq_preds)) %>% #rm all predictors, except ...
  #step_dummy(all_nominal_predictors()) %>% # for glmnet
  step_nzv(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr( all_numeric_predictors(), threshold = .7, skip = TRUE)

train_freq_baked <- bake(freq_recipe %>% prep, new_data = NULL)
final_freq_preds <- setdiff(c(freq_preds[c(-1,-21)], 'offset(log_exposure)'),
                            setdiff(freq_preds, names(train_freq_baked))) 
freq_form <- paste0("n_claims_year~",paste(final_freq_preds, collapse = "+")) %>% as.formula()
library(MASS)
nb_fit <- glm.nb(freq_form,data = train_freq_baked)
nb_summary <- tidy(nb_fit)

# test_set <- data_wide %>% filter(policy_year == "2018") %>%
#   mutate(policy_exposure = 1,log_exposure=log(policy_exposure)) 
# test_set <- bake(freq_recipe, new_data = test_set)
# 
# #test_set$nb.preds <- predict(nb_fit, newdata = test_set)
# test_set <- rename_with(test_set,~ paste0("freq", .x, recycle0 = TRUE), starts_with("."))
library(DHARMa)
#Dharma
nb_residuals <- simulateResiduals(nb_fit)
plot(nb_residuals) # the points sits cleanly on the 45degree line, the overall distr aligns when with the expected uniform distr. The failures of the KS is due to large data points.
# The dashed black lines are mostly flat and centered around 0.5. (no heavy curves, severe narrowing or widening - which would mean heteroscedasticity or missed non-linear relationships)
testDispersion(nb_residuals) # none, pvalue>5%
testZeroInflation(nb_residuals) # none, p-values>5%, and ratio >1. 
# vedict : while the dharma's zeroinfl test indicated significant zero infl (pvalue=0.008), the zero-inlf ratio was negligible(1.0075), indicating that the model's assumptions were met.
# model has statistically signif zero-inlf, but the pratical effect is zero. 

test_df <- assessment(train_test_split$splits[[1]]) %>% group_by(id, policy_year) %>% slice(1) %>% ungroup()
test <- bake(freq_recipe %>% prep, new_data = test_df)
test_df$pred_negbin = pmin(predict(nb_fit, newdata = test, type = "response"),25)

#'*=================================================================================================*
#'*   Severity: 2-stage */*Binomial GLM and Log-Normal GLM*/
#'*=================================================================================================*

predictors <- c('claim_type_count', 'value_vehicle', 'cylinder_capacity',
 'weight', 'type_fuel', 'driver_age', 'area', 'premium', 'power', 'n_doors',
 'length', 'past_claims', "claims_type") 
preprocess_sev <- preprocess_base %>%
  update_role(avg_amount, new_role = "outcome") %>%
  update_role(id, new_role = "id var") %>%
  step_rm(all_predictors(),-all_of(predictors)) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors(), freq_cut = 99/1) %>%
  step_corr(all_numeric_predictors(), threshold = 0.7) 

get_metrics = metric_set(mae ,rmse)

cv_results <- function(resample,recipe_blueprint){
   resample %>% mutate(
  # Fit and store the model for each fold
     model = map(.x = splits, .f = function(split){

   # A: extract training (analysis) and testing (assessment) data
        train_df_raw <- analysis(split)
        # PREP the recipe using ONLY this fold's raw training data
        trained_recipe <- prep(recipe_blueprint, training = train_df_raw)
        train_df_baked <- bake(trained_recipe, new_data = NULL)
        
        fold_threshold <- train_df_raw %>% 
          filter(amount > 0, policy_exposure > 0) %>% 
          summarise(p.975 = quantile(avg_amount, 0.975)) %>% 
          as.numeric()
        train_df <- train_df_raw %>% 
          filter(policy_exposure > 0, amount > 0, avg_amount <= fold_threshold)
        # Filter out the 882 spike to get a clean, smooth continuous distribution & remove very small claims cost (likely admin costs for processing claims or something)
        train_subset <- train_df %>% filter(avg_amount != 882 & avg_amount > 5)
        
        fold_main_predictors <- intersect(predictors, names(train_df_baked))
        # Dynamically construct formulas unique to this fold's surviving data structures
        fold_formula_stage1 <- fold_main_predictors %>% reformulate(response = 'is_882')
        fold_formula_stage2 <- fold_main_predictors %>% reformulate(response = 'log(avg_amount)')
        # B: fit/Model the Operational Spike (Logistic Regression) i.e Train a standard classifier using all your features
        model_stage1 <- glm(fold_formula_stage1,
                            data = train_df, family = binomial)
       
        # C: Model the Continuous Severity (Log-Linear Regression) i.e Train a standard linear model on the log scale (Standard Actuarial Severity GLM)
        model_stage2 <- glm(fold_formula_stage2, data = train_subset, family = gaussian)
        # D. mean baseline for comparisons
        mean_baseline <- mean(train_df$avg_amount)

         list(prob_model = model_stage1, cost_model = model_stage2, train_df = train_df,
              train_subset = train_subset, trained_recipe = trained_recipe, mean_baseline = mean_baseline)
     }),
        # D. MAke predictions 
    predict = map2(splits, model, function(split, fitted_model){
        test_df_raw <- assessment(split)
        test_df <- bake(fitted_model$trained_recipe, new_data = test_df_raw)
        prob_model = fitted_model$prob_model
        cost_model = fitted_model$cost_model
        mean_baseline = fitted_model$mean_baseline
        sigma_sq <- summary(cost_model)$dispersion
        bias_correction <- exp(0.5 * sigma_sq)
        test_df$prob_pred <- predict(prob_model, newdata = test_df, type = "response") # Probability that the claim hits the 882 spike
        pred_log <- predict(cost_model, newdata = test_df) #Predicted cost on the continuous scale 
        test_df$pred_cost <- exp(pred_log) * bias_correction #back-transformed with bias correction
        test_df$sev.pred <- (test_df$prob_pred * 882) + ((1 - test_df$prob_pred) * test_df$pred_cost)
        test_df$mean_baseline_pred <- mean_baseline
          list(test_df = test_df)
    })
  )
}

sev_results <- cv_results(resample = severity_resamples, recipe_blueprint = preprocess_sev)
model_train_df <- sev_results %>% pluck("model",1,"train_df")
library(DHARMa)
#
prob_model1 <- sev_results %>% pluck("model",1,"prob_model")
prob_model_summary1 <- prob_model1 %>% tidy %>% mutate(p.value = scales::pvalue(p.value, accuracy = 0.001),
                                                     across(where(is.numeric),~round(.,5)))
prob_model_aic1 <- summary(prob_model1)$aic
prob_model_sim1 <- simulateResiduals(prob_model1,2000)

prob_model2 <- sev_results %>% pluck("model",2,"prob_model")
prob_model_summary2 <- prob_model2 %>% tidy %>% mutate(p.value = scales::pvalue(p.value, accuracy = 0.001),
                                                     across(where(is.numeric),~round(.,5)))
prob_model_aic2 <- summary(prob_model2)$aic
prob_model_sim2 <- simulateResiduals(prob_model2,2000)
#
cost_model1 <- sev_results %>% pluck("model",1,"cost_model")
cost_model_summary1 <- cost_model1 %>% tidy %>% # low standard errors, especially for significant values, consider collapsing rare claims type
  mutate(p.value = scales::pvalue(p.value, accuracy = 0.05),
         across(where(is.numeric),~round(.,5))) 
cost_model_aic1 <- summary(cost_model1)$aic
cost_model_sim1 <- simulateResiduals(cost_model1, n = 3000)

cost_model2 <- sev_results %>% pluck("model",1,"cost_model")
cost_model_summary2 <- cost_model2 %>% tidy %>% 
  mutate(p.value = scales::pvalue(p.value, accuracy = 0.05),
         across(where(is.numeric),~round(.,5))) 
cost_model_aic2 <- summary(cost_model2)$aic
cost_model_sim2 <- simulateResiduals(cost_model2, n = 3000)

cv_test_df <- sev_results %>% pluck("predict",1,"test_df") %>% filter(avg_amount > 0)

metrics_results <- 
  bind_cols(cv_test_df %>% get_metrics(truth = avg_amount, estimate = sev.pred) %>% 
              rename('model.estimate' = .estimate),
            cv_test_df %>% get_metrics(truth = avg_amount, estimate = mean_baseline_pred) %>%
              select('baseline.estimate' = .estimate))
cv_test_df1 <- sev_results %>% pluck("predict",2,"test_df") %>% filter(avg_amount > 0)

metrics_results1 <- 
  bind_cols(cv_test_df1 %>% get_metrics(truth = avg_amount, estimate = sev.pred) %>% 
              rename('model.estimate' = .estimate),
            cv_test_df1 %>% get_metrics(truth = avg_amount, estimate = mean_baseline_pred) %>%
              select('baseline.estimate' = .estimate))

################# final model

final_results <- cv_results(resample = train_test_split, recipe_blueprint = preprocess_sev)
trained_recipe <- final_results %>% pluck("model",1,"trained_recipe")
final_model_train_df <- final_results %>% pluck("model",1,"train_df")
#
final_prob_model <- final_results %>% pluck("model",1,"prob_model")
final_prob_model_summary <- final_prob_model %>% tidy %>% 
  mutate(p.value = scales::pvalue(p.value, accuracy = 0.05),
         across(where(is.numeric),~round(.,5)))
final_prob_model_aic <- summary(final_prob_model)$aic
final_prob_model_sim <- simulateResiduals(final_prob_model,2000)
# final_prob_model_uniftest <- testUniformity(final_prob_model_sim)
# final_prov_model_outtest <- testOutliers(final_prob_model_sim)
#
final_cost_model <- final_results %>% pluck("model",1,"cost_model")
final_cost_model_summary <- final_cost_model %>% tidy %>% 
  mutate(p.value = scales::pvalue(p.value, accuracy = 0.05),
         across(where(is.numeric),~round(.,5)))
final_cost_model_aic <- summary(final_cost_model)$aic
final_cost_model_sim <- simulateResiduals(final_cost_model, n = 3000)
#final_cost_model_disptest <- testDispersion(final_cost_model_sim)
#plotResiduals(prob_model_sim, prob_model$model$claims_type)

final_test_df <- final_results %>% pluck("predict",1,"test_df") %>% filter(avg_amount > 0)
final_metrics_results <- 
  bind_cols(final_test_df %>% get_metrics(truth = avg_amount, estimate = sev.pred) %>% 
              rename('model.estimate' = .estimate),
            final_test_df %>% get_metrics(truth = avg_amount, estimate = mean_baseline_pred) %>%
              select('baseline.estimate' = .estimate))

# Lorenz Curve data
lorenz_chart_data <- final_test_df %>%
  arrange(sev.pred) %>%
  mutate(
    cum_prop_policies = row_number() / n(),
    cum_losses   = cumsum(avg_amount) / sum(avg_amount))

l0 <- ggplot(lorenz_chart_data, aes(x = cum_prop_policies)) +
  geom_line(aes(y = cum_losses, color = "Model Sort"), linewidth = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
  labs(title = "Out-of-Sample Lorenz Curve", x = "Cumulative Proportion of claims", y = "Cumulative Losses") +
  theme_minimal() +
  guides(color = "none") # successfully seperates low sev from high sev risks (the lowest 50% of claims account for only 30% of actual loses)

gini <- final_test_df %>% gini_coef(truth = avg_amount, estimate = sev.pred) # model successfully sorts risk throughout most of he pop

#'*=================================================================================================*
#'*   Premium calc: */ Multiply frequency and severity /*
#'*=================================================================================================*

baked_sev_test <- bake(trained_recipe, new_data = test_df)
sigma_sq   <- summary(final_cost_model)$dispersion
bias_correction <- exp(0.5 * sigma_sq)

# Calculate conditional severity components
test_df$prob_pred <- predict(final_prob_model, newdata = baked_sev_test, type = "response")
pred_log                 <- predict(final_cost_model, newdata = baked_sev_test)
test_df$pred_cost <- exp(pred_log) * bias_correction
test_df <- test_df %>% mutate(pred_sev = (prob_pred * 882) + (1 - prob_pred)*pred_cost,
                        pure_premium = pred_sev * pred_negbin)

# Define a baseline: Average total loss per policy in the training set
test_df$baseline_pure_premium <- mean(final_model_train_df$cost_claims_year, na.rm = TRUE)

# 1. Calculate Portfolio MAE and RMSE
pure_premium_metrics <- bind_cols(
  test_df %>% 
    get_metrics(truth = cost_claims_year, estimate = pure_premium) %>% 
    rename('model.estimate' = .estimate),
  test_df %>% 
    get_metrics(truth = cost_claims_year, estimate = baseline_pure_premium) %>%
    select('baseline.estimate' = .estimate)
)

# Calculate portfolio Lorenz chart data
portfolio_lorenz_data <- test_df %>%
  arrange(pure_premium) %>%
  mutate(
    cum_prop_policies = row_number() / n(),
    cum_losses        = cumsum(cost_claims_year) / sum(cost_claims_year)
  )

# Plotting the combined performance
l1 <- ggplot(portfolio_lorenz_data, aes(x = cum_prop_policies, y = cum_losses)) +
  geom_line(color = "dodgerblue4", linewidth = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
  labs(
    title = "Portfolio-Wide Pure Premium Lorenz Curve",
    x = "Cumulative Proportion of Total Portfolio Policies",
    y = "Cumulative Proportion of Total Actual Losses"
  ) +
  theme_minimal()

# Calculate the overall Portfolio Gini Coefficient
portfolio_gini <- test_df %>% 
  gini_coef(truth = cost_claims_year, estimate = pure_premium) %>% mutate(.estimate = round(.estimate,3))

