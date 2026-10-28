library(tidyverse)
library(tidymodels)
tidymodels_prefer()

data <- read_delim("Motor vehicle insurance data.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE) %>%
  janitor::clean_names()
claims <- read_delim("sample type claim.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE) %>%
  janitor::clean_names()

#'[ ============================================================================================= ] 
#'[ STRUCTURING DATA(short format): some policies have multiple claims per year, sum & count by...
#'[ ID&Claims_type. Since claims data is incomplete, some policies have claims but no claims info 
#'[ ============================================================================================= ]

claims_policy_level <- claims %>%
  group_by(id , cost_claims_year) %>%
  summarise(claim_count = n(),
            .groups='drop'
          ) #claim_count == n_claims_year
claims_claim_level <- claims %>%
  group_by(id , claims_type , cost_claims_year) %>%
  summarise(claim_type_count = n(), 
            amount = sum(cost_claims_by_type , na.rm = TRUE),
            .groups="drop"
          )
claims_wide <- claims_policy_level %>% 
  left_join(claims_claim_level , by=c('id' , 'cost_claims_year')) %>%
  relocate(claims_type , .after = id)
claims_type <- unique(claims$claims_type)
data_wide <- data %>%
  left_join(claims_claim_level , by=c("id" , "cost_claims_year") , relationship = "many-to-many") %>%
  mutate(claim_type_count = coalesce(claim_type_count , 0L),
         amount = coalesce(amount , 0L))

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

skim_numeric_summary <- data_wide %>% select(where(is.numeric)) %>% 
  my_skim %>% mutate(across(where(is.numeric), ~round(.,4))) %>%
  yank("numeric")
skim_character_summary <-skim(data_wide) %>% yank("character")
#'[ ============================================================================================= ] 
#'[ FEATURE ENGINEERING: Will construct an age and exposure variable                              
#'[ Age= y(current_year 'as at start of year')-y(date_birth) and exposure=DateA-DateB             
#'[ DateA=pmax(start_contract,policy_year_start,last_renewal)                                     
#'[ DateB=pmin(date_lapse,next_renewal,policy_year_end)                                                      
#'[ There are people who got insurance b4 obtaining a drivers licence & others with start date > last renewal
#'[ ============================================================================================= ]

data_wide <- data_wide %>% mutate(across(starts_with("date_") , dmy))
data_wide <- data_wide %>%
  mutate(driver_age = (year(date_last_renewal) - year(date_birth)),
         policy_year = year(date_last_renewal) ,
         policy_quarter = paste0("Q",quarter(date_last_renewal)) ,
         policy_month = month(date_last_renewal, label = TRUE, abbr = TRUE) ,
         date_month = month(date_last_renewal),
         policy_season = case_when(date_month<=8 & date_month>=6 ~ "summer" ,
                                   date_month<=5 & date_month>=3 ~ "spring" ,
                                   date_month<=2 | date_month>=12 ~ "winter" , 
                                   date_month<=11 & date_month>=9 ~ "autumn") ,
         licence_duration = pmax(0,round(difftime(date_last_renewal , date_driving_licence , units = "days") / 365 , 2)
         ) %>% as.numeric ,
         
         date_entry = pmax(date_start_contract , date_last_renewal , as.Date("2015-11-01") , na.rm = TRUE) , #the data ranges from 1nov2015 to dec2018.
         date_exit = pmin(date_next_renewal , date_lapse , as.Date("2018-12-31") , na.rm = TRUE) , #thus start and end date of investigation is 2015-11-01 and 2018-12-31, resp.
         policy_exposure = pmax(round(difftime(date_exit,date_entry,units="days")/365,3)%>%as.numeric,0),
         policy_tenure = pmax(round(difftime(date_next_renewal,date_start_contract,units = "days")/365,0) %>%
                                        as.numeric, 0)
  ) %>% select(-starts_with("date"))

# There are 7 policies with zero exposure but claim amount and count...ids=10387,11736,20977,24619,26140,32165,49091.

data_wide <- data_wide %>% 
  mutate(policy_tenure = case_when(policy_exposure==0 ~ dplyr::lag(policy_tenure),
                                   TRUE ~ policy_tenure)) %>%
  group_by(id) %>%
  arrange(policy_year) %>%
  mutate(
    is_new_client = if_else(policy_tenure == 0,1,0),
    past_claims = dplyr::lag(cumsum(n_claims_year),default = 0))%>%
  arrange(id)%>%
  ungroup()

#'[ ============================================================================================= ] 
#'[ DATA CLEANING: Some columns have NA's                                                         
#'[ Date_lapse-replace by date_next_renewal (policies didn't/hasn't lapsed, will renew year end)  
#'[ claims_type by None to indicate no claim or anonymized for claims with incomplete info&Count>=1  
#'[ 2 separate claims are filed, and 1 has R0, i.e was probably rejected,bcos customer filed a... 
#'[ ...a claim that wasn't actually covered by their policy contract(e.g claiming 4 a complaint...
#'[ ...under a basic travel assistance policy) hence claim's declined,Claim was below excess...   
#'[ .../deductible hence no payout,Fraud (e.g breach of terms like unlisted driver operating ...  
#'[ ...vehicle at time of incident), etc                                                          
#'[ ============================================================================================= ]

#'*check whether all claims have associated claims_type*
# data_wide %>% select(n_claims_year, cost_claims_year, claims_type) %>%
#   filter(cost_claims_year>0 & n_claims_year>0) 

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

check1<-data_wide %>% select(id,n_claims_year,cost_claims_year,claims_type,
                             claim_type_count,amount,avg_amount,policy_year, past_claims,is_new_client) %>%
  filter(amount>0)

character_vars <- c("type_risk","type_fuel","claims_type","policy_quarter",
                    "policy_season","area","payment","second_driver","is_new_client")

# a var level/category needs a bare minimum of 30 rows per year to be statistically useful.
# levels_n <- data_wide %>% #if n is large enough for training is it okay for it to be small (<30) for testing?
#   mutate(across(all_of(character_vars), as.character)) %>%
#   pivot_longer(cols = all_of(character_vars),names_to = "variable",values_to = "level") %>%
#   count(variable,level,policy_year) %>% 
#   filter(n<30) #yrs 2015&2016 must not drop below 30. i.e training must not be below 30
#_____________________________________________________________
# every factor must have more than 200 levels (naive approach) i.e approx 50 levels per year
data_wide <- data_wide %>%
  mutate( 
    # claims_type = case_when(claims_type %in% c("theft","injuries","other","fire") ~ "Rare",
    #                         TRUE ~ claims_type),
    claims_type=case_when(claims_type=="fire" ~ "other", TRUE ~ claims_type)
    )
data_wide <- data_wide %>% 
  mutate( across(all_of(character_vars) , as_factor) )

#'[ ============================================================================================= ] 
#'[ DATA SPLITTING: The data is split chronologically, not randomly.                              
#'[ The yrs 2015-2017 will serve as the training/validation set & the yr 2018 as the test set.    
#'[ ============================================================================================= ]

data_wide %>% filter(policy_exposure<0) %>% nrow # check policies with zero exposure

#'* policy_level_data (one row per policy): The info excluded in data_wide_freq is unknown before a claim occurs*

data_wide_freq <- data_wide %>%
  filter(policy_exposure > 0) %>%
  mutate(log_exposure = log(policy_exposure)) %>%
  select(!c(claims_type, amount, avg_amount, cost_claims_year,claim_type_count)) %>% 
  group_by(id, policy_year) %>%
  slice(1)%>%  #need one row per policy/ID i.e IDs are not repeated each year
  ungroup() 
 
#check each policy ID exists once or less per year
check3 <- data_wide_freq %>% select(id,policy_year) %>% 
  group_by(id)%>% mutate(n_id = n()) %>% filter(n_id >4) # checks 1 row per policy per year

train_freq <- data_wide_freq %>% 
  filter(policy_year != "2018") 
folds_freq <- train_freq %>%
  mutate(policy_yr = as.Date(paste0(policy_year, "-01-01"))) %>%
  arrange(policy_yr) %>%
  sliding_period(index = policy_yr, period = "year", lookback = 1,assess_stop = 1,complete = TRUE)
folds_freq
#remove the newly created policy_yr date column
folds_freq$splits[[1]]$data <- folds_freq$splits[[1]]$data %>% select(-policy_yr)

#'*Severity: we are concerned with knowing that once a claim happens, how much is it.*
#'* claim_level_data (one row per claim): Include only policies where amount>0 bcos we're only concerned with policies that have claims*
#'* diff structure than freq, one row per claim i.e IDs can be repeated*
# need to filter/cap the large claims/outliers. (also <5 costs are likely admin costs for processing claims)

data_wide_sev <- data_wide %>% filter(amount>0, policy_exposure>0) 
threshold <- quantile(data_wide_sev$avg_amount, 0.975)
large_claims <- data_wide_sev %>% filter(avg_amount > threshold)
data_wide_sev <- data_wide_sev %>% filter(avg_amount <= threshold) %>%
  mutate(is_882 = if_else(avg_amount == 882, 1, 0))

data_wide_sev %>% filter(avg_amount<=1000 & avg_amount>=800) %>% 
  select(policy_year, claims_type) %>% group_by(claims_type,) %>% summarise(n=n())

train_test_split <- data_wide_sev %>%
  mutate(policy_yr = as.Date(paste0(policy_year, "-01-01"))) %>%
  arrange(policy_yr) %>%
  sliding_period(index = policy_yr, period = "year", lookback = 2,assess_stop = 1,complete = TRUE)
train_test_split$splits[[1]]$data <- train_test_split$splits[[1]]$data %>% select(-policy_yr)

train_sev <- analysis(train_test_split$splits[[1]]) 
test <- assessment(train_test_split$splits[[1]]) %>%
  mutate(policy_exposure = 1)

preprocess <- recipe(n_claims_year + is_882 + avg_amount ~ . , data = train_sev) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.7) %>%
  step_nzv(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  #step_novel(claims_type, new_level = "new") %>%
  prep()
train_sev <- bake(preprocess, new_data = NULL)
test <- bake(preprocess, new_data = test)

severity_resamples <- train_sev %>%
  mutate(policy_yr = as.Date(paste0(policy_year, "-01-01"))) %>%
  arrange(policy_yr) %>%
  sliding_period(index = policy_yr, period = "year", lookback = Inf,assess_stop = 1,complete = TRUE)
severity_resamples$splits[[1]]$data <- severity_resamples$splits[[1]]$data %>% select(-policy_yr)

#'[ =============================================================================================== ]
#'[ Exploratory Data Analysis (EDA)                                                                 
#'[ Firstly, we do target variable EDA to understand their distribution                             
#'[ we performs univariate EDA (numeric, categorical), bivariate EDA (numerical & categorical)      
#'[ =============================================================================================== ]
source("tidy-eda-toolkit.R")

#'[ ================================================================================================]
#'[ Target variables EDA: n_claims_year and avg_amount                                              
#'[ ================================================================================================]
target_plot1 <- train_freq %>% 
  filter(n_claims_year>0 & n_claims_year < 10) %>%
  plot_categorical(. , n_claims_year, fill_var = policy_year,
                   title = "Distribution of number of claims from 2015 to 2017",
                   legend_title = "Policy Year")

target_plot2 <- train_freq %>% 
  group_by(policy_year, policy_month) %>%
  summarise(`total claim count` = sum(n_claims_year, na.rm = TRUE),
            .groups="drop") %>%
  plot_bivariate(. , policy_month, `total claim count`, color_var = policy_year,
                 xlab = "policy start month", legend_title = "policy start year",
                 title = "Total number of claims for each month from 2015 to 2017")+
  geom_line(aes(x= policy_month, y = `total claim count`, group = policy_year,
                colour = factor(policy_year)))

target_plot3 <- train_sev %>%  #bimodal
  filter(avg_amount > 0 & avg_amount <1500) %>%
  plot_numerical(. , avg_amount, type="both", bins = 35,
                 title = "Distribution of the average claim amount in € for claims < €1,500",
                 subtitle = "excluding policies with €0 claim amounts",
                 ylab = "density", xlab = "average claim amount (€)",scico_palette = "oslo")

target_plot4 <- train_sev %>%  #bimodal
  filter(avg_amount > 0 & avg_amount <1500, avg_amount!=882) %>%
  plot_numerical(. , avg_amount, type="both", bins = 35,
                 title = "Distribution of the average claim amount in € for claims < €1,500",
                 subtitle = "excluding policies with €0 and €882 claim amounts",
                 ylab = "density", xlab = "average claim amount (€)",scico_palette = "oslo")

target_plot5 <- train_sev %>% filter(policy_year == 2017, premium <1000) %>%
  plot_numerical(. , premium, type="both", bins = 50, title = "Distribution of premiums for 2017",
                 subtitle = "The distribution for past years is similar to that of 2017")


#'[ ================================================================================================]
#'[ univariate EDA: categorical variable                                                            
#'[ ================================================================================================]
area_labels <- c('0' = 'rural', '1' = 'urban')
fuel_labels <- c('P' = 'petrol', 'D' = 'Diesel')
payment_labels <- c('0' = 'annual', '1' = 'bi-annual')
distr_channel_labs <- c('0' = 'Agent', '1' = 'Insurance broker')
risk_labels <- c('1' = 'motorbikes', '2' = 'vans', '3' = 'passenger cars', '4' = 'agricultural vehicles')
driver_labels <- c('1' = 'yes', '0' = 'no')

cat_plot1 <- train_sev %>%
  select(area, claims_type,n_claims_year,id) %>% distinct(.) %>%
  plot_boxplot(. , claims_type, n_claims_year, ylab = 'number of claims year', fill_var = area,
               fill_labels = area_labels, xlab = "claims type")

cat_plot2 <- train_freq %>% 
  select(id,type_fuel) %>% distinct %>%
  filter(!is.na(type_fuel)) %>%
  plot_categorical(. , type_fuel, fill_var = type_fuel, fill_labels = fuel_labels, legend_title = "Fuel type",
                   title = "Distribution of fuel types")
cat_plot3 <- train_sev %>%
  select(area, payment,id) %>% distinct(.) %>%
  plot_categorical(., payment, fill_var = area, fill_labels = area_labels, title = "Distribution of policyholders", 
                   subtitle =  "by geographical area and payment timing")+
  scale_x_discrete(labels=payment_labels)

cat_plot4 <- train_sev %>%
  select(area, payment, distribution_channel,id) %>% distinct(.) %>%
  plot_categorical(.,distribution_channel,fill_var = area,fill_labels = area_labels,
                   facet_var = payment,facet_labels = payment_labels, 
                   title = "Count of policyholders by distribution channel for each each payment timing and area")+
  scale_x_discrete(labels=distr_channel_labs)

cat_plot5 <- train_sev %>%
  select(second_driver, type_risk,id) %>% distinct(.) %>%
  plot_categorical(.,type_risk,fill_var = second_driver,fill_labels = driver_labels,
                   title = "Count of policyholders for each insured risk type",
                   subtitle = "grouped by whethere a secondary driver exist or not")+
  scale_x_discrete(labels = risk_labels)

#'[ ================================================================================================]
#'[ Bivariate EDA: numerical and categorical variables                                              
#'[ ================================================================================================]

plot1 <- train_sev %>% 
  plot_boxplot(., claims_type, premium,fill_var = claims_type, xlab = "claims type", ylab = "averge claim amount", scico_palette = "lisbon")

plot2 <- train_sev %>% 
  plot_boxplot(., claims_type, avg_amount, fill_var = type_risk, fill_labels = risk_labels,
               xlab = "claims type", ylab = "averge claim amount", scico_palette = "lisbon")

plot3 <- data_wide %>% filter(policy_year != 2018) %>% group_by(id, policy_year) %>% slice(1) %>%
  plot_bivariate(., power, value_vehicle, color_var = type_risk,
                 facet_var = type_risk, color_labels = risk_labels,
                 subtitle = "vehicle power vs vehicle value for each type of risk for the year 2017")

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

freq_preds <- c('type_risk', 'driver_age', 'licence_duration', 'power', 'value_vehicle', 'cylinder_capacity', 'length', 'weight', 'n_doors', 'policies_in_force', 'area', 'distribution_channel', 'policy_season', 'policy_month', 'policy_tenure', 'policy_year', 'policy_quarter', 'is_new_client', 'past_claims','log_exposure')

freq_recipe <- recipe(freq_preds %>% reformulate("n_claims_year"), data = train_freq) %>%
  step_nzv(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr( all_numeric_predictors(), threshold = .7) %>% prep

train_freq_baked <- bake(freq_recipe, new_data = NULL)
final_freq_preds <- setdiff(c(freq_preds[-20], 'offset(log_exposure)'),
                            setdiff(freq_preds, names(train_freq_baked))) 
freq_form <-paste0("n_claims_year~",paste(final_freq_preds, collapse = "+")) %>% as.formula()
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
#'*=================================================================================================*
#'*   Severity: 2-stage */*Binomial GLM and Log-Normal GLM*/
#'*=================================================================================================*

predictors <- c('claim_type_count', 'value_vehicle', 'cylinder_capacity',
 'weight', 'type_fuel', 'driver_age', 'area', 'premium', 'power', 'n_doors',
 'length', 'past_claims', "claims_type") 
main_predictors <- setdiff(predictors,setdiff(predictors, names(train_sev)))

formula <- main_predictors %>% reformulate(response = 'log(avg_amount)')

metric = metric_set(mae ,rmse)
cv_results <- function(resample){
   resample %>% mutate(
  # Fit and store the model for each fold
     model = map(.x = splits, .f = function(split){

   # A: extract training (analysis) and testing (assessment) data
        train_df <- analysis(split)
        #pmin<- min(train_df$avg_amount)-1e-5
        # Filter out the 882 spike to get a clean, smooth continuous distribution
        # remove very small claims cost (likely admin costs for processing claims or something)
        train_subset <- train_df %>% filter(avg_amount != 882 & avg_amount > 5)
        # B: fit/Model the Operational Spike (Logistic Regression) i.e Train a standard classifier using all your features
        model_stage1 <- glm(main_predictors %>% reformulate(response = 'is_882'), data = train_df, family = binomial)
        #summary(model_stage1)
       
        # C: Model the Continuous Severity (Log-Linear Regression) i.e Train a standard linear model on the log scale (Standard Actuarial Severity GLM)
        model_stage2 <- glm(formula, data = train_subset, family = gaussian)
        # D. mean baseline for comparisons
        mean_baseline <- mean(train_df$avg_amount)

         list(prob_model = model_stage1, cost_model = model_stage2, train_df = train_df, train_subset = train_subset, mean_baseline = mean_baseline)
     }),
        # D. MAke predictions 
    predict = map2(splits, model, function(split, fitted_model){
        test_df <- assessment(split)
        #test_df <- bake(preprocess, new_data = test_df)
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

sev_results <- cv_results(resample = severity_resamples)
model_train_df <- sev_results %>% pluck("model",1,"train_df")
library(DHARMa)
#
prob_model <- sev_results %>% pluck("model",1,"prob_model")
prob_model_summary <- prob_model %>% tidy %>% mutate(p.value = scales::pvalue(p.value, accuracy = 0.001),
                                                     across(where(is.numeric),~round(.,5)))
prob_model_aic <- summary(prob_model)$aic
prob_model_sim <- simulateResiduals(prob_model,2000)
#
cost_model <- sev_results %>% pluck("model",1,"cost_model")
cost_model_summary <- cost_model %>% tidy %>% 
  mutate(p.value = scales::pvalue(p.value, accuracy = 0.05),
         across(where(is.numeric),~round(.,5))) 
cost_model_aic <- summary(cost_model)$aic
cost_model_sim <- simulateResiduals(cost_model, n = 3000)

test_df <- sev_results %>% pluck("predict",1,"test_df")
# remove very small claims cost (likely admin costs for processing claims or something)

################# final model
final_results <- cv_results(resample = train_test_split)
final_model_train_df <- final_results %>% pluck("model",1,"train_df")
#
final_prob_model <- final_results %>% pluck("model",1,"prob_model")
final_prob_model_summary <- final_prob_model %>% tidy %>% 
  mutate(p.value = scales::pvalue(p.value, accuracy = 0.05),
         across(where(is.numeric),~round(.,5)))
final_prob_model_aic <- summary(final_prob_model)$aic
final_prob_model_sim <- simulateResiduals(final_prob_model,2000)
#
final_cost_model <- final_results %>% pluck("model",1,"cost_model")
final_cost_model_summary <- final_cost_model %>% tidy %>% 
  mutate(p.value = scales::pvalue(p.value, accuracy = 0.05),
         across(where(is.numeric),~round(.,5)))
final_cost_model_aic <- summary(final_cost_model)$aic
final_cost_model_sim <- simulateResiduals(final_cost_model, n = 3000)

final_cost_model_diag_plot <- plot(final_cost_model_sim) # check ci bands around lines.
final_cost_model_disptest <- testDispersion(final_cost_model_sim)


final_prob_model_diag_plot <- plot(final_prob_model_sim)
final_prob_model_uniftest <- testUniformity(final_prob_model_sim)
final_prov_model_outtest <- testOutliers(final_prob_model_sim)
#plotResiduals(prob_model_sim, prob_model$model$claims_type)

final_test_df <- final_results %>% pluck("predict",1,"test_df")
final_metrics_results <- 
  bind_cols(final_test_df %>% metric(truth = avg_amount, estimate = sev.pred) %>% 
              rename('model.estimate'=.estimate),
            final_test_df %>% metric(truth = avg_amount, estimate = mean_baseline_pred) %>%
              select('baseline.estimate'=.estimate))

# Lorenz Curve data
lorenz_chart_data <- final_test_df %>%
  arrange(sev.pred) %>%
  mutate(
    cum_policies = row_number() / n(),
    cum_losses   = cumsum(avg_amount) / sum(avg_amount))

