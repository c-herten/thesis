
# packages
source("./install-packages.R")

method_packages <- c("dplyr", "tidyr", "ggplot2",  
                   "fixest", "sandwich", "lmtest", "Synth")

method_git_packages <- c("synth-inference/synthdid", "skranz/xsynthdid")

package_load(cran_packages = method_packages, github_packages = method_git_packages)


# data
data <- read.csv(file.path("data", "final_data.csv"), check.names = FALSE)

data <- data %>% 
  select(-c(ner_upsec_total, ner_upsec_female, ner_upsec_male)) %>% # too many NAs for critical years
  filter(year > 1999 & year < 2022) %>% # set to ranges for which all variables available 
  mutate(across(starts_with("comp"), function(x) x * 100)) # completion rates to percentages for consistency among all columns

# checking multicolinearity among covariates
cor(data$unemp_total, data$gdp_pc)
cor.test(data$unemp_total, data$gdp_pc)


####### ESL rates (2000 to 2021) #######

# --- Visualizations ----

country_colors <- c(
  "Portugal" = "firebrick",    
  "Spain" = "#1F78B4",       
  "Italy" = "#33A02C",       
  "Greece" = "#FF7F00",      
  "Austria" = "#6A3D9A",     
  "France" = "#FB9A99",      
  "Germany" = "#A6CEE3",     
  "Hungary" = "#B2DF8A",     
  "Lithuania" = "#FDBF6F",   
  "Poland" = "#CAB2D6",      
  "Romania" = "#FFFF99",     
  "Serbia" = "#B15928",      
  "Slovenia" = "#999999",    
  "Estonia" = "#66C2A5",     
  "Bulgaria" = "#FC8D62"     
)

# plot 1: Portugal + Spain, Italy, Greece, Austria, France
ggplot(data %>% filter(country %in% c("Portugal", "Spain", "Italy", "Greece", "Austria", "France")),
                aes(x = year, y = log(ESL_total / (100 - ESL_total)), color = country)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  scale_color_manual(values = country_colors) +  
  theme_minimal() +
  labs(title = "Variable Over Time - Group 1",
       x = "Year", y = "variable", color = "Country")

# plot 2: Portugal + Germany, Hungary, Lithuania, Poland, Romania
ggplot(data %>% filter(country %in% c("Portugal", "Germany", "Hungary", "Lithuania", "Poland", "Romania")),
                aes(x = year, y = log(ESL_total / (100 - ESL_total)), color = country)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  scale_color_manual(values = country_colors) +  
  theme_minimal() +
  labs(title = "Variable Over Time - Group 2",
       x = "Year", y = "variable", color = "Country")

# plot 3: Portugal + Serbia, Slovenia, Estonia, Bulgaria
ggplot(data %>% filter(country %in% c("Portugal", "Serbia", "Slovenia", "Estonia", "Bulgaria")),
                aes(x = year, y = log(ESL_total / (100 - ESL_total)), color = country)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  scale_color_manual(values = country_colors) + 
  theme_minimal() +
  labs(title = "Variable Over Time - Group 3",
       x = "Year", y = "variable", color = "Country")


# COMMEnT: Lithuania or Greece as potential control for ESL (logit transformed)

# transform data
logit_transform <- function(x) {
  log(x / (100 - x))
}

logit_esl_subset <- data %>%
  filter(country %in% c("Portugal", "Greece", "Lithuania")) %>%
  select(country, year, ESL_total) %>%
  mutate(log_esl = logit_transform(ESL_total)) %>%
  select(-ESL_total)

# visualization for PRT, GRC and LITH (logit transformed)
ggplot(logit_esl_subset, aes(x = year, y = log_esl, color = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Transformed ESL Rate Over Time",
    y = "log(ESL / (100 - ESL))",
    x = "Year"
  ) +
  scale_color_brewer(palette = "Set1") +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50")

# --- Difference-in-Differences ------------------------------------------------

# --- Testing Parallel Trends ----

# extend dataset for covariates
esl_data_exo <- logit_esl_subset %>%
  filter(country %in% c("Portugal", "Greece", "Lithuania")) %>%
  mutate(treated = ifelse(country == "Portugal", 1, 0)) %>%
  left_join(data %>% select(country, year, gdp_pc, gov_exp_edu, unemp_total), 
            by = c("country", "year"))

pre_treatment_esl <- esl_data_exo %>% 
  filter(year < 2015) 


# --- Parallel trends test for Portugal vs Greece ----
pt_gr_data_esl_pre <- pre_treatment_esl %>%
  filter(country %in% c("Portugal", "Greece")) %>% 
  filter(year >= 2009) %>%
  mutate(time_trend = year - 2014) # last pre-treatment period 

pt_gr_model_esl_pre <- feols(log_esl ~ treated:time_trend + unemp_total + gdp_pc
                             | year+country, # year- and country-fixed effects
                             se = "standard", panel.id=~year+country, 
                             # panel.id needed to be able to apply Newey-West std.err.
                         data = pt_gr_data_esl_pre)

coeftest(pt_gr_model_esl_pre, vcov = vcov_NW(pt_gr_model_esl_pre, lag=1))

# --- Comments ----

# time period starting from 2009
# for PRT vs Grc parallel trends can also not be rejected for 2008 start
# but this is not the case for later DiD w/o year-fixed effects
# for consistency therefore same timeframe

# COMMENT: later Did models were also run for Grc with data from 2008 
# but results did not change

# if including only unemp_total as control, interaction insignificant
# interaction also insignificant when including both but only marginally at alpha 5%

# --- Parallel trends test for Portugal vs Lithuania ----
pt_lt_data_esl_pre <- pre_treatment_esl %>%
  filter(country %in% c("Portugal", "Lithuania")) %>% 
  filter(year >= 2009) %>% # same time period as for PRT vs GRC
  mutate(time_trend = year - 2014)

pt_lt_model_esl_pre <- feols(log_esl ~ treated:time_trend + unemp_total + gdp_pc
                             | year+country, 
                             se = "standard", panel.id=~year+country,
                         data = pt_lt_data_esl_pre)

coeftest(pt_lt_model_esl_pre, vcov = vcov_NW(pt_lt_model_esl_pre, lag=1))

# --- Comments ----
# when including both covariates or only gdp_pc, interaction insignificant 


# --- Difference-in-Differences with year-fixed effects ------------------------

did_data_esl <- esl_data_exo %>%
  mutate(post = ifelse(year >= 2015, 1, 0))

# --- Portugal vs Greece ----

pt_gr_data_esl <- did_data_esl %>%
  filter(country %in% c("Portugal", "Greece")) %>% 
  filter(year >= 2009) %>%
  mutate(time_trend = year - 2014)


# COMMENT: in the following DiD using different combinations of covariates 
# for all combinations parallel trends remain justified 

# --- Unemployment ----

pt_gr_model_esl_unemp <- feols(log_esl ~ treated:post + unemp_total | year+country, 
                               se = "standard", panel.id=~year+country,
                               data = pt_gr_data_esl)

coeftest(pt_gr_model_esl_unemp, vcov = vcov_NW(pt_gr_model_esl_unemp, lag=1))

# --- GDP + Unemployment ----

pt_gr_model_esl_gdp_unemp <- feols(log_esl ~ treated:post + unemp_total + gdp_pc | year+country, 
                                   se = "standard", panel.id=~year+country,
                                   data = pt_gr_data_esl)

coeftest(pt_gr_model_esl_gdp_unemp, vcov = vcov_NW(pt_gr_model_esl_gdp_unemp, lag=1))

# --- Interpretation -----------------------------------------------------------
# coefficient on interaction insignificant across specs with Newey-West std.err.



# --- Portugal vs Lithuania ----
pt_lt_data_esl <- did_data_esl %>%
  filter(country %in% c("Portugal", "Lithuania")) %>% 
  filter(year >= 2009) %>%
  mutate(time_trend = year - 2014)

# --- GDP ----
pt_lt_model_esl_gdp <- feols(log_esl ~ treated:post + gdp_pc 
                                   | year+country, 
                                   se = "standard", panel.id=~year+country, 
                                   data = pt_lt_data_esl)

coeftest(pt_lt_model_esl_gdp, vcov = vcov_NW(pt_lt_model_esl_gdp, lag=1))


# --- GDP + Unemployment ----
pt_lt_model_esl_gdp_unemp <- feols(log_esl ~ treated:post + unemp_total + gdp_pc 
                                   | year+country, 
                                   se = "standard", panel.id=~year+country, 
                                   data = pt_lt_data_esl)

coeftest(pt_lt_model_esl_gdp_unemp, vcov = vcov_NW(pt_lt_model_esl_gdp_unemp, lag=1))


# --- Interpretation -----------------------------------------------------------
# coefficient on interaction term insignificant for both models

# --- Difference-in-Differences without year-fixed effects ---------------------

# --- Testing Parallel Trends ----
# --- Parallel trends test for Portugal vs Greece ----

pt_gr_data_esl_pre_lin <- pre_treatment_esl %>%
  filter(country %in% c("Portugal", "Greece")) %>% 
  filter(year >= 2009) %>%
  mutate(time_trend = year - 2014)

pt_gr_model_esl_pre_lin <- feols(log_esl ~ treated:time_trend + time_trend 
                                 + gdp_pc + unemp_total | country, 
                                 se = "standard", panel.id=~year+country,
                             data = pt_gr_data_esl_pre_lin)

coeftest(pt_gr_model_esl_pre_lin, vcov = vcov_NW(pt_gr_model_esl_pre_lin, lag=1))


# --- Comments ----
# for time starting 2009 parallel trends hold 
# when controlling for both covariates, either one of them 


# --- Parallel trends test for Portugal vs Lithuania ----

pt_lt_data_esl_pre_lin <- pre_treatment_esl %>%
  filter(country %in% c("Portugal", "Lithuania")) %>% 
  filter(year >= 2009) %>%
  mutate(time_trend = year - 2014)

pt_lt_model_esl_pre_lin <- feols(log_esl ~ treated:time_trend + time_trend 
                                 + unemp_total + gdp_pc | country, 
                                 se = "standard", panel.id=~year+country,
                                 data = pt_lt_data_esl_pre_lin)

coeftest(pt_lt_model_esl_pre_lin, vcov = vcov_NW(pt_lt_model_esl_pre_lin, lag=1))


# --- Comments ----
# for Lithuania across all combinations of covariates 
# no evidence against parallel pre-trends
# although when controlling only for unemp_total 
# only marginally insignificant at alpha 5%


# --- Portugal vs Greece ----

pt_gr_data_esl_lin <- did_data_esl %>%
  filter(country %in% c("Portugal", "Greece")) %>% 
  filter(year >= 2009) %>%
  mutate(time_trend = year - 2014)

# --- GDP ----

pt_gr_model_esl_gdp_lin <- feols(log_esl ~ treated:post + time_trend + gdp_pc 
                                 | country, 
                                 se = "standard", 
                                 panel.id=~year+country,
                             data = pt_gr_data_esl_lin)

coeftest(pt_gr_model_esl_gdp_lin, vcov = vcov_NW(pt_gr_model_esl_gdp_lin, lag=1))

# --- Unemployment ----

pt_gr_model_esl_unemp_lin <- feols(log_esl ~ treated:post + time_trend 
                                   + unemp_total | country,
                                   se = "standard", 
                                   panel.id = ~year+country,
                               data = pt_gr_data_esl_lin)

coeftest(pt_gr_model_esl_unemp_lin, vcov = vcov_NW(pt_gr_model_esl_unemp_lin, lag=1))

# --- GDP + Unemployment ----
  
pt_gr_model_esl_gdp_unemp_lin <- feols(log_esl ~ treated:post + time_trend 
                                       + unemp_total + gdp_pc | country,
                                     se = "standard", 
                                     panel.id = ~year+country,
                                     data = pt_gr_data_esl_lin)

coeftest(pt_gr_model_esl_gdp_unemp_lin, vcov = vcov_NW(pt_gr_model_esl_gdp_unemp_lin, lag=1))

# --- Comments ----

# no significant treatment effect on log ESL rates across all specs

# --- Portugal vs Lithuania ----

pt_lt_data_esl_lin <- did_data_esl %>%
  filter(country %in% c("Portugal", "Lithuania")) %>% 
  filter(year >= 2009) %>%
  mutate(time_trend = year - 2014)



# --- GDP ----

pt_lt_model_esl_gdp_lin <- feols(log_esl ~ treated:post + time_trend 
                                 + gdp_pc | country, 
                                 se = "standard", 
                                 panel.id = ~year+country, 
                                 data = pt_lt_data_esl_lin)

coeftest(pt_lt_model_esl_gdp_lin, vcov = vcov_NW(pt_lt_model_esl_gdp_lin, lag=1))

# --- Unemployment ----

pt_lt_model_esl_unemp_lin <- feols(log_esl ~ treated:post + time_trend 
                                   + unemp_total | country,
                                       se = "standard", 
                                       panel.id = ~year+country, 
                                   data = pt_lt_data_esl_lin)

coeftest(pt_lt_model_esl_unemp_lin, vcov = vcov_NW(pt_lt_model_esl_unemp_lin, lag=1))

# --- GDP + Unemployment ----

pt_lt_model_esl_gdp_unemp_lin <- feols(log_esl ~ treated:post + time_trend 
                                       + unemp_total + gdp_pc | country,
                                       se = "standard", 
                                   panel.id = ~year+country, 
                                   data = pt_lt_data_esl_lin)

coeftest(pt_lt_model_esl_gdp_unemp_lin, vcov = vcov_NW(pt_lt_model_esl_gdp_unemp_lin, lag=1))

# --- Comments ----
# no significant treatment effect when including gdp_pc and both covariates
# significant effect when controlling only for unemp_total
# but for this model spec, pretrends were only marginally insignificant at alpha 5%
# so be cautious


# --- Synthetic Difference-in-Differences ----------------------------------------

sdid_data <- data %>%
  mutate(log_esl = logit_transform(ESL_total),
    treated = ifelse(country == "Portugal" & year >= 2015, 1, 0)
  ) %>%
  select(country, year, log_esl, treated)

sdid_data <- sdid_data %>%
  filter(year > 2001) %>% # Bulgaria, Slovenia, Poland with NAs for 2000
  filter(country != "Serbia") # too many NAs for Serbia 


# --- No Covariates ----

setup_no_cov <- panel.matrices(
  sdid_data,
  unit = "country",
  time = "year",
  outcome = "log_esl",
  treatment = "treated"
)

# synthdid estimation
tau_hat_no_cov <- synthdid_estimate(setup_no_cov$Y, setup_no_cov$N0, setup_no_cov$T0)


# check homoskedasticity across units (not time) 
# needed for applying placebo err. according to Arkhangelsky et al.

Y_no_cov_esl <- setup_no_cov$Y
N0_no_cov_esl <- setup_no_cov$N0
T0_no_cov_esl <- setup_no_cov$T0

# pre-treatment only
Y_no_cov_esl_pre <- Y_no_cov_esl[, 1:T0_no_cov_esl]


control_res_no_cov_esl <- as.vector(Y_no_cov_esl_pre[1:N0_no_cov_esl, ])
treated_res_no_cov_esl <- as.vector(Y_no_cov_esl_pre[(N0_no_cov_esl+1), ])


var.test(control_res_no_cov_esl, treated_res_no_cov_esl)

# COMMENT: H0 not rejected, no evidence for heteroskedasticity


se_hat_no_cov <- sqrt(vcov(tau_hat_no_cov, method = "placebo"))
t_stat_no_cov <- tau_hat_no_cov / se_hat_no_cov
p_value_no_cov <- 2 * (1 - pnorm(abs(t_stat_no_cov)))


# weights
weights <- attr(tau_hat_no_cov, "weights")

# unit weights 
unit_weights <- weights$omega
countries <- unique(sdid_data$country[sdid_data$country != "Portugal"])
for(i in 1:length(unit_weights)) {
  if(unit_weights[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%s: %.3f\n", countries[i], unit_weights[i]))
  }
}

# COMMENT: Bulgaria, Greece, Lithuania and Spain used for synthetic control 

# time weights 
time_weights <- weights$lambda
pre_treatment_years <- unique(sdid_data$year[sdid_data$year < 2015])
for(i in 1:length(time_weights)) {
  if(time_weights[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%d: %.3f\n", pre_treatment_years[i], time_weights[i]))
  }
}


# visualization
plot(tau_hat_no_cov)

# treatment effect over time
synthdid_effect_curve(tau_hat_no_cov)



# --- Covariates (synthdid according to Arkhangelsky et al.) -------------------

# --- GDP + Unemployment ----

# inclusion of covariates as outlined by Arkhangelsky et al. (p. 4092)

# data to include covariates
sdid_data_with_covs <- data %>%
  mutate(log_esl = logit_transform(ESL_total),
         treated = ifelse(country == "Portugal" & year >= 2015, 1, 0)) %>%
  select(country, year, log_esl, treated, gdp_pc, unemp_total) %>%
  filter(year > 2001) %>% 
  filter(country != "Serbia") %>%
  filter(!is.na(gdp_pc) & !is.na(unemp_total))

# regress outcome on covariates
covariate_reg <- lm(log_esl ~ gdp_pc + unemp_total, data = sdid_data_with_covs)

# get residuals
sdid_data_with_covs$log_esl_residual <- residuals(covariate_reg)

# sdid on residualized outcome
setup_residual <- panel.matrices(
  sdid_data_with_covs,
  unit = "country",
  time = "year",
  outcome = "log_esl_residual",  # residuals instead of original outcome
  treatment = "treated"
)

# sdid estimation on residuals
tau_hat_with_cov <- synthdid_estimate(setup_residual$Y, setup_residual$N0, setup_residual$T0)
se_hat_with_cov <- sqrt(vcov(tau_hat_with_cov, method = "placebo"))
t_stat_with_cov <- tau_hat_with_cov / se_hat_with_cov
p_value_with_cov <- 2 * (1 - pnorm(abs(t_stat_with_cov)))

# visualization
plot(tau_hat_with_cov)

# --- GDP ----

# data to include GDP covariate only
sdid_data_gdp <- data %>%
  mutate(log_esl = logit_transform(ESL_total),
         treated = ifelse(country == "Portugal" & year >= 2015, 1, 0)) %>%
  select(country, year, log_esl, treated, gdp_pc) %>%
  filter(year > 2001) %>% 
  filter(country != "Serbia") %>%
  filter(!is.na(gdp_pc))

# regress outcome on GDP only
covariate_reg_gdp <- lm(log_esl ~ gdp_pc, data = sdid_data_gdp)

# get residuals
sdid_data_gdp$log_esl_residual <- residuals(covariate_reg_gdp)

# sdid on residualized outcome
setup_residual_gdp <- panel.matrices(
  sdid_data_gdp,
  unit = "country",
  time = "year",
  outcome = "log_esl_residual",
  treatment = "treated"
)

# sdid estimation on residuals
tau_hat_gdp <- synthdid_estimate(setup_residual_gdp$Y, setup_residual_gdp$N0, setup_residual_gdp$T0)
se_hat_gdp <- sqrt(vcov(tau_hat_gdp, method = "placebo"))
t_stat_gdp <- tau_hat_gdp / se_hat_gdp
p_value_gdp <- 2 * (1 - pnorm(abs(t_stat_gdp)))

# --- Unemployment ----

# data to include unemployment covariate only
sdid_data_unemp <- data %>%
  mutate(log_esl = logit_transform(ESL_total),
         treated = ifelse(country == "Portugal" & year >= 2015, 1, 0)) %>%
  select(country, year, log_esl, treated, unemp_total) %>%
  filter(year > 2001) %>% 
  filter(country != "Serbia") %>%
  filter(!is.na(unemp_total))

# regress outcome on unemployment only
covariate_reg_unemp <- lm(log_esl ~ unemp_total, data = sdid_data_unemp)

# get residuals
sdid_data_unemp$log_esl_residual <- residuals(covariate_reg_unemp)

# sdid on residualized outcome
setup_residual_unemp <- panel.matrices(
  sdid_data_unemp,
  unit = "country",
  time = "year",
  outcome = "log_esl_residual",
  treatment = "treated"
)

# sdid estimation on residuals
tau_hat_unemp <- synthdid_estimate(setup_residual_unemp$Y, setup_residual_unemp$N0, setup_residual_unemp$T0)
se_hat_unemp <- sqrt(vcov(tau_hat_unemp, method = "placebo"))
t_stat_unemp <- tau_hat_unemp / se_hat_unemp
p_value_unemp <- 2 * (1 - pnorm(abs(t_stat_unemp)))

# --- Comparison ----
cat("No covariates:        tau =", round(tau_hat_no_cov, 4), 
    ", SE =", round(se_hat_no_cov, 4), 
    ", t =", round(t_stat_no_cov, 2), "\n")

cat("GDP only:             tau =", round(tau_hat_gdp, 4), 
    ", SE =", round(se_hat_gdp, 4), 
    ", t =", round(t_stat_gdp, 2), "\n")

cat("Unemployment only:    tau =", round(tau_hat_unemp, 4), 
    ", SE =", round(se_hat_unemp, 4), 
    ", t =", round(t_stat_unemp, 2), "\n")

cat("GDP + Unemployment:   tau =", round(tau_hat_with_cov, 4), 
    ", SE =", round(se_hat_with_cov, 4), 
    ", t =", round(t_stat_with_cov, 2), "\n")

# --- Interpretation -----------------------------------------------------------

# across all specifications negative treatment effects
# suggesting that policy reduced ESL rates 
# but estimates sensitive to covariate that incl.
# specification without covariates yields largest effect (-0.335)
# while controlling for unemployment reduces impact (-0.188)
# none of estimated coefficients significant 



# --- Covariates (xsynthdid according to Kranz) --------------------------------

# GDP + Unemployment 
sdid_data_with_covs$log_esl_adj_kranz <- adjust.outcome.for.x(
  sdid_data_with_covs,
  unit = "country",
  time = "year", 
  outcome = "log_esl",
  treatment = "treated",
  x = c("gdp_pc", "unemp_total")
)

# GDP only   
sdid_data_gdp$log_esl_adj_kranz_gdp <- adjust.outcome.for.x(
  sdid_data_gdp,
  unit = "country",
  time = "year",
  outcome = "log_esl", 
  treatment = "treated",
  x = "gdp_pc"
)

# Unemployment only 
sdid_data_unemp$log_esl_adj_kranz_unemp <- adjust.outcome.for.x(
  sdid_data_unemp,
  unit = "country", 
  time = "year",
  outcome = "log_esl",
  treatment = "treated", 
  x = "unemp_total"
)

# synthdid on adjusted outcome 

# both covariates
pm_kranz_both <- panel.matrices(
  sdid_data_with_covs,
  unit = "country", 
  time = "year",
  outcome = "log_esl_adj_kranz", 
  treatment = "treated"
)


tau_kranz_both <- synthdid_estimate(Y = pm_kranz_both$Y, N0 = pm_kranz_both$N0, T0 = pm_kranz_both$T0)
se_kranz_both <- sqrt(vcov(tau_kranz_both, method = "placebo"))
t_kranz_both <- tau_kranz_both / se_kranz_both

plot(tau_kranz_both)

# GDP only
pm_kranz_gdp <- panel.matrices(
  sdid_data_gdp,
  unit = "country",
  time = "year", 
  outcome = "log_esl_adj_kranz_gdp",
  treatment = "treated"
)
tau_kranz_gdp <- synthdid_estimate(Y = pm_kranz_gdp$Y, N0 = pm_kranz_gdp$N0, T0 = pm_kranz_gdp$T0)
se_kranz_gdp <- sqrt(vcov(tau_kranz_gdp, method = "placebo"))
t_kranz_gdp <- tau_kranz_gdp / se_kranz_gdp

plot(tau_kranz_gdp)

# Unemployment only
pm_kranz_unemp <- panel.matrices(
  sdid_data_unemp,
  unit = "country", 
  time = "year",
  outcome = "log_esl_adj_kranz_unemp", 
  treatment = "treated"
)


tau_kranz_unemp <- synthdid_estimate(Y = pm_kranz_unemp$Y, N0 = pm_kranz_unemp$N0, T0 = pm_kranz_unemp$T0)
se_kranz_unemp <- sqrt(vcov(tau_kranz_unemp, method = "placebo"))
t_kranz_unemp <- tau_kranz_unemp / se_kranz_unemp

plot(tau_kranz_both)
plot(tau_kranz_gdp)
plot(tau_kranz_unemp)


# Comments ----
# pretreatment fit using only unemployment does not perform well
# fit when using gdp and combination of both covariates 
# seems visually much better than approach by Arkhangelsky et al.

# --- Comparison ---------------------------------------------------------------

cat(" Kranz et al. approach (xsynthdid package) \n")
cat("GDP + Unemployment:   tau =", round(tau_kranz_both, 4), 
    ", SE =", round(se_kranz_both, 4), ", t =", round(t_kranz_both, 2), "\n")
cat("GDP only:             tau =", round(tau_kranz_gdp, 4), 
    ", SE =", round(se_kranz_gdp, 4), ", t =", round(t_kranz_gdp, 2), "\n") 
cat("Unemployment only:    tau =", round(tau_kranz_unemp, 4), 
    ", SE =", round(se_kranz_unemp, 4), ", t =", round(t_kranz_unemp, 2), "\n")

cat(" Arkhangelsky et al. (synthdid) \n")
cat("GDP + Unemployment:   tau =", round(tau_hat_with_cov, 4), 
    ", SE =", round(se_hat_with_cov, 4), ", t =", round(t_stat_with_cov, 2), "\n")
cat("GDP only:             tau =", round(tau_hat_gdp, 4), 
    ", SE =", round(se_hat_gdp, 4), ", t =", round(t_stat_gdp, 2), "\n")
cat("Unemployment only:    tau =", round(tau_hat_unemp, 4), 
    ", SE =", round(se_hat_unemp, 4), ", t =", round(t_stat_unemp, 2), "\n")


# --- Interpretation -----------------------------------------------------------

# Kranz approach estimates covariate effects using only untreated observations, 
# avoiding contamination from post-treatment period
# additionally, argues that simply regressing y on x might lead to correlated residuals 
# and inconsistent estimates - therefore fixed effects model 

# Arkhangelsky et al. uses all data which may absorb true treatment effects into covariate coefficients,
# explaining smaller effect sizes

# Kranz approach with highly significant results across specifications
# Arkhangelsky et al. with non-significant effects 
# Arkhangelsky et al. approach inconsistent (and maybe biased?) since X correlated with time and group

# Kranz approach with GDP controls (only GDP and GDP + unemployment) best visual pre-treatment fit 
# generally with Kranz approach much better parallel trends (apart from when only unemployment incl.)


# --- Weights ----

weights_final <- attr(tau_kranz_both, "weights")

# unit weights 
unit_weights_final <- weights_final$omega
countries <- unique(sdid_data_with_covs$country[sdid_data_with_covs$country != "Portugal"])
for(i in 1:length(unit_weights_final)) {
  if(unit_weights_final[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%s: %.3f\n", countries[i], unit_weights_final[i]))
  }
}

# Austria, Bulgaria, Estonia, Germany and Lithuania used for SC

# time weights 
time_weights_final <- weights_final$lambda
pre_treatment_years_final <- unique(sdid_data_with_covs$year[sdid_data_with_covs$year < 2015])
for(i in 1:length(time_weights_final)) {
  if(time_weights_final[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%d: %.3f\n", pre_treatment_years_final[i], time_weights_final[i]))
  }
}

# 2005, 2006, 2014

# cross-check with only gdp approach
weights_gdp <- attr(tau_kranz_gdp, "weights")

unit_weights_gdp <- weights_gdp$omega
countries <- unique(sdid_data_with_covs$country[sdid_data_with_covs$country != "Portugal"])

for(i in 1:length(unit_weights_gdp)) {
  if(unit_weights_gdp[i] > 0.001) { 
    cat(sprintf("%s: %.3f\n", countries[i], unit_weights_gdp[i]))
  }
}

time_weights_gdp <- weights_gdp$lambda
pre_treatment_years_final <- unique(sdid_data_with_covs$year[sdid_data_with_covs$year < 2015])

for(i in 1:length(time_weights_gdp)) {
  if(time_weights_gdp[i] > 0.001) { 
    cat(sprintf("%d: %.3f\n", pre_treatment_years_final[i], time_weights_gdp[i]))
  }
}

# compare 
cor(unit_weights_final, unit_weights_gdp)
cor(time_weights_final, time_weights_gdp)

# same units included but slightly differently weighted
# time periods 2005, 2012, 2014 instead of 2005, 2006 and 2014 
# but in both cases main weight on 2005 and 2014 anyways


# Placebo ----

synthdid_placebo(tau_kranz_both)
synthdid_placebo_plot(tau_kranz_both)

# placebo effect substantial relative to main effect

synthdid_placebo(tau_hat_with_cov)
synthdid_placebo_plot(tau_hat_with_cov)

# placebo effect larger than main effect of Arkhangelsky et al. approach

# COMMENT: large placebo effects suggest true effect might be smaller
# but no finite answer can be given since std.err. NA s.t. CI cant be calculated


######## Completion Rates ####### 

# NOTE: survey addressed at those aged 3-5 years above graduation age, i.e. 21-23 year-olds
# s.t. first effects as early as 2018 but probably later

# --- Visualization ----

# plot 1: Portugal + Spain, Italy, Greece, Austria, France
ggplot(data %>% filter(country %in% c("Portugal", "Spain", "Italy", "Greece", "Austria", "France")),
       aes(x = year, y = log(comp_upsec_total / (100 - comp_upsec_total)), color = country)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  scale_color_manual(values = country_colors) +
  theme_minimal() +
  labs(title = "Variable Over Time - Group 1",
       x = "Year", y = "variable", color = "Country")

# plot 2: Portugal + Germany, Hungary, Lithuania, Poland, Romania
ggplot(data %>% filter(country %in% c("Portugal", "Germany", "Hungary", "Lithuania", "Poland", "Romania")),
       aes(x = year, y = log(comp_upsec_total / (100 - comp_upsec_total)), color = country)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  scale_color_manual(values = country_colors) +
  theme_minimal() +
  labs(title = "Variable Over Time - Group 2",
       x = "Year", y = "variable", color = "Country")


# plot 3: Portugal + Serbia, Slovenia, Estonia, Bulgaria
ggplot(data %>% filter(country %in% c("Portugal", "Slovenia", "Estonia", "Bulgaria")),
       aes(x = year, y = log(comp_upsec_total / (100 - comp_upsec_total)), color = country)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  scale_color_manual(values = country_colors) +
  theme_minimal() +
  labs(title = "Variable Over Time - Group 3",
       x = "Year", y = "variable", color = "Country")


# COMMENT: Spain or Greece as only potential reliable control 


# --- Difference-in-Differences ------------------------------------------------

comp_data <- data %>% 
  filter(year >= 2006) %>% 
  filter(!country == "Serbia") %>%
  select(c(country, year, comp_upsec_total, gdp_pc, gov_exp_edu, unemp_total)) %>%
  mutate(logit_comp = logit_transform(comp_upsec_total)) 

did_data_comp <- comp_data %>%
  mutate(post = ifelse(year >= 2018, 1, 0), 
         treated = ifelse(country == "Portugal", 1, 0))


# --- Testing Parallel Trends ----

did_data_comp_pre <- did_data_comp %>%
  filter(year < 2018)

# --- Parallel trends test for Portugal vs Greece ----

pt_gr_data_comp_pre <- did_data_comp_pre %>%
  filter(country %in% c("Portugal", "Greece")) %>% 
  filter(year >= 2007) %>%
  mutate(time_trend = year - 2017)


pt_gr_model_comp_pre <- feols(logit_comp ~ treated:time_trend
                              + unemp_total + gdp_pc | year+country, 
                              se = "standard", panel.id=~year+country,
                             data = pt_gr_data_comp_pre)

coeftest(pt_gr_model_comp_pre, vcov = vcov_NW(pt_gr_model_comp_pre, lag=1))


# --- Comments ----

# time period starting from 2007
# for Prt vs Grc would also hold for earlier time period but for consistency 
# across models set 2007 (earliest time period for which PT cannot be rejected for Esp)

# COMMENT: later Did models also implemented for GRC with data from 2000 
# but results did not change

# interaction insignificant when controlling  for both, none or either one of covariates

# --- Parallel trends test for Portugal vs Spain ----

pt_esp_data_comp_pre <- did_data_comp_pre %>%
  filter(country %in% c("Portugal", "Spain")) %>% 
  filter(year >= 2007) %>%
  mutate(time_trend = year - 2017)


pt_esp_model_comp_pre <- feols(logit_comp ~ treated:time_trend + unemp_total
                               | year+country, 
                               se = "standard", panel.id=~year+country,
                              data = pt_esp_data_comp_pre)

coeftest(pt_esp_model_comp_pre, vcov = vcov_NW(pt_esp_model_comp_pre, lag=1))


# --- Comments ----
# interaction insignificant only when including both covariates 



# --- Difference-in-Differences with year-fixed effects ------------------------

# --- Portugal vs Greece ----

pt_gr_data_comp <- did_data_comp %>%
  filter(country %in% c("Portugal", "Greece")) %>% 
  filter(year >= 2007) %>% # time period for which parallel trends holds
  mutate(time_trend = year - 2017)

# COMMENT: all following DiD specs using different combinations of covariates 
# for all combinations parallel trends cannot be rejected 

# --- No Covariates  ----

pt_gr_model_comp <- feols(logit_comp ~ treated:post | year+country, 
                              se = "standard", panel.id=~year+country,
                              data = pt_gr_data_comp)

coeftest(pt_gr_model_comp, vcov = vcov_NW(pt_gr_model_comp, lag=1))

# --- GDP ----

pt_gr_model_comp_gdp <- feols(logit_comp ~ treated:post + gdp_pc | year+country, 
                              se = "standard", panel.id=~year+country,
                             data = pt_gr_data_comp)

coeftest(pt_gr_model_comp_gdp, vcov = vcov_NW(pt_gr_model_comp_gdp, lag=1))


# --- Unemployment ---- 

pt_gr_model_comp_unemp <- feols(logit_comp ~ treated:post + unemp_total | year+country, 
                                se = "standard", panel.id=~year+country,
                               data = pt_gr_data_comp)

coeftest(pt_gr_model_comp_unemp, vcov = vcov_NW(pt_gr_model_comp_unemp, lag=1))


# --- GDP + Unemployment ----

pt_gr_model_comp_gdp_unemp <- feols(logit_comp ~ treated:post 
                                    + unemp_total + gdp_pc | year+country, 
                                    se = "standard", panel.id=~year+country, 
                                    data = pt_gr_data_comp)

coeftest(pt_gr_model_comp_gdp_unemp, vcov = vcov_NW(pt_gr_model_comp_gdp_unemp, lag=1))


# --- Interpretation -----------------------------------------------------------
# coefficient on treated:post consistently non-significant across all specs 

# --- Portugal vs Spain ----

pt_esp_data_comp <- did_data_comp %>%
  filter(country %in% c("Portugal", "Spain")) %>% 
  filter(year >= 2007) %>%
  mutate(time_trend = year - 2017)

# --- GDP + Unemployment ----

pt_esp_model_comp_gdp_unemp <- feols(logit_comp ~ treated:post 
                                     + unemp_total + gdp_pc | year+country, 
                                     se = "standard", panel.id=~year+country, 
                                     data = pt_esp_data_comp)

coeftest(pt_esp_model_comp_gdp_unemp, vcov = vcov_NW(pt_esp_model_comp_gdp_unemp, lag=1))


# --- Interpretation -----------------------------------------------------------
# interaction term marginally significant at alpha 5%
# but keep in mind that pre-trend tests only passed with both covariates




# --- Difference-in-Differences without year-fixed effects ---------------------

did_data_comp_pre_lin <- did_data_comp %>%
  filter(year >= 2015)

# --- Parallel trends test for Portugal vs Greece ----

pt_gr_data_comp_pre_lin <- did_data_comp_pre_lin %>%
  filter(country %in% c("Portugal", "Greece")) %>% 
  filter(year >= 2007) %>%
  mutate(time_trend = year - 2017)


pt_gr_model_comp_pre_lin <- feols(logit_comp ~ treated:time_trend + time_trend 
                                  + gdp_pc + unemp_total | country, 
                                  se = "standard", panel.id=~year+country,
                              data = pt_gr_data_comp_pre_lin)

coeftest(pt_gr_model_comp_pre_lin, vcov = vcov_NW(pt_gr_model_comp_pre_lin, lag=1))


# --- Comments ----

# time period set to start 2007 for consistency with main DiD spec

# interaction insignificant when including both, none or either one of covariates

# --- Parallel trends test for Portugal vs Spain ----

pt_esp_data_comp_pre_lin <- did_data_comp_pre_lin %>%
  filter(country %in% c("Portugal", "Spain")) %>% 
  filter(year >= 2007) %>%
  mutate(time_trend = year - 2017)


pt_esp_model_comp_pre_lin <- feols(logit_comp ~ treated:time_trend + time_trend 
                                   + unemp_total + gdp_pc | country, 
                                   se = "standard", panel.id=~year+country, 
                               data = pt_esp_data_comp_pre_lin)

coeftest(pt_esp_model_comp_pre_lin, vcov = vcov_NW(pt_esp_model_comp_pre_lin, lag=1))


# --- Comments ----

# coefficient insignificant for all covariate combinations and without covariates 
# though when including none or only gdp_pc only marginally insignificant 
# at alpha 5% (but would not be for 10%)




# --- Portugal vs Greece ----

pt_gr_data_comp_lin <- did_data_comp %>%
  filter(country %in% c("Portugal", "Greece")) %>% 
  filter(year >= 2007) %>%
  mutate(time_trend = year - 2017)

# --- No covariates ----
pt_gr_model_comp_lin <- feols(logit_comp ~ treated:post + time_trend | country, 
                              se = "standard", panel.id=~year+country, 
                             data = pt_gr_data_comp_lin)

coeftest(pt_gr_model_comp_lin, vcov = vcov_NW(pt_gr_model_comp_lin, lag=1))

# --- GDP ----

pt_gr_model_comp_gdp_lin <- feols(logit_comp ~ treated:post + gdp_pc | country, 
                                  se = "standard", panel.id=~year+country,
                                 data = pt_gr_data_comp_lin)

coeftest(pt_gr_model_comp_gdp_lin, vcov = vcov_NW(pt_gr_model_comp_gdp_lin, lag=1))

# --- Unemployment ----

pt_gr_model_comp_unemp_lin <- feols(logit_comp ~ treated:post + time_trend 
                                    + unemp_total | country,
                                    se = "standard", panel.id = ~year+country,
                                   data = pt_gr_data_comp_lin)

coeftest(pt_gr_model_comp_unemp_lin, vcov = vcov_NW(pt_gr_model_comp_unemp_lin, lag=1))

# --- GDP + Unemployment ----

pt_gr_model_comp_gdp_unemp_lin <- feols(logit_comp ~ treated:post + time_trend 
                                    + unemp_total+ gdp_pc | country,
                                    se = "standard", panel.id = ~year+country,
                                    data = pt_gr_data_comp_lin)

coeftest(pt_gr_model_comp_gdp_unemp_lin, vcov = vcov_NW(pt_gr_model_comp_gdp_unemp_lin, lag=1))

# --- Comments ----

# no treatment effect when no covariates, unemp_total and both covariates
# when including only gdp_pc highly significant effect

# --- Portugal vs Spain ----

pt_esp_data_comp_lin <- did_data_comp %>%
  filter(country %in% c("Portugal", "Spain")) %>% 
  filter(year >= 2007) %>%
  mutate(time_trend = year - 2017)

# --- No Covariates ----

pt_esp_model_comp_lin <- feols(logit_comp ~ treated:post + time_trend
                                         | country, 
                                         se = "standard", 
                                         panel.id = ~year+country, 
                                         data = pt_esp_data_comp_lin)

coeftest(pt_esp_model_comp_lin, vcov = vcov_NW(pt_esp_model_comp_lin, lag=1))

# --- GDP ----

pt_esp_model_comp_gdp_lin <- feols(logit_comp ~ treated:post + time_trend 
                                   + gdp_pc | country, 
                                         se = "standard", 
                                         panel.id = ~year+country, 
                                   data = pt_esp_data_comp_lin)

coeftest(pt_esp_model_comp_gdp_lin, vcov = vcov_NW(pt_esp_model_comp_gdp_lin, lag=1))

# --- Unemployment ----

pt_esp_model_comp_unemp_lin <- feols(logit_comp ~ treated:post + time_trend 
                                     + unemp_total | country, 
                                         se = "standard", 
                                         panel.id = ~year+country, 
                                     data = pt_esp_data_comp_lin)

coeftest(pt_esp_model_comp_unemp_lin, vcov = vcov_NW(pt_esp_model_comp_unemp_lin, lag=1))

# --- GDP + Unemployment ----

pt_esp_model_comp_gdp_unemp_lin <- feols(logit_comp ~ treated:post + time_trend+ gdp_pc + unemp_total | country, 
                                         se = "standard", 
                                 panel.id = ~year+country, data = pt_esp_data_comp_lin)

coeftest(pt_esp_model_comp_gdp_unemp_lin, vcov = vcov_NW(pt_esp_model_comp_gdp_unemp_lin, lag=1))


# --- Comments ----

# marginally significant effect at alpha 5% when incl no covariates or gdp_pc only

# keep in mind that for both these covariate specs, interaction in pretrends
# also only marginally insignificant at alpha 5%

# marginally insignificant at alpha 5% when incl both covariates or only unemp_total
# so significance found earlier likely just sensitive to covariate inclusion



# --- Synthetic Difference-in-Differences --------------------------------------

sdid_data_comp <- data %>%
  mutate(logit_comp = logit_transform(comp_upsec_total),
         treated = ifelse(country == "Portugal" & year >= 2018, 1, 0)
  ) %>%
  select(country, year, logit_comp, treated)

sdid_data_comp <- sdid_data_comp %>%
  filter(year >= 2006) %>% 
  filter(country != "Serbia") # too many NAs for Serbia 

# --- No Covariates ----

setup_no_cov_comp <- panel.matrices(
  sdid_data_comp,
  unit = "country",
  time = "year",
  outcome = "logit_comp",
  treatment = "treated"
)

# synthdid estimation
tau_hat_no_cov_comp <- synthdid_estimate(setup_no_cov_comp$Y, setup_no_cov_comp$N0, 
                                         setup_no_cov_comp$T0)


# homoskedasticity check
Y_no_cov_comp <- setup_no_cov_comp$Y
N0_no_cov_comp <- setup_no_cov_comp$N0
T0_no_cov_comp <- setup_no_cov_comp$T0

Y_no_cov_comp_pre <- Y_no_cov_comp[, 1:T0_no_cov_comp] # prior intervention

control_res_no_cov_comp <- as.vector(Y_no_cov_comp_pre[1:N0_no_cov_comp, ])
treated_res_no_cov_comp <- as.vector(Y_no_cov_comp_pre[(N0_no_cov_comp+1), ])

var.test(control_res_no_cov_comp, treated_res_no_cov_comp)

# COMMENT: H0 not rejected

se_hat_no_cov_comp <- sqrt(vcov(tau_hat_no_cov_comp, method = "placebo"))
t_stat_no_cov_comp <- tau_hat_no_cov_comp / se_hat_no_cov_comp
p_value_no_cov_comp <- 2 * (1 - pnorm(abs(t_stat_no_cov_comp)))


# weights
weights_comp <- attr(tau_hat_no_cov_comp, "weights")

# unit weights 
unit_weights_comp <- weights_comp$omega
countries <- unique(sdid_data_comp$country[sdid_data_comp$country != "Portugal"])
for(i in 1:length(unit_weights_comp)) {
  if(unit_weights_comp[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%s: %.3f\n", countries[i], unit_weights_comp[i]))
  }
}

# COMMENT: Bulgaria, Spain, Lithuania, Greece and France used for synthetic control 

# time weights 
time_weights_comp <- weights_comp$lambda
pre_treatment_years_comp <- unique(sdid_data_comp$year[sdid_data_comp$year < 2018])
for(i in 1:length(time_weights_comp)) {
  if(time_weights_comp[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%d: %.3f\n", pre_treatment_years_comp[i], time_weights_comp[i]))
  }
}


# visualization
plot(tau_hat_no_cov_comp)

# treatment effect curve
synthdid_effect_curve(tau_hat_no_cov_comp)


# --- Covariates (synthdid according to Arkhangelsky et al.) -------------------

# --- GDP + Unemployment ----

# data to include covariates
sdid_data_with_covs_comp <- data %>%
  mutate(logit_comp = logit_transform(comp_upsec_total),
         treated = ifelse(country == "Portugal" & year >= 2018, 1, 0)) %>%
  select(country, year, logit_comp, treated, gdp_pc, unemp_total) %>%
  filter(year >= 2006) %>% 
  filter(country != "Serbia") %>%
  filter(!is.na(gdp_pc) & !is.na(unemp_total))

# regress outcome on covariates
covariate_reg_comp <- lm(logit_comp ~ gdp_pc + unemp_total, data = sdid_data_with_covs_comp)

# get residuals
sdid_data_with_covs_comp$log_comp_residual <- residuals(covariate_reg_comp)

# sdid on residualized outcome
setup_residual_comp <- panel.matrices(
  sdid_data_with_covs_comp,
  unit = "country",
  time = "year",
  outcome = "log_comp_residual",  # residuals instead of original outcome
  treatment = "treated"
)

# sdid estimation on residuals
tau_hat_with_cov_comp <- synthdid_estimate(setup_residual_comp$Y, setup_residual_comp$N0, 
                                           setup_residual_comp$T0)
se_hat_with_cov_comp <- sqrt(vcov(tau_hat_with_cov_comp, method = "placebo"))
t_stat_with_cov_comp <- tau_hat_with_cov_comp / se_hat_with_cov_comp
p_value_with_cov_comp <- 2 * (1 - pnorm(abs(t_stat_with_cov_comp)))

# visualization
plot(tau_hat_with_cov_comp)

# --- GDP ----

# data to include GDP covariate only
sdid_data_gdp_comp <- data %>%
  mutate(logit_comp = logit_transform(comp_upsec_total),
         treated = ifelse(country == "Portugal" & year >= 2018, 1, 0)) %>%
  select(country, year, logit_comp, treated, gdp_pc) %>%
  filter(year >= 2006) %>% 
  filter(country != "Serbia") %>%
  filter(!is.na(gdp_pc))

# regress outcome on GDP only
covariate_reg_gdp_comp <- lm(logit_comp ~ gdp_pc, data = sdid_data_gdp_comp)

# get residuals
sdid_data_gdp_comp$log_comp_residual <- residuals(covariate_reg_gdp_comp)

# sdid on residualized outcome
setup_residual_gdp_comp <- panel.matrices(
  sdid_data_gdp_comp,
  unit = "country",
  time = "year",
  outcome = "log_comp_residual",
  treatment = "treated"
)

# sdid estimation on residuals
tau_hat_gdp_comp <- synthdid_estimate(setup_residual_gdp_comp$Y, setup_residual_gdp_comp$N0, 
                                      setup_residual_gdp_comp$T0)
se_hat_gdp_comp <- sqrt(vcov(tau_hat_gdp_comp, method = "placebo"))
t_stat_gdp_comp <- tau_hat_gdp_comp / se_hat_gdp_comp
p_value_gdp_comp <- 2 * (1 - pnorm(abs(t_stat_gdp_comp)))

# visualization
plot(tau_hat_gdp_comp)

# --- Unemployment ----

# data to include unemployment covariate only
sdid_data_unemp_comp <- data %>%
  mutate(logit_comp = logit_transform(comp_upsec_total),
         treated = ifelse(country == "Portugal" & year >= 2018, 1, 0)) %>%
  select(country, year, logit_comp, treated, unemp_total) %>%
  filter(year >= 2006) %>% 
  filter(country != "Serbia") %>%
  filter(!is.na(unemp_total))

# regress outcome on unemployment only
covariate_reg_unemp_comp <- lm(logit_comp ~ unemp_total, data = sdid_data_unemp_comp)

# get residuals
sdid_data_unemp_comp$log_comp_residual <- residuals(covariate_reg_unemp_comp)

# sdid on residualized outcome
setup_residual_unemp_comp <- panel.matrices(
  sdid_data_unemp_comp,
  unit = "country",
  time = "year",
  outcome = "log_comp_residual",
  treatment = "treated"
)

# sdid estimation on residuals
tau_hat_unemp_comp <- synthdid_estimate(setup_residual_unemp_comp$Y, 
                                        setup_residual_unemp_comp$N0, 
                                        setup_residual_unemp_comp$T0)
se_hat_unemp_comp <- sqrt(vcov(tau_hat_unemp_comp, method = "placebo"))
t_stat_unemp_comp <- tau_hat_unemp_comp / se_hat_unemp_comp
p_value_unemp_comp <- 2 * (1 - pnorm(abs(t_stat_unemp_comp)))

plot(tau_hat_unemp_comp)


# --- Interpretation -----------------------------------------------------------

# across all specifications positive treatment effects
# estimates very similar irrespective of covariates that incl. 
# when controlling for no covariates effect marginally insignificant at alpha 5%


# --- Covariates (xsynthdid according to Kranz) --------------------------------

# GDP + Unemployment 
sdid_data_with_covs_comp$log_comp_adj_kranz <- adjust.outcome.for.x(
  sdid_data_with_covs_comp,
  unit = "country",
  time = "year", 
  outcome = "logit_comp",
  treatment = "treated",
  x = c("gdp_pc", "unemp_total")
)

# GDP only   
sdid_data_gdp_comp$log_comp_adj_kranz_gdp <- adjust.outcome.for.x(
  sdid_data_gdp_comp,
  unit = "country",
  time = "year",
  outcome = "logit_comp", 
  treatment = "treated",
  x = "gdp_pc"
)

# Unemployment only 
sdid_data_unemp_comp$log_comp_adj_kranz_unemp <- adjust.outcome.for.x(
  sdid_data_unemp_comp,
  unit = "country", 
  time = "year",
  outcome = "logit_comp",
  treatment = "treated", 
  x = "unemp_total"
)

# synthdid on adjusted outcome 

# both covariates
pm_kranz_both_comp <- panel.matrices(
  sdid_data_with_covs_comp,
  unit = "country", 
  time = "year",
  outcome = "log_comp_adj_kranz", 
  treatment = "treated"
)

tau_kranz_both_comp <- synthdid_estimate(Y = pm_kranz_both_comp$Y, 
                                         N0 = pm_kranz_both_comp$N0, 
                                         T0 = pm_kranz_both_comp$T0)
se_kranz_both_comp <- sqrt(vcov(tau_kranz_both_comp, method = "placebo"))
t_kranz_both_comp <- tau_kranz_both_comp / se_kranz_both_comp

plot(tau_kranz_both_comp)

# GDP only
pm_kranz_gdp_comp <- panel.matrices(
  sdid_data_gdp_comp,
  unit = "country",
  time = "year", 
  outcome = "log_comp_adj_kranz_gdp",
  treatment = "treated"
)
tau_kranz_gdp_comp <- synthdid_estimate(Y = pm_kranz_gdp_comp$Y, 
                                        N0 = pm_kranz_gdp_comp$N0, 
                                        T0 = pm_kranz_gdp_comp$T0)
se_kranz_gdp_comp <- sqrt(vcov(tau_kranz_gdp_comp, method = "placebo"))
t_kranz_gdp_comp <- tau_kranz_gdp_comp / se_kranz_gdp_comp

plot(tau_kranz_gdp_comp)

# Unemployment only
pm_kranz_unemp_comp <- panel.matrices(
  sdid_data_unemp_comp,
  unit = "country", 
  time = "year",
  outcome = "log_comp_adj_kranz_unemp", 
  treatment = "treated"
)

tau_kranz_unemp_comp <- synthdid_estimate(Y = pm_kranz_unemp_comp$Y, 
                                          N0 = pm_kranz_unemp_comp$N0, 
                                          T0 = pm_kranz_unemp_comp$T0)
se_kranz_unemp_comp <- sqrt(vcov(tau_kranz_unemp_comp, method = "placebo"))
t_kranz_unemp_comp <- tau_kranz_unemp_comp / se_kranz_unemp_comp

plot(tau_kranz_both_comp)
plot(tau_kranz_gdp_comp)
plot(tau_kranz_unemp_comp)


# COMMENT: pretreatment fit using only unemployment does not perform too well
# fit when using combination of both covariates or solely gdp seems visually better 


# --- Comparison ---------------------------------------------------------------

cat("=== Kranz et al. approach (xsynthdid package) ===\n")
cat("GDP + Unemployment:   tau =", round(tau_kranz_both_comp, 4), 
    ", SE =", round(se_kranz_both_comp, 4), ", t =", round(t_kranz_both_comp, 2), "\n")
cat("GDP only:             tau =", round(tau_kranz_gdp_comp, 4), 
    ", SE =", round(se_kranz_gdp_comp, 4), ", t =", round(t_kranz_gdp_comp, 2), "\n") 
cat("Unemployment only:    tau =", round(tau_kranz_unemp_comp, 4), 
    ", SE =", round(se_kranz_unemp_comp, 4), ", t =", round(t_kranz_unemp_comp, 2), "\n")

cat("=== Arkhangelsky et al. (synthdid) ===\n")
cat("GDP + Unemployment:   tau =", round(tau_hat_with_cov_comp, 4), 
    ", SE =", round(se_hat_with_cov_comp, 4), ", t =", round(t_stat_with_cov_comp, 2), "\n")
cat("GDP only:             tau =", round(tau_hat_gdp_comp, 4), 
    ", SE =", round(se_hat_gdp_comp, 4), ", t =", round(t_stat_gdp_comp, 2), "\n")
cat("Unemployment only:    tau =", round(tau_hat_unemp_comp, 4), 
    ", SE =", round(se_hat_unemp_comp, 4), ", t =", round(t_stat_unemp_comp, 2), "\n")


# --- Interpretation -----------------------------------------------------------

# Kranz approach shows larger positive treatment effects across specifications 
# with GDP+unemployment most significant and unemployment-only weakest   
# when controlling for solely unemp_total, also pretrends dont seem good
# 
# Arkhangelsky et al. approach with stability across specifications
# 
# both methods confirm significant positive treatment effect but differ in magnitude
# GDP as key covariate with consistent strong effects across both approaches
# compared to ESL, Arkhangelsky et al. and Kranz with both controls 
# seem both good visually 

plot(tau_hat_with_cov_comp)
plot(tau_hat_gdp_comp)

plot(tau_kranz_gdp_comp)
plot(tau_kranz_both_comp)

# --- Weights ----

# Kranz' approach 
weights_final_comp <- attr(tau_kranz_both_comp, "weights")

# unit weights 
unit_weights_final_comp <- weights_final_comp$omega
countries <- unique(sdid_data_with_covs_comp$country[sdid_data_with_covs_comp$country != "Portugal"])
for(i in 1:length(unit_weights_final_comp)) {
  if(unit_weights_final_comp[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%s: %.3f\n", countries[i], unit_weights_final_comp[i]))
  }
}

# COMMENT: Lithuania, Bulgaria, Greece, Austria, Germany, France and Spain for SC

# time weights 
time_weights_final_comp <- weights_final_comp$lambda
pre_treatment_years_final_comp <- unique(sdid_data_with_covs_comp$year[sdid_data_with_covs_comp$year < 2018])
for(i in 1:length(time_weights_final_comp)) {
  if(time_weights_final_comp[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%d: %.3f\n", pre_treatment_years_final_comp[i], time_weights_final_comp[i]))
  }
}

# 2017, 2016 and 2008

# Arkhangeslky' approach 
weights_final_ark_comp <- attr(tau_hat_with_cov_comp, "weights")

# unit weights 
unit_weights_final_ark_comp <- weights_final_ark_comp$omega
countries <- unique(sdid_data_with_covs_comp$country[sdid_data_with_covs_comp$country != "Portugal"])
for(i in 1:length(unit_weights_final_ark_comp)) {
  if(unit_weights_final_ark_comp[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%s: %.3f\n", countries[i], unit_weights_final_ark_comp[i]))
  }
}

# COMMENT: Greece, Spain, Lithuania, Bulgaria  

# time weights 
time_weights_final_ark_comp <- weights_final_ark_comp$lambda
pre_treatment_years_final_ark_comp <- unique(sdid_data_with_covs_comp$year[sdid_data_with_covs_comp$year < 2018])
for(i in 1:length(time_weights_final_ark_comp)) {
  if(time_weights_final_ark_comp[i] > 0.001) {  # only meaningful weights
    cat(sprintf("%d: %.3f\n", pre_treatment_years_final_ark_comp[i], time_weights_final_ark_comp[i]))
  }
}

# 2017, 2013, 2016

# COMMENT: both specification arrive at somewhat similar conclusion in terms of unit weigthing

# Placebo ----

synthdid_placebo(tau_kranz_both_comp)
synthdid_placebo_plot(tau_kranz_both_comp)

# placebo effect very large relative to main effect

synthdid_placebo(tau_hat_with_cov_comp)
synthdid_placebo_plot(tau_hat_with_cov_comp)

# placebo effect also not small relative to main effect but seems more robust than Kranz


# COMMENT: Arkhangelsky method preferred due to better placebo effects
# Kranz method shows systematic overestimation (placebo nearly equals main effect)
# no std.err. available s.t. no formal significance testing possible


####### Total Unemployment (after 2015) #######

# COMMENT: period strongly affected by financial crisis 
# SCM as best solution since other countries also affected

# COMMENT: total unemployment rate as outcome variable
# captures broad economic impact of ESL reform on labor market
# tests whether economy can absorb additional workers with higher education level
# but remains questionable (at least in short-term) whether labor market able to respond to labor supply
# or whether there are any effects at all

unemp_data <- data %>% 
  mutate(log_unemp = logit_transform(unemp_total))

# --- Visualizations ----

# plot 1: Portugal + Spain, Italy, Greece, Austria, France
ggplot(unemp_data %>% filter(country %in% c("Portugal", "Spain", "Italy", "Greece", "Austria", "France")),
       aes(x = year, y = log_unemp, color = country)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Variable Over Time - Group 1",
       x = "Year", y = "variable", color = "Country")

# plot 2: Portugal + Germany, Hungary, Lithuania, Poland, Romania
ggplot(unemp_data %>% filter(country %in% c("Portugal", "Germany", "Hungary", "Lithuania", "Poland", "Romania")),
       aes(x = year, y = log_unemp, color = country)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Variable Over Time - Group 2",
       x = "Year", y = "variable", color = "Country")


# plot 3: Portugal + Serbia, Slovenia, Estonia, Bulgaria
ggplot(unemp_data %>% filter(country %in% c("Portugal", "Slovenia", "Estonia", "Bulgaria")),
       aes(x = year, y = log_unemp, color = country)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Variable Over Time - Group 3",
       x = "Year", y = "variable", color = "Country")

# --- Synthetic Control --------------------------------------------------------

# unemployment rates reflect annual averages based on national survey frequencies
# so 2015 rate reflects full-year average (see https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SL.UEM.TOTL.ZS)

# first affected students could graduate in 2015 but entry to labor market may be delayed
# and high grade retention in Portugal may delay graduation and labor force entry 
# therefore, focus on 2016 


synth_data <- data %>%
  group_by(country) %>%
  filter(!any(is.na(unemp_total)) & !any(is.na(gdp_pc))) %>%  # only countries with no NAs
  ungroup() %>%
  mutate(country_id = as.numeric(as.factor(country))) # numeric country identifier


# country name lookup for reference
country_lookup <- synth_data %>% 
  select(country, country_id) %>% 
  distinct() %>% 
  arrange(country_id)

# Portugal's ID 
portugal_id <- country_lookup$country_id[country_lookup$country == "Portugal"]

# control country IDs
control_ids <- country_lookup$country_id[country_lookup$country != "Portugal"]

synth_data <- as.data.frame(synth_data) 

dataprep_out <- dataprep(
  foo = synth_data,
  predictors = "gdp_pc",
  predictors.op = "mean",
  special.predictors = list(
    list("unemp_total", 2003, "mean"), # post €
    list("unemp_total", 2008:2009, "mean"), # financial crisis
    list("unemp_total", 2010:2012, "mean"), # Euro crisis
    list("unemp_total", 2014:2015, "mean") # Euro crisis recovery
  ),
  time.predictors.prior = 2003:2015, # period over which to average outcome predictors
  dependent = "unemp_total",
  unit.variable = "country_id",
  unit.names.variable = "country", 
  time.variable = "year",
  treatment.identifier = portugal_id,
  controls.identifier = control_ids,
  time.optimize.ssr = 2003:2015, # after introduction of euro
  time.plot = 2003:2021
)


# COMMENT: Hungary can be incl. in donor pool since reform was in 1996 
# and reversed just one year afterwards (see Adamecz, 2023)


# run synthetic control 
synth_out <- synth(dataprep_out)


synth_tables <- synth.tab( synth.res = synth_out, dataprep.res = dataprep_out)
print(synth_tables)

# COMMENt: Hungary, Greece and Austria for synthetic control


# visualizations
path.plot(synth_out, dataprep_out, 
          Ylab = "Unemployment Rate (%)",
          Xlab = "Year",
          Legend = c("Portugal", "Synthetic Control"),
          Legend.position = "topright")

abline(v = 2015.5, col = "#FA8072", lty = 2, lwd = 2)

# gap plot for difference 
gaps.plot(synth_out, dataprep_out,
          Ylab = "Gap in Unemployment Rate",
          Xlab = "Year", 
          Main = "Portugal vs Synthetic Portugal")
abline(v = 2015.5, col = "#FA8072", lty = 2, lwd = 2)
abline(h = 0, col = "black", lty = 1)


# get actual vs synthetic values
actual_portugal <- dataprep_out$Y1plot
synthetic_portugal <- dataprep_out$Y0plot %*% synth_out$solution.w

# calculate gaps and split accordingly
gaps <- actual_portugal - synthetic_portugal

pre_treatment_gaps <- gaps[rownames(gaps) < "2016", , drop = FALSE]
post_treatment_gaps <- gaps[rownames(gaps) >= "2016", , drop = FALSE]

effect_pre <- mean(pre_treatment_gaps)
att_post <- mean(post_treatment_gaps) # ATT

# effect for individual years
post_treatment_gaps


# --- Placebo test ----

placebo_treatment_year <- 2014

placebo_dataprep <- dataprep(
  foo = synth_data,
  predictors = "gdp_pc",
  predictors.op = "mean",
  special.predictors = list(
    list("unemp_total", 2003, "mean"), 
    list("unemp_total", 2008:2009, "mean"), 
    list("unemp_total", 2010:2012, "mean")
  ),
  time.predictors.prior = 2003:2013,  
  dependent = "unemp_total",
  unit.variable = "country_id",
  unit.names.variable = "country", 
  time.variable = "year",
  treatment.identifier = portugal_id,
  controls.identifier = control_ids,
  time.optimize.ssr = 2003:2013,  # only on pre-fake-treatment
  time.plot = 2003:2015  
)

# synthetic control
placebo_synth <- synth(placebo_dataprep)

placebo_tables <- synth.tab(synth.res = placebo_synth, dataprep.res = placebo_dataprep)
print(placebo_tables)


# path plot for placebo
path.plot(placebo_synth, placebo_dataprep, 
          Ylab = "Unemployment Rate (%)",
          Xlab = "Year",
          Legend = c("Portugal", "Synthetic Control (Placebo)"),
          Legend.position = "topright",
          Main = "Placebo Test with Treatment in 2014")
abline(v = placebo_treatment_year, col = "#FA8072", lty = 2, lwd = 2)

# gap plot for placebo
gaps.plot(placebo_synth, placebo_dataprep,
          Ylab = "Gap in Unemployment Rate",
          Xlab = "Year", 
          Main = "Placebo Test: Portugal vs Synthetic Portugal")
abline(v = placebo_treatment_year, col = "#FA8072", lty = 2, lwd = 2)
abline(h = 0, col = "black", lty = 1)


# placebo effects
placebo_actual <- placebo_dataprep$Y1plot
placebo_synthetic <- placebo_dataprep$Y0plot %*% placebo_synth$solution.w
placebo_gaps <- placebo_actual - placebo_synthetic

# pre and post placebo treatment
placebo_pre <- placebo_gaps[as.numeric(rownames(placebo_gaps)) < placebo_treatment_year, , drop = FALSE]
placebo_post <- placebo_gaps[as.numeric(rownames(placebo_gaps)) >= placebo_treatment_year, , drop = FALSE]

# placebo effects
placebo_effect_pre <- mean(placebo_pre)
placebo_effect_post <- mean(placebo_post)


