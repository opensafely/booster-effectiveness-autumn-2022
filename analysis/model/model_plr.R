
# # # # # # # # # # # # # # # # # # # # #
# This script:
# imports processed data and restricts it to patients in "cohort"
# fits a pooled logistic regression model adjusted for baseline covariates 
# obtains estimates of risk, risk difference and risk ratio for the two treatment groups
# at each day of follow up. 
# 95% CIs for the estimates are obtained using boot strapping 
#
# The script should be run via an action in the project.yaml
# The script must be accompanied by four arguments,
# `effect`: comparative, incremental
# `match_strategy`: a, b, riskscore_i
# `subgroup`: all, age groups
# `outcome` : the dependent variable in the regression model
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----
## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')
library('splines')
library('parglm')
## Import custom user functions from lib
source(here("analysis", "design.R"))
source(here("analysis", "process", "process_functions.R"))
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "redaction.R"))
source(here("lib", "functions", "survival.R"))

# import command-line arguments ----
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  # use for interactive testing
  removeobs <- FALSE
  effect <- "comparative"
  match_strategy <- "a" 
  subgroup <- "all" 
  outcome <- "covidadmitted"
} else {
  removeobs <- TRUE
  effect <- args[[1]]
  match_strategy <- args[[2]]
  subgroup <- args[[3]]
  outcome <- args[[4]]
}

# save items in the match_strategy list to the global environment
list2env(
  x = get(glue("match_strategy_{match_strategy}")),
  envir = environment()
)

# derive symbolic arguments for programming with
subgroup_sym <- sym(subgroup)

# create output directories ----
output_dir <- ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome)
fs::dir_create(output_dir)


# create special log file ----
cat(glue("## script info for {outcome} ##"), "  \n", 
    file = ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_log_{outcome}.txt")), append = FALSE)
## function to pass additional log text
logoutput <- function(...){
  cat(...,  file = ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_log_{outcome}.txt")), sep = "\n  ", append = TRUE)
  cat("\n", file = ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_log_{outcome}.txt")), sep = "\n  ", append = TRUE)
}


# read and process data_matched ----
read_final <- TRUE
source(here("analysis", "process", "process_postmatch.R"))
model <- "plr"
source(here("analysis", "process", "process_premodel.R"))
# read matching IDs - used in bootstrapping procedure 
data_matchids <- read_rds(here("output", glue("{effect}_{match_strategy}"), "match", "data_matchstatus.rds")) %>% filter(matched==TRUE) # all matched individuals


### print dataset size ----
logoutput(
  glue("data_plr data size = ", nrow(data_plr)),
  glue("data_plr memory usage = ", format(object.size(data_plr), units="GB", standard="SI", digits=3L))
)

# optimisation options ----
parglmparams <- parglm.control(
  method = "LINPACK",
  nthreads = 8,
  maxit = 40 # default = 25
)

# make formulas ----
  ## make formulas ----
  ### model 1 - minimally adjusted vaccination effect model, age and sex 
  ### model 2 - fully adjusted vaccination effect model, baseline demographics + clinical characteristics

model_names = c(
  "Adjusting for time, age and sex" = "1",
  "Adjusting for time + demographics and clinical characteristics" = "2"
) 

  # function for splines on event times
nsevents <- function(x, events, df){
  # this is the same as the `ns` function,
  # except the knot locations are chosen
  # based on the event times only, not on all person-time
  probs <- seq(0,df)/df
  q <- quantile(x[events==1], probs=probs)
  ns(x, knots=q[-c(1, df+1)], Boundary.knots = q[c(1, df+1)])
}

  # base formula to be built on 
formula_outcome <- outcome_event ~ 1

  # minimal adjustment set
formula_minadj <- . ~ . + poly(age, degree=2, raw=TRUE) + sex

  # full adjustment sets for each matching strategy 
if(match_strategy == "a") formula_demog <- . ~ . + poly(age, degree=2, raw=TRUE) + sex + ethnicity + imd_Q5 + bmi + asthma + learndis + sev_mental + immunosuppressed + multimorb + timesince_coviddischarged + flu_vaccine_2122 + cancer
if(match_strategy == "b") formula_demog <- . ~ . + poly(age, degree=2, raw=TRUE) + sex + ethnicity + bmi + timesince_coviddischarged + flu_vaccine_2122
if(match_strategy == "riskscore_i") formula_demog <- . ~ . + poly(age, degree=2, raw=TRUE)  + sex + ethnicity + imd_Q5 + bmi + asthma + learndis + sev_mental + immunosuppressed + multimorb + timesince_coviddischarged + flu_vaccine_2122 + cancer

# mimicking timescale / stratification in simple cox models
formula_timescale_ns <- . ~ . + nsevents(tstop, outcome_event, 4) # spline for timescale only
formula_spacetime <- . ~ . + ns(trial_date, 3)#*stp # spline for space-time adjustment 
formula_timesincevax_ns <- . ~ . + treated + treated:nsevents(tstop, outcome_event, 4)

### natural cubic spline formulae ----
### estimands
formula_vaxonly_ns <- formula_outcome %>% update(formula_timesincevax_ns) %>% update(formula_timescale_ns) %>% update(formula_spacetime)

formula1_ns <- formula_vaxonly_ns %>% update(formula_spacetime) %>% update(formula_minadj)
formula2_ns <- formula_vaxonly_ns %>% update(formula_spacetime) %>% update(formula_demog)

# Define program for model fitting and bootstrapping
  # NOTE that in CausaLab code the bootstrap separately for each group 
  # this will mean running bootstrap twice for all the comparisons we need to make which may be unfeasible - have run bootstrap only once
  # may need to update if decided that we should bootstrap separately for comparison groups 

risk.boot <- function(data, indices, formula, boot=FALSE) {

  # Select individuals into each bootstrapped sample
    # NOTE - selecting on match IDs so for incremental comparisons individuals who are untreated may not contribute for both their untreated and treated trials
  ids <- unique(data$match_id)
  boot.matched.ids <- data.frame(match_id = ids[indices])
  boot.ids <- left_join(boot.matched.ids, data, by = "match_id", relationship = "many-to-many")  %>% dplyr::select(patient_id, trial_date) 
  
  # Subset person-time data to individuals selected into the bootstrapped sample
  d <- left_join(boot.ids, data_plr, by = c("patient_id", "trial_date"), relationship = "many-to-many")
  
  ## Fit pooled logistic regression model 
  plr.mod <- parglm(
    formula = formula, 
    data = d,
    family = binomial(link="logit"),
    control = parglmparams,
    na.action = "na.fail",
    model = FALSE
  )
  
  ## Calculate risks at each time point, standardizing and standardizing risks
    # Creating dataset with all time points for each individual under each 
    # treatment level
    
    # Note: everyone will have 120 rows of data, regardless of whether or not they
    # developed the outcome in the original dataset. These datasets will be used 
    # to store and calculate final results
  
    # Had everyone been untreated
  boot_data_tte <- left_join(boot.ids, data_tte, by = c("patient_id", "trial_date"), relationship = "many-to-one") %>%
    mutate(
      boot.newid = row_number()  # new id number for expanding dataset
    )
  surv.results0 <- uncount(boot_data_tte, weights=fup_params$maxfup, .remove=F)
  surv.results0 <- surv.results0 %>% group_by(boot.newid) %>% 
    mutate(
      tstart=row_number()-1, 
      tstop = row_number(), 
      outcome_event = case_when(
        is.na(tte_outcome) ~ 0, 
        tstop < tte_outcome ~ 0, 
        tstop == tte_outcome ~ 1, 
        tstop > tte_outcome ~ 0 # does not make a difference to predicted values whether this value (times after event occurred) is set to 0 or 1 
      )
    ) 
  surv.results0$treated <- 0
  
    # Had everyone been treated
  surv.results1 <- surv.results0
  surv.results1$treated <- 1
 
  ## Calculating risks from hazards
    # Hazards based on predicted probabilities from PLR
  surv.results0$hazard0 <- predict(plr.mod, newdata=surv.results0, type="response")  
  surv.results1$hazard1 <- predict(plr.mod, newdata=surv.results1, type="response")  
  
    # Survival from cumulative product of (1-hazard) for each individual
  surv.results0 <- arrange(surv.results0, boot.newid, tstart)
  surv.results1 <- arrange(surv.results1, boot.newid, tstart)
  
  surv.results0 <- surv.results0 %>% group_by(boot.newid) %>% mutate(surv0 = cumprod(1-hazard0))
  surv.results1 <- surv.results1 %>% group_by(boot.newid) %>% mutate(surv1 = cumprod(1-hazard1))
  
    # Estimate risks from survival probabilities
    # Risk = 1 - S(t)
  surv.results0$risk0 <- 1 - surv.results0$surv0
  surv.results1$risk1 <- 1 - surv.results1$surv1
  
  ## Standardization by averaging
    # Note: we calculate the averages stratified by time point
  surv.results0 <- surv.results0 %>% ungroup() %>% dplyr::select(treated, tstart, surv0, risk0) # select columns to prevent aggregating unnecessary columns 
  surv.results1 <- surv.results1 %>% ungroup() %>% dplyr::select(treated, tstart, surv1, risk1)
  
  surv.results0 <- aggregate(surv.results0, by=list(surv.results0$tstart), FUN=mean)[c("treated", "tstart", "surv0", "risk0")]
  surv.results1 <- aggregate(surv.results1, by=list(surv.results1$tstart), FUN=mean)[c("treated", "tstart", "surv1", "risk1")]
  
  # Estimate causal comparisons at each time 
  surv.results <- merge(surv.results0, surv.results1, by=c("tstart")) %>% dplyr::select(-treated.x, -treated.y) %>%  
    mutate(
      rd = risk1-risk0, 
      rr = risk1/risk0 
    ) %>% dplyr::select(tstart, risk0, risk1, rd, rr)
  
  if(boot == TRUE) {
    return(surv.results)
  }
  else {
    return(list(surv.results = surv.results, 
                model = plr.mod))
  }
} 

# create function to fit model and run manual bootstrapping 
  # Manually running bootstrapping - function above does not work within boot command - possibly due to conflicting methods for parallelisation
modfit <- function(model_num, nboot){
  
  if(model_num == 1){frm = formula1_ns} 
  if(model_num == 2){frm = formula2_ns} 
  
  # Obtain model estimates 
  holder <- risk.boot(data = data_matchids, indices = seq(1:length(unique(data_matchids$match_id))), formula = frm, boot = FALSE) 
  cat("Fit plr model  \n")
  ests <- as.data.frame(holder["surv.results"])
  colnames(ests) <- names(holder$surv.results)
  plr.model <- holder["model"]
  rm(holder)
  
    ## Save to log and output model 
  print(warnings())
  logoutput(
    glue("model data size = ", nrow(plr.model$model$data)),
    glue("model memory usage = ", format(object.size(plr.model), units="GB", standard="SI", digits=3L)),
    glue("convergence status: ", plr.model$model$converged)
  )
  plr.model$model$data <- NULL 
  write_rds(plr.model$model, ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("plr_model{model_num}.rds")), compress="gz")
  cat("Saved model  \n")
  
  cat("Beginning bootstrapping  \n")
      # creating placeholder datasets for bootstrap samples 
  boot.risk0 <- data.frame(tstart=seq(0,fup_params$maxfup-1))
  boot.risk1 <- data.frame(tstart=seq(0,fup_params$maxfup-1))
  boot.rd    <- data.frame(tstart=seq(0,fup_params$maxfup-1))
  boot.rr    <- data.frame(tstart=seq(0,fup_params$maxfup-1))
  
    # bootstrap to get 95% CI 
  set.seed(4757976)
  nbootstraps = nboot
  for(i in 1:nbootstraps) {
    print(i)
    indices <- sample(seq(1:length(unique(data_matchids$match_id))), length(unique(data_matchids$match_id)), replace=TRUE) 
    risk_ests <- risk.boot(data = data_matchids, indices = indices, formula = frm, boot = TRUE) 
    colnames(risk_ests)[which(colnames(risk_ests)== "risk0")] <-  glue(colnames(risk_ests)[which(colnames(risk_ests)== "risk0")], "_", i) # risk untreated
    colnames(risk_ests)[which(colnames(risk_ests)== "risk1")] <-  glue(colnames(risk_ests)[which(colnames(risk_ests)== "risk1")], "_", i) # risk treated
    colnames(risk_ests)[which(colnames(risk_ests)== "rd")]    <-  glue(colnames(risk_ests)[which(colnames(risk_ests)== "rd")], "_", i) # risk difference
    colnames(risk_ests)[which(colnames(risk_ests)== "rr")]    <-  glue(colnames(risk_ests)[which(colnames(risk_ests)== "rr")], "_", i) # log risk ratio
    
    boot.risk0 <- left_join(boot.risk0, risk_ests[,c(1,which(colnames(risk_ests)== glue("risk0_", i)))], by="tstart") # risk untreated
    boot.risk1 <- left_join(boot.risk1, risk_ests[,c(1,which(colnames(risk_ests)== glue("risk1_", i)))], by="tstart") # risk treated
    boot.rd    <- left_join(boot.rd,    risk_ests[,c(1,which(colnames(risk_ests)== glue("rd_", i)))], by="tstart") # risk difference
    boot.rr    <- left_join(boot.rr,    risk_ests[,c(1,which(colnames(risk_ests)== glue("rr_", i)))], by="tstart") # risk ratio
  }
  cat("Finished bootstrapping  \n")
  
  ests$risk0.lci <- apply(boot.risk0[,-which(names(boot.risk0)=="tstart")], 1, quantile, probs=0.025) # lower quantile from bootstrap  
  ests$risk0.uci <- apply(boot.risk0[,-which(names(boot.risk0)=="tstart")], 1, quantile, probs=0.975) # upper quantile from bootstrap
  ests$risk1.lci <- apply(boot.risk1[,-which(names(boot.risk1)=="tstart")], 1, quantile, probs=0.025)
  ests$risk1.uci <- apply(boot.risk1[,-which(names(boot.risk1)=="tstart")], 1, quantile, probs=0.975)
  ests$rd.lci    <- apply(boot.rd[,-which(names(boot.rd)=="tstart")], 1, quantile, probs=0.025)
  ests$rd.uci    <- apply(boot.rd[,-which(names(boot.rd)=="tstart")], 1, quantile, probs=0.975)
  ests$rr.lci    <- apply(boot.rr[,-which(names(boot.rr)=="tstart")], 1, quantile, probs=0.025)
  ests$rr.uci    <- apply(boot.rr[,-which(names(boot.rr)=="tstart")], 1, quantile, probs=0.975)
  
  
  # output estimates from models 
  write_rds(ests, ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("plr_risk_estimates_model{model_num}.rds")), compress="gz")
  cat("Saved model estimates  \n")
  
  
  # for interactive use 
  if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){ 
    return(list(
        ests = ests, 
        model = plr.model$model
      )
    )
  }
  
  ## print warnings ----
  print(warnings())
  cat("  \n")
  print(gc(reset=TRUE))
}


# call models 
test1 <- modfit(model_num = 1, nboot = 20)
test2 <- modfit(model_num = 2, nboot = 20)








#### MOVE to separate script - will need to output datasets 
# Prepare data
graph.pred <- test1$ests
# Edit data frame to reflect that risks are estimated at the END of each interval
graph.pred$tstop <- graph.pred$tstart + 1
zero <- data.frame(cbind(-1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1))
zero <- setNames(zero,names(graph.pred))
graph <- rbind(zero, graph.pred)

### Construct parametric cumulative incidence (risk) curves ###
## Create plot (without CIs)
plot.plr <- ggplot(graph, 
                   aes(x=tstop)) + # set x and y axes
  geom_line(aes(y = risk1, # create line for vaccine group
                color = comparison_definition$level1_descr[which(comparison_definition$comparison == effect)]),
            linewidth = 1.5) + 
  geom_ribbon(aes(ymin = risk1.lci, ymax = risk1.uci, fill = comparison_definition$level1_descr[which(comparison_definition$comparison == effect)]), alpha = 0.4) +
  geom_line(aes(y = risk0, # create line for no vaccine group
                color = comparison_definition$level0_descr[which(comparison_definition$comparison == effect)]),
            linewidth = 1.5) +
  geom_ribbon(aes(ymin = risk0.lci, ymax = risk0.uci, fill = comparison_definition$level0_descr[which(comparison_definition$comparison == effect)]), alpha = 0.4) +
  xlab("Days") + # label x axis
  scale_x_continuous(limits = c(0, 182), # format x axis
                     breaks=seq(0,182,14)) + 
  ylab("Cumulative Incidence (%)") + # label y axis
  scale_y_continuous(limits=c(0, 0.125), # format y axis
                     breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125),
                     labels=c("0.0%", "2.5%", "5.0%",
                              "7.5%", "10.0%", "12.5%")) + 
  theme_minimal()+ # set plot theme elements
  theme(axis.text = element_text(size=14), legend.position = c(0.2, 0.8),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_color_manual(values=c("#E7B800","#2E9FDF"), # set colors
                     breaks=c(comparison_definition$level1_descr[which(comparison_definition$comparison == effect)],
                              comparison_definition$level0_descr[which(comparison_definition$comparison == effect)]))+ 
  scale_fill_manual(values=c("#E7B800","#2E9FDF"), # set colors
                     breaks=c(comparison_definition$level1_descr[which(comparison_definition$comparison == effect)],
                              comparison_definition$level0_descr[which(comparison_definition$comparison == effect)])) 
# Plot
plot.plr



# risk difference 
plot.rd <- ggplot(graph, 
                   aes(x=tstop)) + # set x and y axes
  geom_line(aes(y = rd, # create line for vaccine group
                color = comparison_definition$level1_descr[which(comparison_definition$comparison == effect)]),
            linewidth = 1.5) + 
  geom_ribbon(aes(ymin = rd.lci, ymax = rd.uci, fill = comparison_definition$level1_descr[which(comparison_definition$comparison == effect)]), alpha = 0.4) +
  xlab("Days") + # label x axis
  scale_x_continuous(limits = c(0, 182), # format x axis
                     breaks=seq(0,182,14)) + 
  ylab("Cumulative Incidence (%)") + # label y axis
  theme_minimal()+ # set plot theme elements
  theme(axis.text = element_text(size=14), legend.position = c(0.2, 0.8),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())#+
plot.rd


plot.rr <- ggplot(graph, 
                  aes(x=tstop)) + # set x and y axes
  geom_line(aes(y = rr, # create line for vaccine group
                color = comparison_definition$level1_descr[which(comparison_definition$comparison == effect)]),
            linewidth = 1.5) + 
  geom_ribbon(aes(ymin = rr.lci, ymax = rr.uci, fill = comparison_definition$level1_descr[which(comparison_definition$comparison == effect)]), alpha = 0.4) +
  xlab("Days") + # label x axis
  scale_x_continuous(limits = c(0, 182), # format x axis
                     breaks=seq(0,182,14)) + 
  ylab("Cumulative Incidence (%)") + # label y axis
  #scale_y_continuous(limits=c(0, 0.175), # format y axis
  #                   breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15,0.175),
  #                   labels=c("0.0%", "2.5%", "5.0%",
  #                            "7.5%", "10.0%", "12.5%", "15.0%", "17.5%")) + 
  theme_minimal()+ # set plot theme elements
  theme(axis.text = element_text(size=14), legend.position = c(0.2, 0.8),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())#+
plot.rr

