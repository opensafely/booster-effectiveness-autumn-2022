# # # # # # # # # # # # # # # # # # # # #
# Purpose: create plots of matching coverage
# arguments: effect
# - effect=comparative: include all boosted individuals, plot coverage 
#   split by brand, matches for comparative effectiveness analysis
# - effect=incremental include all boosted individuals, don't split by brand,
#   matches for relative effectiveness analysis
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----
# Import libraries
library('tidyverse')
library('lubridate')
library('here')
library('glue')

# import local functions and parameters
source(here("analysis", "design.R"))
source(here("analysis", "process", "process_functions.R"))
source(here("lib", "functions", "utility.R"))

# import command-line arguments
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  effect <- "incremental"
  match_strategy <- "a"
} else {
  effect <- args[[1]]
  match_strategy <- args[[2]]
}

effect_match_strategy <- str_c(effect, match_strategy, sep = "_")

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("")) {
  
  # Import released data
  release_date <- "20230522"
  release_dir <- here(glue("release{release_date}"))
  output_dir <- here(glue("report{release_date}"), "figures", effect)
  fs::dir_create(output_dir)
  data_coverage_rounded <- read_csv(file.path(release_dir, str_c("data_coverage_", effect, ".csv")))
  
  
} else {
  
  # create output directories
  output_dir <- here("output", effect_match_strategy, "coverage")
  fs::dir_create(output_dir)
  
  if (effect == "comparative") {
    
    data_matchstatus <- read_rds(here("output", effect_match_strategy, "match", "data_matchstatus.rds"))
    
  }
  
  if (effect == "incremental") {
    
    # get the matched data from all matching rounds
    source(here("analysis", "process", "process_postmatch.R"))
    
    data_matchstatus <- data_matched %>%
      # only keep treated individuals who were successfully matched
      filter(treated == 1) %>%
      select(patient_id, trial_date, match_id) %>%
      # join data from all people eligible for the treated group to include 
      # unmatched treated individuals
      right_join(
        read_rds(here("output", "treated", "eligible", "data_treated.rds")) %>%
          select(patient_id, trial_date = vax_boostautumn_date), 
        by = c("patient_id", "trial_date")
        ) %>%
      mutate(
        treated = 1,
        matched = !is.na(match_id)
      )
      
  }
  
  # match coverage
  data_coverage <-
    data_matchstatus %>%
    group_by(treated, trial_date) %>%
    summarise(
      n_eligible = n(),
      n_matched = sum(matched, na.rm=TRUE),
      .groups = "keep"
    ) %>%
    ungroup() %>%
    mutate(
      n_unmatched = n_eligible - n_matched,
    ) %>%
    pivot_longer(
      cols = c(n_unmatched, n_matched),
      names_to = "status",
      names_prefix = "n_",
      values_to = "n"
    ) %>%
    arrange(treated, trial_date, status) %>%
    group_by(treated, trial_date, status) %>%
    summarise(
      n = sum(n),
      .groups = "keep"
    ) %>%
    ungroup(trial_date) %>%
    complete(
      trial_date = full_seq(.$trial_date, 1), # go X days before to
      fill = list(n=0)
    ) %>%
    mutate(
      cumuln = cumsum(n)
    ) %>%
    ungroup() %>%
    arrange(treated, status, trial_date) 
  
  # save for release
  data_coverage_rounded <-
    data_coverage %>%
    group_by(treated, status) %>%
    mutate(
      cumuln = roundmid_any(cumuln, to = threshold),
      n = diff(c(0,cumuln)),
    ) %>%
    ungroup()
  
  write_csv(data_coverage_rounded, fs::path(output_dir, "data_coverage.csv"))
  
}

## plot match coverage ----

data_plot <- data_coverage_rounded %>%
  mutate(
    n=n*((treated*2) - 1),
    cumuln=cumuln*((treated*2) - 1)
  ) %>%
  add_descr(vars = c("treated", "status"), effect = effect, remove = TRUE) 
  

xmin <- min(data_plot$trial_date)
xmax <- max(data_plot$trial_date)+1

# this is necessary because there is an older version of a package in 
# opensafely and I think it requires breaks to be unique
if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("")) {
  y_labels <- ~scales::label_number(accuracy = 1, big.mark=",")(abs(.x))
} else {
  y_labels <- waiver()
}

colour_palette <- list(
  comparative = c(
    "#e7298a", # dark pink 
    "#7570b3" # dark purple 
  ),
  incremental = c(
    # change this to something different from comparative
    "#d95f02", # orange
    "#1b9e77" # green
  )
)
names(colour_palette[[effect]]) <- names(recoder[[effect]])

# plot daily coverage
plot_coverage_n <-
  data_plot %>%
  ggplot() +
  geom_col(
    aes(
      x=trial_date+0.5,
      y=n,
      group=paste0(treated_descr, status_descr),
      fill=treated_descr,
      alpha=status_descr,
      # alpha=fct_rev(status),
      colour=NULL
    ),
    position=position_stack(reverse=TRUE),
    width=1
  ) +
  geom_hline(yintercept = 0, colour="black") +
  scale_x_date(
    breaks = unique(lubridate::ceiling_date(data_coverage_rounded$trial_date, "1 month")),
    limits = c(xmin-1, NA),
    labels = scales::label_date("%b %Y"),
    expand = expansion(add=7),
  ) +
  scale_y_continuous(
    labels = y_labels,
    expand = expansion(c(0, NA)),
  ) +
  scale_fill_manual(values = colour_palette[[effect]]) +
  scale_colour_manual(values = colour_palette[[effect]]) +
  scale_alpha_discrete(range= c(0.8,0.4))+
  labs(
    x="Date",
    y="Booster vaccines per day",
    colour=NULL,
    fill=NULL,
    alpha=NULL
  ) +
  theme_minimal() +
  theme(
    axis.line.x.bottom = element_line(),
    axis.text.x.top=element_text(hjust=0),
    strip.text.y.right = element_text(angle = 0),
    axis.ticks.x=element_line(),
    legend.position = "bottom"
  ) +
  NULL

ggsave(plot_coverage_n, filename="coverage_count.png", path=output_dir)

# plot cumulative daily coverage
plot_coverage_cumuln <-
  data_plot %>%
  ggplot()+
  geom_area(
    aes(
      x=trial_date+0.5,
      y=cumuln,
      group=paste0(treated_descr, status_descr),
      fill=treated_descr,
      alpha=status_descr,
      # alpha=fct_rev(status),
      colour=NULL
    ),
    position=position_stack(reverse=TRUE),
    width=1
  ) +
  geom_rect(xmin=xmin, xmax= xmax+1, ymin=-6, ymax=6, fill="grey", colour="transparent")+
  scale_x_date(
    breaks = unique(lubridate::ceiling_date(data_coverage_rounded$trial_date, "1 month")),
    limits = c(xmin-1, NA),
    labels = scales::label_date("%b %Y"),
    expand = expansion(add=7),
  )+
  scale_y_continuous(
    labels = y_labels,
    expand = expansion(c(0, NA))
  )+
  scale_fill_manual(values = colour_palette[[effect]]) +
  scale_colour_manual(values = colour_palette[[effect]]) +
  scale_alpha_discrete(range= c(0.8,0.4))+
  labs(
    x="Date",
    y="Cumulative booster vaccines",
    colour=NULL,
    fill=NULL,
    alpha=NULL
  ) +
  theme_minimal()+
  theme(
    axis.line.x.bottom = element_line(),
    axis.title.y = element_text(margin = margin(r=10)),
    axis.text.x.top=element_text(hjust=0),
    strip.text.y.right = element_text(angle = 0),
    axis.ticks.x=element_line(),
    legend.position = "bottom"
  )+
  NULL

ggsave(plot_coverage_cumuln, filename="coverage_stack.png", path=output_dir)
