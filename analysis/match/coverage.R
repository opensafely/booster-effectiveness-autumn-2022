# # # # # # # # # # # # # # # # # # # # #
# Purpose: describe match results
# reports on match coverage
# # # # # # # # # # # # # # # # # # # # #


# Preliminaries ----


## Import libraries ----
library('tidyverse')
library('lubridate')
library('here')
library('glue')

## import local functions and parameters ---

source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  effect <- "relative"
  # effect <- "relative"
} else {
  #FIXME replace with actual eventual action variables
  effect <- args[[1]]
}

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("")) {
  
  ## Import released data ----
  release_dir <- "release20230105"
  
  output_dir <- here("manuscript")
  fs::dir_create(output_dir)
  
  data_coverage_rounded <- read_csv(here(release_dir, "match", "data_coverage.csv"))
  
  
} else {
  
  ## create output directories ----
  
  output_dir <- here("output", effect, "coverage")
  fs::dir_create(output_dir)
  
  if (effect == "comparative") {
    data_matchstatus_path <- here("output", "treated", "match", "data_matchstatus.rds")
  }
  if (effect == "relative") {
    data_matchstatus_path <- ghere("output", "matchround{n_match_rounds}", "controlactual", "match", "data_matchstatus_allrounds.rds")
  }
  data_matchstatus <- read_rds(data_matchstatus_path)
  
  
  # match coverage for boosted people
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
    # mutate(
    #   status = factor(status, levels=unname(recoder$status)),
    # ) %>%
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
    # vax3_type_indicator = vax3_type=="pfizer",
    treated_descr = fct_recoderelevel(treated, recoder[[effect]]), #TODO
    status_descr = fct_recoderelevel(status, recoder$status), #TODO
    n=n*((treated*2) - 1),
    cumuln=cumuln*((treated*2) - 1)
  )

xmin <- min(data_plot$trial_date)
xmax <- max(data_plot$trial_date)+1

# this is necessary because there is an older version of a package in opensafely and I think it requires breaks to be unique
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
  relative = c(
    # change thes to something different from comparative
    "#e7298a", # dark pink 
    "#7570b3" # dark purple 
  )
)
names(colour_palette[[effect]]) <- names(recoder[[effect]])

plot_coverage_n <-
  data_plot %>%
  ggplot() +
  geom_col(
    aes(
      x=trial_date+0.5,
      y=n,
      group=paste0(treated, status),
      fill=treated_descr,
      alpha=status,
      # alpha=fct_rev(status),
      colour=NULL
    ),
    position=position_stack(reverse=TRUE),
    #alpha=0.8,
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

plot_coverage_n

ggsave(plot_coverage_n, filename="coverage_count.png", path=output_dir)

plot_coverage_cumuln <-
  data_plot %>%
  ggplot()+
  geom_area(
    aes(
      x=trial_date+0.5,
      y=cumuln,
      group=paste0(treated_descr,status),
      fill=treated_descr,
      alpha=status,
      # alpha=fct_rev(status),
      colour=NULL
    ),
    position=position_stack(reverse=TRUE),
    width=1
  ) +
  geom_rect(xmin=xmin, xmax= xmax+1, ymin=-6, ymax=6, fill="grey", colour="transparent")+
  scale_x_date(
    breaks = unique(lubridate::ceiling_date(data_coverage_rounded$vax3_date, "1 month")),
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

plot_coverage_cumuln

ggsave(plot_coverage_cumuln, filename="coverage_stack.png", path=output_dir)
