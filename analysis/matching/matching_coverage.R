# # # # # # # # # # # # # # # # # # # # #
# Purpose: describe matching results
# reports on matching coverage
# # # # # # # # # # # # # # # # # # # # #


# Preliminaries ----


## Import libraries ----
library('tidyverse')
library('lubridate')
library('here')
library('glue')
library('arrow')

## import local functions and parameters ---

source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "redaction.R"))

# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  cohort <- "mrna"
} else {
  #FIXME replace with actual eventual action variables
  cohort <- args[[1]]
}


## get cohort-specific parameters study dates and parameters ----
dates <- study_dates[[cohort]]


if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("")) {
  
  ## Import released data ----
  release_dir <- "release20230105"
  
  output_dir <- here("manuscript")
  fs::dir_create(output_dir)
  
  data_coverage_rounded <- read_csv(here(release_dir, "matching", "data_coverage.csv"))
  
  
} else {
  
  ## create output directories ----
  
  output_dir <- here("output", cohort, "match", "coverage")
  fs::dir_create(output_dir)
  
  ## Import data and derive some variables ----
  data_matched <- read_rds(ghere("output", cohort, "match", "data_matched.rds")) 
  
  data_treatedeligible_matchstatus <- read_rds(here("output", cohort, "match", "data_treatedeligible_matchstatus.rds"))
  
  # matching coverage for boosted people
  data_coverage <-
    data_treatedeligible_matchstatus %>%
    mutate(eligible=1) %>%
    group_by(vax3_type, vax3_date) %>%
    summarise(
      n_eligible = sum(eligible, na.rm=TRUE),
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
    arrange(vax3_type, vax3_date, status) %>%
    group_by(vax3_type, vax3_date, status) %>%
    summarise(
      n = sum(n),
      .groups = "keep"
    ) %>%
    ungroup(vax3_date) %>%
    complete(
      vax3_date = full_seq(.$vax3_date, 1), # go X days before to
      fill = list(n=0)
    ) %>%
    mutate(
      cumuln = cumsum(n)
    ) %>%
    ungroup() %>%
    mutate(
      status = factor(status, levels=c("unmatched", "matched")),
    ) %>%
    arrange(vax3_type, status, vax3_date) 
  
  # save for release
  data_coverage_rounded <-
    data_coverage %>%
    group_by(vax3_type, status) %>%
    mutate(
      cumuln = roundmid_any(cumuln, to = threshold),
      n = diff(c(0,cumuln)),
    ) %>%
    ungroup()
  
  write_csv(data_coverage_rounded, fs::path(output_dir, "data_coverage.csv"))
  
}

## plot matching coverage ----

data_plot <- data_coverage_rounded %>%
  mutate(
    vax3_type_indicator = vax3_type=="pfizer",
    treatment_descr = fct_recoderelevel(as.character(vax3_type), recoder$vax3_type),
    status_descr = fct_recoderelevel(status, recoder$status),
    n=n*((vax3_type_indicator*2) - 1),
    cumuln=cumuln*((vax3_type_indicator*2) - 1)
  )

xmin <- min(data_plot$vax3_date )
xmax <- max(data_plot$vax3_date )+1

# this is necessary because there is an older version of a package in opensafely and I think it requires breaks to be unique
if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("")) {
  y_labels <- ~scales::label_number(accuracy = 1, big.mark=",")(abs(.x))
} else {
  y_labels <- waiver()
}

colour_palette <- c(
  "BNT162b2" = "#e7298a", # dark pink 
  "mRNA-1273" = "#7570b3" # dark purple 
)

plot_coverage_n <-
  data_plot %>%
  ggplot() +
  geom_col(
    aes(
      x=vax3_date+0.5,
      y=n,
      group=paste0(vax3_type,status),
      fill=treatment_descr,
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
    breaks = unique(lubridate::ceiling_date(data_coverage_rounded$vax3_date, "1 month")),
    limits = c(xmin-1, NA),
    labels = scales::label_date("%b %Y"),
    expand = expansion(add=7),
  ) +
  scale_y_continuous(
    labels = y_labels,
    expand = expansion(c(0, NA)),
  ) +
  scale_fill_manual(values = colour_palette) +
  scale_colour_manual(values = colour_palette) +
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
      x=vax3_date+0.5,
      y=cumuln,
      group=paste0(vax3_type,status),
      fill=treatment_descr,
      alpha=status,
      # alpha=fct_rev(status),
      colour=NULL
    ),
    position=position_stack(reverse=TRUE),
    width=1
  )+
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
  scale_fill_manual(values = colour_palette) +
  scale_colour_manual(values = colour_palette) +
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
