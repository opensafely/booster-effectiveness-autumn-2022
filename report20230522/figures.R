library(tidyverse)
library(readr)
library(here)
library(glue)
library(flextable)

source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("analysis", "process", "process_functions.R"))


# import command-line arguments
args <- commandArgs(trailingOnly=TRUE)
release_date <- args[[1]]

# create output directories
releasedir <- here(glue("release{release_date}"))
outdir <- here(glue("report{release_date}"), "figures")
fs::dir_create(file.path(outdir, "relative"))
fs::dir_create(file.path(outdir, "comparative"))

# import data
km_estimates_rounded <- read_csv(
  here(releasedir, "km_estimates_rounded.csv")
) 

km_contrasts_rounded <- read_csv(
  here(releasedir, "km_contrasts_rounded.csv")
) 

cox_contrasts_rounded <- read_csv(
  here(releasedir, "cox_contrasts_rounded.csv")
) 

km_plot <- function(.data, select_effect, select_subgroup_level, trunc_fu = NULL) {
  
  title_string <- ""
  if (select_subgroup_level != "all") {
    subgroup_name <- names(recoder$agegroup_match[recoder$agegroup_match==select_subgroup_level])
    title_string <- glue("Subgroup = {subgroup_name}")
  } 
  
  if (!is.null(trunc_fu)) {
    .data <- .data %>% filter(time <= trunc_fu)
  }
  
  p <- .data %>%
    filter(subgroup_level == select_subgroup_level, effect == select_effect) %>%
    group_by(effect, subgroup, treated, outcome) %>%
    group_modify(
      ~add_row(
        .x,
        time=0,
        lagtime=0,
        leadtime=1,
        #interval=1,
        surv=1,
        surv.ll=1,
        surv.ul=1,
        risk=0,
        risk.ll=0,
        risk.ul=0,
        .before=0
      )
    ) %>%
    add_descr(vars = c("treated", "outcome"), effect = select_effect) %>%
    mutate(across(outcome_descr, ~fct_relabel(.x, str_wrap, width = 12))) %>%
    ggplot(aes(group=treated_descr, colour=treated_descr, fill=treated_descr)) +
    geom_step(aes(x=time, y=risk), direction="vh") +
    geom_step(aes(x=time, y=risk), direction="vh", linetype="dashed", alpha=0.5) +
    geom_rect(aes(xmin=lagtime, xmax=time, ymin=risk.ll, ymax=risk.ul), alpha=0.1, colour="transparent")+
    facet_grid(
      rows=vars(outcome_descr), 
      switch = "y",
      scales = "free_y"
      ) +
    scale_color_brewer(type="qual", palette="Set1", na.value="grey") +
    scale_fill_brewer(type="qual", palette="Set1", guide="none", na.value="grey") +
    scale_x_continuous(breaks = seq(0,600,14)) +
    scale_y_continuous(expand = expansion(mult=c(0,0.01))) +
    coord_cartesian(xlim=c(0, NA)) +
    labs(
      x="Days",
      y="Cumulative incidence",
      colour=NULL,
      subtitle = title_string,
      title=NULL,
      caption = glue("Outcomes that rely on hospital admissions data are censored at {study_dates$hospitalisationend}.")
    ) +
    theme_bw() +
    theme(
      axis.line.x = element_line(colour = "black"),
      panel.grid.minor.x = element_blank(),
      
      legend.position=c(.03,.97),
      legend.justification = c(0,1),
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      
      panel.grid.minor.y = element_blank(),
      panel.spacing = unit(0.8, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, size = 10),
      
    )
  
  ggsave(
    plot = p,
    filename = file.path(outdir, select_effect, str_c(str_c("km", select_effect, select_subgroup_level, trunc_fu, sep = "_"), ".png")),
    width = 16, height = 22, units = "cm"
  )
  
  # return(p)
  
}


### cox plots

hr_plot <- function(.data, select_effect, select_subgroup_level) {
  
  comparison <- str_c(names(recoder[[select_effect]][2:1]), collapse = " vs ")
  title_string <- glue("{str_to_title(select_effect)} effectiveness: {comparison}")
  caption_string <- paste0(
    glue("Outcomes that rely on hospital admissions data are censored at {study_dates$hospitalisationend}."),
    "\nLighter points are unajusted estimates, darker points are adjusted estimates."
  )
  leg_pos <- "none"
  select_subgroup <- "all"
  if (!all(select_subgroup_level == "all")) {
    leg_pos <- c(0.5,0.05)
    select_subgroup <- "agegroup_match"
    # leg_pos <- "bottom"
  } 
  
  postbaselinecuts <- fup_params$postbaselinecuts
  
  position_dodge_val <- 12
  
  primary_vax_y1 <- list(breaks = c(0.05, 0.2, 0.5, 1, 2), limits = c(0.05, 2))
  primary_vax_y2 <- list(breaks = c(0,0.5,0.8, 0.95))
  
  formatpercent100 <- function(x,accuracy) {
    formatx <- scales::label_percent(accuracy)(x)
    
    if_else(
      formatx==scales::label_percent(accuracy)(1),
      paste0(">",scales::label_percent(1)((100-accuracy)/100)),
      formatx
    )
  }
  
  p <- .data %>%
    filter(effect == select_effect, subgroup_level %in% select_subgroup_level) %>%
    filter("treated" %in% term) %>% 
    mutate(across(term, ~str_extract(.x, "\\d+-\\d+$"))) %>% 
    filter(!is.na(term)) %>%
    mutate(
      model = factor(model, levels = c("cox_unadj", "conx_adj")),
      period_start = as.integer(str_extract(term, "^\\d+")),
      period_end = as.integer(str_extract(term, "\\d+$")),
      midpoint = period_start + (period_end-period_start)/2
    ) %>%
    add_descr(vars = c("outcome", "subgroup_level"), subgroup = select_subgroup) %>%
    mutate(across(outcome_descr, ~fct_relabel(.x, str_wrap, width = 12))) %>%
    ggplot(
      aes(x = midpoint, colour = subgroup_level_descr, alpha = model)
    ) +
    geom_hline(aes(yintercept=1), colour='grey') +
    geom_linerange(
      aes(ymin = coxhr.ll, ymax = coxhr.ul),
      position = position_dodge(width = position_dodge_val)
    ) +
    geom_point(
      aes(y = coxhr),
      size = 0.75,
      position = position_dodge(width = position_dodge_val),
      size = 2
    ) +
    facet_grid(
      rows = vars(outcome_descr), 
      switch = "y", 
      scales = "free", 
      space = "free_x"
      ) +
    scale_x_continuous(
      name = "Days since third dose",
      breaks = postbaselinecuts,
      limits = c(min(postbaselinecuts), max(postbaselinecuts)),
      expand = expansion(
        add = c(0,0)
        # add=c(
        #   min(.data$midpoint) + 7,
        #   postbaselinecuts[length(postbaselinecuts)] - max(.data$midpoint) + 7
        # )
      )
    ) +
    scale_y_log10(
      name = "Hazard ratio",
      breaks = primary_vax_y1[["breaks"]],
      limits = primary_vax_y1[["limits"]],
      oob = scales::oob_keep,
      sec.axis = sec_axis(
        ~(1-.),
        name="Vaccine effectiveness = 100 x (1 - hazard ratio)",
        breaks = primary_vax_y2[["breaks"]],
        labels = function(x){formatpercent100(x, 1)}
      )
    ) +
    scale_color_discrete(name = NULL) +
    scale_alpha_manual(values = c("cox_unadj" = 0.4, "cox_adj" = 1), guide = "none") +
    guides(colour = guide_legend(nrow = 1)) +
    # scale_shape_manual(name = NULL, values = shape_palette) +
    labs(
      caption = caption_string,
      subtitle = title_string
    ) +
    # theme_bw(base_size = 12) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      axis.line.y = element_line(colour = "black"),
      
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.spacing = unit(0.8, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, size = 10),
      
      plot.title = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, face= "italic"),
      
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y.left = element_text(margin = margin(r=2.5)),
      axis.title.y.right = element_text(margin = margin(l=10)),
      
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      # legend.box = "horizontal",
      legend.position = leg_pos
      
    ) + NULL
  
  # return(p)
  
  filename_subgroup = if_else(all(select_subgroup_level%in%"all"), "all", "age")

  ggsave(
    plot = p,
    filename = file.path(outdir, select_effect, str_c(str_c("cox", select_effect, filename_subgroup, sep = "_"), ".png")),
    width = 16, height = 22, units = "cm"
  )
  
}


for (select_effect in km_estimates_rounded %>% distinct(effect) %>% pull(effect)) {
  cox_contrasts_rounded %>% hr_plot(select_effect, select_subgroup_level = "all")
  cox_contrasts_rounded %>% hr_plot(
    select_effect, 
    select_subgroup_level = cox_contrasts_rounded %>% filter(subgroup!="all") %>% distinct(subgroup_level) %>% pull(subgroup_level)
  )
  for (select_subgroup_level in km_estimates_rounded %>% distinct(subgroup_level) %>% pull(subgroup_level)) {
    km_estimates_rounded %>% km_plot(select_effect, select_subgroup_level)
    km_estimates_rounded %>% km_plot(select_effect, select_subgroup_level, trunc_fu = 28)
  }
}
