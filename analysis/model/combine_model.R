# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine km and cox estimates from different outcomes
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

# import libraries
library(tidyverse)
library(glue)
library(here)

# import command-line arguments
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  effect <- "incremental"
  match_strategy <- "a"
} else {
  effect <- args[[1]]
  match_strategy <- args[[2]]
}

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# define output directory
output_dir <- ghere("output", "{effect}_{match_strategy}", "model")
fs::dir_create(output_dir)

# metaparams for all models that have been run for the given effect
model_args <- model_args[model_args$effect == effect,]

# get all subgroups
subgroups <- model_args %>% distinct(subgroup) %>% pull()

# combine and save outputs ----
combine_and_save_contrasts <- function(
    filenames,
    new_filename = NULL,
    metaparams = model_args
    ) {
  
  model_type <- unique(str_extract(filenames, "km|cox"))
  
  metaparams <- metaparams %>%
    filter(str_detect(model, model_type)) %>%
    uncount(length(filenames), .id="filename") %>% 
    mutate(across(filename, ~filenames[.x])) %>%
    filter(str_detect(filename, model))
  
  rounding_info <- unique(str_extract(filenames, "midpoint\\d+"))
  stopifnot(
    "Do not combine files with different rounding" = 
      length(rounding_info) <= 1
  )
  
  if (length(filenames) > 1) {
    stopifnot(
      "Must specify new_filename when length(filenames) > 1" =
        !is.null(new_filename)
      )
    if (!is.na(rounding_info)) {
      new_filename <- str_c(new_filename, rounding_info, sep = "_")
    }
  } else {
    new_filename <- filenames
  }
  
  metaparams %>%
    mutate(
      data = pmap(
        list(effect, model, subgroup, outcome, filename), 
        function(effect, model, subgroup, outcome, filename)  {
          dat <- try(
            read_csv(
              here("output", glue(effect, "_", match_strategy), "model", model, subgroup, outcome, glue(filename, ".csv"))
              )
            )
          if (inherits(dat, "try-error")) {
            dat <- tibble()
          } else {
            dat <- dat %>%
              add_column(
                subgroup_level = as.character(.[[subgroup]]),
                .before=1
              ) 
          }
          return(dat)
        }
        
      )
    ) %>%
    unnest(data) %>%
    select(-any_of(subgroups)) %>%
    mutate(across(
      starts_with(c("surv", "risk", "inc", "cml.rate", "irr", "cmlirr", "sr", "rd", "rr", "cox")),
      ~round(.x, digits = 5)
    )) %>%
    write_csv(fs::path(output_dir, glue("{new_filename}.csv")))
  
}


# km outputs
combine_and_save_contrasts(
  filenames = glue("km_estimates_midpoint{threshold}")
  ) 
combine_and_save_contrasts(
  filenames = c(
    glue("km_contrasts_cuts_midpoint{threshold}"),
    glue("km_contrasts_overall_midpoint{threshold}")
    ),
  new_filename = "km_contrasts"
  ) 
# cox outputs
combine_and_save_contrasts(
  filenames = c(
    "cox_unadj_contrasts_cuts",
    "cox_adj_contrasts_cuts",
    "cox_unadj_contrasts_overall",
    "cox_adj_contrasts_overall"
    ),
  new_filename = "cox_contrasts"
  )

## plot overall estimates for inspection ----

plot_estimates <- function(.data, estimate, estimate.ll, estimate.ul, name){
  
  colour_labs <- c("comparative", "incremental")
  colour_palette <- RColorBrewer::brewer.pal(n=length(colour_labs), name="Dark2")
  names(colour_palette) <- colour_labs
  
  subgroup_levels_labels <- unlist(unname(recoder[subgroups]))
  
  plot_data <- .data %>%
    mutate(
      subgroup = fct_recoderelevel(subgroup, recoder$subgroups),
      outcome = fct_recoderelevel(outcome,  recoder$outcome),
      subgroup_level = fct_recoderelevel(subgroup_level, subgroup_levels_labels),
      yvar = as.character(if_else(
        subgroup %in% "Main",
        as.character(subgroup),
        as.character(subgroup_level)
      )),
      .before = 1
    ) %>%
    mutate(outcome = fct_relabel(outcome, str_wrap, width=10)) 
  
  ylevels <- plot_data %>%
    distinct(yvar, subgroup, subgroup_level) %>%
    arrange(subgroup, subgroup_level) %>%
    pull(yvar)
  
  plot_temp <- plot_data %>%
    mutate(outcome = fct_relabel(outcome, str_wrap, width=10))  %>%
    mutate(across(yvar, factor, levels = ylevels)) %>%
    ggplot(aes(y=yvar, colour = effect)) +
    geom_vline(aes(xintercept=0), linetype="dotted", colour="darkgrey")+
    geom_point(aes(x={{estimate}}), position=position_dodge(width=-0.3), alpha=0.7)+
    geom_linerange(aes(xmin={{estimate.ll}}, xmax={{estimate.ul}}), position=position_dodge(width=-0.3))+
    facet_grid(rows=vars(yvar), cols=vars(outcome), scales="free", space="free_y", switch="y")+
    scale_x_continuous(expand = expansion(mult=c(0,0.01)))+
    scale_color_manual(values=colour_palette)+
    labs(y=NULL)+
    theme_bw()+
    theme(
      legend.position="bottom",
      axis.text.x.top=element_text(hjust=0),
      
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_blank(),
      strip.placement="outside",
      #strip.text.y.left = element_text(angle=0),
      strip.text.y.left = element_blank(),
      
      panel.border = element_blank(),
      panel.spacing = unit(0.3, "lines"),
    )
  
  ggsave(
    filename=file.path(output_dir, glue("overall_plot_rounded_{name}.png")),
    plot_temp,
    width=30, height=20, units="cm"
  )
  
  plot_temp
  
}

# read data and create plots ----
# km
km_contrasts_overall <- 
  read_csv(fs::path(output_dir, glue("km_contrasts_midpoint{threshold}.csv"))) %>%
  filter(str_detect(filename, "overall"))
km_contrasts_overall %>% plot_estimates(rd, rd.ll, rd.ul, "km_rd")
km_contrasts_overall %>% plot_estimates(rr, rr.ll, rr.ul, "km_rr")

# cox
cox_contrasts_rounded <- read_csv(fs::path(output_dir, glue("cox_contrasts.csv"))) %>%
  filter(str_detect(filename, "overall")) %>%
  filter(str_detect(term, "^treated")) 
# unadjusted
cox_contrasts_rounded %>% 
  filter(model == "cox_unadj") %>%
  plot_estimates(coxhr, coxhr.ll, coxhr.ul, "cox_unadj")
# adjusted
cox_contrasts_rounded %>% 
  filter(model == "cox_adj") %>%
  plot_estimates(coxhr, coxhr.ll, coxhr.ul, "cox_adj")
