# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine km and cox estimates from different outcomes
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

# import libraries
library(tidyverse)
library(glue)
library(here)

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# define output directory
output_dir <- ghere("output", "report", "model")
fs::dir_create(output_dir)

# metaparams for all models that have been run
model_args

# get all subgroups
subgroups <- model_args %>% distinct(subgroup) %>% pull()

# combine and save outputs ----
combine_and_save_contrasts <- function(model_type, filenames, metaparams = model_args) {
  
  metaparams <- metaparams %>%
    filter(str_detect(model, model_type)) %>%
    uncount(length(filenames), .id="filename") %>% 
    mutate(across(filename, ~filenames[.x]))
  
  filename_stem <- unique(str_remove(metaparams$filename, "_.+"))
  
  metaparams %>%
    mutate(
      data = pmap(
        list(effect, model, subgroup, outcome, filename), 
        function(effect, model, subgroup, outcome, filename)  {
          dat <- try(
            read_csv(
              here("output", effect, "model", model, subgroup, outcome, glue("{model}_{filename}_rounded.csv"))
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
      round, digits=5
    )) %>%
    write_csv(fs::path(output_dir, glue("{model_type}_{filename_stem}_rounded.csv")))
  
}


# km outputs
combine_and_save_contrasts(model_type="km", filenames = c("estimates")) 
combine_and_save_contrasts(model_type="km", filenames = c("contrasts_cuts", "contrasts_overall")) 
# cox outputs
combine_and_save_contrasts(model_type="cox", filenames = c("contrasts_cuts", "contrasts_overall"))

## plot overall estimates for inspection ----

plot_estimates <- function(.data, estimate, estimate.ll, estimate.ul, name){
  
  colour_labs <- c("comparative", "relative")
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
    filename=glue("output", "report", "model", "overall_plot_rounded_{name}.png", .sep="/"),
    plot_temp,
    width=30, height=20, units="cm"
  )
  
  plot_temp
  
}

# read data and create plots ----
km_contrasts_overall <- 
  read_csv(fs::path(output_dir, glue("km_contrasts_rounded.csv"))) %>%
  filter(filename == "contrasts_overall")
km_contrasts_overall %>% plot_estimates(rd, rd.ll, rd.ul, "km_rd")
km_contrasts_overall %>% plot_estimates(rr, rr.ll, rr.ul, "km_rr")


cox_contrasts_rounded <- read_csv(fs::path(output_dir, glue("cox_contrasts_rounded.csv"))) %>%
  filter(filename == "contrasts_overall") %>%
  filter(str_detect(term, "^treated")) 

cox_contrasts_rounded %>% 
  filter(model == "cox_unadj") %>%
  plot_estimates(coxhr, coxhr.ll, coxhr.ul, "cox_unadj")

cox_contrasts_rounded %>% 
  filter(model == "cox_adj") %>%
  plot_estimates(coxhr, coxhr.ll, coxhr.ul, "cox_adj")
