# # # # # # # # # # # # # # # # # # # # #
# Purpose: Output Figure of KM plots and period specific HRs
# # # # # # # # # # # # # # # # # # # # #

# import libraries
library(tidyverse)
library(glue)
library(here)
library(gridExtra)
library(forcats)

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "survival.R"))
source(here("analysis", "process", "process_functions.R"))

# set effect and match strategy - file run separately for each effect and match strategy
effect = "incremental" # "comparative" or "incremental"
match_strategy = "a" # "a" or "b"

# define output directory
output_dir <- ghere("output", "postrelease", "{effect}_{match_strategy}", "model")

# load data produced in Reanalysis_KM_output.r
KM_dat <- read_csv(fs::path(output_dir, glue("KM_restimate_output.csv"))) %>% 
    filter(subgroup == "all")
HR_dat <- read_csv(fs::path(output_dir, glue("HR_restimate_output.csv"))) %>%
    select(subgroup, subgroup_level, outcome, period, 
           mantel_cox_hr, se_log_hr, hr_lower, hr_upper) %>%
    filter(subgroup == "all")
HR_dat_period <- read_csv(fs::path(output_dir, glue("HR_restimate_period_output.csv"))) %>%
    select(subgroup, subgroup_level, outcome, period, 
           mantel_cox_hr, se_log_hr, hr_lower, hr_upper) %>%
     filter(subgroup == "all")


# set parameters for plotting HR
incremental_lab <- paste0(comparison_definition$level1_descr[which(comparison_definition$comparison=="incremental")],
                        " vs. ",
                        comparison_definition$level0_descr[which(comparison_definition$comparison=="incremental")], 
                        " (reference)")
comparative_lab <- paste0(comparison_definition$level1_descr[which(comparison_definition$comparison=="comparative")],
                        " vs. ",
                        comparison_definition$level0_descr[which(comparison_definition$comparison=="comparative")], 
                        " (reference)")

if(effect=="incremental"){
    ypostext = c(0.125, 2.5)
    brks = c(0.125, 0.25, 0.5, 1, 2, 4)
    brks_ve = c(0, 0.5, 0.75, 0.875)
} else if(effect=="comparative"){
    ypostext = c(0.3, 2.5)
    brks = c(0.125, 0.25, 0.375, 0.5, 0.75, 1, 1.5, 2, 3, 4)
} 

colour_labs_HR <- c("Main", "50-64 years", "65-74 years", "75+ years", "Not clinically vulnerable", "Clinically vulnerable", "Total follow-up")
colour_palette_HR <- RColorBrewer::brewer.pal(n=length(colour_labs_HR), name="Dark2")
names(colour_palette_HR) <- colour_labs_HR
shapes_palette_HR <- c(15, 15, 16, 17, 15, 16, 15)
names(shapes_palette_HR) <- colour_labs_HR

total_x_pos = 420

subgroup_levels_labels <- unlist(unname(recoder[subgroups]))

formatpercent100 <- function(x,accuracy) {
    formatx <- scales::label_percent(accuracy, drop0trailing=TRUE)(x)
    if_else(
    formatx==scales::label_percent(accuracy)(1),
    paste0(">",scales::label_percent(1)((100-accuracy)/100)),
    formatx
    )
}

# prepare HR data for plotting
HR_comb_dat <- rbind(HR_dat, HR_dat_period) %>%
    mutate(
      subgroup = fct_recoderelevel(subgroup, recoder$subgroups),
      outcome = fct_recoderelevel(outcome,  recoder$outcome),
      subgroup_level = fct_recoderelevel(subgroup_level, subgroup_levels_labels),
      subgroup_plot_init = case_when(
        period == 0 ~ "Total follow-up",
        TRUE ~  as.character(subgroup_level)
      ),
      subgroup_plot = fct_expand(factor(subgroup_plot_init), "Total follow-up"),
    
      period_mid = case_when(
        period == 0 ~ total_x_pos, 
        period == 1 ~ (1+14)/2,
        period == 2 ~ (14+70)/2,
        period == 3 ~ (70+126)/2,
        period == 4 ~ (126+182)/2,
        period == 5 ~ (182+238)/2,
        period == 6 ~ (238+294)/2,
        period == 7 ~ (294+350)/2
      ), 

      effect_lab = case_when(
        effect == "incremental" ~ incremental_lab,
        effect == "comparative" ~ comparative_lab
      ),
      effect_fct = factor(effect_lab, levels=c(incremental_lab, comparative_lab))

    ) 
#view(HR_comb_dat)


# Create HR plot
plot_temp_HR <- HR_comb_dat %>% 
    ggplot(aes(colour = subgroup_plot, shape = subgroup_plot)) +
    geom_hline(aes(yintercept=1), linetype="dashed", colour="darkgrey")+
    geom_vline(aes(xintercept=fup_params$postbaselinecuts[14]+40), linetype="solid", colour="black") +
    geom_point(aes(y=mantel_cox_hr, x=period_mid), alpha=0.7)+
    geom_linerange(aes(ymin=hr_lower, ymax=hr_upper, x=period_mid))+
    facet_grid(rows=vars(outcome), cols=vars(effect_fct), switch="y") +
    scale_x_continuous(breaks = c(-5, 21,fup_params$postbaselinecuts[c(4,6,8,10,12,14)], total_x_pos),
                       labels = c(paste0("\n",c(1,fup_params$postbaselinecuts[c(2,4,6,8,10,12,14)])), "Total\nfollow-up"), 
                       limits = c(-5, total_x_pos+5)
                       )+
    xlab("Days since vaccination")+
    scale_color_manual(name=NULL, values=colour_palette_HR, na.translate = FALSE)+
    scale_shape_manual(name=NULL, values=shapes_palette_HR, na.translate = FALSE)+
    labs(y=NULL, 
         title="Hazard ratio"
    )+
    theme_bw()+
    theme(
      legend.position= "none",
      axis.text.x.top=element_text(hjust=0),
      axis.text.x=element_text(hjust=0.5, vjust=0.5, angle=0),
      
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_blank(),
      strip.placement="outside",
      #strip.text.y.left = element_text(angle=0),
      strip.text.y.left = element_blank(),
      strip.text.x.top = element_blank(),
      
      #panel.border = element_blank(),
      panel.spacing = unit(0.3, "lines"),
    ) + 
    geom_text(
      aes(x = period_mid, y = mantel_cox_hr), color="black", show_guide=FALSE,
      data = data.frame(period_mid = rep(180,2), mantel_cox_hr = ypostext, lab = "null", 
                        outcome = rep(fct_recoderelevel(outcomes[1],  recoder$outcome),2), 
                        subgroup = rep(fct_recoderelevel("all", recoder$subgroups),2), 
                        subgroup_plot = rep("Main",2)), 
      label = c(paste0(comparison_definition$level0_descr[which(comparison_definition$comparison==effect)], " higher risk"), 
                paste0(comparison_definition$level1_descr[which(comparison_definition$comparison==effect)], " higher risk"))
    )

  if(effect == "incremental"){
    plot_temp_HR <- plot_temp_HR + 
        scale_y_continuous(transform=scales::transform_log(),
                           labels = scales::label_number(drop0trailing=TRUE, accuracy=0.001), 
                           #name = "Hazard ratio",
                           breaks = brks,
                           #breaks = c(0.125, 0.25, 0.5, 1, 2),
                           #limits = c(0.125,2),
                           oob = scales::oob_keep, 
                           sec.axis = sec_axis(
                             ~(1-.),
                             name='Vaccine effectiveness = 100 x (1 - hazard ratio)',
                             breaks = brks_ve,
                             labels = function(x){formatpercent100(x, 0.5)}))
  } else if(effect == "comparative"){
    plot_temp_HR <- plot_temp_HR + 
      scale_y_continuous(transform=scales::transform_log(),
                         labels = scales::label_number(drop0trailing=TRUE, accuracy=0.001), 
                         #name = "Hazard ratio",
                         breaks = brks,
                         #breaks = c(0.125, 0.25, 0.5, 1, 2),
                         #limits = c(0.125,2),
                         oob = scales::oob_keep)
  }  



# prepare parameters and datasets for cumulative incidence
colour_labs_cuminc <- c("Unboosted", "Boosted", treatment_lookup$treatment_descr[1], treatment_lookup$treatment_descr[2])
colour_palette_cuminc <- RColorBrewer::brewer.pal(n=length(colour_labs_cuminc), name="Set1")
names(colour_palette_cuminc) <- colour_labs_cuminc
  
plot_data_incidence <- KM_dat %>% 
    mutate(subgroup = fct_recoderelevel(subgroup, recoder$subgroups),
           outcome = fct_recoderelevel(outcome,  recoder$outcome),
           subgroup_level = fct_recoderelevel(subgroup_level, subgroup_levels_labels)) %>%
    group_by(effect, outcome, subgroup, subgroup_level, treated) %>% 
    group_modify(
      ~add_row(
        .x,
        time=1,
        lagtime=1,
        leadtime=2,
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
    ungroup() %>% 
    mutate(
      inc_per_1000    = 1000*risk, 
      inc_per_1000.ll = 1000*risk.ll,  
      inc_per_1000.ul = 1000*risk.ul
    ) %>%
    add_descr(vars = "treated", effect = effect) 
  
# create cumulative incidence plot
plot_incidence <- plot_data_incidence %>%   
    ggplot(aes(group=treated_descr, colour=treated_descr, fill=treated_descr)) +
    geom_step(aes(x=time, y=inc_per_1000), direction="vh") +
    #geom_step(aes(x=time, y=inc_per_1000), direction="vh", linetype="dashed", alpha=0.5) +
    geom_rect(aes(xmin=lagtime, xmax=time, ymin=inc_per_1000.ll, ymax=inc_per_1000.ul), alpha=0.1, colour="transparent")+
    facet_grid(rows=vars(outcome), scales="free", switch="y") +
    scale_x_continuous(expand = c(0,0), 
                       breaks = c(1,seq(28,350,28)),
                       labels = paste0("\n",c(1,seq(28,350,28))),
                       limits = c(1, 350)) +
    scale_y_continuous(expand = expansion(mult=c(0,0.01)), 
                       oob = scales::oob_keep) +
    scale_color_manual(name=NULL, values=colour_palette_cuminc)+
    scale_fill_manual(name=NULL, values=colour_palette_cuminc)+
    coord_cartesian(xlim=c(0, NA)) +
    labs(
      x="Days since vaccination",
      y=NULL,
      colour=NULL,
      title="Cumulative incidence per 1000"
    ) +
    theme_bw() +
    theme(
      axis.line.x = element_line(colour = "black"),
      legend.position=c(.01,.97),
      legend.justification = c(0,1),
      #legend.position="bottom",
      
      #legend.title=element_text("Subgroup level"),
      axis.text.x.top=element_text(hjust=0),
      axis.text.x=element_text(hjust=0.5, vjust=0.5, angle=0),
      
      axis.title.y = element_blank(),
      
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_blank(),
      strip.placement="outside",
      #strip.text.y.left = element_text(angle=0),
      #strip.text.y.left = element_blank(),
      
      #panel.border = element_blank(),
      panel.spacing = unit(0.3, "lines"),
    )


plot_combined <- grid.arrange(plot_incidence, plot_temp_HR, ncol=2)
  
ggsave(
    filename=file.path(output_dir, glue("cumincHR_plot_rounded_recalc.png")),
    plot_combined,
    width=20, height=30, units="cm"
)

plot_combined
