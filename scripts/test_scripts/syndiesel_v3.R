setwd('/Users/michaeltofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/Figs')

library(tidyverse)
library(ggridges)
library(patchwork)

mu <- 1e-8

configs <- c(1,3,6)
splits  <- c(10000,20000,40000,80000,160000)

event_types <- c("Prob_1_Event","Prob_2_Events","Prob_3_Events",
                 "Prob_4_Events","Prob_5_Events","Prob_6_Events")

split_labels <- c(
  "10000"  = "1*N[e]",
  "20000"  = "2*N[e]",
  "40000"  = "4*N[e]",
  "80000"  = "8*N[e]",
  "160000" = "16*N[e]"
)

config_labels <- c(
  "NDiv=1" = "N[Div]==1",
  "NDiv=3" = "N[Div]==3",
  "NDiv=6" = "N[Div]==6"
)

fill_colors <- c(
  "True # events = Highest mean probability" = "purple4",
  "True # events" = "green",
  "Highest mean probability" = "hotpink",
  "Other events" = "gray"
)

migration_regimes <- list(
  list(folder="0_migration",     title="No Migration"),
  list(folder="10e-7_migration", title="Low Migration (1×10^-7)"),
  list(folder="10e-2_migration", title="High Migration (1×10^-2)"),
  list(folder="10e-5_migration", title="Moderte Migration (1×10^-5)")

)

results_root <- "/Users/michaeltofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results"

for (regime in migration_regimes) {
  
  base_dir <- file.path(results_root, regime$folder)
  if (!dir.exists(base_dir)) stop("Directory missing: ", base_dir)
  
  get_dir_name <- function(cfg, sp) {
    all_dirs <- list.dirs(base_dir, recursive = FALSE)
    all_dirs <- basename(all_dirs)
    match_dir <- all_dirs[
      grepl(paste0("config_", cfg), all_dirs) &
        grepl(as.character(sp), all_dirs)
    ]
    if (length(match_dir) == 0) return(NULL)
    match_dir[1]
  }
  
  # ======================================================
  # PROBABILITY PANEL
  # ======================================================
  
  all_data <- tibble()
  
  for (cfg in configs) {
    for (sp in splits) {
      
      dir_name <- get_dir_name(cfg, sp)
      if (is.null(dir_name)) next
      
      file_path <- file.path(base_dir, dir_name, "summary_output.txt")
      if (!file.exists(file_path)) next
      
      df <- read.table(file_path, header=TRUE, sep="\t", stringsAsFactors=FALSE)
      
      prob_df <- df %>%
        select(all_of(event_types)) %>%
        pivot_longer(everything(),
                     names_to="EventType",
                     values_to="Probability") %>%
        mutate(Probability=as.numeric(Probability))
      
      true_event <- if (cfg == 1) {
        "Prob_1_Event"
      } else {
        paste0("Prob_", cfg, "_Events")
      }
      
      highest_mean_event <- prob_df %>%
        group_by(EventType) %>%
        summarise(m=mean(Probability, na.rm=TRUE), .groups="drop") %>%
        arrange(desc(m), EventType) %>%
        slice(1) %>%
        pull(EventType)
      
      prob_df <- prob_df %>%
        mutate(
          FillGroup = case_when(
            EventType == true_event & EventType == highest_mean_event ~
              "True # events = Highest mean probability",
            EventType == true_event ~ "True # events",
            EventType == highest_mean_event ~ "Highest mean probability",
            TRUE ~ "Other events"
          ),
          Config=factor(paste0("NDiv=",cfg),
                        levels=paste0("NDiv=",configs)),
          Split=factor(sp, levels=splits),
          EventType=factor(EventType, levels=event_types)
        ) %>%
        select(Config, Split, EventType, Probability, FillGroup)
      
      all_data <- bind_rows(all_data, prob_df)
    }
  }
  
  probability_panel <- ggplot(all_data,
                              aes(x=Probability,
                                  y=EventType,
                                  fill=FillGroup)) +
    geom_density_ridges(scale=8,
                        alpha=0.75,
                        color="black",
                        size=0.25,
                        rel_min_height=0.01) +
    scale_fill_manual(values=fill_colors, name="Key") +
    scale_y_discrete(labels=c(
      "Prob_1_Event"="1",
      "Prob_2_Events"="2",
      "Prob_3_Events"="3",
      "Prob_4_Events"="4",
      "Prob_5_Events"="5",
      "Prob_6_Events"="6"
    )) +
    facet_grid(Config~Split,
               axes="all",
               labeller=labeller(
                 Split=as_labeller(split_labels,label_parsed),
                 Config=as_labeller(config_labels,label_parsed))) +
    coord_cartesian(xlim=c(0,1)) +
    labs(x="Probability",
         y="Number of divergence events") +
    theme_minimal(base_size=11) +
    theme(
      panel.grid=element_blank(),
      panel.border=element_rect(color="black", fill=NA, linewidth = 1.2),
      strip.background=element_blank(),
      strip.text.x = element_text(size=15, face="bold"),
      strip.text.y = element_text(size=15, face="bold"),
      legend.position="bottom",
      panel.spacing=unit(0.2,"lines"),
      plot.margin=margin(t=0,r=5,b=5,l=5)
    ) +
    guides(fill=guide_legend(nrow=1))
  
  # ======================================================
  # RECOVERY PANEL
  # ======================================================
  
  all_points  <- tibble()
  all_summary <- tibble()
  panel_labels <- tibble()
  
  for (cfg in configs) {
    for (sp in splits) {
      
      dir_name <- get_dir_name(cfg, sp)
      if (is.null(dir_name)) next
      
      file_path <- file.path(base_dir, dir_name, "summary_output.txt")
      if (!file.exists(file_path)) next
      
      df <- read.table(file_path, header=TRUE, sep="\t", check.names=FALSE)
      
      df_mean <- df %>%
        select(Run, starts_with("Mean_Divergence")) %>%
        pivot_longer(-Run,
                     names_to="PopPair",
                     values_to="MeanDivergence")
      
      df_true <- df %>%
        select(Run, starts_with("True_Divergence")) %>%
        pivot_longer(-Run,
                     names_to="PopPair_True",
                     values_to="TrueValue") %>%
        mutate(PopPair=gsub("True_Divergence_",
                            "Mean_Divergence_",
                            PopPair_True))
      
      df_combined <- inner_join(df_mean, df_true,
                                by=c("Run","PopPair")) %>%
        mutate(
          MeanDivergence=as.numeric(MeanDivergence),
          TrueValue=as.numeric(TrueValue),
          TrueValue_scaled=TrueValue*mu
        ) %>%
        filter(!is.na(MeanDivergence),
               !is.na(TrueValue_scaled),
               MeanDivergence>=0)
      
      if (nrow(df_combined)==0) next
      
      ci_summary <- df_combined %>%
        group_by(TrueValue_scaled) %>%
        summarise(
          Lower95=quantile(MeanDivergence,0.025),
          Upper95=quantile(MeanDivergence,0.975),
          MeanEstimate=mean(MeanDivergence),
          .groups="drop"
        )
      
      rmse_overall <- sqrt(
        mean((df_combined$MeanDivergence -
                df_combined$TrueValue_scaled)^2)
      )
      
      coverage_prop <- mean(
        (ci_summary$Lower95 <= ci_summary$TrueValue_scaled) &
          (ci_summary$TrueValue_scaled <= ci_summary$Upper95)
      )
      
      label_df <- tibble(
        Config=factor(paste0("NDiv=",cfg),
                      levels=paste0("NDiv=",configs)),
        Split=factor(sp,levels=splits),
        label=deparse(
          bquote(
            p(t %in% CI) == .(sprintf("%.2f",coverage_prop)) ~
              "," ~ RMSE == .(formatC(rmse_overall,format="e",digits=2))
          )
        )
      )
      
      all_points  <- bind_rows(all_points,
                               df_combined %>%
                                 select(TrueValue_scaled, MeanDivergence) %>%
                                 mutate(Config=factor(paste0("NDiv=",cfg),
                                                      levels=paste0("NDiv=",configs)),
                                        Split=factor(sp,levels=splits)))
      
      all_summary <- bind_rows(all_summary,
                               ci_summary %>%
                                 mutate(Config=factor(paste0("NDiv=",cfg),
                                                      levels=paste0("NDiv=",configs)),
                                        Split=factor(sp,levels=splits)))
      
      panel_labels <- bind_rows(panel_labels, label_df)
    }
  }
  
  recovery_panel <- ggplot(all_points,
                           aes(x=TrueValue_scaled,
                               y=MeanDivergence)) +
    geom_vline(data=all_summary,
               aes(xintercept=TrueValue_scaled),
               linetype="dashed",
               color="black",
               linewidth=0.4,
               alpha=0.6) +
    geom_point(size=1.1, alpha=0.6, color="gray50") +
    geom_errorbar(data=all_summary,
                  aes(x=TrueValue_scaled,
                      ymin=Lower95,
                      ymax=Upper95),
                  inherit.aes=FALSE,
                  width=0.0003,
                  color="orange3",
                  linewidth=0.4) +
    geom_point(data=all_summary,
               aes(x=TrueValue_scaled,
                   y=MeanEstimate),
               inherit.aes=FALSE,
               size=1,
               color="orange3") +
    geom_abline(slope=1, intercept=0) +
    facet_grid(Config~Split,
               axes="all",
               labeller=labeller(
                 Split=as_labeller(split_labels,label_parsed),
                 Config=as_labeller(config_labels,label_parsed))) +
    scale_x_continuous(breaks=seq(0,0.012,by=0.003),
                       limits=c(0,0.012)) +
    scale_y_continuous(breaks=seq(0,0.012,by=0.003)) +
    geom_text(data=panel_labels,
              aes(x=0,
                  y=0.0115,
                  label=label),
              parse=TRUE,
              inherit.aes=FALSE,
              hjust=0,
              size=2.3) +
    labs(x=expression(True~divergence),
         y=expression(Estimated~divergence)) +
    theme_bw(base_size=11) +
    theme(
      panel.grid=element_blank(),
      strip.background=element_blank(),
      strip.text.x = element_text(size=15, face="bold"),
      strip.text.y = element_text(size=15, face="bold"),
      panel.spacing=unit(0.2,"lines"),
      plot.margin=margin(t=5,r=5,b=0,l=5),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 1.2   # ← increase this
      ),
      
      # ⬇ Increase axis label size
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      
      # ⬇ Decrease tick number size
      axis.text.x  = element_text(size=7),
      axis.text.y  = element_text(size=7)
    )
  
  combined_panel <- (recovery_panel /
                       probability_panel) +
    plot_layout(heights=c(1,1),
                tag_level="new") +
    plot_annotation(
      title=regime$title,
      tag_levels="A"
    ) &
    theme(
      plot.title=element_text(size=18,hjust=0.5),
      plot.tag=element_text(size=25)
    )
  
  ggsave(paste0(regime$folder,"_combined_6x5_panel.pdf"),
         combined_panel,
         width=13,
         height=17,
         dpi=600,
         bg="white")
}

