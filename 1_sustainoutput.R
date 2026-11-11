#plotting SuStaIn biomarker trajectories (heatmap)

library(dplyr)
library(tidyr)
library(ggplot2)

# Feature order
feature_levels <- c(
  "frontal WMH volume",
  "temporal WMH volume",
  "parietal WMH volume",
  "occipital WMH volume",
  "frontal lobe atrophy",
  "temporal lobe atrophy",
  "parietal lobe atrophy",
  "occipital lobe atrophy",
  "subcortical atrophy",
  "parahippocampal atrophy",
  "ventricular volume"
)

# Score colours
score_cols <- c(
  "1" = "#A8754F",
  "2" = "#6D8F3D",
  "3" = "#526FAE"
)

# Function to prepare data
prepare_heatmap_data <- function(score_file, prob_file) {
  
  scores <- read.csv(score_file)
  probs  <- read.csv(prob_file)
  
  scores_long <- scores %>%
    tidyr::pivot_longer(
      cols = -X,
      names_to = "stage",
      values_to = "score"
    )
  
  probs_long <- probs %>%
    tidyr::pivot_longer(
      cols = -X,
      names_to = "stage",
      values_to = "prob"
    )
  
  scores_long %>%
    left_join(probs_long, by = c("X", "stage")) %>%
    mutate(
      stage = 1 + as.numeric(gsub("^X", "", stage)),
      feature = factor(X, levels = feature_levels),
      score = factor(score, levels = names(score_cols))
    )
}

# Plotting function
plot_heatmap <- function(score_file, prob_file, title) {
  
  heatmap_data <- prepare_heatmap_data(score_file, prob_file)
  
  ggplot(heatmap_data, aes(x = stage, y = feature)) +
    geom_hline(
      yintercept = c(1:11),
      linewidth = 0.3,
      colour = "grey80"
    ) +
    geom_tile(aes(fill = score, alpha = prob)) +
    
    scale_fill_manual(
      values = score_cols,
      breaks = c("1", "2", "3"),
      labels = c("0.7", "1.4", "2.1"),
      name = "W-score threshold"
    ) +
    
    scale_alpha(
      range = c(0, 1),
      name = "Stage assignment probability"
    ) +
    
    scale_x_continuous(
      breaks = c(0, 5, 10, 15, 20, 25, 30)
    ) +
    
    scale_y_discrete(
      limits = rev(feature_levels)
    ) +
    
    labs(
      x = "SuStaIn stage",
      y = "Feature",
      title = title
    ) +
    
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(colour = "black"),
          title = element_text(hjust = 0.5,colour = "black",size=14),
          axis.title = element_text(colour = "black",size = 14),
          axis.text  = element_text(colour = "black",size = 14),
          legend.title = element_text(colour = "black",size = 13),
          legend.text  = element_text(colour = "black",size = 13),
          strip.text = element_text(colour = "black",size = 14),
          panel.grid = element_blank(),axis.title.y = element_blank()
    )+
    guides(fill=guide_legend(
      override.aes = list(size = 4,alpha=1)),
      alpha=guide_legend(
        override.aes = list(size = 4))
    )
}

# Subtype 1
p1 <- plot_heatmap(
  "samples_sequence_s1.csv",
  "maxprob_s1.csv",
  "Subtype 1: Atrophy-first (n = 446)"
)

# Subtype 2
p2 <- plot_heatmap(
  "samples_sequence_s2.csv",
  "maxprob_s2.csv",
  "Subtype 2: WMH-first (n = 278)"
)

top_plot <- ggarrange(p1,p2,ncol=2,common.legend = TRUE,legend = 'bottom')

full_plot <- cowplot::plot_grid(top_plot,bottom_plot, plot2,ncol = 1,labels = c('A','','D'),
                                rel_heights = c(1.5,1,.5))
ggsave(
  filename = 'plots/preprint_fig1_ver4.jpg',
  plot = full_plot,
  width = 15,
  height = 9,
  dpi = 600
)
full_plot <- cowplot::plot_grid(top_plot,bottom_plot,ncol = 1,labels = c('A',''),
                                rel_heights = c(1.4,1))
