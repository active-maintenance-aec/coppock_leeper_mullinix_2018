# coppock_leeper_mullinix_2018/maintained/figure_1_across_study_correspondence.R
# Output: output/figure_1_across_study_correspondence.pdf, .png, .csv
# Depends on: helpers.R, estimate_cates.R
# Description: Figure 1. One panel per demographic subgroup, each plotting the
#   original CATE against the replication CATE for all 27 study pairs, with
#   confidence intervals on both axes and colour marking whether the two differ
#   significantly.

source(here::here("maintained", "helpers.R"))

gg_df <- read_csv(file.path(out_dir, "cate_scatter.csv"),
                  show_col_types = FALSE) |>
  mutate(
    group_label = factor(group_label, levels = group_figure_labels),
    difference = factor(if_else(diff_in_cates_significant,
                                "Significant", "Not Significant"),
                        levels = c("Significant", "Not Significant"))
  ) |>
  select(study_label, group_label, difference,
         estimate_mt, conf.low_mt, conf.high_mt,
         estimate_original, conf.low_original, conf.high_original)

write_csv(gg_df, file.path(out_dir, "figure_1_across_study_correspondence.csv"))

g <- ggplot(gg_df, aes(x = estimate_mt, y = estimate_original)) +
  geom_hline(yintercept = 0, color = "lightgray") +
  geom_vline(xintercept = 0, color = "lightgray") +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5, color = "lightgray") +
  geom_linerange(aes(ymin = conf.low_original, ymax = conf.high_original,
                     color = difference), alpha = 0.3) +
  geom_linerange(aes(xmin = conf.low_mt, xmax = conf.high_mt,
                     color = difference), alpha = 0.3) +
  geom_point(aes(color = difference, shape = difference)) +
  geom_rug() +
  coord_fixed(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5)) +
  scale_color_manual(values = c("red", "black")) +
  scale_shape_manual(values = c(15, 16)) +
  facet_wrap(~ group_label, ncol = 6, drop = FALSE, dir = "v") +
  labs(
    x = "Mechanical Turk Version Standardized Estimate",
    y = "Original Version Standardized Estimate",
    color = "Difference in CATEs",
    shape = "Difference in CATEs"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "figure_1_across_study_correspondence.pdf"), g,
       height = 6, width = 9)
ggsave(file.path(out_dir, "figure_1_across_study_correspondence.png"), g,
       height = 6, width = 9, dpi = 300)

print(paste("Figure 1 plots", nrow(gg_df), "comparisons across",
            n_distinct(gg_df$group_label), "subgroups"))
