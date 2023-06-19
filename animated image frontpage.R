library(tidyverse)
library(DeclareDesign)
library(patchwork)

design <- 
  declare_model(
    N = 100,
    SD_U = runif(N, min = 0.1, max = 5),
    U = rnorm(N, sd = SD_U),
    potential_outcomes(Y ~ 0.1 * Z + U)
  ) + 
  declare_inquiries(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_assignment(Z = complete_ra(N),
                     Z_fct = factor(Z, levels = c(0, 1), labels = c("Control", "Treatment"))) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
  declare_estimator(Y ~ Z, .method = lm_robust)

dat_list <- map(1:250, ~draw_data(design))
est_list <- map(dat_list, ~get_estimates(design, data = .))
estimands <- draw_estimands(design)

for(i in 1:250) {
  
  summary_df <- 
    dat_list[[i]] |> 
    group_by(Z_fct) |> 
    summarise(tidy(lm_robust(Y ~ 1))) |> 
    mutate(Y = estimate)
  
  yz_plot <- 
    ggplot(dat_list[[i]]) + 
    aes(Y, Z_fct) + 
    geom_vline(xintercept = 0, lty = "dashed", color = "darkgray") + 
    geom_jitter(height = 0.05, color = gray(0.8)) + 
    geom_point(data = summary_df, size = 4) +
    geom_linerange(data = summary_df, aes(xmin = conf.low, xmax = conf.high)) +
    coord_cartesian(xlim = c(-2.2, 2.2)) + 
    labs(x = "", y = "", title = "Simulated data") + 
    theme_minimal() + 
    theme(
      panel.grid.minor.x = element_blank()
    )
  
  # yz_plot
  
  diff_plot <-
    ggplot(est_list[[i]] |> mutate(estimator = "Difference-\nin-means")) + 
    geom_point(aes(y = estimator, x = estimate)) + 
    geom_vline(xintercept = 0, lty = "dashed", color = "darkgray") + 
    geom_linerange(aes(y = estimator, xmin = conf.low, xmax = conf.high)) + 
    coord_cartesian(xlim = c(-2.2, 2.2)) + 
    labs(y = "", x = "", title = "Simulated estimate") + 
    theme_minimal() + 
    theme(
      # axis.title.y = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
          title = element_text(size = 10))
  
  hist_plot <- 
    ggplot(est_list |> bind_rows() |> slice(1:i)) + 
    geom_histogram(aes(x = estimate, y = (after_stat(count))/sum(after_stat(count))), col = NA) + 
    geom_histogram(aes(x = est_list[[i]]$estimate, y = (after_stat(count))/sum(after_stat(count))), col = NA, fill = "lightblue") + 
    scale_y_continuous("", labels = scales::percent) +
    scale_x_continuous("") +
    coord_cartesian(xlim = c(-2.2, 2.2), ylim = c(0, 0.1)) +
    labs(title = "Sampling distribution of estimates") + 
    theme_minimal() +
    theme(title = element_text(size = 10))
  
  sims_tmp <- est_list[1:i] |> 
    bind_rows(.id = "sim_ID") |> 
    mutate(design = "design") |> 
    bind_cols(estimands)
  
  text_df <- 
    diagnose_design(sims_tmp) |> 
    tidy() |> 
    filter(diagnosand %in% c("bias", "rmse", "power", "coverage")) |> 
    bind_rows(tibble(diagnosand = "n_sims", estimate = i)) |> 
    mutate(diagnosand_ID = 1:n(),
           diagnosand = 
             factor(diagnosand, 
                    levels = c("n_sims", "coverage", "power", "rmse", "bias"),
                    labels = c("N sims", "Coverage", "Power", "RMSE", "Bias"))) |>
    mutate(label = if_else(diagnosand == "N sims", DeclareDesign:::format_num(estimate, 0),
                           DeclareDesign:::format_num(estimate, 2))) |>
    pivot_longer(c(diagnosand, label)) |>
    mutate(x = if_else(name == "diagnosand", -0.5, 0.5),
           y = diagnosand_ID,
           hjust = if_else(name == "Diagnosand", 0, 1)) 
  
  text_plot <- 
    ggplot(data = text_df) + 
    geom_text(aes(label = value, x = x, y = y, hjust = hjust), size = 3) +
    labs(title = "Diagnosands", y = "", x = "") + 
    coord_cartesian(xlim = c(-1, 1)) +
    theme_minimal() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          title = element_text(size = 10),
          text = element_text(size = 4))
  
  text_plot
  
  # gg <- (text_plot + yz_plot) / ( (diff_plot + hist_plot) + plot_layout(widths = c(1, 3))) 
  # gg
  
  gg <- (yz_plot / diff_plot / hist_plot / text_plot) + plot_layout(heights = c(3, 1, 3, 1.85))
    
  # gg
  # 
  #   (text_plot + yz_plot) / ( (diff_plot + hist_plot) + plot_layout(widths = c(1, 3)))
  # gg
  
  system("mkdir static/img")
  ggsave(gg, file = paste0("static/img/img", i, ".png"), width = 5, height = 7.1)
  
  #1.42
  
}

i <- 250
hist_plot <- 
  ggplot(est_list |> bind_rows() |> slice(1:i)) + 
  geom_histogram(aes(x = estimate, y = (after_stat(count))/sum(after_stat(count))), col = NA) + 
  # geom_histogram(aes(x = est_list[[i]]$estimate, y = (after_stat(count))/sum(after_stat(count))), col = NA, fill = "lightblue") + 
  scale_y_continuous("Percent of sampling distribution", labels = scales::percent) +
  scale_x_continuous("") +
  coord_flip(xlim = c(-2.2, 2.2), ylim = c(0, 0.1)) + 
  labs(title = "Sampling distribution of estimates") + 
  theme_minimal() +
  theme(title = element_text(size = 10))

gg <- (yz_plot / diff_plot / hist_plot / text_plot) + plot_layout(heights = c(3, 1, 3, 1.85))
gg

ggsave(gg, file = paste0("static/img/img", i, "-final.png"), width = 5, height = 7.1)


library(gifski)

png_files <- c(paste0("static/img/img", c(1:250), ".png"), "static/img/img250-final.png")
gifski(png_files, "static/frontpage.gif", 
       width = 4000, height = 4000, 
       delay = 0.01, loop = FALSE)

