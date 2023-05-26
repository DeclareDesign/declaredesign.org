
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
  
  yz_plot <- 
    ggplot(dat_list[[i]]) + 
    aes(Z_fct, Y) + 
    geom_hline(yintercept = 0, lty = "dashed", color = "darkgray") + 
    geom_jitter(width = 0.05, color = gray(0.8)) + 
    coord_cartesian(ylim = c(-3, 3)) + 
    labs(y = "Outcome variable (Y)", x = "Treatment condition (Z)", title = "Simulated data") + 
    theme_minimal()
  
  # yz_plot
  
  diff_plot <-
    ggplot(est_list[[i]]) + 
    geom_point(aes(x = 0, y = estimate)) + 
    geom_hline(yintercept = 0, lty = "dashed", color = "darkgray") + 
    geom_errorbar(aes(x = 0, ymin = conf.low, ymax = conf.high), width = 0.01) + 
    coord_cartesian(xlim = c(-0.1, 0.1), ylim = c(-2.2, 2.2)) + 
    labs(y = "Estimate", title = "Simulated estimate") + 
    theme_minimal() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          title = element_text(size = 10))
  
  hist_plot <- 
    ggplot(est_list |> bind_rows() |> slice(1:i)) + 
    geom_histogram(aes(x = estimate, y = (after_stat(count))/sum(after_stat(count))), col = NA) + 
    geom_histogram(aes(x = est_list[[i]]$estimate, y = (after_stat(count))/sum(after_stat(count))), col = NA, fill = "lightblue") + 
    scale_y_continuous("Percent of sampling distribution", labels = scales::percent) +
    scale_x_continuous("") +
    coord_flip(xlim = c(-2.2, 2.2), ylim = c(0, 0.1)) + 
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
                    labels = c("N sims", "Coverage", "Power", "RMSE", "Bias"))) 
  
  # text_plot <- 
  #   ggplot(data = text_df) + 
  #   geom_point(aes(x = estimate, y = diagnosand)) +
  #   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = diagnosand), height = 0.1)  + 
  #   labs(title = "Diagnosands", y = "", x = "") + 
  #   theme_minimal() + 
  #   theme(axis.title.x = element_blank(),
  #         panel.grid.minor.x = element_blank(),
  #         panel.grid.minor.y = element_blank(),
  #         title = element_text(size = 10))
  
  text_plot <- 
    ggplot(data = text_df) + 
    geom_text(aes(label = round(estimate, 2), x = 0, y = diagnosand)) +
    labs(title = "Diagnosands", y = "", x = "") + 
    theme_minimal() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          title = element_text(size = 10))
  
  text_plot
  
  gg <- (text_plot + yz_plot) / ( (diff_plot + hist_plot) + plot_layout(widths = c(1, 3))) 
  gg
  
  system("mkdir static/img")
  ggsave(gg, file = paste0("static/img/img", i, ".png"), width = 7, height = 5)
  
}

library(gifski)

png_files <- paste0("static/img/img", c(1:250, rep(250, 100)), ".png")
gifski(png_files, "static/frontpage.gif", width = 4000, height = 4000, delay = 0.01)

