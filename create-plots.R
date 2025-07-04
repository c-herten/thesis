
# packages
source("./install-packages.R")

plot_packages <- c("stargazer", "ggplot2", "xtable")

plot_git_packages <- c("synth-inference/synthdid", "skranz/xsynthdid")

package_load(cran_packages = plot_packages, github_packages = plot_git_packages)

# loading models and results 
source("./methodology.R")

# --- Plots: ESL Rates ----

# Diff in Diff ----
did_results <- data.frame(
  method = c(
    rep("Portugal vs Greece (Year FE)", 2),
    rep("Portugal vs Lithuania (Year FE)", 2),
    rep("Portugal vs Greece (Linear)", 3),
    rep("Portugal vs Lithuania (Linear)", 3)
  ),
  specification = c(
    "Unemployment", "GDP + Unemployment",
    "GDP", "GDP + Unemployment", 
    "GDP", "Unemployment", "GDP + Unemployment",
    "GDP", "Unemployment", "GDP + Unemployment"
  ),
  estimate = c(
    0.1288456, 0.0968100,
    -0.0424570, 0.0395910,
    0.0490630, 0.0535049, 0.0507060,
    -0.1464600, -0.4367560, -0.1532100
  ),
  se = c(
    0.0648517, 0.1325000,
    0.1159900, 0.0890960,
    0.1072700, 0.0916053, 0.1054200,
    0.1190400, 0.1484380, 0.1357500
  )
)

did_results$ci_lower <- did_results$estimate - 1.96 * did_results$se
did_results$ci_upper <- did_results$estimate + 1.96 * did_results$se

did_colors <- c(
  "Portugal vs Greece (Year FE)" = "#FA8072",       
  "Portugal vs Lithuania (Year FE)" = "#4682B4",   
  "Portugal vs Greece (Linear)" = "#2F4F4F",         
  "Portugal vs Lithuania (Linear)" = "#8FBC8F"     
)

did_plot <- ggplot(did_results, aes(x = specification, y = estimate, color = method)) +
  geom_point(size = 3.2, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, size = 0.8, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.8) +
  scale_color_manual(values = did_colors) +
  coord_flip() +
  labs(
    x = "",
    y = "Treatment Effect (95% CI)",
    color = "Method"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, margin = margin(r = 15)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(ncol = 2))

print(did_plot)

# SDID ----

sdid_results <- data.frame(
  method = c(
    rep("SDiD (Arkhangelsky et al.)", 4),
    rep("SDiD (Kranz)", 3)
  ),
  specification = c(
    "No covariates", "GDP", "Unemployment", "GDP + Unemployment",
    "GDP", "Unemployment", "GDP + Unemployment"
  ),
  estimate = c(
    -0.3346, -0.3233, -0.1877, -0.1885,
    -0.7338, -0.9170, -0.8259
  ),
  se = c(
    0.179, 0.2112, 0.130, 0.1645,
    0.2232, 0.2789, 0.2228
  )
)

sdid_results$ci_lower <- sdid_results$estimate - 1.96 * sdid_results$se
sdid_results$ci_upper <- sdid_results$estimate + 1.96 * sdid_results$se

sdid_colors <- c(
  "SDiD (Arkhangelsky et al.)" = "#FA8072",        
  "SDiD (Kranz)" = "#4682B4"                       
)

sdid_plot <- ggplot(sdid_results, aes(x = specification, y = estimate, color = method)) +
  geom_point(size = 3.2, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, size = 0.8, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.8) +
  scale_color_manual(values = sdid_colors) +
  coord_flip() +
  labs(
    x = "", 
    y = "Treatment Effect (95% CI)",
    color = "Method"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, margin = margin(r = 15)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(ncol = 2))

print(sdid_plot)

pdf(file.path("plots", "arkhangelsky_with_cov.pdf"), width = 10, height = 6)
synthdid_plot(tau_hat_with_cov, 
              overlay = 0,
              effect.alpha = 0,
              diagram.alpha = 0,
              onset.alpha = 0,
              line.width = 1,
              point.size = 2.5,
              trajectory.alpha = 1,
              treated.name = "Portugal",
              control.name = "Synthetic Control") +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, margin = margin(r = 15)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    x = "Year",
    y = "ESL Rate (Logit Transformed)",
    color = ""
  )
dev.off()

pdf(file.path("plots", "kranz_both.pdf"), width = 10, height = 6)
synthdid_plot(tau_kranz_both, 
              overlay = 0,
              diagram.alpha = 0,
              line.width = 1,                    
              point.size = 2.5,                    
              trajectory.alpha = 1,              
              treated.name = "Portugal",           
              control.name = "Synthetic Control") +
  theme_minimal(base_size = 16) +                 
  theme(
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, margin = margin(r = 15)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.position = "top",                      
    panel.grid.minor = element_blank(),           
    plot.margin = margin(20, 20, 20, 20)         
  ) +
  labs(
    x = "Year",
    y = "ESL Rate (Logit Transformed)",
    color = ""                                    
  )
dev.off()

pdf(file.path("plots", "sdid_units_esl.pdf"), width = 10, height = 6)
synthdid_units_plot(tau_kranz_both, se.method = "placebo")
dev.off()

pdf(file.path("plots", "sdid_units_esl_arkhangelsky.pdf"), width = 10, height = 6)
synthdid_units_plot(tau_hat_with_cov, se.method = "placebo")
dev.off()


# --- Tables: ESL Rates ----

library(stargazer)

unit_weights_df <- data.frame(
  Country = countries[unit_weights_final > 0.001],
  Weight = unit_weights_final[unit_weights_final > 0.001]
)

time_weights_df <- data.frame(
  Year = pre_treatment_years_final[time_weights_final > 0.001],
  Weight = time_weights_final[time_weights_final > 0.001]
)

stargazer(unit_weights_df,
          summary = FALSE,
          title = "Unit Weights for \\ac{SDiD} Following \\textcite{Kranz2022} with GDP and Unemployment Rates as Covariates",
          label = "tab:unit_weights_sdid_esl",
          table.placement = "H",
          digits = 3,
          header = FALSE)

stargazer(time_weights_df,
          summary = FALSE, 
          title = "Time Weights for \\ac{SDiD} Following \\textcite{Kranz2022} with GDP and Unemployment Rates as Covariates", 
          label = "tab:time_weights_sdid_esl",
          table.placement = "H",
          digits = 3,
          header = FALSE)


# --- Plots: Completion Rates ----

# DiD ----

did_comp_results <- data.frame(
  method = c(
    rep("Portugal vs Greece (Year FE)", 4),
    rep("Portugal vs Spain (Year FE)", 1),
    rep("Portugal vs Greece (Linear)", 4),  
    rep("Portugal vs Spain (Linear)", 4)
  ),
  specification = c(
    "No covariates", "GDP", "Unemployment", "GDP + Unemployment",
    "GDP + Unemployment",
    "No covariates", "GDP", "Unemployment", "GDP + Unemployment", 
    "No covariates", "GDP", "Unemployment", "GDP + Unemployment"
  ),
  estimate = c(
    0.21045, -0.031179, 0.1514896, -0.26158,
    0.32367,
    0.21143, 1.1704, 0.1316460, 0.11609,  
    0.403804, 0.39345, 0.3770502, 0.39251
  ),
  se = c(
    0.20773, 0.22122, 0.2286384, 0.28867,
    0.13050,
    0.16357, 0.28454, 0.2032814, 0.23399, 
    0.187712, 0.18891, 0.1834184, 0.19118
  )
)

did_comp_results$ci_lower <- did_comp_results$estimate - 1.96 * did_comp_results$se
did_comp_results$ci_upper <- did_comp_results$estimate + 1.96 * did_comp_results$se

did_comp_colors <- c(
  "Portugal vs Greece (Year FE)" = "#FA8072",        
  "Portugal vs Spain (Year FE)" = "#4682B4",         
  "Portugal vs Greece (Linear)" = "#2F4F4F",         
  "Portugal vs Spain (Linear)" = "#8FBC8F"           
)

did_comp_plot <- ggplot(did_comp_results, aes(x = specification, y = estimate, color = method)) +
  geom_point(size = 3.2, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, size = 0.8, position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.8) +
  scale_color_manual(values = did_comp_colors) +
  coord_flip() +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "Treatment Effect (95% CI)",
    color = "Method"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, margin = margin(r = 15)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(ncol = 2))


print(did_comp_plot)


# SDID ----
sdid_comp_results <- data.frame(
  method = c(
    rep("SDiD (Arkhangelsky et al.)", 4),
    rep("SDiD (Kranz)", 3)
  ),
  specification = c(
    "No covariates", "GDP", "Unemployment", "GDP + Unemployment",
    "GDP", "Unemployment", "GDP + Unemployment"
  ),
  estimate = c(
    0.2641, 0.3124, 0.2898, 0.2977,
    0.5240, 0.3308, 0.5167
  ),
  se = c(
    0.1473, 0.1551, 0.1299, 0.1303,
    0.1472, 0.1733, 0.1460
  )
)

sdid_comp_results$ci_lower <- sdid_comp_results$estimate - 1.96 * sdid_comp_results$se
sdid_comp_results$ci_upper <- sdid_comp_results$estimate + 1.96 * sdid_comp_results$se

sdid_comp_colors <- c(
  "SDiD (Arkhangelsky et al.)" = "#FA8072",  
  "SDiD (Kranz)" = "#4682B4"                 
)

sdid_comp_plot <- ggplot(sdid_comp_results, aes(x = specification, y = estimate, color = method)) +
  geom_point(size = 3.2, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, size = 0.8, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.8) +
  scale_color_manual(values = sdid_comp_colors) +
  coord_flip() +
  labs(
    title = "",
    subtitle = "",
    x = "", 
    y = "Treatment Effect (95% CI)",
    color = "Method"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  )

print(sdid_comp_plot)

pdf(file.path("plots", "arkhangelsky_with_cov_comp.pdf"), width = 10, height = 6)
synthdid_plot(tau_hat_with_cov_comp, 
              overlay = 0,
              effect.alpha = 0,
              diagram.alpha = 0,
              onset.alpha = 0,
              line.width = 1,
              point.size = 2.5,
              trajectory.alpha = 1,
              treated.name = "Portugal",
              control.name = "Synthetic Control") +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, margin = margin(r = 15)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    x = "Year",
    y = "Completion Rate (Logit Transformed)",
    color = ""
  )
dev.off()

pdf(file.path("plots", "kranz_both_comp.pdf"), width = 10, height = 6)
synthdid_plot(tau_kranz_both_comp, 
              overlay = 0,
              diagram.alpha = 0,
              line.width = 1,                    
              point.size = 2.5,                    
              trajectory.alpha = 1,              
              treated.name = "Portugal",           
              control.name = "Synthetic Control") +
  theme_minimal(base_size = 16) +                 
  theme(
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, margin = margin(r = 15)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.position = "top",                      
    panel.grid.minor = element_blank(),           
    plot.margin = margin(20, 20, 20, 20)         
  ) +
  labs(
    x = "Year",
    y = "Completion Rate (Logit Transformed)",
    color = ""                                    
  )
dev.off()

pdf(file.path("plots", "sdid_units_comp_kranz.pdf"), width = 10, height = 6)
synthdid_units_plot(tau_kranz_both_comp, se.method = "placebo")
dev.off()

pdf(file.path("plots", "sdid_units_comp_arkhangelsky.pdf"), width = 10, height = 6)
synthdid_units_plot(tau_hat_with_cov_comp, se.method = "placebo")
dev.off()


# --- Plots: Unemployment Rates ----

years <- as.numeric(rownames(dataprep_out$Y1plot))
portugal_actual <- as.vector(dataprep_out$Y1plot)
portugal_synthetic <- as.vector(dataprep_out$Y0plot %*% synth_out$solution.w)

plot_data <- data.frame(
  year = rep(years, 2),
  unemployment = c(portugal_actual, portugal_synthetic),
  type = rep(c("Portugal", "Synthetic Portugal"), each = length(years))
)

unemployment_plot <- ggplot(plot_data, aes(x = year, y = unemployment, color = type, linetype = type)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2015.5, color = "black", linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Portugal" = "black", "Synthetic Portugal" = "black")) +
  scale_linetype_manual(
    values = c("Portugal" = "solid", "Synthetic Portugal" = "dashed")
  ) +
  guides(color = "none") +  
  
  labs(
    title = "",
    subtitle = "",
    x = "Year",
    y = "Unemployment Rate (%)",
    linetype = ""
  ) +
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, margin = margin(r = 15)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.position = "bottom", 
    legend.key.width = unit(2.5, "cm")
  ) +
  annotate("text", x = 2016, y = max(plot_data$unemployment) * 0.95, 
           label = "", color = "black", size = 4, hjust = 0)

print(unemployment_plot)

# Placebo 
placebo_years <- as.numeric(rownames(placebo_dataprep$Y1plot))
placebo_actual <- as.vector(placebo_dataprep$Y1plot)
placebo_synthetic <- as.vector(placebo_dataprep$Y0plot %*% placebo_synth$solution.w)

placebo_plot_data <- data.frame(
  year = rep(placebo_years, 2),
  unemployment = c(placebo_actual, placebo_synthetic),
  type = rep(c("Portugal", "Synthetic Portugal"), each = length(placebo_years))
)

placebo_unemployment_plot <- ggplot(placebo_plot_data, aes(x = year, y = unemployment, color = type, linetype = type)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = placebo_treatment_year, color = "black", linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Portugal" = "black", "Synthetic Portugal" = "black")) +
  scale_linetype_manual(
    values = c("Portugal" = "solid", "Synthetic Portugal" = "dashed")
  ) +
  guides(color = "none") +  
  
  labs(
    title = "",
    subtitle = "",
    x = "Year",
    y = "Unemployment Rate (%)",
    linetype = ""
  ) +
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, margin = margin(r = 15)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.position = "bottom", 
    legend.key.width = unit(2.5, "cm")
  ) +
  scale_x_continuous(breaks = seq(min(placebo_years), max(placebo_years), 5),
                     labels = seq(min(placebo_years), max(placebo_years), 5)) 

print(placebo_unemployment_plot)


# Tables ----

# pre-treatment balance 
xtable(synth_tables$tab.pred, caption = "Pre-Treatment Balance")

# unit weights   
xtable(synth_tables$tab.w, caption = "Unit Weights for Synthetic Control")

# predictor weights 
xtable(synth_tables$tab.v, caption = "Predictor Weights")


# Saving ----

# DID plots
ggsave(file.path("plots", "did_esl_plot.pdf"), did_plot, width = 10, height = 6)
ggsave(file.path("plots", "did_completion_plot.pdf"), did_comp_plot, width = 10, height = 6)


# SDID plots  
ggsave(file.path("plots", "sdid_esl_plot.pdf"), sdid_plot, width = 10, height = 6)
ggsave(file.path("plots", "sdid_completion_plot.pdf"), sdid_comp_plot, width = 10, height = 6)


# SCM plots
ggsave(file.path("plots", "scm_unemployment_plot.pdf"), unemployment_plot, width = 10, height = 6)
ggsave(file.path("plots", "placebo_unemployment_plot.pdf"), placebo_unemployment_plot, width = 10, height = 6)
