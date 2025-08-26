# Descriptive statistics of statistics anxiety
# Descriptive statistics of general anxiety
# Descriptive statistics of module performance

descriptive_variables <- learning_statistics |>
  datawizard::describe_distribution(
    c(stats_anxiety,gen_anxiety,stats_exam),
    range = FALSE,
  )

descriptive_variables <- kableExtra::kable(descriptive_variables)

# Descriptive statistics of module performance grouped by universities

descriptive_uni_grouped <- learning_statistics |>
  group_by(uni) |>
  datawizard::describe_distribution(
    stats_exam,
    range = FALSE
    )

# Histogram of statistics anxiety

hist_stats_anxiety <- ggplot(learning_statistics, aes(stats_anxiety)) +
  geom_histogram(binwidth = 1, fill = "#4682b4") +
  labs(x = "Statistics Anxiety", y = "Density") +
  theme_bw() 

# Histogram of general anxiety

hist_general_anxiety <- ggplot(learning_statistics, aes(gen_anxiety)) +
  geom_histogram(binwidth = 1, fill = "#4682b4") +
  labs(x = "General Anxiety", y = "Density") +
  theme_bw()

# Histogram of module performance

hist_stats_exam <- ggplot(learning_statistics, aes(stats_exam)) +
  geom_histogram(binwidth = 1 , fill = "#4682b4") +
  labs(x = "Students' Performance" , y = "Density") +
  theme_bw()

# Box plot of statistics anxiety

box_stats_anxiety <- ggplot(learning_statistics, aes(stats_anxiety)) +
  geom_boxplot(fill = "#ff6347") +
  labs(x = "Statistics Anxiety") +
  theme_bw()

# Box plot of general anxiety

box_general_anxiety <- ggplot(learning_statistics, aes(gen_anxiety)) +
  geom_boxplot(fill = "#ff6347") +
  labs(x = "General Anxiety") +
  theme_bw()

# Box plot of module performance

box_stats_exam <- ggplot(learning_statistics, aes(stats_exam)) +
  geom_boxplot(fill = "#ff6347") +
  labs(x = "Students' Performance") +
  theme_bw()

# Table of outlier data

learning_statistics <- learning_statistics |>
  dplyr::mutate(
    z_stats_performance = (stats_exam - mean(stats_exam, na.rm = T)) / sd(stats_exam, na.rm = T)
  )

exam_outlier <- learning_statistics |>
  dplyr::filter(
    dplyr::if_any(
      .cols = z_stats_performance,
      .fn = \(column) abs(column) >= 2.58
    )
  )

exam_outlier <- kableExtra::kable(exam_outlier)

# Scatter plot of statistics anxiety against module performance

scat_stats_exam <- ggplot(learning_statistics, aes(stats_anxiety, stats_exam)) + 
  geom_point(alpha = 0.8) + 
  geom_smooth(method = "lm") +
  labs(x = "Statistics Anxiety" , y = "Students Performance") +
  theme_bw()

# Scatter plot of general anxiety against module performance

scat_general_exam <- ggplot(learning_statistics, aes(gen_anxiety, stats_exam)) + 
  geom_point(alpha = 0.8) + 
  geom_smooth(method = "lm") +
  labs(x = "General Anxiety" , y = "Students Performance") +
  theme_bw()


# fitting the general linear model

stats_performance_lm <- lm(stats_exam ~ gen_anxiety + stats_anxiety, data = learning_statistics)

# make the results tidy and adding confidence intervals

inf_stats_oerformance_lm <- broom::tidy(stats_performance_lm, conf.int = TRUE)



# Influential cases

Influential_cases <- ggplot2::autoplot(stats_performance_lm, which = 5)

# Linearity and Homoscedasticity assumptions

linearity_homoscedasticity <- ggplot2::autoplot(stats_performance_lm, which = 1)

# Independence (standardized residuals should be made first)

learning_statistics$model_residuals <- stats_performance_lm$residuals

learning_statistics$standardized_model_residuals <- learning_statistics$model_residuals / sd(learning_statistics$model_residuals)


independece_scatter <- ggplot(learning_statistics, aes(id , standardized_model_residuals)) +
  geom_point() +
  geom_smooth() +
  labs(x = "case id" , y = "Standardized Residuals")


# Normality

normality_model <- ggplot2::autoplot(stats_performance_lm, which = 2)

robust_model_stats <- parameters::parameters(stats_performance_lm, vcov = "HC4")
robust_model_stats <- kableExtra::kable(robust_model_stats, digits = 2)