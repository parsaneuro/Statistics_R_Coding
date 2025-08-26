# Descriptive statistics of statistics anxiety
# Descriptive statistics of students' performance

descriptive_variables <- stats_tib |>
  datawizard::describe_distribution(
    c(stats_anxiety , stats_exam),
    range = FALSE
  ) |>
  knitr::kable(digits = 3)

# Histogram of statistics anxiety

hist_stats_anxiety <- ggplot(stats_tib, aes(stats_anxiety)) +
  geom_histogram(binwidth = 1, fill = "#984ea3") +
  labs(x = "Statistics Anxiety" , y = "Density") +
  theme_bw()

# Histogram of students' performance

hist_stats_exam <- ggplot(stats_tib, aes(stats_exam)) +
  geom_histogram(binwidth = 1 , fill = "#984ea3") +
  labs(x = "Students' Performance" , y = "Density") +
  theme_bw()

# Box plot of statistics anxiety

box_stats_anxiety <- ggplot(stats_tib, aes(stats_anxiety)) + 
  geom_boxplot(fill = "#ff7f00") +
  labs(x = "Statistics Anxiety" , y = "Density") +
  theme_bw()

# Box plot of students' performance

box_stats_exam <- ggplot(stats_tib, aes(stats_exam)) + 
  geom_boxplot(fill = "#ff7f00") +
  labs(x = "Students' Performance" , y = "Density") +
  theme_bw()

# Descriptive statistics of statistics anxiety grouped by community group
# Descriptive statistics of students' performance grouped by community group

descriptive_variables_grouped <- stats_tib |>
  group_by(community) |>
  datawizard::describe_distribution(
    c(stats_anxiety , stats_exam),
    range = FALSE
  ) |>
  knitr::kable(digits = 3)

# Violin plot of statistics anxiety grouped by community group

violin_stats_anxiety <- ggplot(stats_tib, aes(community, stats_anxiety , color = community)) +
  geom_violin() +
  stat_summary(fun = mean , geom = "point") +
  stat_summary(fun.data = mean_cl_normal , geom = "errorbar") +
  labs(x = "Community" , y = "Statistics Anxiety") +
  theme_bw()

# Violin plot of students' performance grouped by community group

violin_stats_exam <- ggplot(stats_tib, aes(community, stats_exam , color = community)) +
  geom_violin() +
  stat_summary(fun = mean , geom = "point") +
  stat_summary(fun.data = mean_cl_normal , geom = "errorbar") +
  labs(x = "Community" , y = "Students' Performance") +
  theme_bw()
  
# Scatter plot of statistics anxiety against students' performance

linearity_scatter <- ggplot(stats_tib, aes(stats_anxiety , stats_exam)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  labs(x = "Statistics Anxiety" , y = "Students' Performance") +
  theme_bw()

# Correlation matrix between statistics anxiety and students' performance
# grouped by community group

matrix_cor <- stats_tib |>
  group_by(community) |>
  summarise(correlation = cor(stats_anxiety , stats_exam)) |>
  knitr::kable(digits = 3)

# Scatter plot of the statistics anxirty against students' performance
# grouped by community group

community_cor_scatter <- ggplot(stats_tib, aes(stats_anxiety , stats_exam, color = community)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  labs(x = "Statistics Anxiety" , y = "Students' Performance") +
  theme_bw()

# Defining contrasts prior to checking the impact of differtent levels of community 
# on the relationship between statistics anxiety and students' performance

# Contrast_1 for the difference between None and students in any community group

community_vs_none <- c(-2/3 , 1/3 , 1/3)

# Contrast_2 for the difference between Online and In person community groups

inperson_vs_online <- c(0 , -1/2 , 1/2)

# Adjusting coded contrasts to community 

stats_tib_contrast <- stats_tib
contrasts(stats_tib_contrast$community) <- cbind(community_vs_none , inperson_vs_online)

# Fitting the general linear model to see the moderation effect

normal_lm <- lm(stats_exam ~ stats_anxiety * community , data = stats_tib_contrast)

# Defining the result related to the main effect and interaction effect

main_inter_result <- car::Anova(normal_lm , type = 3) |>
  knitr::kable(digits = 3)

# Defining the effect size using partial omega square

effect_omega_result <- car::Anova(normal_lm , type = 3) |>
  effectsize::omega_squared(ci = 0.95) |>
  knitr::kable(digits = 3)

# Simple slopes

simple_slopes <- emmeans::joint_tests(normal_lm , "community") |>
  knitr::kable(digits = 3)

# make the results tidy and adding confidence intervals

estimate_result <- broom::tidy(normal_lm , conf.int = TRUE) |>
  knitr::kable(digits = 3)

# Influential cases

outlier_cases <- ggplot2::autoplot(normal_lm, which = 4)

# Linearity and Homoscedasticity assumptions

linearity_homoscedasticity <- ggplot2::autoplot(normal_lm, which = 1)

# Independence (standardized residuals should be made first)

stats_tib_contrast$model_residuals <- normal_lm$residuals

stats_tib_contrast$standardized_model_residuals <- stats_tib_contrast$model_residuals / sd(stats_tib_contrast$model_residuals)


independece_scatter <- ggplot(stats_tib_contrast, aes(id , standardized_model_residuals)) +
  geom_point() +
  geom_smooth() +
  labs(x = "case id" , y = "Standardized Residuals")


# Normality

normality_model <- ggplot2::autoplot(normal_lm, which = 2)

# A new robust model will be defined

stats_tib_contrast_rob <- stats_tib_contrast

normal_rob_lm <- robust::lmRob(stats_exam ~ stats_anxiety * community , data = stats_tib_contrast_rob)

# Defining the bias between models

normal_rob_lm_bias <- summary(normal_rob_lm)

bias_models <- normal_rob_lm_bias$biasTest |>
  knitr::kable(digits = 3)

# Defining the result related to the main effect and interaction effect

normal_rob_lm_result <- car::Anova(normal_rob_lm , type = 3) |>
  knitr::kable(digits = 2)


# Robust model fitted since the homoscedasticity assumption was not met

normal_rob <- parameters::model_parameters(normal_lm , ROBUST = TRUE , vcov = "HC4") |>
  knitr::kable(digits = 3)