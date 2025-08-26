# Descriptive statistics of statistics anxiety
# Descriptive statistics of general anxiety
# Descriptive statistics of students' performance


descriptive_variables <- stats_tib |>
  datawizard::describe_distribution(
    c(stats_anxiety , gen_anxiety , stats_exam),
    range = FALSE
    ) |>
    knitr::kable(digits = 3)


# Box plot of statistics anxiety

box_stats_anxiety <- ggplot(stats_tib, aes(stats_anxiety)) + 
  geom_boxplot(fill = "#ff7f00") +
  labs(x = "Statistics Anxiety") +
  theme_bw()

# Box plot of general anxiety

box_general_anxiety <- ggplot(stats_tib, aes(gen_anxiety)) + 
  geom_boxplot(fill = "#ff7f00") +
  labs(x = "General Anxiety") +
  theme_bw()

# Box plot of students' performance

box_stats_exam <- ggplot(stats_tib, aes(stats_exam)) + 
  geom_boxplot(fill = "#ff7f00") +
  labs(x = "Students' Performance") +
  theme_bw()

# Descriptive statistics of statistics anxiety grouped by community group
# Descriptive statistics of general anxiety grouped by community group
# Descriptive statistics of students' performance grouped by community group

descriptive_variables_grouped <- stats_tib |>
  group_by(community) |>
  datawizard::describe_distribution(
    c(stats_anxiety , gen_anxiety , stats_exam),
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

# Violin plot of general anxiety grouped by community group

violin_stats_anxiety <- ggplot(stats_tib, aes(community, gen_anxiety , color = community)) +
  geom_violin() +
  stat_summary(fun = mean , geom = "point") +
  stat_summary(fun.data = mean_cl_normal , geom = "errorbar") +
  labs(x = "Community" , y = "General Anxiety") +
  theme_bw()

# Violin plot of students' performance grouped by community group

violin_stats_exam <- ggplot(stats_tib, aes(community, stats_exam , color = community)) +
  geom_violin() +
  stat_summary(fun = mean , geom = "point") +
  stat_summary(fun.data = mean_cl_normal , geom = "errorbar") +
  labs(x = "Community" , y = "Students' Performance") +
  theme_bw()

# Correlation matrix between statistics anxiety and students' performance
# grouped by community group

matrix_cor_community <- stats_tib |>
  group_by(community) |>
  summarise(correlation = cor(stats_anxiety , stats_exam)) |>
  knitr::kable(digits = 3)

# Plotting a matrix of scatter plots, correlations, and distribution

desc_summary_matrix <- GGally::ggscatmat(stats_tib, columns = c("stats_anxiety" ,
                                                                "gen_anxiety" ,
                                                                "stats_exam"))

# Scatter plot of the statistics anxiety against students' performance
# grouped by community group

community_cor_scatter <- ggplot(stats_tib, aes(stats_anxiety , stats_exam, color = community)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  labs(x = "Statistics Anxiety" , y = "Students' Performance") +
  theme_bw()

# Descriptive statistics of statistics anxiety grouped by universities
# Descriptive statistics of students' performance grouped by universities

descriptive_variables_grouped_uni <- stats_tib |>
  group_by(uni) |>
  datawizard::describe_distribution(
    c(stats_anxiety , stats_exam) ,
    range = FALSE) |>
  knitr::kable(digits = 3)

# Scatter plot for the relationship between statistics anxiety and students' performance split by universities

per_anx_uni <- ggplot2::ggplot(stats_tib, aes(stats_anxiety, stats_exam)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", linewidth = 0.5) +
  coord_cartesian(xlim = c(0, 65), ylim = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  labs(x = "level of statistics anxiety", y = "performance on the exam") +
  facet_wrap(~ uni, ncol = 4) +
  theme_bw()

# Defining contrasts prior to checking the impact of differtent levels of community 
# on the relationship between statistics anxiety and students' performance

# Contrast_1 for the difference between None and students in any community group

community_vs_none <- c(-2/3 , 1/3 , 1/3)

# Contrast_2 for the difference between Online and In person community groups

inperson_vs_online <- c(0 , -1/2 , 1/2)

# Adjusting coded contrasts to community 

contrasts(stats_tib$community) <- cbind(community_vs_none , inperson_vs_online)

# Fitting a pooled model

pooled_fixed_model <- lm(stats_exam ~ gen_anxiety + stats_anxiety * community, data = stats_tib)

# Fitting the pooled model for each university after nesting these variables for unviersities

university_lms <- stats_tib  |>
  dplyr::arrange(uni) |> 
  dplyr::group_by(uni)  |>  
  tidyr::nest()  |> 
  dplyr::mutate(
    model = purrr::map(.x = data, .f = \(uni_nest) lm(stats_exam ~ stats_anxiety * community + gen_anxiety, data = uni_nest)),
    coefs = purrr::map(model, broom::tidy, conf.int = TRUE)
    )

# Unnesting the estimates for university while ignoring data and model

uni_models_tib <- university_lms  |>
  dplyr::select(-c(data, model)) |> 
  tidyr::unnest(coefs) 

# Distribution plot of coeff estimates for the intercept and different slopes

random_distribution_plot <- ggplot(uni_models_tib, aes(estimate)) +
  geom_density() +
  facet_wrap(~term , scales = "free") +
  theme_bw()

# Fitting a multilevel model

final_random_model <- lmerTest::lmer(stats_exam ~ gen_anxiety + stats_anxiety * community + (stats_anxiety | uni), 
                                     data = stats_tib,
                                     control = lme4::lmerControl(optimizer = "bobyqa"))

# Extracting F statistics regarding the fixed effect of predictors on the outcome variable

f_value_model <- anova(final_random_model) |>
  knitr::kable(digits = 3)

# Defining moderation effect according to the community group 

simple_slopes_tib <- modelbased::estimate_slopes(final_random_model,
                            trend = "stats_anxiety",
                            by = "community",
                            ci = 0.95) |>
  knitr::kable(digits = 3)

# Plotting the moderation effect

moderation_plot <- interactions::interact_plot(
  model = final_random_model,
  pred = stats_anxiety,
  modx = community,
  interval = TRUE,
  x.label = "level of statistics anxiety",
  y.label = "performance on the statistics exam",
  legend.main = "assigned community"
  )

# Extracting beta estimates for fixed values

fixed_effect_estimates <- broom.mixed::tidy(final_random_model, conf.int = T , effects = "fixed") |>
  knitr::kable(digits = 3)


# Extracting standard deviations and slopes for random effects

random_effect_estimates <- broom.mixed::tidy(final_random_model, conf.int = T , effects = "ran_pars") |>
  knitr::kable(digits = 3)

# fitting a model only with random intercepts

final_random_model_intercept_only <- lmerTest::lmer(stats_exam ~ gen_anxiety + stats_anxiety * community + (1 | uni), 
                                     data = stats_tib,
                                     control = lme4::lmerControl(optimizer = "bobyqa"))

# Defining the significance of adding statistics anxiety slope as a random effect

comparing_multilevel_models <- anova(final_random_model_intercept_only, final_random_model) |>
  knitr::kable(digits = 3)

# Diagnostic plot for linearity assumption

linearity_assumption <- performance::check_model(final_random_model,
                                                   check = c("linearity"))

# Diagnostic plot for homogeneity assumption

homogeneity_assumption <- performance::check_model(final_random_model,
                                                   check = c("homogeneity"))

# Diagnostic plot for outliers 

model_outliers <- performance::check_model(final_random_model,
                                           check = c("outliers"))

# Diagnostic plot for normality assumption

normality_assumption <- performance::check_model(final_random_model, 
                                                 check = c("qq", "reqq"))