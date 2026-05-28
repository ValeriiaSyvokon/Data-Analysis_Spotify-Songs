suppressPackageStartupMessages({
  required_packages <- c(
    "boot", "dplyr", "forcats", "ggplot2", "grid", "purrr",
    "readr", "stringr", "tibble", "tidyr"
  )
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Missing packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install them before running the script."
    )
  }

  library(boot)
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(grid)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

options(stringsAsFactors = FALSE, scipen = 999, warn = -1)

RUN_FIRST_PART <- getOption("lab2.run_first_part", TRUE)
RUN_INFERENCE_PART <- getOption("lab2.run_inference_part", TRUE)
RUN_Q5 <- getOption("lab2.run_q5", TRUE)
RUN_Q6 <- getOption("lab2.run_q6", TRUE)
RUN_Q7 <- getOption("lab2.run_q7", TRUE)
RUN_Q9 <- getOption("lab2.run_q9", TRUE)
OUTPUT_DIR <- getOption("lab2.output_dir", file.path(getwd(), "lab2_final_outputs"))

section_title <- function(title) {
  cat("\n\n", strrep("=", 72), "\n", title, "\n", strrep("=", 72), "\n", sep = "")
}

print_table <- function(x, n = Inf, width = Inf) {
  if (inherits(x, "data.frame")) {
    print(x, n = n, width = width)
  } else {
    print(x)
  }
}

set.seed(42)
B <- getOption("lab2.main_boot_reps", 10)
alpha <- 0.05
z_val <- qnorm(1 - alpha / 2)

input_path <- file.path(getwd(), "songs_clean.rds")
if (!file.exists(input_path)) {
  stop("File not found: ", input_path)
}

section_title("Завантаження даних")
songs_df_clean <- readRDS(input_path)
cat(sprintf("Дані завантажено: %s рядків, %s колонок\n", format(nrow(songs_df_clean), big.mark = " "), ncol(songs_df_clean)))
cat(sprintf("B для першої частини: %d\n", B))
cat(sprintf("Перша частина: %s\n", ifelse(RUN_FIRST_PART, "запускається", "пропущена")))
cat(sprintf("Друга частина: %s\n", ifelse(RUN_INFERENCE_PART, "запускається", "пропущена")))
cat(sprintf("Питання другої частини: Q5=%s, Q6=%s, Q7=%s, Q9=%s\n", RUN_Q5, RUN_Q6, RUN_Q7, RUN_Q9))





median_stat <- function(data, indices) {
  sample_x <- data[indices]
  med <- median(sample_x, na.rm = TRUE)
  
  inner_meds <- replicate(50, median(sample(sample_x, replace = TRUE), na.rm = TRUE))
  var_med <- var(inner_meds)
  
  return(c(med, var_med)) 
}

bootstrap_median_ci_boot <- function(x, R = 100, conf = 0.95) {
  boot_obj <- boot(data = x, statistic = median_stat, R = R)
  med_val <- median(x, na.rm = TRUE)
  
  if (var(boot_obj$t[, 1]) == 0) {
    val_str <- sprintf("(%.2f ; %.2f)", med_val, med_val)
    return(tibble(
      norm_ci  = val_str, basic_ci = val_str,
      stud_ci  = val_str, perc_ci  = val_str
    ))
  }
  
  safe_extract_ci <- function(b_obj, ci_type) {
    res <- tryCatch({ boot.ci(b_obj, type = ci_type, conf = conf) }, error = function(e) return(NULL))
    if (is.null(res)) return("(NA ; NA)")
    
    if (ci_type == "norm")  return(sprintf("(%.2f ; %.2f)", res$normal[2],  res$normal[3]))
    if (ci_type == "basic") return(sprintf("(%.2f ; %.2f)", res$basic[4],   res$basic[5]))
    if (ci_type == "stud")  return(sprintf("(%.2f ; %.2f)", res$student[4], res$student[5]))
    if (ci_type == "perc")  return(sprintf("(%.2f ; %.2f)", res$percent[4], res$percent[5]))
  }
  
  tibble(
    norm_ci  = safe_extract_ci(boot_obj, "norm"),
    basic_ci = safe_extract_ci(boot_obj, "basic"),
    stud_ci  = safe_extract_ci(boot_obj, "stud"),
    perc_ci  = safe_extract_ci(boot_obj, "perc")
  )
}

calc_median <- function(data, indices) {
  return(median(data[indices], na.rm = TRUE))
}

print_wald_results <- function(results_df, title) {
  cat(paste0("\n--- ", title, " ---\n"))
  formatted_df <- results_df %>%
    select(genre, starts_with("diff"), W_stat, p_value, p_adj_BH, reject_H0) %>%
    mutate(
      W_stat = round(W_stat, 3),
      p_value = formatC(p_value, format = "e", digits = 2),
      p_adj_BH = formatC(p_adj_BH, format = "e", digits = 2)
    )
  print(formatted_df, width = Inf)
}

run_wald_test_medians <- function(df, B_iter = 2000) {
  stats_median <- df %>%
    mutate(explicit_cat = ifelse(explicit == "Explicit", "Explicit", "Clean")) %>%
    group_by(genre, explicit_cat) %>%
    filter(n() >= 10) %>%
    summarise(
      n = n(),
      median_pop = median(popularity, na.rm = TRUE),
      se_boot = sd(boot(data = popularity, statistic = calc_median, R = B_iter)$t),
      var_hat = se_boot^2,
      .groups = "drop"
    )
  
  results <- stats_median %>%
    select(genre, explicit_cat, median_pop, var_hat) %>%
    pivot_wider(names_from = explicit_cat, values_from = c(median_pop, var_hat)) %>%
    drop_na() %>%
    mutate(
      diff_median = median_pop_Explicit - median_pop_Clean,
      se_diff = sqrt(var_hat_Explicit + var_hat_Clean),
      W_stat = diff_median / se_diff,
      p_value = pnorm(W_stat, lower.tail = FALSE),
      p_adj_BH = p.adjust(p_value, method = "BH"),
      reject_H0 = p_adj_BH < 0.05
    ) %>%
    arrange(p_adj_BH)
  return(results)
}

run_wald_test_means <- function(df) {
  stats_mean <- df %>%
    mutate(explicit_cat = ifelse(explicit == "Explicit", "Explicit", "Clean")) %>%
    group_by(genre, explicit_cat) %>%
    filter(n() >= 10) %>%
    summarise(
      n = n(),
      mean_pop = mean(popularity, na.rm = TRUE),
      var_hat = var(popularity, na.rm = TRUE) / n(),
      .groups = "drop"
    )
  
  results <- stats_mean %>%
    select(genre, explicit_cat, mean_pop, var_hat) %>%
    pivot_wider(names_from = explicit_cat, values_from = c(mean_pop, var_hat)) %>%
    drop_na() %>%
    mutate(
      diff_mean = mean_pop_Explicit - mean_pop_Clean,
      se_diff = sqrt(var_hat_Explicit + var_hat_Clean),
      W_stat = diff_mean / se_diff,
      p_value = pnorm(W_stat, lower.tail = FALSE),
      p_adj_BH = p.adjust(p_value, method = "BH"),
      reject_H0 = p_adj_BH < 0.05
    ) %>%
    arrange(p_adj_BH)
  return(results)
}

calc_mean_ci_wald <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if(n < 2) return(tibble(mean = NA, ci_lower = NA, ci_upper = NA))
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  tibble(mean = m, ci_lower = m - z_val * se, ci_upper = m + z_val * se)
}

calc_prop_ci_wald <- function(successes, total) {
  p_hat <- successes / total
  se <- sqrt((p_hat * (1 - p_hat)) / total)
  tibble(p_hat = p_hat, ci_lower = p_hat - z_val * se, ci_upper = p_hat + z_val * se)
}

calc_category_counts_ci <- function(data, group_col) {
  N_total <- nrow(data)
  z_val_975 <- qnorm(0.975)
  data %>%
    group_by(!!sym(group_col)) %>%
    summarise(count = n(), .groups = "drop") %>%
    drop_na() %>%
    mutate(
      p_hat = count / N_total,
      se_count = sqrt(N_total * p_hat * (1 - p_hat)),
      ci_count_lower = count - z_val_975 * se_count,
      ci_count_upper = count + z_val_975 * se_count,
      se_prop = sqrt((p_hat * (1 - p_hat)) / N_total),
      ci_prop_lower = p_hat - z_val_975 * se_prop,
      ci_prop_upper = p_hat + z_val_975 * se_prop,
      CI_Count = sprintf("(%.0f ; %.0f)", ci_count_lower, ci_count_upper),
      CI_Prop = sprintf("(%.4f ; %.4f)", ci_prop_lower, ci_prop_upper)
    ) %>%
    arrange(desc(count)) %>%
    select(Category = !!sym(group_col), Count = count, CI_Count, Prop = p_hat, CI_Prop)
}

bootstrap_median_ci <- function(x, R_val = B) {
  x <- na.omit(x)
  if(length(x) < 5) return(tibble(median = NA, ci_lower = NA, ci_upper = NA))
  boot_obj <- boot(data = x, statistic = median_stat, R = R_val)
  if (var(boot_obj$t[, 1]) == 0) {
    return(tibble(median = median(x), ci_lower = boot_obj$t[1, 1], ci_upper = boot_obj$t[1, 1]))
  }
  ci <- boot.ci(boot_obj, type = "perc")
  tibble(median = median(x), ci_lower = ci$percent[4], ci_upper = ci$percent[5])
}


if (RUN_FIRST_PART) {
cat("\n\n=============== БЛОКИ ВЛАДА ===============\n")

result_popularity <- songs_df_clean %>%
  group_by(genre) %>%
  summarise(
    n = n(),
    median = median(popularity, na.rm = TRUE),
    stats = list({
      cat(sprintf("Обробка: %s | n = %d\n", genre[1], n()))
      bootstrap_median_ci_boot(popularity, R = B, conf = 1 - alpha)
    }),
    .groups = "drop"
  ) %>%
  unnest(stats) %>%
  arrange(desc(median))

cat("\n--- Довірчі інтервали популярності пісень за жанрами ---\n")
print(result_popularity, width = Inf)

popular_songs_ci <- songs_df_clean %>%
  group_by(genre) %>%
  summarise(
    total_n = n(),
    hit_count = sum(popularity >= 50), 
    .groups = "drop"
  ) %>%
  mutate(
    p_hat = hit_count / total_n,
    se_count = sqrt(total_n * p_hat * (1 - p_hat)),
    z_val_975 = qnorm(0.975),
    ci_lower = hit_count - z_val_975 * se_count,
    ci_upper = hit_count + z_val_975 * se_count,
    p_hat = round(p_hat, 4),
    se_count = round(se_count, 2),
    ci_formatted = sprintf("(%.2f ; %.2f)", ci_lower, ci_upper)
  ) %>%
  arrange(desc(hit_count)) %>%
  select(genre, total_n, hit_count, ci_formatted, p_hat, se_count)

cat("\n--- Довірчі інтервали кількості хітів (Popularity >= 50) ---\n")
print(popular_songs_ci)

result_pop_50_plus <- songs_df_clean %>%
  filter(popularity >= 50) %>%
  group_by(genre) %>%
  summarise(
    n = n(),
    median = median(popularity, na.rm = TRUE),
    stats = list(bootstrap_median_ci_boot(popularity, R = B, conf = 1 - alpha)),
    .groups = "drop"
  ) %>%
  unnest(stats) %>%
  arrange(desc(median))

cat("\n--- Довірчі інтервали для пісень з популярністю 50+ ---\n")
print(result_pop_50_plus)

result_yearly_genre <- songs_df_clean %>%
  group_by(year, genre) %>%
  filter(n() >= 10) %>% 
  summarise(
    n = n(),
    median = median(popularity, na.rm = TRUE),
    stats = list(bootstrap_median_ci_boot(popularity, R = B, conf = 1 - alpha)),
    .groups = "drop"
  ) %>%
  unnest(stats) %>%
  arrange(desc(year), genre)

cat("\n--- Довірчі інтервали за роками та жанрами ---\n")
print(result_yearly_genre)

result_duration <- songs_df_clean %>%
  filter(duration_ms <= 600000) %>%
  mutate(duration_min = duration_ms / 60000) %>%
  group_by(genre) %>%
  filter(n() >= 10) %>% 
  summarise(
    n = n(),
    median = median(duration_min, na.rm = TRUE),
    stats = list({
      cat(sprintf("Обробка: %s | n = %d\n", genre[1], n()))
      bootstrap_median_ci_boot(duration_min, R = B, conf = 1 - alpha)
    }),
    .groups = "drop"
  ) %>%
  unnest(stats) %>%
  arrange(desc(median))

cat("\n--- Довірчі інтервали для тривалості (<= 10 хв) за жанрами ---\n")
print(result_duration, width = Inf)

result_explicit <- songs_df_clean %>%
  group_by(explicit) %>%
  filter(n() >= 10) %>% 
  summarise(
    n = n(),
    median = median(popularity, na.rm = TRUE), 
    stats = list(bootstrap_median_ci_boot(popularity, R = B, conf = 1 - alpha)),
    .groups = "drop"
  ) %>%
  unnest(stats) %>%
  arrange(desc(median))

cat("\n--- Довірчі інтервали популярності за наявністю Explicit контенту ---\n")
print(result_explicit)

result_genre_explicit <- songs_df_clean %>%
  mutate(explicit_cat = ifelse(explicit == "Explicit", "Explicit", "Clean")) %>%
  group_by(genre, explicit_cat) %>%
  filter(n() >= 10) %>% 
  summarise(
    n = n(),
    median_val = median(popularity, na.rm = TRUE),
    stats = list({
      cat(sprintf("Обробка: %s (категорія: %s) | Кількість пісень: %d\n", 
                  genre[1], explicit_cat[1], n()))
      bootstrap_median_ci_boot(popularity, R = B, conf = 1 - alpha)
    }),
    .groups = "drop"
  ) %>%
  unnest(stats) %>%
  group_by(genre) %>%
  mutate(genre_avg_median = mean(median_val, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(genre_avg_median), explicit_cat) %>%
  select(-genre_avg_median, -median_val)

medians_df <- songs_df_clean %>%
  mutate(explicit_cat = ifelse(explicit == "Explicit", "Explicit", "Clean")) %>%
  group_by(genre, explicit_cat) %>%
  summarise(median = median(popularity, na.rm = TRUE), .groups = "drop")

result_genre_explicit <- result_genre_explicit %>%
  select(-any_of(c("median", "median_val"))) %>% 
  left_join(medians_df, by = c("genre", "explicit_cat")) %>%
  relocate(median, .after = n)

cat("\n--- Довірчі інтервали (5 типів) популярності за жанрами та наявністю матів ---\n")
print(result_genre_explicit, width = Inf)



cat("\n\n=============== БЛОКИ ЛЄРИ: ГРУПА 1 ===============\n")

cat("\n--- ДІ для кількості за ЖАНРАМИ ---\n")
ci_genres <- calc_category_counts_ci(songs_df_clean, "genre")
print(ci_genres)

cat("\n--- ДІ для кількості за МУЗИЧНИМ ЛАДОМ (Mode) ---\n")
ci_mode <- calc_category_counts_ci(songs_df_clean, "mode")
print(ci_mode)

cat("\n--- ДІ для кількості за EXPLICIT контентом ---\n")
ci_explicit <- calc_category_counts_ci(songs_df_clean, "explicit")
print(ci_explicit)

cat("\n--- ДІ для кількості за ТОНАЛЬНІСТЮ (Key) ---\n")
ci_key <- calc_category_counts_ci(songs_df_clean, "key")
print(ci_key)

cat("\n--- ДІ для кількості за РОКАМИ (Топ-10 наймасовіших) ---\n")
ci_years <- calc_category_counts_ci(songs_df_clean, "year")
print(head(ci_years, 10))

n_total <- nrow(songs_df_clean)
n_zero <- sum(songs_df_clean$popularity == 0, na.rm = TRUE)
zero_pop_ci <- calc_prop_ci_wald(n_zero, n_total)

cat("\n1.1 Частка треків з popularity == 0:\n")
cat(sprintf("Оцінка: %.4f, 95%% CI: [%.4f; %.4f]\n", zero_pop_ci$p_hat, zero_pop_ci$ci_lower, zero_pop_ci$ci_upper))

metrics_for_mean <- c("tempo", "duration_ms", "total_artist_followers", 
                      "avg_artist_popularity", "danceability", "energy", "loudness")

cat("\n1.2 Асимптотичні ДІ для середніх значень всього датасету:\n")
means_ci_results <- map_dfr(metrics_for_mean, function(metric) {
  res <- calc_mean_ci_wald(songs_df_clean[[metric]])
  tibble(Metric = metric, Mean = res$mean, CI = sprintf("[%.3f; %.3f]", res$ci_lower, res$ci_upper))
})
print(means_ci_results)

cat("\n\n--- 1.3a ДІ для часток explicit: Топ-15 за ЧАСТКОЮ (Найбільш explicit) ---\n")
explicit_highest_niches_ci <- songs_df_clean %>%
  filter(!is.na(niche_genres), !is.na(explicit)) %>%
  group_by(niche_genres, genre) %>%
  summarise(
    total_tracks = n(),
    explicit_count = sum(explicit == "Explicit"),
    .groups = 'drop'
  ) %>%
  filter(total_tracks >= 50) %>%
  mutate(p_hat_temp = explicit_count / total_tracks) %>%
  arrange(desc(p_hat_temp)) %>%
  slice_head(n = 15) %>%
  rowwise() %>%
  mutate(
    ci_res = list(calc_prop_ci_wald(explicit_count, total_tracks)),
    p_hat = ci_res$p_hat,
    CI = sprintf("[%.3f; %.3f]", ci_res$ci_lower, ci_res$ci_upper),
    label_full = paste0(niche_genres, " (", genre, ")")
  ) %>%
  select(label_full, total_tracks, explicit_count, p_hat, CI)

print(explicit_highest_niches_ci)

cat("\n\n--- 1.3b ДІ для часток explicit: Топ-15 НАЙМАСОВІШИХ нішевих жанрів ---\n")
top_15_massive_niches <- songs_df_clean %>%
  filter(!is.na(niche_genres)) %>%
  count(niche_genres, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(niche_genres)

explicit_massive_niches_ci <- songs_df_clean %>%
  filter(niche_genres %in% top_15_massive_niches, !is.na(explicit)) %>%
  group_by(niche_genres) %>%
  summarise(
    total_tracks = n(),
    explicit_count = sum(explicit == "Explicit"),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(
    ci_res = list(calc_prop_ci_wald(explicit_count, total_tracks)),
    p_hat = ci_res$p_hat,
    CI = sprintf("[%.3f; %.3f]", ci_res$ci_lower, ci_res$ci_upper)
  ) %>%
  arrange(desc(p_hat)) %>%
  select(niche_genres, total_tracks, explicit_count, p_hat, CI)

print(explicit_massive_niches_ci)

cat("\n\n--- 1.4 ДІ для Топ-15 нішевих жанрів (Count, Danceability, Energy, Popularity) ---\n")
N_total <- nrow(songs_df_clean) 

top_niches_stats <- songs_df_clean %>%
  filter(!is.na(niche_genres)) %>%
  group_by(niche_genres) %>%
  summarise(
    count = n(),
    mean_dance = mean(danceability, na.rm = TRUE),
    sd_dance = sd(danceability, na.rm = TRUE),
    mean_energy = mean(energy, na.rm = TRUE),
    sd_energy = sd(energy, na.rm = TRUE),
    mean_pop = mean(popularity, na.rm = TRUE),
    sd_pop = sd(popularity, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(count > 500) %>%
  slice_max(order_by = count, n = 15)

top_niches_ci <- top_niches_stats %>%
  mutate(
    p_hat = count / N_total,
    se_count = sqrt(N_total * p_hat * (1 - p_hat)),
    CI_Count = sprintf("[%d ; %d]", round(count - z_val * se_count), round(count + z_val * se_count)),
    
    se_dance = sd_dance / sqrt(count),
    CI_Dance = sprintf("[%.3f ; %.3f]", mean_dance - z_val * se_dance, mean_dance + z_val * se_dance),
    
    se_energy = sd_energy / sqrt(count),
    CI_Energy = sprintf("[%.3f ; %.3f]", mean_energy - z_val * se_energy, mean_energy + z_val * se_energy),
    
    se_pop = sd_pop / sqrt(count),
    CI_Pop = sprintf("[%.2f ; %.2f]", mean_pop - z_val * se_pop, mean_pop + z_val * se_pop)
  ) %>%
  select(
    niche_genres, 
    Count = count, CI_Count, 
    Mean_Dance = mean_dance, CI_Dance, 
    Mean_Energy = mean_energy, CI_Energy, 
    Mean_Pop = mean_pop, CI_Pop
  )

print(top_niches_ci, width = Inf)

cat("\n\n--- 1.5 ДІ для середньої популярності Топ-10 альбомів (Група 1) ---\n")
top_albums_ci <- songs_df_clean %>%
  filter(!is.na(album_name)) %>%
  group_by(album_name) %>%
  summarise(
    track_count = n(),
    avg_album_pop = mean(popularity, na.rm = TRUE),
    sd_album_pop = sd(popularity, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(track_count >= 8) %>%
  slice_max(order_by = avg_album_pop, n = 10, with_ties = FALSE) %>%
  rowwise() %>%
  mutate(
    se_pop = sd_album_pop / sqrt(track_count),
    CI_Z_Normal = sprintf("[%.2f ; %.2f]", 
                          avg_album_pop - z_val * se_pop, 
                          avg_album_pop + z_val * se_pop),
    t_val = qt(0.975, df = track_count - 1), 
    CI_T_Student = sprintf("[%.2f ; %.2f]", 
                           avg_album_pop - t_val * se_pop, 
                           avg_album_pop + t_val * se_pop)
  ) %>%
  select(album_name, track_count, avg_album_pop, CI_Z_Normal, CI_T_Student) %>%
  arrange(desc(avg_album_pop))

print(top_albums_ci, width = Inf)

album_types_data <- songs_df_clean %>%
  filter(!is.na(album_name)) %>%
  group_by(album_name) %>%
  summarise(
    track_count = n(), 
    max_pop = max(popularity, na.rm = TRUE), 
    median_pop = median(popularity, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  filter(track_count >= 6, max_pop >= 75) %>%
  mutate(album_type = case_when(
    median_pop >= 55 ~ "Bestseller",
    median_pop < 30 ~ "One-hit",
    TRUE ~ "Middle"
  ))

n_albums <- nrow(album_types_data)
cat("\n1.6 Частки типів успішних альбомів:\n")
album_types_data %>%
  count(album_type) %>%
  rowwise() %>%
  mutate(
    ci_res = list(calc_prop_ci_wald(n, n_albums)),
    p_hat = ci_res$p_hat,
    CI = sprintf("[%.3f; %.3f]", ci_res$ci_lower, ci_res$ci_upper)
  ) %>% select(album_type, n, p_hat, CI) %>% print()


cat("\n\n=============== БЛОКИ ЛЄРИ: ГРУПА 2 ===============\n")

cat("\n2.1 Бутстреп ДІ для загальних медіан (може зайняти час):\n")
medians_ci_results <- map_dfr(metrics_for_mean, function(metric) {
  res <- bootstrap_median_ci(songs_df_clean[[metric]])
  tibble(Metric = metric, Median = res$median, CI = sprintf("[%.3f; %.3f]", res$ci_lower, res$ci_upper))
})
print(medians_ci_results)

cat("\n--- 2.2 Анатомія боксплотів: Q1, Медіана, Q3 у нішах Classical (n >= 20) ---\n")
classical_niches <- songs_df_clean %>%
  filter(genre == "Classical", !is.na(niche_genres), !is.na(popularity)) %>%
  group_by(niche_genres) %>% 
  filter(n() >= 20) %>% 
  ungroup()

boxplot_stats_fun <- function(data, indices) {
  return(quantile(data[indices], probs = c(0.25, 0.50, 0.75), na.rm = TRUE))
}

bootstrap_boxplot_ci <- function(x, R_val = B) {
  x <- na.omit(x)
  if(length(x) < 5) return(tibble(Q1=NA, CI_Q1=NA, Median=NA, CI_Median=NA, Q3=NA, CI_Q3=NA))
  
  boot_obj <- boot(data = x, statistic = boxplot_stats_fun, R = R_val)
  
  get_safe_ci <- function(b_obj, idx) {
    if (var(b_obj$t[, idx]) == 0) {
      return(c(b_obj$t[1, idx], b_obj$t[1, idx]))
    } else {
      return(boot.ci(b_obj, type = "perc", index = idx)$percent[4:5])
    }
  }
  
  ci_q1  <- get_safe_ci(boot_obj, 1)
  ci_med <- get_safe_ci(boot_obj, 2)
  ci_q3  <- get_safe_ci(boot_obj, 3)
  
  tibble(
    Q1 = boot_obj$t0[1],
    CI_Q1 = sprintf("[%.2f; %.2f]", ci_q1[1], ci_q1[2]),
    Median = boot_obj$t0[2],
    CI_Median = sprintf("[%.2f; %.2f]", ci_med[1], ci_med[2]),
    Q3 = boot_obj$t0[3],
    CI_Q3 = sprintf("[%.2f; %.2f]", ci_q3[1], ci_q3[2])
  )
}

classical_boxplot_ci <- classical_niches %>%
  group_by(niche_genres) %>%
  summarise(
    n = n(),
    stats = list(bootstrap_boxplot_ci(popularity, R_val = B)), 
    .groups = "drop"
  ) %>%
  unnest(stats) %>%
  arrange(desc(Median))

print(classical_boxplot_ci, n = Inf, width = Inf)

cat("\n\n--- 2.3 Анатомія боксплотів: Топ-10 альбомів (Q1, Медіана, Q3) ---\n")
top_10_albums_list <- songs_df_clean %>%
  filter(!is.na(album_name)) %>%
  group_by(album_name) %>%
  mutate(track_count = n(), avg_album_pop = mean(popularity, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(track_count >= 8) %>%
  filter(dense_rank(desc(avg_album_pop)) <= 10) %>%
  pull(album_name) %>%
  unique()

top_albums_boxplot_ci <- songs_df_clean %>%
  filter(album_name %in% top_10_albums_list) %>%
  group_by(album_name) %>%
  summarise(
    track_count = n(),
    stats = list(bootstrap_boxplot_ci(popularity, R_val = B)),
    .groups = "drop"
  ) %>%
  unnest(stats) %>%
  arrange(desc(Median))

print(top_albums_boxplot_ci, n = Inf, width = Inf)


cat("\n\n=============== БЛОКИ ЛЄРИ: ГРУПА 3 (СКЛАДНІ ФУНКЦІОНАЛИ) ===============\n")

skew_boot_fun <- function(data, indices) {
  x <- na.omit(data[indices])
  n <- length(x)
  if(n < 3) return(NA) 
  return((sum((x - mean(x))^3) / n) / (var(x)^(3/2)))
}

cat("\n3.1 Бутстреп ДІ для Коефіцієнта Асиметрії (Skewness):\n")
skew_features <- c("tempo", "valence", "speechiness", "energy", "loudness", 
                   "danceability", "acousticness", "instrumentalness", "liveness")

skew_ci_results <- map_dfr(skew_features, function(feat) {
  boot_skew <- boot(data = songs_df_clean[[feat]], statistic = skew_boot_fun, R = B)
  ci_skew <- boot.ci(boot_skew, type = "perc")
  
  tibble(
    Metric = feat,
    Skewness = boot_skew$t0,
    Direction = ifelse(boot_skew$t0 > 0, "Правостороння (> 0)", "Лівостороння (< 0)"),
    CI = sprintf("[%.4f; %.4f]", ci_skew$percent[4], ci_skew$percent[5])
  )
})
print(skew_ci_results)

cat("\n\n--- 3.2 Бутстреп ДІ для pop_gap: Топ-10 альбомів 'Одного хіта' (Група 3) ---\n")
one_hit_albums_list <- album_types_data %>%
  filter(album_type == "One-hit") %>%
  arrange(desc(max_pop - median_pop)) %>%
  slice_head(n = 10) %>%
  pull(album_name)

pop_gap_boot_fun <- function(data, indices) {
  x <- na.omit(data[indices])
  return(max(x) - median(x))
}

bootstrap_gap_ci <- function(x, R_val = B) {
  x <- na.omit(x)
  if(length(x) < 3) return(tibble(Gap = NA, CI_Gap = NA))
  
  boot_obj <- boot(data = x, statistic = pop_gap_boot_fun, R = R_val)
  
  if (var(boot_obj$t[, 1]) == 0) {
    ci_formatted <- sprintf("[%.1f; %.1f]", boot_obj$t[1, 1], boot_obj$t[1, 1])
  } else {
    ci <- boot.ci(boot_obj, type = "perc")
    ci_formatted <- sprintf("[%.1f; %.1f]", ci$percent[4], ci$percent[5])
  }
  tibble(Gap = boot_obj$t0, CI_Gap = ci_formatted)
}

one_hit_albums_ci <- songs_df_clean %>%
  filter(album_name %in% one_hit_albums_list) %>%
  group_by(album_name) %>%
  summarise(
    track_count = n(),
    stats = list(bootstrap_gap_ci(popularity, R_val = B)),
    .groups = "drop"
  ) %>%
  unnest(stats) %>%
  arrange(desc(Gap))

print(one_hit_albums_ci, width = Inf)

get_mode_density <- function(x) {
  x <- na.omit(x)
  d <- density(x)
  return(d$x[which.max(d$y)])
}

diff_mode_boot_fun <- function(data, indices) {
  d <- data[indices, ]
  mode_major <- get_mode_density(d$valence[d$mode == "Major"])
  mode_minor <- get_mode_density(d$valence[d$mode == "Minor"])
  return(mode_minor - mode_major) 
}

cat("\n3.3 Різниця піків щільності (Мод) Valence (Minor - Major):\n")
valence_data <- songs_df_clean %>% filter(!is.na(mode), !is.na(valence))
valence_sample <- valence_data %>% group_by(mode) %>% slice_sample(n = 10000) %>% ungroup()

boot_mode_diff <- boot(data = valence_sample, statistic = diff_mode_boot_fun, R = B)
ci_mode_diff <- boot.ci(boot_mode_diff, type = "perc")

cat(sprintf("Різниця Мод (Minor - Major): %.4f\n", boot_mode_diff$t0))
cat(sprintf("95%% Percentile CI: [%.4f; %.4f]\n", ci_mode_diff$percent[4], ci_mode_diff$percent[5]))


cat("\n\n=============== ДОДАТКОВІ СТАТИСТИКИ (ІЗ ТАБЛИЦІ) ===============\n")

cat("\n\n--- Дод. 1: Різниця часток хітів (Explicit vs Clean) ---\n")
hit_prop_data <- songs_df_clean %>%
  filter(!is.na(explicit)) %>%
  group_by(explicit) %>%
  summarise(total = n(), hits = sum(popularity >= 75, na.rm = TRUE), p_hat = hits/total, .groups="drop")

p_exp <- hit_prop_data$p_hat[hit_prop_data$explicit == "Explicit"]
n_exp <- hit_prop_data$total[hit_prop_data$explicit == "Explicit"]
p_cln <- hit_prop_data$p_hat[hit_prop_data$explicit == "Clean"]
n_cln <- hit_prop_data$total[hit_prop_data$explicit == "Clean"]

diff_p <- p_exp - p_cln
se_diff_p <- sqrt((p_exp*(1-p_exp)/n_exp) + (p_cln*(1-p_cln)/n_cln))
wald_W_prop <- diff_p / se_diff_p
p_val_prop <- 2 * (1 - pnorm(abs(wald_W_prop)))

cat(sprintf("Частка хітів (Explicit): %.4f, (Clean): %.4f\n", p_exp, p_cln))
cat(sprintf("Різниця: %.4f, 95%% CI: [%.4f; %.4f]\n", diff_p, diff_p - z_val*se_diff_p, diff_p + z_val*se_diff_p))
cat(sprintf("Тест Волда: W = %.2f, p-value = %e\n", wald_W_prop, p_val_prop))

cat("\n\n--- Дод. 2: Різниця медіан acousticness (До 1980 vs З 2000 року) ---\n")
evo_data <- songs_df_clean %>%
  filter(!is.na(year), !is.na(acousticness)) %>%
  mutate(period = case_when(year < 1980 ~ "Old", year >= 2000 ~ "New", TRUE ~ "Middle")) %>%
  filter(period != "Middle")

diff_median_boot_fun <- function(data, indices) {
  d <- data[indices, ]
  return(median(d$acousticness[d$period == "Old"], na.rm=TRUE) - median(d$acousticness[d$period == "New"], na.rm=TRUE))
}

evo_sample <- evo_data %>% group_by(period) %>% slice_sample(n = 10000) %>% ungroup()

boot_diff <- boot(data = evo_sample, statistic = diff_median_boot_fun, R = B)
ci_diff <- boot.ci(boot_diff, type = "perc")
diff_est <- boot_diff$t0
se_diff_boot <- sd(boot_diff$t)

wald_W_med <- diff_est / se_diff_boot
p_val_med <- 2 * (1 - pnorm(abs(wald_W_med)))

cat(sprintf("Різниця медіан (Old - New): %.4f\n", diff_est))
cat(sprintf("95%% Percentile CI: [%.4f; %.4f]\n", ci_diff$percent[4], ci_diff$percent[5]))
cat(sprintf("Тест Волда (SE з бутстрепу): W = %.2f, p-value = %e\n", wald_W_med, p_val_med))

cat("\n\n--- Дод. 3: Різниця медіан popularity (Explicit vs Clean) у Hip-Hop (Група 2) ---\n")
hiphop_data <- songs_df_clean %>%
  filter(genre == "Hip-Hop", !is.na(explicit), !is.na(popularity))

diff_median_hiphop_fun <- function(data, indices) {
  d <- data[indices, ]
  return(median(d$popularity[d$explicit == "Explicit"], na.rm = TRUE) - median(d$popularity[d$explicit == "Clean"], na.rm = TRUE))
}

boot_hiphop_diff <- boot(data = hiphop_data, statistic = diff_median_hiphop_fun, R = B)
ci_hiphop <- boot.ci(boot_hiphop_diff, type = "perc")
diff_est_hiphop <- boot_hiphop_diff$t0
se_boot_hiphop <- sd(boot_hiphop_diff$t) 

wald_W_hiphop <- diff_est_hiphop / se_boot_hiphop
p_val_hiphop <- 2 * (1 - pnorm(abs(wald_W_hiphop)))

cat(sprintf("Різниця медіан (Explicit - Clean): %.2f балів\n", diff_est_hiphop))
cat(sprintf("95%% Percentile CI: [%.2f; %.2f]\n", ci_hiphop$percent[4], ci_hiphop$percent[5]))
cat(sprintf("Бутстреп SE: %.4f\n", se_boot_hiphop))
cat(sprintf("Тест Волда: W = %.2f, p-value = %e\n", wald_W_hiphop, p_val_hiphop))

cat("\n\n--- Дод. 4: Кореляція фан-бази та популярності (Малі vs Великі артисти) ---\n")
followers_data <- songs_df_clean %>%
  filter(!is.na(total_artist_followers), !is.na(popularity)) %>%
  mutate(size_group = case_when(
    total_artist_followers < 1000000 ~ "Small",     
    total_artist_followers > 10000000 ~ "Big",      
    TRUE ~ "Medium"
  )) %>%
  filter(size_group != "Medium")

followers_sample <- followers_data %>% group_by(size_group) %>% slice_sample(n = 5000) %>% ungroup()

cor_diff_boot_fun <- function(data, indices) {
  d <- data[indices, ]
  d_small <- d[d$size_group == "Small", ]
  rho_small <- cor(log10(d_small$total_artist_followers + 1), d_small$popularity, method = "spearman")
  
  d_big <- d[d$size_group == "Big", ]
  rho_big <- cor(log10(d_big$total_artist_followers + 1), d_big$popularity, method = "spearman")
  
  return(c(rho_small, rho_big, rho_small - rho_big))
}

boot_cor_diff <- boot(data = followers_sample, statistic = cor_diff_boot_fun, R = B)

get_safe_ci_cor <- function(b_obj, idx) {
  if (var(b_obj$t[, idx]) == 0) {
    return(c(b_obj$t[1, idx], b_obj$t[1, idx]))
  } else {
    return(boot.ci(b_obj, type = "perc", index = idx)$percent[4:5])
  }
}

ci_small <- get_safe_ci_cor(boot_cor_diff, 1)
ci_big   <- get_safe_ci_cor(boot_cor_diff, 2)
ci_diff  <- get_safe_ci_cor(boot_cor_diff, 3)

cat(sprintf("1. Кореляція (Малі артисти < 1 млн): %.3f, 95%% CI: [%.3f; %.3f]\n", 
            boot_cor_diff$t0[1], ci_small[1], ci_small[2]))
cat(sprintf("2. Кореляція (Великі артисти > 10 млн): %.3f, 95%% CI: [%.3f; %.3f]\n", 
            boot_cor_diff$t0[2], ci_big[1], ci_big[2]))
cat(sprintf("3. РІЗНИЦЯ (Small - Big): %.3f, 95%% CI: [%.3f; %.3f]\n", 
            boot_cor_diff$t0[3], ci_diff[1], ci_diff[2]))

cat("\n\n--- Дод. 5: Акустична гомогенізація (Відстані між жанрами у Топ vs Low децилях) ---\n")
audio_features <- c("danceability", "energy", "acousticness", "valence", "speechiness", "instrumentalness")

homogen_data <- songs_df_clean %>%
  filter(!is.na(genre), !is.na(popularity)) %>%
  drop_na(all_of(audio_features)) %>%
  mutate(across(all_of(audio_features), ~ as.numeric(scale(.)))) 

p90_threshold <- quantile(homogen_data$popularity, 0.90)

homogen_data <- homogen_data %>%
  mutate(pop_group = ifelse(popularity >= p90_threshold, "Top_Decile", "Low_Deciles"))

homogen_sample <- homogen_data %>% group_by(pop_group) %>% slice_sample(n = 10000) %>% ungroup()

homogen_boot_fun <- function(data, indices) {
  d <- data[indices, ]
  get_mean_genre_dist <- function(df) {
    profiles <- df %>%
      group_by(genre) %>%
      summarise(across(all_of(audio_features), mean), .groups = "drop") %>%
      select(-genre)
    return(mean(dist(profiles)))
  }
  return(get_mean_genre_dist(d[d$pop_group == "Low_Deciles", ]) - get_mean_genre_dist(d[d$pop_group == "Top_Decile", ]))
}

boot_homogen <- boot(data = homogen_sample, statistic = homogen_boot_fun, R = B)
ci_homogen <- boot.ci(boot_homogen, type = "perc")

cat(sprintf("Різниця відстаней (D_low - D_top): %.4f\n", boot_homogen$t0))
cat(sprintf("95%% Percentile CI: [%.4f; %.4f]\n", ci_homogen$percent[4], ci_homogen$percent[5]))


cat("\n\n=============== ТЕСТИ ГІПОТЕЗ ===============\n")
options(scipen = 0) 

results_medians <- run_wald_test_medians(songs_df_clean, B_iter = B)
print_wald_results(results_medians, "Результати тесту Волда (МЕДІАНИ: Explicit > Clean) з поправкою BH")

results_means <- run_wald_test_means(songs_df_clean)
print_wald_results(results_means, "Результати тесту Волда (СЕРЕДНІ: Explicit > Clean) з поправкою BH")


cat("\n\n=============== ДІ ДЛЯ АУДІО-ХАРАКТЕРИСТИК ===============\n")
audio_features_ci <- songs_df_clean %>%
  select(all_of(audio_features)) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(feature) %>%
  summarise(
    total_n = n(),
    mean_val = mean(value),
    sd_val = sd(value),
    .groups = "drop"
  ) %>%
  mutate(
    se_mean = sd_val / sqrt(total_n),
    z_val_975 = qnorm(0.975),
    ci_lower = mean_val - z_val_975 * se_mean,
    ci_upper = mean_val + z_val_975 * se_mean,
    mean_val = round(mean_val, 4),
    se_mean = round(se_mean, 4),
    ci_formatted = sprintf("(%.3f ; %.3f)", ci_lower, ci_upper)
  ) %>%
  arrange(feature) %>%
  select(feature, total_n, mean_val, ci_formatted, sd_val, se_mean)

cat("\n--- Середнє значення та 95% Асимптотичний ДІ для аудіо-характеристик ---\n")
print(audio_features_ci, width = Inf)


cat("\n\n=============== БУДУЄМО ГРАФІКИ ===============\n")

plot_data_explicit <- result_genre_explicit %>%
  select(-any_of("bca_ci")) %>%
  pivot_longer(cols = ends_with("_ci"), names_to = "method", values_to = "ci_str") %>%
  mutate(
    ci_clean = str_remove_all(ci_str, "[\\(\\)]"),
    method = case_when(
      method == "norm_ci" ~ "Normal",
      method == "basic_ci" ~ "Basic",
      method == "stud_ci" ~ "Studentized",
      method == "perc_ci" ~ "Percentile"
    )
  ) %>%
  separate(ci_clean, into = c("ci_lower", "ci_upper"), sep = " ; ", convert = TRUE) %>%
  mutate(
    method = factor(method, levels = c("Studentized", "Percentile", "Normal", "Basic")),
    explicit_label = paste0(explicit_cat, "\n(", n, ")"),
    genre = fct_reorder(genre, median, .fun = sum, .desc = TRUE)
  )

p1 <- plot_data_explicit %>%
  ggplot(aes(x = explicit_label, color = method)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = 0.7), width = 0.5, linewidth = 0.8) +
  geom_point(aes(y = median), position = position_dodge(width = 0.7), size = 3) +
  facet_grid(~ genre, switch = "x", scales = "free_x", space = "free_x") +
  scale_color_manual(values = c("Basic" = "#F8766D", "Normal" = "#00BA38", "Percentile" = "#00BFC4", "Studentized" = "#E76BF3")) +
  labs(title = "Довірчі інтервали медіанної популярності", subtitle = "Clean vs Explicit", x = "Жанр", y = "Популярність", color = "Метод") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.2, "lines")
  )
print(p1)

plot_data_pop <- result_popularity %>%
  select(-any_of("bca_ci")) %>%
  pivot_longer(cols = ends_with("_ci"), names_to = "method", values_to = "ci_str") %>%
  mutate(
    ci_clean = str_remove_all(ci_str, "[\\(\\)]"),
    method = case_when(
      method == "norm_ci" ~ "Normal",
      method == "basic_ci" ~ "Basic",
      method == "stud_ci" ~ "Studentized",
      method == "perc_ci" ~ "Percentile"
    )
  ) %>%
  separate(ci_clean, into = c("ci_lower", "ci_upper"), sep = " ; ", convert = TRUE) %>%
  mutate(
    method = factor(method, levels = c("Studentized", "Percentile", "Normal", "Basic")),
    genre_label = paste0(genre, "\n(", n, ")"),
    genre_label = fct_reorder(genre_label, median, .desc = TRUE)
  )

p2 <- plot_data_pop %>%
  ggplot(aes(x = genre_label, color = method)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = 0.6), width = 0.4, linewidth = 0.8) +
  geom_point(aes(y = median), position = position_dodge(width = 0.6), size = 3) +
  scale_color_manual(values = c("Basic" = "#F8766D", "Normal" = "#00BA38", "Percentile" = "#00BFC4", "Studentized" = "#E76BF3")) +
  labs(title = "Загальна медіанна популярність за жанрами", x = "Жанр", y = "Популярність", color = "Метод") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.major.x = element_blank()
  )
print(p2)

plot_data_duration <- result_duration %>%
  select(-any_of("bca_ci")) %>%
  pivot_longer(cols = ends_with("_ci"), names_to = "method", values_to = "ci_str") %>%
  mutate(
    ci_clean = str_remove_all(ci_str, "[\\(\\)]"),
    method = case_when(
      method == "norm_ci" ~ "Normal",
      method == "basic_ci" ~ "Basic",
      method == "stud_ci" ~ "Studentized",
      method == "perc_ci" ~ "Percentile"
    )
  ) %>%
  separate(ci_clean, into = c("ci_lower", "ci_upper"), sep = " ; ", convert = TRUE) %>%
  mutate(
    method = factor(method, levels = c("Studentized", "Percentile", "Normal", "Basic")),
    genre_label = paste0(genre, "\n(", n, ")"),
    genre_label = fct_reorder(genre_label, median, .desc = TRUE)
  )

p3 <- plot_data_duration %>%
  ggplot(aes(x = genre_label, color = method)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = 0.6), width = 0.4, linewidth = 0.8) +
  geom_point(aes(y = median), position = position_dodge(width = 0.6), size = 3) +
  scale_color_manual(values = c("Basic" = "#F8766D", "Normal" = "#00BA38", "Percentile" = "#00BFC4", "Studentized" = "#E76BF3")) +
  labs(title = "Довірчі інтервали медіанної тривалості пісень", subtitle = "Лише пісні коротші за 10 хв", x = "Жанр", y = "Тривалість (хвилини)", color = "Метод") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.major.x = element_blank()
  )
print(p3)

cat("\nПерша частина завершена.\n")
} else {
  cat("\nПерша частина пропущена.\n")
}

section_title("Друга частина: функції для питань 5, 6, 7, 9")

options(stringsAsFactors = FALSE, scipen = 999)


BOOT_REPS <- 400
INNER_BOOT_REPS <- 49
ALPHA <- 0.05
SEED_BASE <- 20260426

output_dir <- OUTPUT_DIR
figures_dir <- file.path(output_dir, "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)


audio_features <- c(
  "danceability", "energy", "loudness", "speechiness",
  "acousticness", "instrumentalness", "liveness",
  "valence", "tempo"
)

all_genres <- c(
  "Blues", "Classical", "Country", "Electronic", "Folk",
  "Hip-Hop", "Jazz", "Pop", "R&B", "Rock"
)

pair_similarity <- tribble(
  ~genre_a,    ~genre_b,
  "Rock",      "Jazz",
  "Jazz",      "Electronic"
)

all_genre_pairs <- combn(all_genres, 2, simplify = FALSE)

default_input_path <- function() {
  rds_path <- file.path(getwd(), "songs_clean.rds")
  csv_path <- file.path(getwd(), "songs_clean.csv")

  if (file.exists(rds_path)) {
    return(rds_path)
  }

  csv_path
}

read_input_data <- function(input_path = default_input_path()) {
  progress_note("Loading data from ", input_path)
  started_at <- Sys.time()

  data_obj <- if (grepl("\\.rds$", input_path, ignore.case = TRUE)) {
    readRDS(input_path)
  } else {
    readr::read_csv(input_path, show_col_types = FALSE)
  }

  elapsed <- round(as.numeric(difftime(Sys.time(), started_at, units = "secs")), 1)
  progress_note(
    "Data loaded | rows = ", format(nrow(data_obj), big.mark = " "),
    " | cols = ", ncol(data_obj),
    " | elapsed = ", elapsed, " sec"
  )

  tibble::as_tibble(data_obj)
}

ntile10_base <- function(x) {
  n <- length(x)
  order_idx <- order(x, seq_along(x))
  out <- integer(n)
  out[order_idx] <- ceiling(seq_len(n) * 10 / n)
  out
}

wald_pvalue <- function(estimate, se, null_value = 0, alternative = "two.sided") {
  z_score <- (estimate - null_value) / se
  if (alternative == "greater") {
    return(1 - pnorm(z_score))
  }
  if (alternative == "less") {
    return(pnorm(z_score))
  }
  2 * pmin(pnorm(z_score), 1 - pnorm(z_score))
}

wald_one_sided_bound <- function(estimate, se, alternative = "two.sided", alpha = ALPHA) {
  z_crit <- qnorm(1 - alpha)

  if (alternative == "greater") {
    return(list(type = "lower", value = estimate - z_crit * se))
  }

  if (alternative == "less") {
    return(list(type = "upper", value = estimate + z_crit * se))
  }

  list(type = "two.sided", value = NA_real_)
}

extract_ci <- function(ci_object, component) {
  if (is.null(ci_object)) {
    return(c(NA_real_, NA_real_))
  }

  piece <- ci_object[[component]]
  if (is.null(piece)) {
    return(c(NA_real_, NA_real_))
  }

  if (component == "normal") {
    return(piece[2:3])
  }

  piece[4:5]
}

timestamp_label <- function() {
  format(Sys.time(), "%H:%M:%S")
}

progress_note <- function(...) {
  cat("[", timestamp_label(), "] ", paste0(..., collapse = ""), "\n", sep = "")
}

run_boot_object <- function(data, statistic, R, strata = NULL) {
  if (is.null(strata)) {
    return(boot::boot(data = data, statistic = statistic, R = R))
  }

  boot::boot(data = data, statistic = statistic, R = R, strata = strata)
}

bootstrap_single_stat <- function(
  data,
  stat_fun,
  stat_id,
  question,
  stat_group,
  alternative,
  null_value = 0,
  strata_col = NULL,
  n_boot = BOOT_REPS,
  n_boot_inner = INNER_BOOT_REPS,
  alpha = ALPHA,
  seed = SEED_BASE,
  show_progress = TRUE,
  compute_studentized = TRUE,
  compute_bca = TRUE,
  bca_max_n = 100000
) {
  make_boot_strata <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    as.integer(factor(x))
  }

  stat_start <- Sys.time()
  n_obs <- nrow(data)
  effective_compute_studentized <- isTRUE(compute_studentized)
  effective_compute_bca <- isTRUE(compute_bca)

  if (effective_compute_studentized && n_boot_inner < 5 && isTRUE(show_progress)) {
    progress_note(
      "Note ", stat_id,
      " | studentized requested with very small B* = ", n_boot_inner,
      " | interval may be unstable"
    )
  }

  if (effective_compute_bca && n_obs > bca_max_n) {
    if (isTRUE(show_progress)) {
      progress_note(
        "Skip BCa for ", stat_id,
        " | n = ", format(n_obs, big.mark = " "),
        " exceeds bca_max_n = ", format(bca_max_n, big.mark = " "),
        " | exact BCa is too slow on this dataset"
      )
    }
    effective_compute_bca <- FALSE
  }

  if (isTRUE(show_progress)) {
    progress_note(
      "Start ", stat_id,
      " | n = ", format(n_obs, big.mark = " "),
      " | B = ", n_boot,
      " | B* = ", n_boot_inner
    )
  }

  outer_call_count <- 0L
  pb <- NULL
  if (isTRUE(show_progress) && interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = n_boot, style = 3)
  }

  boot_stat <- function(dat, indices, estimate_var = effective_compute_studentized) {
    outer_call_count <<- outer_call_count + 1L
    if (!is.null(pb) && outer_call_count > 1L) {
      utils::setTxtProgressBar(pb, outer_call_count - 1L)
    }

    sampled <- dat[indices, , drop = FALSE]
    theta_hat <- stat_fun(sampled)

    if (!estimate_var) {
      return(theta_hat)
    }

    inner_stat <- function(inner_dat, inner_indices) {
      stat_fun(inner_dat[inner_indices, , drop = FALSE])
    }

    inner_strata <- if (is.null(strata_col)) NULL else make_boot_strata(sampled[[strata_col]])
    inner_boot <- run_boot_object(
      data = sampled,
      statistic = inner_stat,
      R = n_boot_inner,
      strata = inner_strata
    )

    c(theta_hat, stats::var(inner_boot$t[, 1], na.rm = TRUE))
  }

  outer_strata <- if (is.null(strata_col)) NULL else make_boot_strata(data[[strata_col]])
  set.seed(seed)
  boot_out <- run_boot_object(
    data = data,
    statistic = boot_stat,
    R = n_boot,
    strata = outer_strata
  )

  if (!is.null(pb)) {
    close(pb)
  }

  if (isTRUE(show_progress)) {
    progress_note("CI stage ", stat_id, " | computing normal/basic/percentile")
  }

  ci_main <- tryCatch(
    suppressWarnings(
      boot::boot.ci(boot_out, conf = 1 - alpha, index = 1, type = c("norm", "basic", "perc"))
    ),
    error = function(e) NULL
  )

  ci_student <- if (isTRUE(effective_compute_studentized)) {
    if (isTRUE(show_progress)) {
      progress_note("CI stage ", stat_id, " | computing studentized")
    }

    tryCatch(
      suppressWarnings(
        boot::boot.ci(boot_out, conf = 1 - alpha, index = c(1, 2), type = "stud")
      ),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  ci_bca <- if (isTRUE(effective_compute_bca)) {
    if (isTRUE(show_progress)) {
      progress_note("CI stage ", stat_id, " | computing BCa")
    }

    tryCatch(
      suppressWarnings(
        boot::boot.ci(boot_out, conf = 1 - alpha, index = 1, type = "bca")
      ),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  estimate <- unname(boot_out$t0[1])
  se_boot <- stats::sd(boot_out$t[, 1], na.rm = TRUE)
  z_wald <- (estimate - null_value) / se_boot
  p_value <- wald_pvalue(estimate, se_boot, null_value = null_value, alternative = alternative)
  one_sided_bound <- wald_one_sided_bound(estimate, se_boot, alternative = alternative, alpha = alpha)

  if (isTRUE(show_progress)) {
    elapsed <- round(as.numeric(difftime(Sys.time(), stat_start, units = "secs")), 1)
    progress_note(
      "Done ", stat_id,
      " | estimate = ", sprintf("%.6f", estimate),
      " | se = ", sprintf("%.6f", se_boot),
      " | elapsed = ", elapsed, " sec"
    )
  }

  tibble(
    stat_id = stat_id,
    question = question,
    group = stat_group,
    alternative = alternative,
    null_value = null_value,
    estimate = estimate,
    se_boot = se_boot,
    ci_normal_low = extract_ci(ci_main, "normal")[1],
    ci_normal_high = extract_ci(ci_main, "normal")[2],
    ci_basic_low = extract_ci(ci_main, "basic")[1],
    ci_basic_high = extract_ci(ci_main, "basic")[2],
    ci_student_low = extract_ci(ci_student, "student")[1],
    ci_student_high = extract_ci(ci_student, "student")[2],
    ci_percentile_low = extract_ci(ci_main, "percent")[1],
    ci_percentile_high = extract_ci(ci_main, "percent")[2],
    ci_bca_low = extract_ci(ci_bca, "bca")[1],
    ci_bca_high = extract_ci(ci_bca, "bca")[2],
    wald_z = z_wald,
    one_sided_bound_type = one_sided_bound$type,
    one_sided_bound_value = one_sided_bound$value,
    p_value_raw = p_value
  )
}

proportion_wald_row <- function(
  estimate,
  n,
  stat_id,
  question,
  alternative,
  null_value = 0,
  alpha = ALPHA
) {
  z_crit <- qnorm(1 - alpha / 2)
  se <- sqrt(estimate * (1 - estimate) / n)
  z_wald <- (estimate - null_value) / se
  one_sided_bound <- wald_one_sided_bound(estimate, se, alternative = alternative, alpha = alpha)

  tibble(
    stat_id = stat_id,
    question = question,
    group = "1",
    alternative = alternative,
    null_value = null_value,
    estimate = estimate,
    se_boot = se,
    ci_normal_low = estimate - z_crit * se,
    ci_normal_high = estimate + z_crit * se,
    ci_basic_low = NA_real_,
    ci_basic_high = NA_real_,
    ci_student_low = NA_real_,
    ci_student_high = NA_real_,
    ci_percentile_low = NA_real_,
    ci_percentile_high = NA_real_,
    ci_bca_low = NA_real_,
    ci_bca_high = NA_real_,
    wald_z = z_wald,
    one_sided_bound_type = one_sided_bound$type,
    one_sided_bound_value = one_sided_bound$value,
    p_value_raw = wald_pvalue(estimate, se, null_value = null_value, alternative = alternative)
  )
}


col_medians_numeric <- function(mat) {
  apply(mat, 2, stats::median)
}

group_median_matrix <- function(feature_matrix, group_factor, group_levels) {
  out <- matrix(
    NA_real_,
    nrow = length(group_levels),
    ncol = ncol(feature_matrix),
    dimnames = list(group_levels, colnames(feature_matrix))
  )

  for (i in seq_along(group_levels)) {
    idx <- group_factor == group_levels[i]
    if (any(idx)) {
      out[i, ] <- col_medians_numeric(feature_matrix[idx, , drop = FALSE])
    }
  }

  out
}

compute_deciles_within_genre <- function(genre_vec, popularity_vec, genre_levels) {
  deciles <- integer(length(popularity_vec))

  for (genre_name in genre_levels) {
    idx <- genre_vec == genre_name
    if (any(idx)) {
      deciles[idx] <- ntile10_base(popularity_vec[idx])
    }
  }

  deciles
}

build_decile_profile_array <- function(feature_matrix, genre_vec, popularity_vec, genre_levels) {
  deciles <- compute_deciles_within_genre(genre_vec, popularity_vec, genre_levels)
  n_features <- ncol(feature_matrix)
  profile_array <- array(
    NA_real_,
    dim = c(length(genre_levels), 10L, n_features),
    dimnames = list(genre_levels, as.character(1:10), colnames(feature_matrix))
  )

  for (g_idx in seq_along(genre_levels)) {
    genre_name <- genre_levels[g_idx]
    genre_mask <- genre_vec == genre_name

    if (!any(genre_mask)) {
      next
    }

    genre_features <- feature_matrix[genre_mask, , drop = FALSE]
    genre_deciles <- deciles[genre_mask]

    for (d in 1:10) {
      decile_mask <- genre_deciles == d
      if (any(decile_mask)) {
        profile_array[g_idx, d, ] <- col_medians_numeric(genre_features[decile_mask, , drop = FALSE])
      }
    }
  }

  flat_profiles <- do.call(
    rbind,
    lapply(seq_along(genre_levels), function(g_idx) profile_array[g_idx, , , drop = FALSE][1, , ])
  )
  scaled_profiles <- scale(flat_profiles)

  cursor <- 1L
  for (g_idx in seq_along(genre_levels)) {
    profile_array[g_idx, , ] <- scaled_profiles[cursor:(cursor + 9L), , drop = FALSE]
    cursor <- cursor + 10L
  }

  profile_array
}

pair_distance_by_decile_array <- function(profile_array, genre_a, genre_b) {
  mat_a <- profile_array[genre_a, , , drop = FALSE][1, , ]
  mat_b <- profile_array[genre_b, , , drop = FALSE][1, , ]
  sqrt(rowSums((mat_a - mat_b) ^ 2))
}

genre_profile_matrix <- function(dat) {
  features <- as.matrix(dat[, audio_features, drop = FALSE])
  genre_vec <- as.character(dat$genre)
  group_median_matrix(features, genre_vec, all_genres)
}

stat_q5_contrast_diff <- function(dat) {
  genre_medians <- genre_profile_matrix(dat)
  genre_scaled <- scale(genre_medians)
  rownames(genre_scaled) <- all_genres

  dist_rock_jazz <- sqrt(sum((genre_scaled["Rock", ] - genre_scaled["Jazz", ])^2))
  dist_jazz_electronic <- sqrt(sum((genre_scaled["Jazz", ] - genre_scaled["Electronic", ])^2))

  dist_rock_jazz - dist_jazz_electronic
}

distance_top_minus_bottom <- function(dat, genre_a, genre_b) {
  keep_mask <- dat$genre %in% c(genre_a, genre_b)
  features <- as.matrix(dat[keep_mask, audio_features, drop = FALSE])
  genre_vec <- as.character(dat$genre[keep_mask])
  popularity_vec <- dat$popularity[keep_mask]

  profile_array <- build_decile_profile_array(
    feature_matrix = features,
    genre_vec = genre_vec,
    popularity_vec = popularity_vec,
    genre_levels = c(genre_a, genre_b)
  )

  distances <- pair_distance_by_decile_array(profile_array, genre_a, genre_b)
  distances[10] - distances[1]
}

genre_decile_profiles_z <- function(dat) {
  build_decile_profile_array(
    feature_matrix = as.matrix(dat[, audio_features, drop = FALSE]),
    genre_vec = as.character(dat$genre),
    popularity_vec = dat$popularity,
    genre_levels = all_genres
  )
}

pairwise_distance_by_decile <- function(profile_z, genre_a, genre_b) {
  tibble(
    pop_decile = 1:10,
    distance = pair_distance_by_decile_array(profile_z, genre_a, genre_b)
  )
}

stat_q5_D_low <- function(dat) {
  profiles_z <- genre_decile_profiles_z(dat)

  purrr::map_dbl(all_genre_pairs, function(pair) {
    dist_df <- pairwise_distance_by_decile(profiles_z, pair[[1]], pair[[2]])
    mean(dist_df$distance[dist_df$pop_decile %in% c(1, 2, 3)])
  }) %>%
    mean()
}

stat_q5_D_top <- function(dat) {
  profiles_z <- genre_decile_profiles_z(dat)

  purrr::map_dbl(all_genre_pairs, function(pair) {
    dist_df <- pairwise_distance_by_decile(profiles_z, pair[[1]], pair[[2]])
    dist_df$distance[dist_df$pop_decile == 10]
  }) %>%
    mean()
}

stat_q5_D_low_minus_D_top <- function(dat) {
  stat_q5_D_low(dat) - stat_q5_D_top(dat)
}

make_similarity_stat <- function(genre_a, genre_b) {
  force(genre_a)
  force(genre_b)
  function(dat) distance_top_minus_bottom(dat, genre_a = genre_a, genre_b = genre_b)
}

make_spearman_stat <- function(x_var, y_var) {
  force(x_var)
  force(y_var)
  function(dat) stats::cor(dat[[x_var]], dat[[y_var]], method = "spearman")
}

stat_q6_max_abs_genre_feature_popularity <- function(dat) {
  genre_vec <- as.character(dat$genre)
  popularity_vec <- dat$popularity
  max_abs_rho <- 0

  for (genre_name in unique(genre_vec)) {
    idx <- genre_vec == genre_name
    if (!any(idx)) {
      next
    }

    for (feature_name in audio_features) {
      rho_val <- suppressWarnings(stats::cor(dat[[feature_name]][idx], popularity_vec[idx], method = "spearman"))
      if (is.finite(rho_val)) {
        max_abs_rho <- max(max_abs_rho, abs(rho_val))
      }
    }
  }

  max_abs_rho
}

stat_q7_rho_small_logfollowers <- function(dat) {
  followers <- dat$total_artist_followers
  popularity <- dat$popularity
  mask <- followers < 1000000
  stats::cor(log10(followers[mask] + 1), popularity[mask], method = "spearman")
}

stat_q7_rho_big_logfollowers <- function(dat) {
  followers <- dat$total_artist_followers
  popularity <- dat$popularity
  mask <- followers > 10000000
  stats::cor(log10(followers[mask] + 1), popularity[mask], method = "spearman")
}

stat_q7_rho_small_minus_big_doc <- function(dat) {
  stat_q7_rho_small_logfollowers(dat) - stat_q7_rho_big_logfollowers(dat)
}

stat_q9_median_acousticness <- function(dat) {
  year_vec <- dat$year
  acousticness_vec <- dat$acousticness
  stats::median(acousticness_vec[year_vec < 1980]) - stats::median(acousticness_vec[year_vec >= 2000])
}

stat_q9_median_energy <- function(dat) {
  year_vec <- dat$year
  energy_vec <- dat$energy
  stats::median(energy_vec[year_vec >= 2000]) - stats::median(energy_vec[year_vec < 1980])
}

empty_results <- function() {
  tibble(
    stat_id = character(),
    question = integer(),
    group = character(),
    alternative = character(),
    null_value = numeric(),
    estimate = numeric(),
    se_boot = numeric(),
    ci_normal_low = numeric(),
    ci_normal_high = numeric(),
    ci_basic_low = numeric(),
    ci_basic_high = numeric(),
    ci_student_low = numeric(),
    ci_student_high = numeric(),
    ci_percentile_low = numeric(),
    ci_percentile_high = numeric(),
    ci_bca_low = numeric(),
    ci_bca_high = numeric(),
    wald_z = numeric(),
    one_sided_bound_type = character(),
    one_sided_bound_value = numeric(),
    p_value_raw = numeric()
  )
}

plot_ci_by_question <- function(results_tbl, question_number, output_path) {
  plot_data <- results_tbl %>%
    filter(question == question_number) %>%
    mutate(
      ci_low = if_else(!is.na(ci_percentile_low), ci_percentile_low, ci_normal_low),
      ci_high = if_else(!is.na(ci_percentile_high), ci_percentile_high, ci_normal_high),
      label = case_when(
        stat_id == "q5_contrast_diff_RockJazz_minus_JazzElectronic" ~ "d(Rock,Jazz) - d(Jazz,Electronic)",
        stat_id == "q5_similarity_topminusbottom_Rock_vs_Jazz" ~ "top-bottom distance: Rock vs Jazz",
        stat_id == "q5_similarity_topminusbottom_Jazz_vs_Electronic" ~ "top-bottom distance: Jazz vs Electronic",
        stat_id == "q5_D_low_mean_pairwise_distance_bottom3" ~ "D_low: mean pairwise distance, deciles 1-3",
        stat_id == "q5_D_top_mean_pairwise_distance_top1" ~ "D_top: mean pairwise distance, decile 10",
        stat_id == "q5_D_low_minus_D_top_doc" ~ "D_low - D_top",
        stat_id == "q6_rho_energy_loudness" ~ "rho(energy, loudness)",
        stat_id == "q6_rho_energy_acousticness" ~ "rho(energy, acousticness)",
        stat_id == "q6_max_abs_genre_feature_popularity_corr" ~ "max |rho(feature, popularity | genre)|",
        grepl("^q6_rho_audio_.*_popularity$", stat_id) ~ paste0(
          "rho(",
          sub("^q6_rho_audio_(.*)_popularity$", "\\1", stat_id),
          ", popularity)"
        ),
        stat_id == "q7_rho_small_logfollowers_popularity_lt1m" ~ "rho(log followers, popularity | <1M)",
        stat_id == "q7_rho_big_logfollowers_popularity_gt10m" ~ "rho(log followers, popularity | >10M)",
        stat_id == "q7_delta_rho_small_minus_big_doc" ~ "rho_small(<1M) - rho_big(>10M)",
        stat_id == "q7_share_zero_popularity_megastars" ~ "P(popularity = 0 | >100M followers)",
        stat_id == "q9_diff_median_acousticness_old_minus_modern" ~ "med(acoustic old) - med(acoustic modern)",
        stat_id == "q9_diff_median_energy_modern_minus_old" ~ "med(energy modern) - med(energy old)",
        TRUE ~ stat_id
      )
    ) %>%
    arrange(estimate) %>%
    mutate(label = factor(label, levels = label))

  p <- ggplot(plot_data, aes(x = estimate, y = label)) +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.15, color = "#1f77b4") +
    geom_point(color = "#d62728", size = 2.4) +
    geom_vline(aes(xintercept = null_value), linetype = "dashed", color = "gray50") +
    labs(
      title = paste("Confidence intervals for Question", question_number),
      x = "Estimate",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  suppressMessages(
    ggsave(output_path, p, width = 11, height = 0.75 * nrow(plot_data) + 1.4, dpi = 250)
  )
}

save_question_results <- function(question_results, question_number) {
  if (nrow(question_results) == 0) {
    return(invisible(NULL))
  }

  write_csv(
    question_results,
    file.path(output_dir, paste0("question_", question_number, "_summary.csv"))
  )
}

lab2_init_context <- function(
  input_path = default_input_path(),
  output_dir_path = OUTPUT_DIR,
  n_boot = BOOT_REPS,
  n_boot_inner = INNER_BOOT_REPS,
  compute_studentized = TRUE,
  compute_bca = TRUE,
  alpha = ALPHA,
  seed_base = SEED_BASE
) {
  figures_dir_path <- file.path(output_dir_path, "figures")
  dir.create(output_dir_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(figures_dir_path, showWarnings = FALSE, recursive = TRUE)

  list(
    songs = read_input_data(input_path),
    input_path = input_path,
    output_dir = output_dir_path,
    figures_dir = figures_dir_path,
    n_boot = n_boot,
    n_boot_inner = n_boot_inner,
    compute_studentized = compute_studentized,
    compute_bca = compute_bca,
    alpha = alpha,
    seed_base = seed_base
  )
}

lab2_write_shared_outputs <- function(ctx) {
  songs <- ctx$songs

  overall_corr_matrix <- songs %>%
    select(
      all_of(audio_features),
      year, popularity, total_artist_followers, avg_artist_popularity
    ) %>%
    stats::cor(method = "spearman", use = "complete.obs")

  overall_corr_matrix %>%
    as.data.frame() %>%
    tibble::rownames_to_column("variable") %>%
    write_csv(file.path(ctx$output_dir, "overall_spearman_matrix.csv"))

  genre_feature_corrs <- songs %>%
    filter(!is.na(genre)) %>%
    group_by(genre) %>%
    summarise(
      n = n(),
      across(
        all_of(audio_features),
        ~ suppressWarnings(stats::cor(.x, popularity, method = "spearman"))
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(-genre, names_to = "feature", values_to = "rho") %>%
    mutate(abs_rho = abs(rho)) %>%
    arrange(desc(abs_rho), genre, feature)

  write_csv(genre_feature_corrs, file.path(ctx$output_dir, "genre_feature_popularity_correlations.csv"))
}

lab2_finalize_results <- function(results) {
  if (nrow(results) == 0) {
    return(results)
  }

  results %>%
    mutate(
      p_value_bh = p.adjust(p_value_raw, method = "BH"),
      p_value_bonf = p.adjust(p_value_raw, method = "bonferroni"),
      reject_0_05_raw = p_value_raw < 0.05,
      reject_0_05_bh = p_value_bh < 0.05,
      reject_0_05_bonf = p_value_bonf < 0.05
    )
}

lab2_round_results_for_export <- function(results, digits = 3) {
  if (nrow(results) == 0) {
    return(results)
  }

  round_cols <- intersect(
    c(
      "estimate",
      "se_boot",
      "null_value",
      "ci_normal_low", "ci_normal_high",
      "ci_basic_low", "ci_basic_high",
      "ci_student_low", "ci_student_high",
      "ci_percentile_low", "ci_percentile_high",
      "ci_bca_low", "ci_bca_high",
      "wald_z",
      "one_sided_bound_value"
    ),
    names(results)
  )

  results %>%
    mutate(across(all_of(round_cols), ~ round(.x, digits)))
}

lab2_write_question_outputs <- function(ctx, question_results, question_number) {
  if (nrow(question_results) == 0) {
    return(invisible(NULL))
  }

  export_results <- lab2_round_results_for_export(question_results, digits = 3)

  write_csv(
    export_results,
    file.path(ctx$output_dir, paste0("question_", question_number, "_summary.csv"))
  )

  plot_ci_by_question(
    question_results,
    question_number,
    file.path(ctx$figures_dir, paste0("question_", question_number, "_ci_plot.png"))
  )
}

lab2_write_master_outputs <- function(ctx, results) {
  if (nrow(results) == 0) {
    return(invisible(NULL))
  }

  export_results <- lab2_round_results_for_export(results, digits = 3)

  write_csv(export_results, file.path(ctx$output_dir, "lab2_my_part_summary_master.csv"))
  writeLines(capture.output(sessionInfo()), file.path(ctx$output_dir, "sessionInfo.txt"))
}

lab2_run_single_question <- function(
  question_number,
  input_path = default_input_path(),
  output_dir_path = OUTPUT_DIR,
  n_boot = BOOT_REPS,
  n_boot_inner = INNER_BOOT_REPS,
  compute_studentized = TRUE,
  compute_bca = TRUE,
  alpha = ALPHA,
  seed_base = SEED_BASE,
  write_shared_outputs = FALSE
) {
  ctx <- lab2_init_context(
    input_path = input_path,
    output_dir_path = output_dir_path,
    n_boot = n_boot,
    n_boot_inner = n_boot_inner,
    compute_studentized = compute_studentized,
    compute_bca = compute_bca,
    alpha = alpha,
    seed_base = seed_base
  )

  if (isTRUE(write_shared_outputs)) {
    lab2_write_shared_outputs(ctx)
  }

  question_results <- switch(
    as.character(question_number),
    "5" = run_question_5(ctx),
    "6" = run_question_6(ctx),
    "7" = run_question_7(ctx),
    "9" = run_question_9(ctx),
    stop("Unsupported question_number. Use one of: 5, 6, 7, 9.")
  )

  question_results <- lab2_finalize_results(question_results)
  lab2_write_question_outputs(ctx, question_results, question_number)
  question_results
}

run_question_5 <- function(ctx) {
  progress_note("Preparing Question 5 datasets")
  songs <- ctx$songs

  q5_contrast_data <- songs %>%
    filter(
      genre %in% all_genres,
      if_all(all_of(audio_features), ~ !is.na(.x))
    ) %>%
    select(genre, all_of(audio_features))

  q5_similarity_data <- songs %>%
    filter(
      genre %in% unique(c(pair_similarity$genre_a, pair_similarity$genre_b)),
      !is.na(popularity),
      if_all(all_of(audio_features), ~ !is.na(.x))
    ) %>%
    select(genre, popularity, all_of(audio_features))

  q5_doc_distance_data <- songs %>%
    filter(
      genre %in% all_genres,
      !is.na(popularity),
      if_all(all_of(audio_features), ~ !is.na(.x))
    ) %>%
    select(genre, popularity, all_of(audio_features))

  out <- bind_rows(
    bootstrap_single_stat(
      data = q5_contrast_data,
      stat_fun = stat_q5_contrast_diff,
      stat_id = "q5_contrast_diff_RockJazz_minus_JazzElectronic",
      question = 5,
      stat_group = "3",
      alternative = "greater",
      null_value = 0,
      strata_col = "genre",
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 1
    ),
    bootstrap_single_stat(
      data = q5_similarity_data,
      stat_fun = make_similarity_stat("Rock", "Jazz"),
      stat_id = "q5_similarity_topminusbottom_Rock_vs_Jazz",
      question = 5,
      stat_group = "3",
      alternative = "less",
      null_value = 0,
      strata_col = "genre",
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 2
    ),
    bootstrap_single_stat(
      data = q5_similarity_data,
      stat_fun = make_similarity_stat("Jazz", "Electronic"),
      stat_id = "q5_similarity_topminusbottom_Jazz_vs_Electronic",
      question = 5,
      stat_group = "3",
      alternative = "less",
      null_value = 0,
      strata_col = "genre",
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 3
    ),
    bootstrap_single_stat(
      data = q5_doc_distance_data,
      stat_fun = stat_q5_D_low,
      stat_id = "q5_D_low_mean_pairwise_distance_bottom3",
      question = 5,
      stat_group = "3",
      alternative = "greater",
      null_value = 0,
      strata_col = "genre",
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 40
    ),
    bootstrap_single_stat(
      data = q5_doc_distance_data,
      stat_fun = stat_q5_D_top,
      stat_id = "q5_D_top_mean_pairwise_distance_top1",
      question = 5,
      stat_group = "3",
      alternative = "greater",
      null_value = 0,
      strata_col = "genre",
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 41
    ),
    bootstrap_single_stat(
      data = q5_doc_distance_data,
      stat_fun = stat_q5_D_low_minus_D_top,
      stat_id = "q5_D_low_minus_D_top_doc",
      question = 5,
      stat_group = "3",
      alternative = "greater",
      null_value = 0,
      strata_col = "genre",
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 42
    )
  )

  progress_note("Question 5 finished")
  out
}

run_question_6 <- function(ctx) {
  progress_note("Preparing Question 6 datasets")
  songs <- ctx$songs

  q6_corr_data <- songs %>%
    filter(
      if_all(c("energy", "loudness", "acousticness"), ~ !is.na(.x))
    ) %>%
    select(energy, loudness, acousticness)

  q6_max_data <- songs %>%
    filter(
      !is.na(genre),
      !is.na(popularity),
      if_all(all_of(audio_features), ~ !is.na(.x))
    ) %>%
    select(genre, popularity, all_of(audio_features))

  q6_audio_popularity_data <- songs %>%
    filter(
      !is.na(popularity),
      if_all(all_of(audio_features), ~ !is.na(.x))
    ) %>%
    select(popularity, all_of(audio_features))

  q6_audio_popularity_results <- purrr::map2_dfr(
    audio_features,
    seq_along(audio_features),
    function(feature_name, feature_index) {
      bootstrap_single_stat(
        data = q6_audio_popularity_data %>% select(popularity, all_of(feature_name)),
        stat_fun = make_spearman_stat(feature_name, "popularity"),
        stat_id = paste0("q6_rho_audio_", feature_name, "_popularity"),
        question = 6,
        stat_group = "3",
        alternative = "two.sided",
        null_value = 0,
        n_boot = ctx$n_boot,
        n_boot_inner = ctx$n_boot_inner,
        compute_studentized = ctx$compute_studentized,
        compute_bca = ctx$compute_bca,
        alpha = ctx$alpha,
        seed = ctx$seed_base + 80 + feature_index
      )
    }
  )

  out <- bind_rows(
    q6_audio_popularity_results,
    bootstrap_single_stat(
      data = q6_corr_data,
      stat_fun = make_spearman_stat("energy", "loudness"),
      stat_id = "q6_rho_energy_loudness",
      question = 6,
      stat_group = "3",
      alternative = "greater",
      null_value = 0,
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 5
    ),
    bootstrap_single_stat(
      data = q6_corr_data,
      stat_fun = make_spearman_stat("energy", "acousticness"),
      stat_id = "q6_rho_energy_acousticness",
      question = 6,
      stat_group = "3",
      alternative = "less",
      null_value = 0,
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 6
    ),
    bootstrap_single_stat(
      data = q6_max_data,
      stat_fun = stat_q6_max_abs_genre_feature_popularity,
      stat_id = "q6_max_abs_genre_feature_popularity_corr",
      question = 6,
      stat_group = "3",
      alternative = "less",
      null_value = 0.20,
      strata_col = "genre",
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 8
    )
  )

  progress_note("Question 6 finished")
  out
}

run_question_7 <- function(ctx) {
  progress_note("Preparing Question 7 datasets")
  songs <- ctx$songs

  q7_data <- songs %>%
    filter(!is.na(total_artist_followers), !is.na(popularity)) %>%
    select(total_artist_followers, popularity)

  out <- bind_rows(
    bootstrap_single_stat(
      data = q7_data,
      stat_fun = stat_q7_rho_small_logfollowers,
      stat_id = "q7_rho_small_logfollowers_popularity_lt1m",
      question = 7,
      stat_group = "3",
      alternative = "greater",
      null_value = 0,
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 43
    ),
    bootstrap_single_stat(
      data = q7_data,
      stat_fun = stat_q7_rho_big_logfollowers,
      stat_id = "q7_rho_big_logfollowers_popularity_gt10m",
      question = 7,
      stat_group = "3",
      alternative = "greater",
      null_value = 0,
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 44
    ),
    bootstrap_single_stat(
      data = q7_data,
      stat_fun = stat_q7_rho_small_minus_big_doc,
      stat_id = "q7_delta_rho_small_minus_big_doc",
      question = 7,
      stat_group = "3",
      alternative = "greater",
      null_value = 0,
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 45
    ),
    proportion_wald_row(
      estimate = mean(
        q7_data %>%
          filter(total_artist_followers > 100000000) %>%
          pull(popularity) == 0
      ),
      n = nrow(q7_data %>% filter(total_artist_followers > 100000000)),
      stat_id = "q7_share_zero_popularity_megastars",
      question = 7,
      alternative = "greater",
      null_value = 0.05,
      alpha = ctx$alpha
    )
  )

  progress_note("Question 7 finished")
  out
}

run_question_9 <- function(ctx) {
  progress_note("Preparing Question 9 datasets")
  songs <- ctx$songs

  q9_median_data <- songs %>%
    filter(
      year <= 2023,
      !is.na(acousticness), !is.na(energy)
    ) %>%
    select(year, acousticness, energy)

  out <- bind_rows(
    bootstrap_single_stat(
      data = q9_median_data,
      stat_fun = stat_q9_median_acousticness,
      stat_id = "q9_diff_median_acousticness_old_minus_modern",
      question = 9,
      stat_group = "2",
      alternative = "greater",
      null_value = 0,
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 10
    ),
    bootstrap_single_stat(
      data = q9_median_data,
      stat_fun = stat_q9_median_energy,
      stat_id = "q9_diff_median_energy_modern_minus_old",
      question = 9,
      stat_group = "2",
      alternative = "greater",
      null_value = 0,
      n_boot = ctx$n_boot,
      n_boot_inner = ctx$n_boot_inner,
      compute_studentized = ctx$compute_studentized,
      compute_bca = ctx$compute_bca,
      alpha = ctx$alpha,
      seed = ctx$seed_base + 11
    )
  )

  progress_note("Question 9 finished")
  out
}

run_all_questions <- function(
  run_q5 = TRUE,
  run_q6 = TRUE,
  run_q7 = TRUE,
  run_q9 = TRUE,
  input_path = default_input_path(),
  output_dir_path = OUTPUT_DIR,
  n_boot = BOOT_REPS,
  n_boot_inner = INNER_BOOT_REPS,
  compute_studentized = TRUE,
  compute_bca = TRUE,
  alpha = ALPHA,
  seed_base = SEED_BASE
) {
  ctx <- lab2_init_context(
    input_path = input_path,
    output_dir_path = output_dir_path,
    n_boot = n_boot,
    n_boot_inner = n_boot_inner,
    compute_studentized = compute_studentized,
    compute_bca = compute_bca,
    alpha = alpha,
    seed_base = seed_base
  )

  lab2_write_shared_outputs(ctx)

  results_list <- list()
  if (run_q5) {
    results_list$q5 <- run_question_5(ctx)
  }
  if (run_q6) {
    results_list$q6 <- run_question_6(ctx)
  }
  if (run_q7) {
    results_list$q7 <- run_question_7(ctx)
  }
  if (run_q9) {
    results_list$q9 <- run_question_9(ctx)
  }

  results <- bind_rows(results_list)
  results <- lab2_finalize_results(results)

  if (run_q5 && any(results$question == 5)) {
    lab2_write_question_outputs(ctx, filter(results, question == 5), 5)
  }
  if (run_q6 && any(results$question == 6)) {
    lab2_write_question_outputs(ctx, filter(results, question == 6), 6)
  }
  if (run_q7 && any(results$question == 7)) {
    lab2_write_question_outputs(ctx, filter(results, question == 7), 7)
  }
  if (run_q9 && any(results$question == 9)) {
    lab2_write_question_outputs(ctx, filter(results, question == 9), 9)
  }

  lab2_write_master_outputs(ctx, results)
  results
}



run_all_questions_loaded <- function(
  songs,
  run_q5 = TRUE,
  run_q6 = TRUE,
  run_q7 = TRUE,
  run_q9 = TRUE,
  output_dir_path = OUTPUT_DIR,
  n_boot = getOption("lab2.inference_boot_reps", 10),
  n_boot_inner = getOption("lab2.inference_inner_boot_reps", 3),
  compute_studentized = getOption("lab2.compute_studentized", FALSE),
  compute_bca = getOption("lab2.compute_bca", FALSE),
  alpha = ALPHA,
  seed_base = SEED_BASE
) {
  figures_dir_path <- file.path(output_dir_path, "figures")
  dir.create(output_dir_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(figures_dir_path, showWarnings = FALSE, recursive = TRUE)

  ctx <- list(
    songs = tibble::as_tibble(songs),
    input_path = "songs_df_clean (already loaded)",
    output_dir = output_dir_path,
    figures_dir = figures_dir_path,
    n_boot = n_boot,
    n_boot_inner = n_boot_inner,
    compute_studentized = compute_studentized,
    compute_bca = compute_bca,
    alpha = alpha,
    seed_base = seed_base
  )

  lab2_write_shared_outputs(ctx)

  results_list <- list()
  if (run_q5) results_list$q5 <- run_question_5(ctx)
  if (run_q6) results_list$q6 <- run_question_6(ctx)
  if (run_q7) results_list$q7 <- run_question_7(ctx)
  if (run_q9) results_list$q9 <- run_question_9(ctx)

  results <- bind_rows(results_list)
  results <- lab2_finalize_results(results)

  if (run_q5 && any(results$question == 5)) lab2_write_question_outputs(ctx, filter(results, question == 5), 5)
  if (run_q6 && any(results$question == 6)) lab2_write_question_outputs(ctx, filter(results, question == 6), 6)
  if (run_q7 && any(results$question == 7)) lab2_write_question_outputs(ctx, filter(results, question == 7), 7)
  if (run_q9 && any(results$question == 9)) lab2_write_question_outputs(ctx, filter(results, question == 9), 9)

  lab2_write_master_outputs(ctx, results)
  results
}

print_inference_results <- function(results) {
  section_title("Підсумок питань 5, 6, 7, 9")
  if (nrow(results) == 0) {
    cat("Немає результатів для виводу.\n")
    return(invisible(NULL))
  }

  summary_tbl <- results %>%
    transmute(
      question,
      statistic = stat_id,
      estimate = round(estimate, 4),
      se_boot = round(se_boot, 4),
      ci_percentile = sprintf("[%.4f; %.4f]", ci_percentile_low, ci_percentile_high),
      wald_z = round(wald_z, 4),
      p_raw = formatC(p_value_raw, format = "e", digits = 3),
      p_BH = formatC(p_value_bh, format = "e", digits = 3),
      reject_BH_0_05 = reject_0_05_bh
    )

  print_table(summary_tbl)
}

if (RUN_INFERENCE_PART) {
  section_title("Запуск другої частини")
  cat(sprintf("B для другої частини: %d\n", getOption("lab2.inference_boot_reps", 10)))
  cat(sprintf("B* для другої частини: %d\n", getOption("lab2.inference_inner_boot_reps", 3)))
  cat(sprintf("Studentized CI: %s\n", getOption("lab2.compute_studentized", FALSE)))
  cat(sprintf("BCa CI: %s\n", getOption("lab2.compute_bca", FALSE)))

  results_all <- run_all_questions_loaded(
    songs = songs_df_clean,
    run_q5 = RUN_Q5,
    run_q6 = RUN_Q6,
    run_q7 = RUN_Q7,
    run_q9 = RUN_Q9,
    output_dir_path = OUTPUT_DIR
  )

  print_inference_results(results_all)
} else {
  section_title("Друга частина пропущена")
}

section_title("Готово")
cat("Скрипт завершив роботу без помилок.\n")
cat(sprintf("Папка для CSV та графіків: %s\n", OUTPUT_DIR))
