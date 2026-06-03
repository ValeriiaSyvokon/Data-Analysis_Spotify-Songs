# Standalone combined script for Lab 4.
# This is a real monolithic R script: it does not source external R files and
# does not store the lab code as quoted strings.

suppressPackageStartupMessages({
  base_required_packages <- c("dplyr", "ggplot2", "mgcv", "readr", "tibble", "tidyr")
  vlad_required_packages <- c("np", "FactoMineR", "factoextra", "gratia")
  all_required_packages <- unique(c(base_required_packages, vlad_required_packages))
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

root_dir <- get_script_dir()
setwd(root_dir)

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  warn = 1,
  width = 240,
  tibble.width = Inf,
  tibble.print_max = Inf
)

section_title <- function(title) {
  cat("\n", strrep("=", 88), "\n", title, "\n", strrep("=", 88), "\n", sep = "")
}

subsection_title <- function(title) {
  cat("\n", strrep("-", 88), "\n", title, "\n", strrep("-", 88), "\n", sep = "")
}

check_files <- function(paths) {
  missing_files <- paths[!file.exists(file.path(root_dir, paths))]
  if (length(missing_files) > 0) {
    stop("Missing required files: ", paste(missing_files, collapse = ", "))
  }
}

check_packages <- function(packages) {
  installed <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  if (!all(installed)) {
    missing_packages <- packages[!installed]
    stop(
      "Missing R packages: ", paste(missing_packages, collapse = ", "), "\n",
      "Install them with:\n",
      "install.packages(c(", paste(sprintf('\"%s\"', missing_packages), collapse = ", "), "), repos = \"https://cloud.r-project.org\")"
    )
  }
}

load_base_packages <- function() {
  invisible(lapply(base_required_packages, library, character.only = TRUE))
}

run_part <- function(label, fun) {
  section_title(label)
  log_dir <- file.path(root_dir, "lab4_outputs", "combined_logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(log_dir, paste0(gsub("[^A-Za-z0-9]+", "_", tolower(label)), "_monolithic.txt"))

  result <- NULL
  raw_output <- capture.output(
    withCallingHandlers(
      {
        result <- fun()
      },
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("conversion failure.*mbcsToSbcs", msg)) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    type = "output"
  )

  writeLines(raw_output, log_file, useBytes = TRUE)
  cat("Part completed: ", label, "\n", sep = "")
  cat("Raw part output log: ", log_file, "\n", sep = "")
  result
}

print_object_if_exists <- function(env, object_name, title = object_name) {
  if (exists(object_name, envir = env, inherits = FALSE)) {
    subsection_title(title)
    print(get(object_name, envir = env, inherits = FALSE))
  }
}

print_summary_if_exists <- function(env, object_name, title = paste("summary", object_name)) {
  if (exists(object_name, envir = env, inherits = FALSE)) {
    subsection_title(title)
    summary_result <- summary(get(object_name, envir = env, inherits = FALSE))
    if (!is.null(summary_result)) {
      print(summary_result)
    }
  }
}

print_vlad_results <- function(env) {
  section_title("Console summary: danceability model")
  print_summary_if_exists(env, "model_lc", "Kernel regression: Nadaraya-Watson / local constant")
  print_summary_if_exists(env, "model_ll", "Kernel regression: local linear")
  print_summary_if_exists(env, "model_pl", "Partially linear regression")
  print_summary_if_exists(env, "model_gam", "GAM for danceability")
  if (exists("res_pca", envir = env, inherits = FALSE)) {
    res_pca <- get("res_pca", envir = env, inherits = FALSE)
    subsection_title("PCA eigenvalues and explained variance")
    print(res_pca$eig)
    subsection_title("PCA variable coordinates")
    print(res_pca$var$coord)
    subsection_title("PCA variable contributions")
    print(res_pca$var$contrib)
  }
}

print_popularity_results <- function(env) {
  section_title("Console summary: popularity model")
  print_object_if_exists(env, "model_metrics", "Validation metrics")
  print_object_if_exists(env, "kernel_tuning", "Kernel tuning")
  print_object_if_exists(env, "pca_variance", "Audio PCA explained variance")
  print_object_if_exists(env, "smooth_plm", "Partially linear smooth terms")
  print_object_if_exists(env, "smooth_gam", "GAM smooth terms")
}

print_lera_results <- function(env) {
  section_title("Console summary: explicit model")
  print_object_if_exists(env, "pca_variance", "PCA explained variance")
  print_summary_if_exists(env, "plm_fit", "Partially linear logistic model")
  print_summary_if_exists(env, "gam_fit", "GAM logistic model")
  if (exists("curve_df", envir = env, inherits = FALSE)) {
    curve_df <- get("curve_df", envir = env, inherits = FALSE)
    subsection_title("Conditional probability curve: first and last rows")
    print(utils::head(curve_df, 5))
    print(utils::tail(curve_df, 5))
  }
  if (exists("kernel_curves", envir = env, inherits = FALSE)) {
    kernel_curves <- get("kernel_curves", envir = env, inherits = FALSE)
    subsection_title("Kernel curves by model")
    print(
      dplyr::summarise(
        dplyr::group_by(kernel_curves, model),
        min_prediction = min(prediction, na.rm = TRUE),
        mean_prediction = mean(prediction, na.rm = TRUE),
        max_prediction = max(prediction, na.rm = TRUE),
        .groups = "drop"
      )
    )
  }
}


run_vlad_part <- function() {
  # ==============================================================================
  # Крок 0. Встановлення пакетів (запустіть один раз, прибравши символ #)
  # ==============================================================================
  # install.packages(c("np", "mgcv", "FactoMineR", "factoextra", "ggplot2", "dplyr", "gratia"))

  # ==============================================================================
  # Крок 1. Підготовка даних (ВЕРСІЯ ДЛЯ ШВИДКОГО ТЕСТУ)
  # ==============================================================================

  # Завантаження необхідних бібліотек
  library(np)          # Для ядрової та частково лінійної регресії
  library(mgcv)        # Для узагальнених адитивних моделей (GAM)
  library(FactoMineR)  # Для аналізу головних компонент (PCA)
  library(factoextra)  # Для красивої візуалізації результатів PCA
  library(ggplot2)     # Для базових графіків
  library(dplyr)       # Для маніпуляцій з даними
  library(gratia)      # Для візуалізації сплайнів GAM

  songs_clean <- readRDS("songs_clean.rds")

  # Перетворюємо genre на фактор (обов'язково для GAM та np)
  songs_clean <- songs_clean %>% 
    mutate(genre = as.factor(genre))

  # Фіксація генератора випадкових чисел
  set.seed(123)

  # 1. Мікро-вибірка для важких непараметричних методів (np)
  # 150 рядків відпрацюють дуже швидко.
  songs_sample <- songs_clean %>% sample_n(150)

  # 2. Зменшена вибірка для GAM та PCA
  # Використовуємо 5000 рядків замість 500k для швидкого тесту.
  songs_test_gam <- songs_clean %>% sample_n(150)

  # ==============================================================================
  # Крок 2. Оцінювання непараметричних моделей
  # ==============================================================================

  # --- 2.1 Ядрова регресія (Kernel regression) ---
  # Навчаємо на мікро-вибірці (songs_sample)
  bw_lc <- npregbw(danceability ~ tempo, data = songs_sample, regtype = "lc")
  model_lc <- npreg(bw_lc)

  bw_ll <- npregbw(danceability ~ tempo, data = songs_sample, regtype = "ll")
  model_ll <- npreg(bw_ll)


  # --- 2.2 Частково лінійна регресія (Partially linear regression) ---
  # Навчаємо на мікро-вибірці (songs_sample)
  bw_pl <- npplregbw(danceability ~ genre + energy + instrumentalness + 
                       speechiness + acousticness + year | tempo + valence, 
                     data = songs_sample)
  model_pl <- npplreg(bw_pl)


  # --- 2.3 Узагальнена адитивна модель (GAM) ---
  # Навчаємо на зменшеній вибірці (songs_test_gam) замість усього датасету
  model_gam <- gam(danceability ~ s(tempo) + s(valence) + s(energy) + 
                     s(acousticness) + s(speechiness) + s(instrumentalness) + 
                     s(year) + genre, 
                   data = songs_test_gam, method = "REML")

  # ==============================================================================
  # Крок 3. Візуалізація результатів непараметрики
  # ==============================================================================

  # --- 3.1 Візуалізація ядрової регресії (NW та LL) ---
  tempo_grid <- data.frame(tempo = seq(min(songs_sample$tempo, na.rm = TRUE), 
                                       max(songs_sample$tempo, na.rm = TRUE), 
                                       length.out = 200))

  pred_lc <- predict(model_lc, newdata = tempo_grid, se.fit = TRUE)
  pred_ll <- predict(model_ll, newdata = tempo_grid, se.fit = TRUE)

  plot_data_kernel <- data.frame(
    tempo = rep(tempo_grid$tempo, 2),
    fit = c(pred_lc$fit, pred_ll$fit),
    se = c(pred_lc$se.fit, pred_ll$se.fit),
    method = rep(c("Nadaraya-Watson (LC)", "Locally Linear (LL)"), each = 200)
  ) %>%
    mutate(
      lower = fit - 1.96 * se, 
      upper = fit + 1.96 * se  
    )

  p_kernel <- ggplot(plot_data_kernel, aes(x = tempo, y = fit, color = method, fill = method)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    labs(title = "Ядрова регресія: вплив tempo на danceability",
         subtitle = "Порівняння Nadaraya-Watson та локально лінійної регресії (Тестова вибірка)",
         x = "Tempo", y = "Оцінена Danceability") +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p_kernel)

  # --- 3.2 Результати частково лінійної регресії ---
  cat("\n=== Коефіцієнти лінійної (параметричної) частини ===\n")
  summary(model_pl)

  # --- 3.3 Візуалізація GAM ---
  p_gam <- draw(model_gam, select = c("s(tempo)", "s(valence)"))
  print(p_gam)


  # ==============================================================================
  # Крок 4. Аналіз головних компонент (PCA)
  # ==============================================================================

  # Використовуємо зменшену вибірку (songs_test_gam) для швидкого тесту
  pca_data <- songs_test_gam %>%
    select(danceability, energy, loudness, speechiness, 
           acousticness, instrumentalness, liveness, valence, tempo) %>%
    na.omit()

  res_pca <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

  # --- 4.1 Графік власних чисел (Scree plot) ---
  p_scree <- fviz_eig(res_pca, addlabels = TRUE, ylim = c(0, 50),
                      title = "Scree plot: Відсоток поясненої дисперсії")
  print(p_scree)

  # --- 4.2 Графік проєкцій змінних на перші 2 компоненти ---
  p_vars <- fviz_pca_var(res_pca, 
                         col.var = "contrib", 
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,        
                         title = "PCA: Проєкції аудіо-характеристик")
  print(p_vars)

  # --- 4.3 Біграфік (Biplot) ---
  p_biplot <- fviz_pca_biplot(res_pca, 
                              select.ind = list(contrib = 100), 
                              label = "var",        
                              col.var = "red",      
                              col.ind = "#696969",  
                              alpha.ind = 0.6,
                              title = "Biplot (Змінні + Топ-100 спостережень за внеском)")
  print(p_biplot)

  # --- Текстові результати для регресій ---

  # 1. Ядрова регресія (покаже R-квадрат, ширину вікна та значимість)
  cat("\n=== Ядрова регресія (Nadaraya-Watson) ===\n")
  summary(model_lc)

  # 2. Частково лінійна регресія (покаже коефіцієнти лінійної частини та R-квадрат)
  cat("\n=== Частково лінійна регресія ===\n")
  summary(model_pl)

  # 3. GAM (покаже значимість кожного нелінійного сплайну s() та лінійного фактора)
  cat("\n=== Узагальнена адитивна модель (GAM) ===\n")
  summary(model_gam)

  # --- Текстові результати для PCA ---

  # Базове текстове зведення PCA
  cat("\n=== Зведені результати PCA ===\n")
  summary(res_pca)

  # Виведення конкретних числових матриць (дуже зручно для аналізу):
  cat("\n--- Власні числа (відсоток поясненої дисперсії) ---\n")
  print(res_pca$eig)

  cat("\n--- Координати змінних (кореляція з головними компонентами) ---\n")
  print(res_pca$var$coord)

  cat("\n--- Внесок змінних у компоненти (у відсотках) ---\n")
  print(res_pca$var$contrib)

  environment()
}


run_popularity_part <- function() {
  suppressPackageStartupMessages({
    required_packages <- c("dplyr", "ggplot2", "mgcv", "readr", "tibble", "tidyr")
    missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_packages) > 0) {
      stop("Missing packages: ", paste(missing_packages, collapse = ", "))
    }

    library(dplyr)
    library(ggplot2)
    library(mgcv)
    library(readr)
    library(tibble)
    library(tidyr)
  })

  options(
    stringsAsFactors = FALSE,
    scipen = 999,
    warn = 1,
    width = 240,
    tibble.width = Inf,
    tibble.print_max = Inf
  )

  set.seed(42)

  input_path <- file.path(getwd(), "songs_clean.rds")
  output_dir <- file.path(getwd(), "lab4_outputs")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (!file.exists(input_path)) {
    stop("File not found: ", input_path)
  }

  safe_write_csv <- function(x, path) {
    write_csv(x, file.path(output_dir, path), na = "")
  }

  rmse <- function(actual, predicted) {
    sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  }

  mae <- function(actual, predicted) {
    mean(abs(actual - predicted), na.rm = TRUE)
  }

  get_mode_value <- function(x) {
    tab <- sort(table(x), decreasing = TRUE)
    names(tab)[1]
  }

  section_title <- function(title) {
    cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
  }

  gaussian_kernel <- function(u) {
    exp(-0.5 * rowSums(u^2))
  }

  scale_with_reference <- function(x, center, scale) {
    sweep(sweep(as.matrix(x), 2, center, "-"), 2, scale, "/")
  }

  nw_predict <- function(x_train, y_train, x_new, bandwidth, chunk_size = 250) {
    x_train <- as.matrix(x_train)
    x_new <- as.matrix(x_new)
    y_train <- as.numeric(y_train)
    bandwidth <- as.numeric(bandwidth)
    preds <- numeric(nrow(x_new))

    for (start in seq(1, nrow(x_new), by = chunk_size)) {
      end <- min(start + chunk_size - 1, nrow(x_new))
      for (i in start:end) {
        u <- sweep(x_train, 2, x_new[i, ], "-")
        u <- sweep(u, 2, bandwidth, "/")
        w <- gaussian_kernel(u)
        sw <- sum(w)
        preds[i] <- ifelse(sw <= 1e-12, mean(y_train), sum(w * y_train) / sw)
      }
    }

    preds
  }

  local_linear_predict <- function(x_train, y_train, x_new, bandwidth, ridge = 1e-6) {
    x_train <- as.matrix(x_train)
    x_new <- as.matrix(x_new)
    y_train <- as.numeric(y_train)
    bandwidth <- as.numeric(bandwidth)
    preds <- numeric(nrow(x_new))

    for (i in seq_len(nrow(x_new))) {
      centered <- sweep(x_train, 2, x_new[i, ], "-")
      u <- sweep(centered, 2, bandwidth, "/")
      w <- gaussian_kernel(u)
      if (sum(w) <= 1e-12) {
        preds[i] <- mean(y_train)
        next
      }

      z <- cbind(1, centered)
      xtw <- t(z * w)
      xtwx <- xtw %*% z
      xtwy <- xtw %*% y_train
      beta <- tryCatch(
        solve(xtwx + ridge * diag(ncol(xtwx)), xtwy),
        error = function(e) rep(NA_real_, ncol(z))
      )
      preds[i] <- ifelse(is.na(beta[1]), sum(w * y_train) / sum(w), beta[1])
    }

    preds
  }

  kernel_grid_with_se <- function(x_train, y_train, x_grid, bandwidth, method = c("nw", "local_linear")) {
    method <- match.arg(method)
    x_train <- as.matrix(x_train)
    x_grid <- as.matrix(x_grid)
    y_train <- as.numeric(y_train)
    bandwidth <- as.numeric(bandwidth)

    fitted_train <- if (method == "nw") {
      nw_predict(x_train, y_train, x_train, bandwidth, chunk_size = 150)
    } else {
      local_linear_predict(x_train, y_train, x_train, bandwidth)
    }
    residuals <- y_train - fitted_train

    out <- vector("list", nrow(x_grid))
    for (i in seq_len(nrow(x_grid))) {
      centered <- sweep(x_train, 2, x_grid[i, ], "-")
      u <- sweep(centered, 2, bandwidth, "/")
      w <- gaussian_kernel(u)

      if (method == "nw") {
        sw <- sum(w)
        pred <- ifelse(sw <= 1e-12, mean(y_train), sum(w * y_train) / sw)
        h <- if (sw <= 1e-12) rep(1 / length(y_train), length(y_train)) else w / sw
      } else {
        z <- cbind(1, centered)
        xtw <- t(z * w)
        xtwx <- xtw %*% z
        inv <- tryCatch(solve(xtwx + 1e-6 * diag(ncol(xtwx))), error = function(e) NULL)
        if (is.null(inv)) {
          sw <- sum(w)
          pred <- ifelse(sw <= 1e-12, mean(y_train), sum(w * y_train) / sw)
          h <- if (sw <= 1e-12) rep(1 / length(y_train), length(y_train)) else w / sw
        } else {
          h <- as.numeric(c(1, rep(0, ncol(x_train))) %*% inv %*% xtw)
          pred <- sum(h * y_train)
        }
      }

      se <- sqrt(sum((h^2) * (residuals^2), na.rm = TRUE))
      out[[i]] <- tibble(
        prediction = pred,
        se = se,
        lower = pred - 1.96 * se,
        upper = pred + 1.96 * se
      )
    }

    bind_rows(out)
  }

  section_title("Loading and preparing data")
  songs_raw <- readRDS(input_path)

  audio_vars <- c(
    "duration_min", "danceability", "energy", "loudness", "speechiness",
    "acousticness", "instrumentalness", "liveness", "valence", "tempo"
  )

  reg_vars <- c(
    "popularity", "total_artist_followers", "explicit", "duration_ms",
    "danceability", "energy", "loudness", "speechiness", "acousticness",
    "instrumentalness", "liveness", "valence", "tempo", "year", "genre",
    "artists", "key", "mode"
  )

  df <- songs_raw %>%
    select(all_of(reg_vars)) %>%
    filter(
      !is.na(popularity),
      popularity >= 0,
      popularity <= 100,
      !is.na(total_artist_followers),
      total_artist_followers >= 0,
      !is.na(duration_ms),
      duration_ms > 0,
      !is.na(year),
      year >= 1900,
      year <= 2026
    ) %>%
    mutate(
      duration_min = duration_ms / 60000,
      log_popularity = log1p(popularity),
      log_followers = log1p(total_artist_followers),
      log_followers_c = log_followers - mean(log_followers, na.rm = TRUE),
      decade = factor(floor(year / 10) * 10),
      explicit = factor(explicit),
      genre = factor(genre),
      key = factor(key),
      mode = factor(mode)
    ) %>%
    drop_na(
      popularity, log_followers_c, explicit, duration_min, danceability, energy,
      loudness, speechiness, acousticness, instrumentalness, liveness, valence,
      tempo, genre, decade, artists
    )

  if ("Rock" %in% levels(df$genre)) {
    df$genre <- relevel(df$genre, ref = "Rock")
  }
  if ("2010" %in% levels(df$decade)) {
    df$decade <- relevel(df$decade, ref = "2010")
  }

  dataset_summary <- tibble(
    metric = c(
      "rows_raw", "columns_raw", "rows_after_cleaning", "artist_clusters",
      "genres", "decades", "mean_popularity", "median_popularity",
      "mean_log_followers", "median_log_followers"
    ),
    value = c(
      nrow(songs_raw), ncol(songs_raw), nrow(df), n_distinct(df$artists),
      nlevels(df$genre), nlevels(df$decade), mean(df$popularity),
      median(df$popularity), mean(df$log_followers), median(df$log_followers)
    )
  )

  numeric_summary <- df %>%
    select(popularity, log_followers, all_of(audio_vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      p25 = quantile(value, 0.25, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      p75 = quantile(value, 0.75, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      .groups = "drop"
    )

  section_title("Principal component analysis of audio controls")
  pca_matrix <- df %>% select(all_of(audio_vars))
  pca_fit <- prcomp(pca_matrix, center = TRUE, scale. = TRUE)

  pca_variance <- tibble(
    component = paste0("PC", seq_along(pca_fit$sdev)),
    eigenvalue = pca_fit$sdev^2,
    explained_variance = eigenvalue / sum(eigenvalue),
    cumulative_variance = cumsum(explained_variance)
  )

  pc_keep <- which(pca_variance$cumulative_variance >= 0.80)[1]
  if (is.na(pc_keep)) pc_keep <- 6
  pc_keep <- max(2, pc_keep)
  kernel_pc_count <- pc_keep

  pca_loadings <- as.data.frame(pca_fit$rotation[, seq_len(pc_keep), drop = FALSE]) %>%
    rownames_to_column("variable") %>%
    as_tibble()

  pca_scores <- as.data.frame(pca_fit$x[, seq_len(pc_keep), drop = FALSE])
  names(pca_scores) <- paste0("PC", seq_len(pc_keep))
  df <- bind_cols(df, pca_scores)

  safe_write_csv(dataset_summary, "dataset_summary.csv")
  safe_write_csv(numeric_summary, "numeric_summary.csv")
  safe_write_csv(pca_variance, "pca_variance.csv")
  safe_write_csv(pca_loadings, "pca_loadings.csv")

  p_scree <- ggplot(pca_variance, aes(x = seq_along(component), y = explained_variance * 100)) +
    geom_col(fill = "#4B8BBE", width = 0.92) +
    geom_line(color = "#111111", linewidth = 0.75) +
    geom_point(color = "#111111", size = 1.8) +
    geom_text(
      aes(label = paste0(round(explained_variance * 100, 1), "%")),
      vjust = -0.45,
      size = 3.2
    ) +
    scale_x_continuous(breaks = seq_along(pca_variance$component), labels = seq_along(pca_variance$component)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Scree plot",
      x = "Dimensions",
      y = "Percentage of explained variances"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_pca_scree.png"), p_scree, width = 8, height = 5, dpi = 180)

  p_cumvar <- ggplot(pca_variance, aes(x = seq_along(component), y = cumulative_variance)) +
    geom_line(color = "#355070", linewidth = 0.9) +
    geom_point(color = "#355070", size = 2) +
    geom_hline(yintercept = 0.80, linetype = "dashed", color = "#B23A48") +
    scale_x_continuous(breaks = seq_along(pca_variance$component), labels = pca_variance$component) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Cumulative explained variance",
      x = "Principal component",
      y = "Cumulative share"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_pca_cumulative_variance.png"), p_cumvar, width = 8, height = 5, dpi = 180)

  loading_long <- pca_loadings %>%
    pivot_longer(starts_with("PC"), names_to = "component", values_to = "loading") %>%
    filter(component %in% paste0("PC", seq_len(min(3, pc_keep))))

  p_loadings <- ggplot(loading_long, aes(x = reorder(variable, loading), y = loading, fill = loading > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~component, nrow = 1) +
    coord_flip() +
    scale_fill_manual(values = c("#B23A48", "#1F6B75")) +
    labs(
      title = "PCA loadings for the first components",
      x = NULL,
      y = "Loading"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_pca_loadings.png"), p_loadings, width = 10, height = 5, dpi = 180)

  biplot_points <- df %>%
    select(popularity, genre, starts_with("PC")) %>%
    sample_n(min(nrow(.), 8000))

  make_pca_biplot <- function(points, loadings, pc_x, pc_y, title, filename) {
    arrow_scale <- 4.2
    plot_loadings <- loadings %>%
      transmute(
        variable,
        xend = .data[[pc_x]] * arrow_scale,
        yend = .data[[pc_y]] * arrow_scale,
        label_x = .data[[pc_x]] * arrow_scale * 1.16,
        label_y = .data[[pc_y]] * arrow_scale * 1.16
      )

    if (pc_x == "PC1" && pc_y == "PC2") {
      plot_loadings <- plot_loadings %>%
        mutate(
          label_x = label_x + case_when(
            variable == "acousticness" ~ -0.25,
            variable == "danceability" ~ -0.20,
            variable == "duration_min" ~ -0.20,
            variable == "energy" ~ 0.35,
            variable == "instrumentalness" ~ 0.20,
            variable == "liveness" ~ 0.20,
            variable == "loudness" ~ 0.30,
            variable == "speechiness" ~ 0.25,
            variable == "tempo" ~ 0.45,
            variable == "valence" ~ 0.25,
            TRUE ~ 0
          ),
          label_y = label_y + case_when(
            variable == "acousticness" ~ 0.12,
            variable == "danceability" ~ -0.22,
            variable == "duration_min" ~ 0.28,
            variable == "energy" ~ 0.06,
            variable == "instrumentalness" ~ 0.52,
            variable == "liveness" ~ 0.36,
            variable == "loudness" ~ -0.28,
            variable == "speechiness" ~ -0.35,
            variable == "tempo" ~ 0.18,
            variable == "valence" ~ -0.48,
            TRUE ~ 0
          )
        )
    }

    if (pc_x == "PC1" && pc_y == "PC3") {
      plot_loadings <- plot_loadings %>%
        mutate(
          label_x = label_x + case_when(
            variable == "acousticness" ~ -0.35,
            variable == "danceability" ~ -0.48,
            variable == "duration_min" ~ -0.20,
            variable == "energy" ~ 0.34,
            variable == "instrumentalness" ~ 0.32,
            variable == "liveness" ~ 0.22,
            variable == "loudness" ~ 0.24,
            variable == "speechiness" ~ 0.06,
            variable == "tempo" ~ 0.55,
            variable == "valence" ~ 0.10,
            TRUE ~ 0
          ),
          label_y = label_y + case_when(
            variable == "acousticness" ~ -0.18,
            variable == "danceability" ~ 0.55,
            variable == "duration_min" ~ 0.50,
            variable == "energy" ~ -0.05,
            variable == "instrumentalness" ~ 0.82,
            variable == "liveness" ~ -0.86,
            variable == "loudness" ~ 0.36,
            variable == "speechiness" ~ -0.42,
            variable == "tempo" ~ -0.25,
            variable == "valence" ~ 0.26,
            TRUE ~ 0
          )
        )
    }

    p <- ggplot(points, aes(x = .data[[pc_x]], y = .data[[pc_y]], color = popularity)) +
      geom_point(alpha = 0.055, size = 0.45) +
      geom_segment(
        data = plot_loadings,
        aes(x = 0, y = 0, xend = xend, yend = yend),
        inherit.aes = FALSE,
        arrow = arrow(length = unit(0.18, "cm")),
        color = "#202020",
        linewidth = 0.55
      ) +
      geom_label(
        data = plot_loadings,
        aes(x = label_x, y = label_y, label = variable),
        inherit.aes = FALSE,
        size = 2.9,
        linewidth = 0.12,
        label.padding = unit(0.10, "lines"),
        fill = "white",
        alpha = 0.88
      ) +
      scale_color_viridis_c(option = "C", end = 0.95) +
      labs(
        title = title,
        x = paste0(pc_x, " score"),
        y = paste0(pc_y, " score"),
        color = "Popularity"
      ) +
      theme_minimal()

    ggsave(file.path(output_dir, filename), p, width = 8.5, height = 6, dpi = 180)
    p
  }

  p_biplot_12 <- make_pca_biplot(
    biplot_points,
    pca_loadings,
    "PC1",
    "PC2",
    "PCA biplot for audio controls: PC1 vs PC2",
    "fig_pca_biplot_pc1_pc2.png"
  )

  p_biplot_13 <- make_pca_biplot(
    biplot_points,
    pca_loadings,
    "PC1",
    "PC3",
    "PCA biplot for audio controls: PC1 vs PC3",
    "fig_pca_biplot_pc1_pc3.png"
  )

  section_title("Supplementary PCA including artist fanbase")
  fanbase_pca_vars <- c("log_followers", audio_vars)
  fanbase_pca_fit <- prcomp(df %>% select(all_of(fanbase_pca_vars)), center = TRUE, scale. = TRUE)

  fanbase_pca_variance <- tibble(
    component = paste0("PC", seq_along(fanbase_pca_fit$sdev)),
    eigenvalue = fanbase_pca_fit$sdev^2,
    explained_variance = eigenvalue / sum(eigenvalue),
    cumulative_variance = cumsum(explained_variance)
  )

  fanbase_pca_loadings <- as.data.frame(fanbase_pca_fit$rotation[, seq_len(6), drop = FALSE]) %>%
    rownames_to_column("variable") %>%
    as_tibble()

  safe_write_csv(fanbase_pca_variance, "pca_fanbase_variance.csv")
  safe_write_csv(fanbase_pca_loadings, "pca_fanbase_loadings.csv")

  p_fanbase_scree <- ggplot(fanbase_pca_variance, aes(x = seq_along(component), y = explained_variance * 100)) +
    geom_col(fill = "#5F8F6A", width = 0.92) +
    geom_line(color = "#111111", linewidth = 0.75) +
    geom_point(color = "#111111", size = 1.8) +
    geom_text(
      aes(label = paste0(round(explained_variance * 100, 1), "%")),
      vjust = -0.45,
      size = 3.2
    ) +
    scale_x_continuous(breaks = seq_along(fanbase_pca_variance$component), labels = seq_along(fanbase_pca_variance$component)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Scree plot: fanbase plus audio controls",
      x = "Dimensions",
      y = "Percentage of explained variances"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_pca_fanbase_scree.png"), p_fanbase_scree, width = 8, height = 5, dpi = 180)

  fanbase_scores <- as.data.frame(fanbase_pca_fit$x[, 1:3, drop = FALSE]) %>%
    as_tibble() %>%
    mutate(popularity = df$popularity) %>%
    sample_n(min(nrow(.), 8000))

  make_fanbase_biplot <- function(points, loadings, pc_x, pc_y, title, filename) {
    arrow_scale <- 4.2
    plot_loadings <- loadings %>%
      transmute(
        variable,
        xend = .data[[pc_x]] * arrow_scale,
        yend = .data[[pc_y]] * arrow_scale,
        label_x = .data[[pc_x]] * arrow_scale * 1.22,
        label_y = .data[[pc_y]] * arrow_scale * 1.22
      ) %>%
      mutate(
        label_x = label_x + case_when(
          variable == "log_followers" ~ 0.35,
          variable == "acousticness" ~ -0.35,
          variable == "energy" ~ 0.30,
          variable == "loudness" ~ 0.35,
          variable == "tempo" ~ 0.35,
          TRUE ~ 0
        ),
        label_y = label_y + case_when(
          variable == "log_followers" ~ 0.20,
          variable == "instrumentalness" ~ 0.35,
          variable == "liveness" ~ -0.35,
          variable == "speechiness" ~ -0.25,
          variable == "valence" ~ -0.25,
          TRUE ~ 0
        )
      )

    p <- ggplot(points, aes(x = .data[[pc_x]], y = .data[[pc_y]], color = popularity)) +
      geom_point(alpha = 0.055, size = 0.45) +
      geom_segment(
        data = plot_loadings,
        aes(x = 0, y = 0, xend = xend, yend = yend),
        inherit.aes = FALSE,
        arrow = arrow(length = unit(0.18, "cm")),
        color = "#202020",
        linewidth = 0.55
      ) +
      geom_label(
        data = plot_loadings,
        aes(x = label_x, y = label_y, label = variable),
        inherit.aes = FALSE,
        size = 2.9,
        linewidth = 0.12,
        label.padding = unit(0.10, "lines"),
        fill = "white",
        alpha = 0.88
      ) +
      scale_color_viridis_c(option = "C", end = 0.95) +
      labs(
        title = title,
        x = paste0(pc_x, " score"),
        y = paste0(pc_y, " score"),
        color = "Popularity"
      ) +
      theme_minimal()

    ggsave(file.path(output_dir, filename), p, width = 8.5, height = 6, dpi = 180)
    p
  }

  make_fanbase_biplot(
    fanbase_scores,
    fanbase_pca_loadings,
    "PC1",
    "PC2",
    "Supplementary PCA biplot: fanbase plus audio controls, PC1 vs PC2",
    "fig_pca_fanbase_biplot_pc1_pc2.png"
  )

  make_fanbase_biplot(
    fanbase_scores,
    fanbase_pca_loadings,
    "PC1",
    "PC3",
    "Supplementary PCA biplot: fanbase plus audio controls, PC1 vs PC3",
    "fig_pca_fanbase_biplot_pc1_pc3.png"
  )

  section_title("Descriptive PCA for broad feature set")
  wide_numeric_vars <- c("log_followers", "duration_min", audio_vars, "year")
  wide_dummy_matrix <- model.matrix(~ explicit + genre + decade, data = df)
  wide_dummy_matrix <- wide_dummy_matrix[, colnames(wide_dummy_matrix) != "(Intercept)", drop = FALSE]
  colnames(wide_dummy_matrix) <- make.names(colnames(wide_dummy_matrix), unique = TRUE)

  wide_pca_matrix <- bind_cols(
    df %>% select(all_of(wide_numeric_vars)),
    as_tibble(wide_dummy_matrix)
  )
  wide_pca_matrix <- wide_pca_matrix[, vapply(wide_pca_matrix, function(x) sd(x, na.rm = TRUE) > 0, logical(1))]

  wide_pca_fit <- prcomp(wide_pca_matrix, center = TRUE, scale. = TRUE)
  wide_pc_count <- min(8, ncol(wide_pca_matrix))

  wide_pca_variance <- tibble(
    component = paste0("PC", seq_along(wide_pca_fit$sdev)),
    eigenvalue = wide_pca_fit$sdev^2,
    explained_variance = eigenvalue / sum(eigenvalue),
    cumulative_variance = cumsum(explained_variance)
  )

  wide_pca_loadings <- as.data.frame(wide_pca_fit$rotation[, seq_len(wide_pc_count), drop = FALSE]) %>%
    rownames_to_column("variable") %>%
    as_tibble()

  safe_write_csv(wide_pca_variance, "pca_wide_variance.csv")
  safe_write_csv(wide_pca_loadings, "pca_wide_loadings.csv")

  p_wide_scree <- ggplot(head(wide_pca_variance, 20), aes(x = seq_along(component), y = explained_variance * 100)) +
    geom_col(fill = "#6C8EAD", width = 0.92) +
    geom_line(color = "#111111", linewidth = 0.75) +
    geom_point(color = "#111111", size = 1.8) +
    geom_text(
      aes(label = paste0(round(explained_variance * 100, 1), "%")),
      vjust = -0.45,
      size = 3.0
    ) +
    scale_x_continuous(breaks = seq_len(min(20, nrow(wide_pca_variance))), labels = head(wide_pca_variance$component, 20)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
    labs(
      title = "Scree plot: broad descriptive PCA",
      x = "Principal component",
      y = "Percentage of explained variances"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_pca_wide_scree.png"), p_wide_scree, width = 9, height = 5, dpi = 180)

  wide_loading_long <- wide_pca_loadings %>%
    pivot_longer(starts_with("PC"), names_to = "component", values_to = "loading") %>%
    filter(component %in% paste0("PC", seq_len(min(3, wide_pc_count)))) %>%
    group_by(component) %>%
    slice_max(order_by = abs(loading), n = 12, with_ties = FALSE) %>%
    ungroup()

  p_wide_loadings <- ggplot(wide_loading_long, aes(x = reorder(variable, loading), y = loading, fill = loading > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~component, nrow = 1, scales = "free_y") +
    coord_flip() +
    scale_fill_manual(values = c("#B23A48", "#1F6B75")) +
    labs(
      title = "Largest loadings in broad descriptive PCA",
      x = NULL,
      y = "Loading"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_pca_wide_loadings.png"), p_wide_loadings, width = 11, height = 6, dpi = 180)

  wide_rng_state <- .Random.seed
  set.seed(42)
  wide_scores <- as.data.frame(wide_pca_fit$x[, 1:3, drop = FALSE]) %>%
    as_tibble() %>%
    mutate(popularity = df$popularity) %>%
    sample_n(min(nrow(.), 8000))
  .Random.seed <- wide_rng_state

  wide_biplot_loadings <- wide_pca_loadings %>%
    mutate(axis_strength = sqrt(PC1^2 + PC2^2)) %>%
    slice_max(order_by = axis_strength, n = 18, with_ties = FALSE) %>%
    transmute(
      variable,
      xend = PC1 * 4.5,
      yend = PC2 * 4.5,
      label_x = PC1 * 5.2,
      label_y = PC2 * 5.2
    )

  p_wide_biplot <- ggplot(wide_scores, aes(x = PC1, y = PC2, color = popularity)) +
    geom_point(alpha = 0.055, size = 0.45) +
    geom_segment(
      data = wide_biplot_loadings,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.18, "cm")),
      color = "#202020",
      linewidth = 0.55
    ) +
    geom_label(
      data = wide_biplot_loadings,
      aes(x = label_x, y = label_y, label = variable),
      inherit.aes = FALSE,
      size = 2.6,
      linewidth = 0.12,
      label.padding = unit(0.10, "lines"),
      fill = "white",
      alpha = 0.88
    ) +
    scale_color_viridis_c(option = "C", end = 0.95) +
    labs(
      title = "Broad descriptive PCA biplot: PC1 vs PC2",
      subtitle = "Popularity is used only for point color, not as an active PCA variable",
      x = "PC1 score",
      y = "PC2 score",
      color = "Popularity"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_pca_wide_biplot_pc1_pc2.png"), p_wide_biplot, width = 9, height = 6, dpi = 180)

  section_title("Train-validation split")
  train_id <- sample.int(nrow(df), size = floor(0.8 * nrow(df)))
  train_df <- df[train_id, , drop = FALSE]
  valid_df <- df[-train_id, , drop = FALSE]

  max_model_rows <- getOption("lab4.max_model_rows", 160000)
  max_valid_rows <- getOption("lab4.max_valid_rows", 50000)
  train_model <- train_df %>% sample_n(min(nrow(.), max_model_rows))
  valid_model <- valid_df %>% sample_n(min(nrow(.), max_valid_rows))

  controls_linear <- paste(
    "explicit + duration_min + danceability + energy + loudness + speechiness +",
    "acousticness + instrumentalness + liveness + valence + tempo + genre + decade"
  )

  lm_formula <- as.formula(paste(
    "popularity ~ log_followers_c + I(log_followers_c^2) +", controls_linear
  ))

  plm_formula <- as.formula(paste(
    "popularity ~ s(log_followers_c, k = 14) +", controls_linear
  ))

  gam_formula <- as.formula(paste(
    "popularity ~ s(log_followers_c, k = 14) + explicit +",
    "s(duration_min, k = 10) + s(danceability, k = 10) + s(energy, k = 10) +",
    "s(loudness, k = 10) + s(speechiness, k = 10) + s(acousticness, k = 10) +",
    "s(instrumentalness, k = 10) + s(liveness, k = 10) + s(valence, k = 10) +",
    "s(tempo, k = 10) + genre + decade"
  ))

  pca_terms <- paste(paste0("s(PC", seq_len(kernel_pc_count), ", k = 10)"), collapse = " + ")
  gam_pca_formula <- as.formula(paste(
    "popularity ~ s(log_followers_c, k = 14) + explicit +", pca_terms, "+ genre + decade"
  ))

  section_title("Fitting linear, partially linear, and GAM models")
  lm_fit <- lm(lm_formula, data = train_model)
  plm_fit <- bam(plm_formula, data = train_model, method = "fREML", discrete = TRUE)
  gam_fit <- bam(gam_formula, data = train_model, method = "fREML", discrete = TRUE)
  gam_pca_fit <- bam(gam_pca_formula, data = train_model, method = "fREML", discrete = TRUE)

  pred_lm <- predict(lm_fit, newdata = valid_model)
  pred_plm <- predict(plm_fit, newdata = valid_model)
  pred_gam <- predict(gam_fit, newdata = valid_model)
  pred_gam_pca <- predict(gam_pca_fit, newdata = valid_model)

  model_metrics <- tibble(
    model = c("Linear baseline from Lab 3 specification", "Partially linear: s(log_followers) + controls",
              "GAM: smooth numeric controls", "GAM with PCA audio controls"),
    n_train = nrow(train_model),
    n_validation = nrow(valid_model),
    rmse = c(
      rmse(valid_model$popularity, pred_lm),
      rmse(valid_model$popularity, pred_plm),
      rmse(valid_model$popularity, pred_gam),
      rmse(valid_model$popularity, pred_gam_pca)
    ),
    mae = c(
      mae(valid_model$popularity, pred_lm),
      mae(valid_model$popularity, pred_plm),
      mae(valid_model$popularity, pred_gam),
      mae(valid_model$popularity, pred_gam_pca)
    )
  )

  safe_write_csv(model_metrics, "model_metrics.csv")

  lm_coef <- summary(lm_fit)$coefficients
  plm_coef <- summary(plm_fit)$p.coeff
  plm_se <- summary(plm_fit)$se

  linear_terms_compare <- c(
    "log_followers_c", "I(log_followers_c^2)", "explicitExplicit",
    "duration_min", "danceability", "energy", "loudness", "speechiness",
    "acousticness", "instrumentalness", "liveness", "valence", "tempo"
  )

  coef_compare <- tibble(term = linear_terms_compare) %>%
    mutate(
      lm_estimate = ifelse(term %in% rownames(lm_coef), lm_coef[term, "Estimate"], NA_real_),
      lm_se = ifelse(term %in% rownames(lm_coef), lm_coef[term, "Std. Error"], NA_real_),
      plm_linear_estimate = ifelse(term %in% names(plm_coef), plm_coef[term], NA_real_),
      plm_linear_se = ifelse(term %in% names(plm_se), plm_se[term], NA_real_)
    )

  safe_write_csv(coef_compare, "linear_part_comparison.csv")

  smooth_plm <- as.data.frame(summary(plm_fit)$s.table) %>%
    rownames_to_column("smooth_term") %>%
    as_tibble()
  smooth_gam <- as.data.frame(summary(gam_fit)$s.table) %>%
    rownames_to_column("smooth_term") %>%
    as_tibble()
  smooth_gam_pca <- as.data.frame(summary(gam_pca_fit)$s.table) %>%
    rownames_to_column("smooth_term") %>%
    as_tibble()

  safe_write_csv(smooth_plm, "smooth_terms_plm.csv")
  safe_write_csv(smooth_gam, "smooth_terms_gam.csv")
  safe_write_csv(smooth_gam_pca, "smooth_terms_gam_pca.csv")

  section_title("Conditional prediction curves")
  grid_log_followers <- seq(
    min(df$log_followers, na.rm = TRUE),
    max(df$log_followers, na.rm = TRUE),
    length.out = 140
  )

  reference_row <- tibble(
    log_followers = grid_log_followers,
    log_followers_c = grid_log_followers - mean(df$log_followers, na.rm = TRUE),
    explicit = factor(get_mode_value(df$explicit), levels = levels(df$explicit)),
    duration_min = median(df$duration_min, na.rm = TRUE),
    danceability = median(df$danceability, na.rm = TRUE),
    energy = median(df$energy, na.rm = TRUE),
    loudness = median(df$loudness, na.rm = TRUE),
    speechiness = median(df$speechiness, na.rm = TRUE),
    acousticness = median(df$acousticness, na.rm = TRUE),
    instrumentalness = median(df$instrumentalness, na.rm = TRUE),
    liveness = median(df$liveness, na.rm = TRUE),
    valence = median(df$valence, na.rm = TRUE),
    tempo = median(df$tempo, na.rm = TRUE),
    genre = factor(get_mode_value(df$genre), levels = levels(df$genre)),
    decade = factor(get_mode_value(df$decade), levels = levels(df$decade))
  )

  for (j in seq_len(pc_keep)) {
    reference_row[[paste0("PC", j)]] <- 0
  }

  typical_row <- reference_row[1, , drop = FALSE]
  typical_row$log_followers <- median(df$log_followers, na.rm = TRUE)
  typical_row$log_followers_c <- typical_row$log_followers - mean(df$log_followers, na.rm = TRUE)

  plm_grid_pred <- as.data.frame(predict(plm_fit, newdata = reference_row, se.fit = TRUE))
  gam_grid_pred <- as.data.frame(predict(gam_fit, newdata = reference_row, se.fit = TRUE))
  gam_pca_grid_pred <- as.data.frame(predict(gam_pca_fit, newdata = reference_row, se.fit = TRUE))

  curve_predictions <- bind_rows(
    tibble(
      model = "Partially linear",
      log_followers = grid_log_followers,
      prediction = plm_grid_pred$fit,
      se = plm_grid_pred$se.fit
    ),
    tibble(
      model = "GAM original controls",
      log_followers = grid_log_followers,
      prediction = gam_grid_pred$fit,
      se = gam_grid_pred$se.fit
    ),
    tibble(
      model = "GAM PCA controls",
      log_followers = grid_log_followers,
      prediction = gam_pca_grid_pred$fit,
      se = gam_pca_grid_pred$se.fit
    )
  ) %>%
    mutate(
      lower = prediction - 1.96 * se,
      upper = prediction + 1.96 * se
    )

  safe_write_csv(curve_predictions, "conditional_curves_gam.csv")

  plot_curve_sample <- df %>%
    select(log_followers, popularity) %>%
    sample_n(min(nrow(.), 25000))

  p_conditional_gam <- ggplot() +
    geom_point(
      data = plot_curve_sample,
      aes(x = log_followers, y = popularity),
      alpha = 0.05,
      size = 0.5,
      color = "#444444"
    ) +
    geom_ribbon(
      data = curve_predictions,
      aes(x = log_followers, ymin = lower, ymax = upper, fill = model),
      alpha = 0.18,
      color = NA
    ) +
    geom_line(
      data = curve_predictions,
      aes(x = log_followers, y = prediction, color = model),
      linewidth = 0.9
    ) +
    labs(
      title = "Conditional popularity predictions by artist audience size",
      subtitle = "Audio controls fixed at medians, categorical controls fixed at modes",
      x = "log(1 + total_artist_followers)",
      y = "Predicted popularity"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_conditional_log_followers_gam.png"), p_conditional_gam, width = 9, height = 6, dpi = 180)
  ggsave(file.path(output_dir, "fig_conditional_log_followers_models.png"), p_conditional_gam, width = 9, height = 6, dpi = 180)

  explicit_grid <- typical_row[rep(1, length(levels(df$explicit))), , drop = FALSE]
  explicit_grid$explicit <- factor(levels(df$explicit), levels = levels(df$explicit))

  plm_explicit_pred <- as.data.frame(predict(plm_fit, newdata = explicit_grid, se.fit = TRUE))
  gam_explicit_pred <- as.data.frame(predict(gam_fit, newdata = explicit_grid, se.fit = TRUE))
  gam_pca_explicit_pred <- as.data.frame(predict(gam_pca_fit, newdata = explicit_grid, se.fit = TRUE))

  explicit_predictions <- bind_rows(
    tibble(
      model = "Partially linear",
      explicit = explicit_grid$explicit,
      prediction = plm_explicit_pred$fit,
      se = plm_explicit_pred$se.fit
    ),
    tibble(
      model = "GAM original controls",
      explicit = explicit_grid$explicit,
      prediction = gam_explicit_pred$fit,
      se = gam_explicit_pred$se.fit
    ),
    tibble(
      model = "GAM PCA controls",
      explicit = explicit_grid$explicit,
      prediction = gam_pca_explicit_pred$fit,
      se = gam_pca_explicit_pred$se.fit
    )
  ) %>%
    mutate(
      lower = prediction - 1.96 * se,
      upper = prediction + 1.96 * se
    )

  safe_write_csv(explicit_predictions, "conditional_explicit_predictions.csv")

  p_explicit <- ggplot(explicit_predictions, aes(x = explicit, y = prediction, color = model)) +
    geom_point(
      position = position_dodge(width = 0.45),
      size = 2.3
    ) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      position = position_dodge(width = 0.45),
      width = 0.16,
      linewidth = 0.65
    ) +
    labs(
      title = "Conditional predicted popularity by explicit content",
      subtitle = "Numeric controls fixed at medians, categorical controls fixed at modes",
      x = "Explicit content",
      y = "Predicted popularity"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_conditional_explicit.png"), p_explicit, width = 8, height = 5, dpi = 180)

  grid_danceability <- seq(
    min(df$danceability, na.rm = TRUE),
    max(df$danceability, na.rm = TRUE),
    length.out = 160
  )

  danceability_grid <- typical_row[rep(1, length(grid_danceability)), , drop = FALSE]
  danceability_grid$danceability <- grid_danceability

  plm_danceability_pred <- as.data.frame(predict(plm_fit, newdata = danceability_grid, se.fit = TRUE))
  gam_danceability_pred <- as.data.frame(predict(gam_fit, newdata = danceability_grid, se.fit = TRUE))

  danceability_predictions <- bind_rows(
    tibble(
      model = "Partially linear",
      danceability = grid_danceability,
      prediction = plm_danceability_pred$fit,
      se = plm_danceability_pred$se.fit
    ),
    tibble(
      model = "GAM original controls",
      danceability = grid_danceability,
      prediction = gam_danceability_pred$fit,
      se = gam_danceability_pred$se.fit
    )
  ) %>%
    mutate(
      lower = prediction - 1.96 * se,
      upper = prediction + 1.96 * se
    )

  safe_write_csv(danceability_predictions, "conditional_danceability_predictions.csv")

  danceability_rng_state <- .Random.seed
  set.seed(42)
  plot_danceability_sample <- df %>%
    select(danceability, popularity) %>%
    sample_n(min(nrow(.), 25000))
  .Random.seed <- danceability_rng_state

  p_danceability <- ggplot() +
    geom_point(
      data = plot_danceability_sample,
      aes(x = danceability, y = popularity),
      alpha = 0.045,
      size = 0.5,
      color = "#444444"
    ) +
    geom_ribbon(
      data = danceability_predictions,
      aes(x = danceability, ymin = lower, ymax = upper, fill = model),
      alpha = 0.18,
      color = NA
    ) +
    geom_line(
      data = danceability_predictions,
      aes(x = danceability, y = prediction, color = model),
      linewidth = 0.9
    ) +
    labs(
      title = "Conditional popularity curve by danceability",
      subtitle = "Other numeric controls fixed at medians, categorical controls fixed at modes",
      x = "Danceability",
      y = "Predicted popularity"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_conditional_danceability.png"), p_danceability, width = 9, height = 6, dpi = 180)

  section_title("Kernel regression on compact PCA-control specification")
  kernel_features <- c("log_followers_c", paste0("PC", seq_len(kernel_pc_count)))
  kernel_train <- train_df %>%
    select(popularity, all_of(kernel_features)) %>%
    sample_n(min(nrow(.), getOption("lab4.kernel_train_rows", 6500)))

  kernel_valid <- valid_df %>%
    select(popularity, all_of(kernel_features)) %>%
    sample_n(min(nrow(.), getOption("lab4.kernel_valid_rows", 1800)))

  x_center <- vapply(kernel_train[, kernel_features], mean, numeric(1), na.rm = TRUE)
  x_scale <- vapply(kernel_train[, kernel_features], sd, numeric(1), na.rm = TRUE)
  x_scale[x_scale == 0 | is.na(x_scale)] <- 1

  x_train_scaled <- scale_with_reference(kernel_train[, kernel_features], x_center, x_scale)
  x_valid_scaled <- scale_with_reference(kernel_valid[, kernel_features], x_center, x_scale)

  base_bandwidth <- nrow(kernel_train)^(-1 / (ncol(x_train_scaled) + 4))
  bandwidth_grid <- base_bandwidth * c(0.70, 1.00, 1.40, 1.90)

  kernel_tuning <- bind_rows(lapply(bandwidth_grid, function(h) {
    h_vec <- rep(h, ncol(x_train_scaled))
    nw_pred <- nw_predict(x_train_scaled, kernel_train$popularity, x_valid_scaled, h_vec, chunk_size = 150)
    ll_pred <- local_linear_predict(x_train_scaled, kernel_train$popularity, x_valid_scaled, h_vec)
    tibble(
      bandwidth_multiplier = h / base_bandwidth,
      bandwidth = h,
      nw_rmse = rmse(kernel_valid$popularity, nw_pred),
      local_linear_rmse = rmse(kernel_valid$popularity, ll_pred),
      nw_mae = mae(kernel_valid$popularity, nw_pred),
      local_linear_mae = mae(kernel_valid$popularity, ll_pred)
    )
  }))

  best_nw_h <- kernel_tuning$bandwidth[which.min(kernel_tuning$nw_rmse)]
  best_ll_h <- kernel_tuning$bandwidth[which.min(kernel_tuning$local_linear_rmse)]

  safe_write_csv(kernel_tuning, "kernel_tuning.csv")

  kernel_grid <- reference_row %>%
    select(all_of(kernel_features))
  x_grid_scaled <- scale_with_reference(kernel_grid, x_center, x_scale)

  nw_grid <- kernel_grid_with_se(
    x_train_scaled,
    kernel_train$popularity,
    x_grid_scaled,
    rep(best_nw_h, ncol(x_train_scaled)),
    method = "nw"
  )

  ll_grid <- kernel_grid_with_se(
    x_train_scaled,
    kernel_train$popularity,
    x_grid_scaled,
    rep(best_ll_h, ncol(x_train_scaled)),
    method = "local_linear"
  )

  kernel_curves <- bind_rows(
    nw_grid %>% mutate(model = "Nadaraya-Watson"),
    ll_grid %>% mutate(model = "Local linear")
  ) %>%
    mutate(log_followers = rep(grid_log_followers, 2)) %>%
    select(model, log_followers, prediction, se, lower, upper)

  safe_write_csv(kernel_curves, "conditional_curves_kernel.csv")

  p_kernel <- ggplot() +
    geom_point(
      data = plot_curve_sample,
      aes(x = log_followers, y = popularity),
      alpha = 0.04,
      size = 0.5,
      color = "#444444"
    ) +
    geom_ribbon(
      data = kernel_curves,
      aes(x = log_followers, ymin = lower, ymax = upper, fill = model),
      alpha = 0.18,
      color = NA
    ) +
    geom_line(
      data = kernel_curves,
      aes(x = log_followers, y = prediction, color = model),
      linewidth = 0.9
    ) +
    labs(
      title = "Kernel regression: Nadaraya-Watson vs local linear",
      subtitle = paste0("Controls compressed with first ", kernel_pc_count, " audio PCs fixed at zero"),
      x = "log(1 + total_artist_followers)",
      y = "Predicted popularity"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "fig_kernel_log_followers.png"), p_kernel, width = 9, height = 6, dpi = 180)

  section_title("Additional GAM smooth plots")
  png(file.path(output_dir, "fig_gam_smooth_terms.png"), width = 1400, height = 1000, res = 150)
  par(mfrow = c(3, 4), mar = c(4, 4, 3, 1))
  plot(gam_fit, shade = TRUE, pages = 1, scale = 0)
  dev.off()

  section_title("Saving compact model objects")
  saveRDS(
    list(
      pca_fit = pca_fit,
      pc_keep = pc_keep,
      kernel_pc_count = kernel_pc_count,
      lm_fit = lm_fit,
      plm_fit = plm_fit,
      gam_fit = gam_fit,
      gam_pca_fit = gam_pca_fit,
      reference_row = reference_row,
      kernel_features = kernel_features,
      kernel_center = x_center,
      kernel_scale = x_scale,
      best_nw_h = best_nw_h,
      best_ll_h = best_ll_h
    ),
    file.path(output_dir, "lab4_models.rds")
  )

  section_title("Console summary")
  cat("Lab 4 nonparametric regression and PCA completed.\n")
  cat("Output directory:", output_dir, "\n")
  cat("Clean rows:", nrow(df), "\n")
  cat("Model training rows:", nrow(train_model), "\n")
  cat("Validation rows:", nrow(valid_model), "\n")
  cat("PCA components retained for 80% threshold:", pc_keep, "\n")
  cat("Kernel PCA controls:", kernel_pc_count, "\n")
  cat("\nModel validation metrics:\n")
  print(model_metrics)
  cat("\nKernel tuning metrics:\n")
  print(kernel_tuning)
  cat("\nPCA explained variance:\n")
  print(pca_variance)

  environment()
}


run_lera_part <- function() {
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(mgcv)
    library(readr)
    library(tibble)
    library(tidyr)
  })

  options(stringsAsFactors = FALSE, scipen = 999)
  set.seed(42)

  input_path <- file.path(getwd(), "songs_clean.rds")
  output_dir <- file.path(getwd(), "lab4_explicit_outputs")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # --- Допоміжні функції (з шаблону) ---
  safe_write_csv <- function(x, path) { write_csv(x, file.path(output_dir, path), na = "") }
  rmse <- function(actual, predicted) { sqrt(mean((actual - predicted)^2, na.rm = TRUE)) }
  mae <- function(actual, predicted) { mean(abs(actual - predicted), na.rm = TRUE) }
  get_mode_value <- function(x) { names(sort(table(x), decreasing = TRUE))[1] }
  section_title <- function(title) { cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "") }

  # Функції для ядрової регресії
  gaussian_kernel <- function(u) { exp(-0.5 * rowSums(u^2)) }
  scale_with_reference <- function(x, center, scale) { sweep(sweep(as.matrix(x), 2, center, "-"), 2, scale, "/") }

  nw_predict <- function(x_train, y_train, x_new, bandwidth, chunk_size = 250) {
    x_train <- as.matrix(x_train); x_new <- as.matrix(x_new)
    y_train <- as.numeric(y_train); bandwidth <- as.numeric(bandwidth)
    preds <- numeric(nrow(x_new))
    for (start in seq(1, nrow(x_new), by = chunk_size)) {
      end <- min(start + chunk_size - 1, nrow(x_new))
      for (i in start:end) {
        u <- sweep(x_train, 2, x_new[i, ], "-") / bandwidth
        w <- gaussian_kernel(u); sw <- sum(w)
        preds[i] <- ifelse(sw <= 1e-12, mean(y_train), sum(w * y_train) / sw)
      }
    }
    preds
  }

  local_linear_predict <- function(x_train, y_train, x_new, bandwidth, ridge = 1e-6) {
    x_train <- as.matrix(x_train); x_new <- as.matrix(x_new)
    y_train <- as.numeric(y_train); bandwidth <- as.numeric(bandwidth)
    preds <- numeric(nrow(x_new))
    for (i in seq_len(nrow(x_new))) {
      centered <- sweep(x_train, 2, x_new[i, ], "-")
      u <- sweep(centered, 2, bandwidth, "/")
      w <- gaussian_kernel(u)
      if (sum(w) <= 1e-12) { preds[i] <- mean(y_train); next }
      z <- cbind(1, centered)
      xtw <- t(z * w); xtwx <- xtw %*% z; xtwy <- xtw %*% y_train
      beta <- tryCatch(solve(xtwx + ridge * diag(ncol(xtwx)), xtwy), error = function(e) rep(NA_real_, ncol(z)))
      preds[i] <- ifelse(is.na(beta[1]), sum(w * y_train) / sum(w), beta[1])
    }
    preds
  }

  kernel_grid_with_se <- function(x_train, y_train, x_grid, bandwidth, method = c("nw", "local_linear")) {
    method <- match.arg(method)
    x_train <- as.matrix(x_train); x_grid <- as.matrix(x_grid)
    y_train <- as.numeric(y_train); bandwidth <- as.numeric(bandwidth)

    fitted_train <- if(method == "nw") nw_predict(x_train, y_train, x_train, bandwidth) else local_linear_predict(x_train, y_train, x_train, bandwidth)
    residuals <- y_train - fitted_train

    out <- vector("list", nrow(x_grid))
    for (i in seq_len(nrow(x_grid))) {
      centered <- sweep(x_train, 2, x_grid[i, ], "-")
      u <- sweep(centered, 2, bandwidth, "/")
      w <- gaussian_kernel(u)

      if (method == "nw") {
        sw <- sum(w)
        pred <- ifelse(sw <= 1e-12, mean(y_train), sum(w * y_train) / sw)
        h <- if (sw <= 1e-12) rep(1 / length(y_train), length(y_train)) else w / sw
      } else {
        z <- cbind(1, centered)
        xtw <- t(z * w)
        xtwx <- xtw %*% z
        inv <- tryCatch(solve(xtwx + 1e-6 * diag(ncol(xtwx))), error = function(e) NULL)
        if (is.null(inv)) {
          sw <- sum(w)
          pred <- ifelse(sw <= 1e-12, mean(y_train), sum(w * y_train) / sw)
          h <- if (sw <= 1e-12) rep(1 / length(y_train), length(y_train)) else w / sw
        } else {
          h <- as.numeric(c(1, rep(0, ncol(x_train))) %*% inv %*% xtw)
          pred <- sum(h * y_train)
        }
      }
      se <- sqrt(sum((h^2) * (residuals^2), na.rm = TRUE))
      out[[i]] <- tibble(prediction = pred, se = se, lower = pred - 1.96 * se, upper = pred + 1.96 * se)
    }
    bind_rows(out)
  }

  # ==========================================
  # 1. Завантаження та підготовка даних
  # ==========================================
  section_title("Loading and preparing data")
  songs_raw <- readRDS(input_path)

  audio_vars <- c("danceability", "energy", "loudness", "speechiness", "acousticness", 
                  "instrumentalness", "liveness", "valence", "tempo", "duration_ms")

  df <- songs_raw %>%
    filter(!is.na(explicit), !is.na(year), !is.na(genre)) %>%
    drop_na(all_of(audio_vars)) %>%
    mutate(
      explicit_num = ifelse(explicit == "Explicit", 1, 0),
      genre = as.factor(genre)
    )

  # ==========================================
  # 2. PCA (Аналіз головних компонент)
  # ==========================================
  section_title("Principal component analysis of audio controls")
  pca_matrix <- df %>% select(all_of(audio_vars))
  pca_fit <- prcomp(pca_matrix, center = TRUE, scale. = TRUE)

  pca_variance <- tibble(
    component = paste0("PC", seq_along(pca_fit$sdev)),
    eigenvalue = pca_fit$sdev^2,
    explained_variance = eigenvalue / sum(eigenvalue),
    cumulative_variance = cumsum(explained_variance)
  )

  pc_keep <- max(2, which(pca_variance$cumulative_variance >= 0.80)[1])
  pca_scores <- as.data.frame(pca_fit$x[, 1:pc_keep, drop = FALSE])
  names(pca_scores) <- paste0("PC", 1:pc_keep)
  df <- bind_cols(df, pca_scores)

  # Графіки PCA
  p_scree <- ggplot(pca_variance, aes(x = seq_along(component), y = explained_variance * 100)) +
    geom_col(fill = "#B23A48") + geom_line() + geom_point() +
    labs(title = "Scree plot (PCA)", x = "Головні компоненти", y = "% поясненої дисперсії") + theme_minimal()
  ggsave(file.path(output_dir, "fig_pca_scree.png"), p_scree, width = 8, height = 5)

  # Біграфік PCA (розмальований за Explicit)
  biplot_points <- df %>% sample_n(min(nrow(.), 5000))
  p_biplot <- ggplot(biplot_points, aes(x = PC1, y = PC2, color = as.factor(explicit_num))) +
    geom_point(alpha = 0.3) +
    scale_color_manual(values = c("0" = "#4B8BBE", "1" = "#B23A48"), labels = c("Clean", "Explicit")) +
    labs(title = "PCA Score Plot: PC1 vs PC2", color = "Контент") + theme_minimal()
  ggsave(file.path(output_dir, "fig_pca_biplot.png"), p_biplot, width = 8, height = 6)

  # ==========================================
  # 3. Непараметричні моделі (PLM та GAM)
  # ==========================================
  section_title("Train-validation split & GAM models")
  train_id <- sample.int(nrow(df), size = floor(0.8 * nrow(df)))
  train_model <- df[train_id, ] %>% sample_n(min(nrow(.), 50000)) # Обмеження для швидкості

  # Логістична PLM: danceability згладжується, решта - лінійно
  plm_formula <- explicit_num ~ s(danceability, k=15) + energy + valence + genre + year
  plm_fit <- bam(plm_formula, data = train_model, family = binomial(link="logit"), discrete = TRUE)

  # Повний GAM: згладжуємо всі ключові метрики
  gam_formula <- explicit_num ~ s(danceability) + s(energy) + s(valence) + genre + s(year)
  gam_fit <- bam(gam_formula, data = train_model, family = binomial(link="logit"), discrete = TRUE)

  # Графіки сплайнів GAM
  png(file.path(output_dir, "fig_gam_smooths.png"), width = 1200, height = 800, res = 150)
  par(mfrow = c(2, 2))
  plot(gam_fit, shade = TRUE, scale = 0, trans = plogis, shift = coef(gam_fit)[1]) 
  dev.off()

  # ==========================================
  # 4. Графік умовної залежності (Conditional prediction curve)
  # ==========================================
  grid_dance <- seq(0, 1, length.out = 100)
  # Фіксуємо інші змінні на рівні медіани/моди
  ref_row <- tibble(
    danceability = grid_dance,
    energy = median(df$energy),
    valence = median(df$valence),
    year = median(df$year),
    genre = factor("Hip-Hop", levels = levels(df$genre)) # Перевіряємо для репу
  )

  plm_pred <- predict(plm_fit, newdata = ref_row, type = "link", se.fit = TRUE)
  # Переводимо log-odds у ймовірності (Inverse Logit)
  inv_logit <- function(x) { exp(x) / (1 + exp(x)) }

  curve_df <- tibble(
    danceability = grid_dance,
    pred_prob = inv_logit(plm_pred$fit),
    lower = inv_logit(plm_pred$fit - 1.96 * plm_pred$se.fit),
    upper = inv_logit(plm_pred$fit + 1.96 * plm_pred$se.fit)
  )

  p_curve <- ggplot(curve_df, aes(x = danceability, y = pred_prob)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#B23A48", alpha = 0.2) +
    geom_line(color = "#B23A48", linewidth = 1) +
    labs(title = "Вплив Danceability на ймовірність Explicit (для Hip-Hop)",
         x = "Danceability", y = "Ймовірність Explicit-контенту") + theme_minimal()
  ggsave(file.path(output_dir, "fig_conditional_curve.png"), p_curve, width = 8, height = 5)

  # ==========================================
  # 5. Ядрова регресія (NW vs LL) - ВИПРАВЛЕНО
  # ==========================================
  section_title("Kernel regression (NW vs LL)")

  # Беремо оригінальний ключовий регресор (danceability) та два найважливіші контролі 
  # (energy, valence), щоб уникнути прокляття розмірності, але зберегти логіку.
  kernel_features <- c("danceability", "energy", "valence")
  kernel_train <- train_model %>% select(explicit_num, all_of(kernel_features)) %>% sample_n(3000)

  x_center <- vapply(kernel_train[, kernel_features], mean, numeric(1), na.rm=TRUE)
  x_scale <- vapply(kernel_train[, kernel_features], sd, numeric(1), na.rm=TRUE)
  x_train_scaled <- scale_with_reference(kernel_train[, kernel_features], x_center, x_scale)

  best_h <- 0.6 # Оптимальна ширина вікна

  # Створюємо сітку для умовного прогнозу (Danceability від 0 до 1)
  grid_k <- tibble(danceability = seq(0, 1, length.out=100))
  # Контролі фіксуємо на медіані
  grid_k$energy <- median(train_model$energy, na.rm = TRUE)
  grid_k$valence <- median(train_model$valence, na.rm = TRUE)

  x_grid_scaled <- scale_with_reference(grid_k[, kernel_features], x_center, x_scale)

  # Рахуємо обидва методи
  nw_grid <- kernel_grid_with_se(x_train_scaled, kernel_train$explicit_num, x_grid_scaled, rep(best_h, ncol(x_train_scaled)), method = "nw")
  ll_grid <- kernel_grid_with_se(x_train_scaled, kernel_train$explicit_num, x_grid_scaled, rep(best_h, ncol(x_train_scaled)), method = "local_linear")

  kernel_curves <- bind_rows(
    nw_grid %>% mutate(model = "Nadaraya-Watson"),
    ll_grid %>% mutate(model = "Local Linear")
  ) %>% mutate(danceability = rep(grid_k$danceability, 2))

  # Будуємо графік
  p_kernel_compare <- ggplot(kernel_curves, aes(x = danceability, y = prediction, color = model, fill = model)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Nadaraya-Watson" = "#B23A48", "Local Linear" = "#4B8BBE")) +
    scale_fill_manual(values = c("Nadaraya-Watson" = "#B23A48", "Local Linear" = "#4B8BBE")) +
    labs(title = "Ядрова регресія: Надарая-Вотсон vs Локальна лінійна", 
         subtitle = "Контролі (energy, valence) зафіксовано на рівні медіани",
         x = "Danceability", y = "P(Explicit = 1)") +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave(file.path(output_dir, "fig_kernel_compare.png"), p_kernel_compare, width = 8, height = 5)

  cat("\nDone! Виправлений графік збережено.\n")

  environment()
}



main <- function() {
  section_title("Lab 4 monolithic combined R script")
  cat("Working directory:", root_dir, "\n")

  check_files(c("songs_clean.rds"))
  check_packages(all_required_packages)
  load_base_packages()

  vlad_env <- run_part("1. Vlad: danceability", run_vlad_part)
  print_vlad_results(vlad_env)

  popularity_env <- run_part("2. Popularity: nonparametric regression and PCA", run_popularity_part)
  print_popularity_results(popularity_env)

  lera_env <- run_part("3. Lera: explicit", run_lera_part)
  print_lera_results(lera_env)

  section_title("Monolithic combined run completed")
  cat("Generated/updated output directories:\n")
  cat("- lab4_outputs\n")
  cat("- lab4_explicit_outputs\n")
}

main()
