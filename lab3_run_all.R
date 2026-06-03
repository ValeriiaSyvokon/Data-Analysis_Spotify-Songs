RUN_EXACT_EXPLICIT_AME <- FALSE

suppressPackageStartupMessages({
  required_packages <- c("dplyr", "tidyr", "readr", "tibble", "ggplot2", "MASS")
  if (RUN_EXACT_EXPLICIT_AME) {
    required_packages <- c(required_packages, "marginaleffects")
  }
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
  }

  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(ggplot2)
})

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  warn = 1,
  width = 300,
  tibble.width = Inf,
  tibble.print_max = Inf,
  pillar.width = Inf
)

set.seed(42)

DATA_PATH <- file.path(getwd(), "songs_clean.rds")
VLAD_OUT <- file.path(getwd(), "vlad_outputs")
POP_OUT <- file.path(getwd(), "lab3_outputs")

dir.create(VLAD_OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(POP_OUT, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(DATA_PATH)) {
  stop("File not found: ", DATA_PATH)
}

write_vlad <- function(x, name) {
  readr::write_csv(x, file.path(VLAD_OUT, name), na = "")
}

write_pop <- function(x, name) {
  readr::write_csv(x, file.path(POP_OUT, name), na = "")
}

fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), as.numeric(x)))
}

fmt_p <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "", ifelse(x == 0, "<1e-300", ifelse(x < 0.001, sprintf("%.2e", x), sprintf("%.4f", x))))
}

print_table <- function(title, x) {
  cat("\n=== ", title, " ===\n", sep = "")
  print(as.data.frame(x), row.names = FALSE, right = FALSE)
}

format_coef_table <- function(x, se_col = "robust_se", digits = 4) {
  if (!se_col %in% names(x)) {
    se_col <- if ("std_error" %in% names(x)) "std_error" else if ("cluster_se" %in% names(x)) "cluster_se" else NA_character_
  }
  out <- x
  numeric_cols <- intersect(c("estimate", se_col, "statistic", "t_value", "p_value"), names(out))
  for (col in numeric_cols) {
    out[[col]] <- if (col == "p_value") fmt_p(out[[col]]) else fmt_num(out[[col]], digits)
  }
  out
}

print_coef_table <- function(title, x, max_rows = 80, se_col = "robust_se") {
  if (nrow(x) > max_rows) {
    x <- x %>% slice_head(n = max_rows)
    title <- paste0(title, " (first ", max_rows, " rows)")
  }
  print_table(title, format_coef_table(x, se_col = se_col))
}

# =========================================================
# Shared inference helpers
# =========================================================

hc1_vcov_lm <- function(model) {
  b <- coef(model)
  keep <- !is.na(b)
  X <- model.matrix(model)[, keep, drop = FALSE]
  u <- residuals(model)
  n <- nrow(X)
  k <- ncol(X)
  bread <- MASS::ginv(crossprod(X))
  meat <- crossprod(X * as.numeric(u^2))
  V <- (n / (n - k)) * bread %*% meat %*% bread
  rownames(V) <- colnames(V) <- names(b)[keep]
  V
}

coef_table_lm <- function(model, model_name, terms = NULL) {
  b <- coef(model)
  b <- b[!is.na(b)]
  V <- hc1_vcov_lm(model)
  se <- sqrt(diag(V))
  stat <- b / se
  p <- 2 * pnorm(abs(stat), lower.tail = FALSE)
  out <- tibble(
    model = model_name,
    term = names(b),
    estimate = as.numeric(b),
    robust_se = as.numeric(se),
    statistic = as.numeric(stat),
    p_value = as.numeric(p)
  )
  if (!is.null(terms)) out <- out %>% filter(term %in% terms)
  out
}

wald_lm <- function(model, pattern = NULL, terms = NULL, label) {
  b <- coef(model)
  b <- b[!is.na(b)]
  V <- hc1_vcov_lm(model)
  if (!is.null(pattern)) terms <- grep(pattern, names(b), value = TRUE)
  terms <- intersect(terms, names(b))
  if (length(terms) == 0) {
    return(tibble(test = label, q = 0, statistic = NA_real_, p_value = NA_real_, terms = ""))
  }
  bb <- matrix(b[terms], ncol = 1)
  VV <- V[terms, terms, drop = FALSE]
  stat <- as.numeric(t(bb) %*% MASS::ginv(VV) %*% bb)
  tibble(
    test = label,
    q = length(terms),
    statistic = stat,
    p_value = pchisq(stat, df = length(terms), lower.tail = FALSE),
    terms = paste(terms, collapse = "; ")
  )
}

coef_table_glm_robust <- function(model, model_name, terms = NULL) {
  if (!inherits(model, "glm")) {
    stop("coef_table_glm_robust expected a glm object for ", model_name, ", got class: ", paste(class(model), collapse = ", "))
  }
  s <- summary(model)$coefficients
  out <- tibble(
    model = model_name,
    term = rownames(s),
    estimate = as.numeric(s[, 1]),
    std_error = as.numeric(s[, 2]),
    statistic = as.numeric(s[, 3]),
    p_value = as.numeric(s[, 4])
  )
  if (!is.null(terms)) out <- out %>% filter(term %in% terms)
  out
}

robust_vcov_glm <- function(model) {
  mf <- model.frame(model)
  y <- model.response(mf)
  X <- model.matrix(model)
  eta <- as.numeric(X %*% coef(model))
  mu <- as.numeric(fitted(model))
  eps <- 1e-8
  mu <- pmin(pmax(mu, eps), 1 - eps)

  dmu_deta <- switch(
    model$family$link,
    logit = mu * (1 - mu),
    probit = dnorm(eta),
    stop("Unsupported GLM link for robust_vcov_glm: ", model$family$link)
  )

  score_factor <- (y - mu) * dmu_deta / (mu * (1 - mu))
  hessian_weight <- (dmu_deta^2) / (mu * (1 - mu))

  bread <- MASS::ginv(crossprod(X, X * hessian_weight))
  meat <- crossprod(X * score_factor)
  n <- nrow(X)
  k <- ncol(X)
  V <- (n / (n - k)) * bread %*% meat %*% bread
  rownames(V) <- colnames(V) <- colnames(X)
  V
}

glm_ame_fast <- function(model, terms, model_name, link = c("logit", "probit")) {
  link <- match.arg(link)
  beta <- coef(model)
  X <- model.matrix(model)
  eta <- as.numeric(X %*% beta)
  factor <- if (link == "logit") {
    p <- 1 / (1 + exp(-eta))
    mean(p * (1 - p), na.rm = TRUE)
  } else {
    mean(dnorm(eta), na.rm = TRUE)
  }
  terms <- intersect(terms, names(beta))
  tibble(
    model = model_name,
    term = terms,
    estimate = as.numeric(beta[terms]) * factor,
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    note = "Fast AME approximation; set RUN_EXACT_EXPLICIT_AME <- TRUE for marginaleffects SE."
  )
}

glm_ame_by_genre_fast <- function(model, model_name, link = c("logit", "probit")) {
  link <- match.arg(link)
  mf <- model.frame(model)
  X <- model.matrix(model)
  eta <- as.numeric(X %*% coef(model))
  factor <- if (link == "logit") {
    p <- 1 / (1 + exp(-eta))
    p * (1 - p)
  } else {
    dnorm(eta)
  }
  beta <- coef(model)
  bind_rows(lapply(levels(mf$genre), function(g) {
    interaction_name <- paste0("danceability:genre", g)
    slope <- beta["danceability"] + ifelse(interaction_name %in% names(beta), beta[interaction_name], 0)
    rows <- mf$genre == g
    tibble(
      model = model_name,
      genre = g,
      term = "danceability",
      estimate = as.numeric(slope) * mean(factor[rows], na.rm = TRUE),
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      n = sum(rows),
      note = "Fast by-genre AME approximation; set RUN_EXACT_EXPLICIT_AME <- TRUE for marginaleffects SE."
    )
  }))
}

vif_numeric <- function(data, vars) {
  bind_rows(lapply(vars, function(v) {
    others <- setdiff(vars, v)
    fit <- lm(as.formula(paste(v, "~", paste(others, collapse = " + "))), data = data)
    r2 <- summary(fit)$r.squared
    tibble(variable = v, r2 = r2, vif = 1 / (1 - r2))
  })) %>% arrange(desc(vif))
}

# =========================================================
# Popularity helpers with clustered SE by artist
# =========================================================

cluster_vcov <- function(X, residuals, cluster) {
  cluster <- as.factor(cluster)
  keep <- !is.na(cluster)
  X <- X[keep, , drop = FALSE]
  residuals <- residuals[keep]
  cluster <- droplevels(cluster[keep])

  n <- nrow(X)
  k <- ncol(X)
  g <- nlevels(cluster)
  xtx_inv <- solve(crossprod(X))
  xu <- X * as.numeric(residuals)
  summed <- rowsum(xu, group = cluster, reorder = FALSE)
  meat <- crossprod(as.matrix(summed))
  correction <- (g / (g - 1)) * ((n - 1) / (n - k))
  correction * xtx_inv %*% meat %*% xtx_inv
}

hc1_vcov_matrix <- function(X, residuals) {
  n <- nrow(X)
  k <- ncol(X)
  xtx_inv <- solve(crossprod(X))
  meat <- crossprod(X * as.numeric(residuals))
  (n / (n - k)) * xtx_inv %*% meat %*% xtx_inv
}

fit_ols_clustered <- function(formula, data, cluster_var = "artists") {
  data <- as.data.frame(data)
  data$.row_id_internal <- seq_len(nrow(data))
  mf <- model.frame(formula, data = data, na.action = na.omit)
  row_id <- as.integer(rownames(mf))
  cluster <- data[[cluster_var]][row_id]
  y <- model.response(mf)
  X_full <- model.matrix(attr(mf, "terms"), data = mf)
  qr_X <- qr(X_full)
  keep_cols <- sort(qr_X$pivot[seq_len(qr_X$rank)])
  X <- X_full[, keep_cols, drop = FALSE]
  fit <- lm.fit(x = X, y = y)

  beta <- coef(fit)
  residuals <- fit$residuals
  fitted <- as.vector(X %*% beta)
  rss <- sum(residuals^2)
  sigma2 <- rss / (length(y) - length(beta))
  vcov_ols <- sigma2 * solve(crossprod(X))
  vcov_cl <- cluster_vcov(X, residuals, cluster)
  vcov_hc <- hc1_vcov_matrix(X, residuals)
  se_ols <- sqrt(diag(vcov_ols))
  se_cl <- sqrt(diag(vcov_cl))
  se_hc <- sqrt(diag(vcov_hc))
  t_ols <- beta / se_ols
  t_cl <- beta / se_cl

  sst <- sum((y - mean(y))^2)
  r2 <- 1 - rss / sst
  adj_r2 <- 1 - (1 - r2) * (length(y) - 1) / (length(y) - length(beta))

  list(
    formula = formula,
    beta = beta,
    residuals = residuals,
    fitted = fitted,
    vcov_ols = vcov_ols,
    vcov_cl = vcov_cl,
    vcov_hc = vcov_hc,
    se_ols = se_ols,
    se_cl = se_cl,
    se_hc = se_hc,
    t_ols = t_ols,
    p_ols = 2 * pnorm(abs(t_ols), lower.tail = FALSE),
    t_cl = t_cl,
    p_cl = 2 * pnorm(abs(t_cl), lower.tail = FALSE),
    n = length(y),
    k = length(beta),
    clusters = nlevels(as.factor(cluster)),
    r2 = r2,
    adj_r2 = adj_r2
  )
}

coef_row_clustered <- function(mod, model_name, coef_names) {
  existing <- intersect(coef_names, names(mod$beta))
  bind_rows(lapply(existing, function(term) {
    tibble(
      model = model_name,
      term = term,
      estimate = unname(mod$beta[term]),
      conventional_se = unname(mod$se_ols[term]),
      cluster_se = unname(mod$se_cl[term]),
      hc1_se = unname(mod$se_hc[term]),
      t_value_conventional = unname(mod$t_ols[term]),
      p_value_conventional = unname(mod$p_ols[term]),
      t_value = unname(mod$t_cl[term]),
      p_value = unname(mod$p_cl[term])
    )
  }))
}

model_gof_clustered <- function(mod, model_name) {
  tibble(
    model = model_name,
    n = mod$n,
    clusters_artists = mod$clusters,
    k = mod$k,
    r2 = mod$r2,
    adj_r2 = mod$adj_r2
  )
}

wald_clustered <- function(model, pattern = NULL, terms = NULL, label) {
  cn <- names(model$beta)
  if (!is.null(pattern)) terms <- grep(pattern, cn, value = TRUE)
  terms <- intersect(terms, cn)
  if (length(terms) == 0) {
    return(tibble(test = label, q = 0, statistic = NA_real_, p_value = NA_real_, terms = ""))
  }
  b <- matrix(model$beta[terms], ncol = 1)
  V <- model$vcov_cl[terms, terms, drop = FALSE]
  stat <- as.numeric(t(b) %*% solve(V) %*% b)
  tibble(
    test = label,
    q = length(terms),
    statistic = stat,
    p_value = pchisq(stat, df = length(terms), lower.tail = FALSE),
    terms = paste(terms, collapse = "; ")
  )
}

wald_clustered_compare <- function(model, terms, label) {
  terms <- intersect(terms, names(model$beta))
  if (length(terms) == 0) {
    return(tibble(test = label, q = 0, statistic_conventional = NA_real_, p_value_conventional = NA_real_, statistic_cluster = NA_real_, p_value_cluster = NA_real_, terms = ""))
  }
  b <- matrix(model$beta[terms], ncol = 1)
  V_ols <- model$vcov_ols[terms, terms, drop = FALSE]
  V_cl <- model$vcov_cl[terms, terms, drop = FALSE]
  stat_ols <- as.numeric(t(b) %*% solve(V_ols) %*% b)
  stat_cl <- as.numeric(t(b) %*% solve(V_cl) %*% b)
  tibble(
    test = label,
    q = length(terms),
    statistic_conventional = stat_ols,
    p_value_conventional = pchisq(stat_ols, df = length(terms), lower.tail = FALSE),
    statistic_cluster = stat_cl,
    p_value_cluster = pchisq(stat_cl, df = length(terms), lower.tail = FALSE),
    terms = paste(terms, collapse = "; ")
  )
}

make_reg_table <- function(models, terms) {
  term_labels <- c(
    "(Intercept)" = "Intercept",
    "log_followers_c" = "log followers, centered",
    "I(log_followers_c^2)" = "log followers^2",
    "explicitExplicit" = "Explicit",
    "duration_min" = "Duration, minutes",
    "danceability" = "Danceability",
    "energy" = "Energy",
    "loudness" = "Loudness",
    "speechiness" = "Speechiness",
    "acousticness" = "Acousticness",
    "instrumentalness" = "Instrumentalness",
    "liveness" = "Liveness",
    "valence" = "Valence",
    "tempo" = "Tempo",
    "log_followers_c:explicitExplicit" = "log followers x Explicit"
  )
  out <- tibble(term = terms, label = ifelse(terms %in% names(term_labels), term_labels[terms], terms))
  for (model_name in names(models)) {
    model <- models[[model_name]]
    out[[model_name]] <- vapply(terms, function(term) {
      if (!term %in% names(model$beta)) return("")
      sprintf("%.4f\n(%.4f)", model$beta[[term]], model$se_cl[[term]])
    }, character(1))
  }
  out
}

# =========================================================
# Part 1: duration
# =========================================================

run_duration <- function(songs_clean) {
  df_duration <- songs_clean %>%
    filter(duration_ms < 600000, duration_ms > 0, !is.na(year), !is.na(tempo)) %>%
    mutate(
      genre = as.factor(genre),
      explicit = as.factor(explicit),
      log_duration = log(duration_ms),
      year_centered = year - 2000
    ) %>%
    drop_na(log_duration, year, year_centered, tempo, genre, danceability, energy, acousticness, explicit)

  df_modern <- df_duration %>% filter(year >= 2010)

  d1 <- lm(log_duration ~ year + tempo + genre, data = df_duration)
  d2 <- lm(log_duration ~ year + tempo + genre + danceability + energy + acousticness + explicit, data = df_duration)
  d3 <- lm(log_duration ~ year * genre + tempo + danceability + energy + acousticness + explicit, data = df_duration)
  d4 <- lm(log_duration ~ year_centered * genre + I(year_centered^2) + tempo + I(tempo^2) +
             energy + danceability + acousticness + explicit, data = df_duration)
  d5 <- lm(log_duration ~ year * genre + tempo + I(tempo^2) + energy + danceability + acousticness + explicit,
           data = df_modern)

  duration_terms <- c("year", "year_centered", "I(year_centered^2)", "tempo", "I(tempo^2)",
                      "danceability", "energy", "acousticness", "explicitExplicit")
  duration_coefs <- bind_rows(
    coef_table_lm(d1, "D1_base", duration_terms),
    coef_table_lm(d2, "D2_controls", duration_terms),
    coef_table_lm(d3, "D3_year_genre_interaction", duration_terms),
    coef_table_lm(d4, "D4_poly_time_tempo", duration_terms),
    coef_table_lm(d5, "D5_modern_2010plus", duration_terms)
  )

  duration_gof <- bind_rows(lapply(list(
    D1_base = d1,
    D2_controls = d2,
    D3_year_genre_interaction = d3,
    D4_poly_time_tempo = d4,
    D5_modern_2010plus = d5
  ), function(model) {
    tibble(n = nobs(model), r2 = summary(model)$r.squared, adj_r2 = summary(model)$adj.r.squared)
  }), .id = "model")

  duration_tests <- bind_rows(
    wald_lm(d1, pattern = "^genre", label = "D1: joint genre effects"),
    wald_lm(d3, pattern = "year:genre", label = "D3: year x genre interactions"),
    wald_lm(d4, terms = c("year_centered", "I(year_centered^2)"), label = "D4: time polynomial"),
    wald_lm(d5, pattern = "year:genre", label = "D5: modern year x genre interactions"),
    wald_lm(d5, terms = c("tempo", "I(tempo^2)"), label = "D5: tempo polynomial")
  )

  b_d5 <- coef(d5)
  tempo_delta <- function(a, b) {
    delta <- b_d5["tempo"] * (b - a) + b_d5["I(tempo^2)"] * (b^2 - a^2)
    100 * (exp(delta) - 1)
  }
  duration_examples <- tibble(
    scenario = c("tempo 80 -> 90 BPM", "tempo 120 -> 130 BPM"),
    percent_change_duration = c(tempo_delta(80, 90), tempo_delta(120, 130))
  )

  write_vlad(duration_coefs, "duration_coefficients.csv")
  write_vlad(duration_gof, "duration_gof.csv")
  write_vlad(duration_tests, "duration_tests.csv")
  write_vlad(duration_examples, "duration_examples.csv")

  list(gof = duration_gof, tests = duration_tests, coefficients = duration_coefs)
}

# =========================================================
# Part 2: danceability
# =========================================================

run_danceability <- function(songs_clean) {
  df_dance <- songs_clean %>%
    mutate(genre = as.factor(genre)) %>%
    drop_na(danceability, tempo, energy, instrumentalness, valence, loudness, year, genre, speechiness, acousticness)

  a1 <- lm(danceability ~ tempo + energy + instrumentalness + valence, data = df_dance)
  a2 <- lm(danceability ~ tempo + energy + instrumentalness + valence + loudness + year + genre + speechiness + acousticness, data = df_dance)
  a3 <- lm(danceability ~ tempo + I(tempo^2) + energy + instrumentalness + valence + I(valence^2) +
             loudness + year + genre + speechiness + acousticness, data = df_dance)
  a4 <- lm(danceability ~ tempo + I(tempo^2) + energy + instrumentalness + valence + I(valence^2) +
             loudness + year + genre + speechiness + acousticness + valence:genre, data = df_dance)
  a5 <- lm(danceability ~ tempo + I(tempo^2) + energy + instrumentalness + valence + I(valence^2) +
             year + genre + speechiness + acousticness + valence:genre, data = df_dance)

  dance_terms <- c("tempo", "I(tempo^2)", "energy", "instrumentalness", "valence", "I(valence^2)",
                   "loudness", "year", "speechiness", "acousticness")
  dance_coefs <- bind_rows(
    coef_table_lm(a1, "A1_base", dance_terms),
    coef_table_lm(a2, "A2_controls", dance_terms),
    coef_table_lm(a3, "A3_polynomial", dance_terms),
    coef_table_lm(a4, "A4_valence_genre_interaction", dance_terms),
    coef_table_lm(a5, "A5_no_loudness", dance_terms)
  )

  dance_gof <- bind_rows(lapply(list(
    A1_base = a1,
    A2_controls = a2,
    A3_polynomial = a3,
    A4_valence_genre_interaction = a4,
    A5_no_loudness = a5
  ), function(model) {
    tibble(n = nobs(model), r2 = summary(model)$r.squared, adj_r2 = summary(model)$adj.r.squared)
  }), .id = "model")

  dance_tests <- bind_rows(
    wald_lm(a3, terms = c("tempo", "I(tempo^2)"), label = "A3: tempo polynomial"),
    wald_lm(a2, pattern = "^genre", label = "A2: joint genre effects"),
    wald_lm(a4, pattern = "valence|valence:genre", label = "A4: valence and valence x genre")
  )

  dance_vif_with_loudness <- vif_numeric(df_dance, c("tempo", "energy", "instrumentalness", "valence", "loudness", "year", "speechiness", "acousticness"))
  dance_vif_without_loudness <- vif_numeric(df_dance, c("tempo", "energy", "instrumentalness", "valence", "year", "speechiness", "acousticness"))

  b_a3 <- coef(a3)
  valence_delta <- function(a, b) b_a3["valence"] * (b - a) + b_a3["I(valence^2)"] * (b^2 - a^2)
  dance_examples <- tibble(
    scenario = c("valence 0.30 -> 0.60", "energy +0.10", "acousticness +0.10"),
    predicted_change_danceability = c(
      valence_delta(0.30, 0.60),
      b_a3["energy"] * 0.10,
      b_a3["acousticness"] * 0.10
    )
  )

  write_vlad(dance_coefs, "danceability_coefficients.csv")
  write_vlad(dance_gof, "danceability_gof.csv")
  write_vlad(dance_tests, "danceability_tests.csv")
  write_vlad(dance_vif_with_loudness, "danceability_vif_with_loudness.csv")
  write_vlad(dance_vif_without_loudness, "danceability_vif_without_loudness.csv")
  write_vlad(dance_examples, "danceability_examples.csv")

  list(gof = dance_gof, tests = dance_tests, coefficients = dance_coefs)
}

# =========================================================
# Part 3: explicit
# =========================================================

run_explicit <- function(songs_clean) {
  cat("Explicit: preparing data...\n")
  df_explicit <- songs_clean %>%
    mutate(
      explicit_num = if_else(explicit == "Explicit", 1, 0),
      genre = as.factor(genre)
    ) %>%
    drop_na(explicit_num, energy, valence, danceability, year, genre)

  cat("Explicit: fitting Logit models...\n")
  logit_1 <- glm(explicit_num ~ energy + valence + danceability, data = df_explicit, family = binomial(link = "logit"))
  logit_2 <- glm(explicit_num ~ energy + valence + danceability + year + genre, data = df_explicit, family = binomial(link = "logit"))
  logit_3 <- glm(explicit_num ~ energy + valence + danceability * genre + year, data = df_explicit, family = binomial(link = "logit"))

  cat("Explicit: fitting Probit models...\n")
  probit_1 <- glm(explicit_num ~ energy + valence + danceability, data = df_explicit, family = binomial(link = "probit"))
  probit_2 <- glm(explicit_num ~ energy + valence + danceability + year + genre, data = df_explicit, family = binomial(link = "probit"))
  probit_3 <- glm(explicit_num ~ energy + valence + danceability * genre + year, data = df_explicit, family = binomial(link = "probit"))

  explicit_terms <- c("energy", "valence", "danceability", "year")
  cat("Explicit: building coefficient tables...\n")
  explicit_logit_coefs <- bind_rows(
    coef_table_glm_robust(logit_1, "E1_logit_base", explicit_terms),
    coef_table_glm_robust(logit_2, "E2_logit_controls", explicit_terms),
    coef_table_glm_robust(logit_3, "E3_logit_interaction", explicit_terms)
  )
  explicit_probit_coefs <- bind_rows(
    coef_table_glm_robust(probit_1, "E1_probit_base", explicit_terms),
    coef_table_glm_robust(probit_2, "E2_probit_controls", explicit_terms),
    coef_table_glm_robust(probit_3, "E3_probit_interaction", explicit_terms)
  )

  cat("Explicit: computing AME tables, exact = ", RUN_EXACT_EXPLICIT_AME, "...\n", sep = "")
  if (RUN_EXACT_EXPLICIT_AME) {
    rob_se_logit_1 <- robust_vcov_glm(logit_1)
    rob_se_logit_2 <- robust_vcov_glm(logit_2)
    rob_se_logit_3 <- robust_vcov_glm(logit_3)
    rob_se_probit_1 <- robust_vcov_glm(probit_1)
    rob_se_probit_2 <- robust_vcov_glm(probit_2)
    rob_se_probit_3 <- robust_vcov_glm(probit_3)

    explicit_logit_ames <- bind_rows(
      as_tibble(marginaleffects::avg_slopes(logit_1, vcov = rob_se_logit_1)) %>% mutate(model = "E1_logit_base"),
      as_tibble(marginaleffects::avg_slopes(logit_2, vcov = rob_se_logit_2)) %>% mutate(model = "E2_logit_controls"),
      as_tibble(marginaleffects::avg_slopes(logit_3, vcov = rob_se_logit_3, variables = explicit_terms)) %>% mutate(model = "E3_logit_interaction")
    ) %>% select(model, everything())

    explicit_probit_ames <- bind_rows(
      as_tibble(marginaleffects::avg_slopes(probit_1, vcov = rob_se_probit_1)) %>% mutate(model = "E1_probit_base"),
      as_tibble(marginaleffects::avg_slopes(probit_2, vcov = rob_se_probit_2)) %>% mutate(model = "E2_probit_controls"),
      as_tibble(marginaleffects::avg_slopes(probit_3, vcov = rob_se_probit_3, variables = explicit_terms)) %>% mutate(model = "E3_probit_interaction")
    ) %>% select(model, everything())
  } else {
    explicit_logit_ames <- bind_rows(
      glm_ame_fast(logit_1, explicit_terms, "E1_logit_base", "logit"),
      glm_ame_fast(logit_2, explicit_terms, "E2_logit_controls", "logit"),
      glm_ame_fast(logit_3, explicit_terms, "E3_logit_interaction", "logit")
    )

    explicit_probit_ames <- bind_rows(
      glm_ame_fast(probit_1, explicit_terms, "E1_probit_base", "probit"),
      glm_ame_fast(probit_2, explicit_terms, "E2_probit_controls", "probit"),
      glm_ame_fast(probit_3, explicit_terms, "E3_probit_interaction", "probit")
    )
  }

  cat("Explicit: computing Wald/LPM tests...\n")
  explicit_tests <- bind_rows(
    wald_lm(
      lm(explicit_num ~ energy + valence + danceability + year + genre, data = df_explicit),
      pattern = "^genre",
      label = "LPM proxy: joint genre effects"
    ),
    wald_lm(
      lm(explicit_num ~ energy + valence + danceability * genre + year, data = df_explicit),
      pattern = "danceability:genre",
      label = "LPM proxy: danceability x genre"
    )
  )

  explicit_distribution <- df_explicit %>%
    count(explicit_num) %>%
    mutate(label = if_else(explicit_num == 1, "Explicit", "Clean"), share = n / sum(n))

  cat("Explicit: building examples and by-genre AME...\n")
  ame_e2 <- explicit_logit_ames %>% filter(model == "E2_logit_controls")
  explicit_examples <- tibble(
    scenario = c("danceability +0.10", "energy +0.10", "valence +0.10"),
    approx_change_probability_pp = c(
      (ame_e2 %>% filter(term == "danceability") %>% pull(estimate)) * 0.10 * 100,
      (ame_e2 %>% filter(term == "energy") %>% pull(estimate)) * 0.10 * 100,
      (ame_e2 %>% filter(term == "valence") %>% pull(estimate)) * 0.10 * 100
    )
  )

  if (RUN_EXACT_EXPLICIT_AME) {
    logit_dance_by_genre <- as_tibble(
      marginaleffects::avg_slopes(logit_3, vcov = rob_se_logit_3, variables = "danceability", by = "genre")
    )
    probit_dance_by_genre <- as_tibble(
      marginaleffects::avg_slopes(probit_3, vcov = rob_se_probit_3, variables = "danceability", by = "genre")
    )
  } else {
    logit_dance_by_genre <- glm_ame_by_genre_fast(logit_3, "E3_logit_interaction", "logit")
    probit_dance_by_genre <- glm_ame_by_genre_fast(probit_3, "E3_probit_interaction", "probit")
  }

  write_vlad(explicit_logit_coefs, "explicit_logit_coefficients.csv")
  write_vlad(explicit_probit_coefs, "explicit_probit_coefficients.csv")
  write_vlad(explicit_logit_ames, "explicit_logit_ames.csv")
  write_vlad(explicit_probit_ames, "explicit_probit_ames.csv")
  write_vlad(logit_dance_by_genre, "explicit_logit_danceability_ame_by_genre.csv")
  write_vlad(probit_dance_by_genre, "explicit_probit_danceability_ame_by_genre.csv")
  write_vlad(explicit_tests, "explicit_tests.csv")
  write_vlad(explicit_distribution, "explicit_distribution.csv")
  write_vlad(explicit_examples, "explicit_examples.csv")

  cat("Explicit: outputs written.\n")
  list(
    distribution = explicit_distribution,
    tests = explicit_tests,
    logit_coefficients = explicit_logit_coefs,
    probit_coefficients = explicit_probit_coefs,
    logit_ames = explicit_logit_ames,
    probit_ames = explicit_probit_ames
  )
}

# =========================================================
# Part 4: popularity
# =========================================================

run_popularity <- function(songs_raw) {
  dataset_summary <- tibble(metric = c("rows_raw", "cols_raw"), value = c(nrow(songs_raw), ncol(songs_raw)))

  numeric_ranges <- songs_raw %>%
    summarise(
      popularity_min = min(popularity, na.rm = TRUE),
      popularity_max = max(popularity, na.rm = TRUE),
      year_min = min(year, na.rm = TRUE),
      year_max = max(year, na.rm = TRUE),
      duration_min_ms = min(duration_ms, na.rm = TRUE),
      duration_max_ms = max(duration_ms, na.rm = TRUE),
      followers_min = min(total_artist_followers, na.rm = TRUE),
      followers_max = max(total_artist_followers, na.rm = TRUE)
    )

  top_genres <- songs_raw %>% count(genre, sort = TRUE) %>% mutate(share = n / sum(n))
  explicit_counts <- songs_raw %>% count(explicit, sort = TRUE) %>% mutate(share = n / sum(n))

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
      decade = relevel(factor(floor(year / 10) * 10), ref = "2010"),
      explicit = factor(explicit),
      genre = relevel(factor(genre), ref = "Rock"),
      key = factor(key),
      mode = factor(mode)
    ) %>%
    drop_na(
      popularity, log_followers_c, explicit, duration_min, danceability, energy,
      loudness, speechiness, acousticness, instrumentalness, liveness, valence,
      tempo, genre, decade, artists
    )

  dataset_summary <- bind_rows(
    dataset_summary,
    tibble(
      metric = c("rows_regression_after_cleaning", "artist_clusters", "genres", "decades"),
      value = c(nrow(df), n_distinct(df$artists), nlevels(df$genre), nlevels(df$decade))
    )
  )

  base_audio <- paste(
    "duration_min + danceability + energy + loudness + speechiness + acousticness +",
    "instrumentalness + liveness + valence + tempo"
  )

  formulas <- list(
    M0 = as.formula("popularity ~ log_followers_c"),
    M1 = as.formula(paste("popularity ~ log_followers_c + explicit +", base_audio)),
    M2 = as.formula(paste("popularity ~ log_followers_c + explicit +", base_audio, "+ genre + decade")),
    M3 = as.formula(paste("popularity ~ log_followers_c + I(log_followers_c^2) + explicit +", base_audio, "+ genre + decade")),
    M4 = as.formula(paste("popularity ~ log_followers_c * explicit + I(log_followers_c^2) +", base_audio, "+ genre + decade")),
    M7_LOGY = as.formula(paste("log_popularity ~ log_followers_c + I(log_followers_c^2) + explicit +", base_audio, "+ genre + decade"))
  )

  models <- lapply(formulas, fit_ols_clustered, data = df, cluster_var = "artists")

  key_terms <- c(
    "(Intercept)", "log_followers_c", "I(log_followers_c^2)", "explicitExplicit",
    "duration_min", "danceability", "energy", "loudness", "speechiness",
    "acousticness", "instrumentalness", "liveness", "valence", "tempo",
    "log_followers_c:explicitExplicit"
  )

  coef_long <- bind_rows(lapply(names(models), function(model_name) coef_row_clustered(models[[model_name]], model_name, key_terms)))
  gof <- bind_rows(lapply(names(models), function(model_name) model_gof_clustered(models[[model_name]], model_name)))
  reg_table <- make_reg_table(models[c("M0", "M1", "M2", "M3", "M4")], key_terms)

  audio_terms <- c("duration_min", "danceability", "energy", "loudness", "speechiness",
                   "acousticness", "instrumentalness", "liveness", "valence", "tempo")

  walds <- bind_rows(
    wald_clustered(models$M3, terms = "I(log_followers_c^2)", label = "M3: nonlinear fan-base term"),
    wald_clustered(models$M4, pattern = "log_followers_c:explicit", label = "M4: interaction log_followers x explicit"),
    wald_clustered(models$M3, pattern = "^genre", label = "M3: all genre fixed effects"),
    wald_clustered(models$M3, pattern = "^decade", label = "M3: all decade fixed effects"),
    wald_clustered(models$M3, terms = audio_terms, label = "M3: all audio controls")
  )

  audio_control_tests <- bind_rows(
    wald_clustered_compare(models$M3, audio_terms, "All audio controls"),
    wald_clustered_compare(models$M3, c("danceability", "energy", "loudness", "valence", "tempo"), "Core sound controls"),
    wald_clustered_compare(models$M3, c("speechiness", "acousticness", "instrumentalness", "liveness"), "Texture/context controls"),
    bind_rows(lapply(audio_terms, function(term) wald_clustered_compare(models$M3, term, paste("Control:", term))))
  )

  se_comparison_m3 <- tibble(term = c("log_followers_c", "I(log_followers_c^2)", "explicitExplicit", audio_terms)) %>%
    filter(term %in% names(models$M3$beta)) %>%
    mutate(
      estimate = as.numeric(models$M3$beta[term]),
      conventional_se = as.numeric(models$M3$se_ols[term]),
      hc1_se = as.numeric(models$M3$se_hc[term]),
      cluster_se = as.numeric(models$M3$se_cl[term]),
      p_value_conventional = as.numeric(models$M3$p_ols[term]),
      p_value_cluster = as.numeric(models$M3$p_cl[term]),
      cluster_to_conventional_se = cluster_se / conventional_se
    )

  num_vars <- c("popularity", "log_followers", "duration_min", "danceability", "energy",
                "loudness", "speechiness", "acousticness", "instrumentalness", "liveness",
                "valence", "tempo")
  cor_matrix <- cor(df[, num_vars], use = "pairwise.complete.obs")
  cor_long <- as.data.frame(as.table(cor_matrix), stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    rename(var1 = Var1, var2 = Var2, correlation = Freq) %>%
    filter(var1 < var2) %>%
    arrange(desc(abs(correlation)))

  vif_vars <- c("log_followers", "duration_min", "danceability", "energy", "loudness",
                "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")
  vif_df <- vif_numeric(df, vif_vars)

  fit_robust <- function(data, label) {
    model <- fit_ols_clustered(formulas$M3, data = data, cluster_var = "artists")
    tibble(
      sample = label,
      n = model$n,
      clusters_artists = model$clusters,
      log_followers_est = unname(model$beta["log_followers_c"]),
      log_followers_se = unname(model$se_cl["log_followers_c"]),
      log_followers_sq_est = unname(model$beta["I(log_followers_c^2)"]),
      log_followers_sq_se = unname(model$se_cl["I(log_followers_c^2)"]),
      r2 = model$r2
    )
  }

  top_001 <- quantile(df$total_artist_followers, probs = 0.999, na.rm = TRUE)
  robustness <- bind_rows(
    fit_robust(df, "full M3 sample"),
    fit_robust(df %>% filter(popularity > 0), "without popularity = 0"),
    fit_robust(df %>% filter(year >= 2000), "year >= 2000"),
    fit_robust(df %>% filter(total_artist_followers < top_001), "without top 0.1% followers")
  )

  marginal_effects <- tibble(
    point = c("p10 log followers", "median log followers", "p90 log followers"),
    log_followers_c = as.numeric(quantile(df$log_followers_c, probs = c(0.10, 0.50, 0.90), na.rm = TRUE))
  ) %>%
    mutate(
      m3_marginal_effect = unname(models$M3$beta["log_followers_c"]) +
        2 * unname(models$M3$beta["I(log_followers_c^2)"]) * log_followers_c
    )

  logy_effects <- marginal_effects %>%
    transmute(
      point,
      log_followers_c,
      logy_marginal_effect = unname(models$M7_LOGY$beta["log_followers_c"]) +
        2 * unname(models$M7_LOGY$beta["I(log_followers_c^2)"]) * log_followers_c,
      pct_change_1plus_popularity_for_10pct_more_followers =
        100 * (exp(logy_marginal_effect * log(1.10)) - 1)
    )

  logy_summary <- coef_row_clustered(models$M7_LOGY, "M7_LOGY", c("log_followers_c", "I(log_followers_c^2)", "explicitExplicit")) %>%
    mutate(dependent_variable = "log(1 + popularity)") %>%
    select(model, dependent_variable, everything())

  category_baselines <- tibble(
    variable = c("genre", "decade"),
    baseline = c(levels(df$genre)[1], levels(df$decade)[1]),
    reason = c("largest genre in the regression sample and easier interpretation", "recent large decade and easier interpretation")
  )

  genre_terms <- grep("^genre", names(models$M3$beta), value = TRUE)
  genre_effects <- bind_rows(
    tibble(term = "(base)", estimate = 0, cluster_se = NA_real_, t_value = NA_real_, p_value = NA_real_, genre = levels(df$genre)[1]),
    coef_row_clustered(models$M3, "M3", genre_terms) %>%
      mutate(genre = sub("^genre", "", term)) %>%
      select(term, estimate, cluster_se, t_value, p_value, genre)
  )

  plot_df <- df %>% sample_n(min(nrow(df), 25000))
  p1 <- ggplot(plot_df, aes(x = log_followers, y = popularity)) +
    geom_point(alpha = 0.08, size = 0.6, color = "#2C3E50") +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#C0392B", fill = "#F5B7B1") +
    labs(title = "Popularity vs log(1 + total_artist_followers)", x = "log(1 + total_artist_followers)", y = "Popularity") +
    theme_minimal()
  ggsave(file.path(POP_OUT, "fig_popularity_log_followers.png"), p1, width = 8, height = 5, dpi = 180)

  diag_df <- tibble(fitted = models$M3$fitted, residuals = models$M3$residuals) %>% sample_n(min(nrow(.), 25000))
  p2 <- ggplot(diag_df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.08, size = 0.6, color = "#34495E") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#C0392B") +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#2980B9") +
    labs(title = "M3 diagnostics: residuals vs fitted values", x = "Fitted values", y = "Residuals") +
    theme_minimal()
  ggsave(file.path(POP_OUT, "fig_m3_residuals_fitted.png"), p2, width = 8, height = 5, dpi = 180)

  p3 <- ggplot(diag_df, aes(x = residuals)) +
    geom_histogram(bins = 80, fill = "#7DCEA0", color = "white") +
    labs(title = "M3 residual distribution", x = "Residuals", y = "Count") +
    theme_minimal()
  ggsave(file.path(POP_OUT, "fig_m3_residual_hist.png"), p3, width = 8, height = 5, dpi = 180)

  p4 <- ggplot(vif_df, aes(x = reorder(variable, vif), y = vif)) +
    geom_col(fill = "#5DADE2") +
    coord_flip() +
    labs(title = "VIF for numeric regressors", x = NULL, y = "VIF") +
    theme_minimal()
  ggsave(file.path(POP_OUT, "fig_vif_numeric.png"), p4, width = 8, height = 5, dpi = 180)

  write_pop(dataset_summary, "dataset_summary.csv")
  write_pop(numeric_ranges, "numeric_ranges.csv")
  write_pop(top_genres, "top_genres.csv")
  write_pop(explicit_counts, "explicit_counts.csv")
  write_pop(coef_long, "model_coefficients_long.csv")
  write_pop(gof, "model_gof.csv")
  write_pop(reg_table, "regression_table_main.csv")
  write_pop(walds, "wald_tests.csv")
  write_pop(audio_control_tests, "audio_control_tests.csv")
  write_pop(se_comparison_m3, "se_comparison_m3.csv")
  write_pop(cor_long, "correlations_long.csv")
  write_pop(vif_df, "vif_numeric.csv")
  write_pop(robustness, "robustness_m3.csv")
  write_pop(marginal_effects, "marginal_effects_m3.csv")
  write_pop(logy_summary, "log_popularity_model.csv")
  write_pop(logy_effects, "log_popularity_marginal_effects.csv")
  write_pop(category_baselines, "category_baselines.csv")
  write_pop(genre_effects, "genre_effects_m3.csv")

  list(
    gof = gof,
    walds = walds,
    audio_tests = audio_control_tests,
    regression_table = reg_table,
    coefficients = coef_long,
    regression_sample = nrow(df)
  )
}

run_all <- function() {
  songs_clean <- readRDS(DATA_PATH)

  cat("Running Lab 3 combined script...\n")
  cat("Data:", DATA_PATH, "\n")

  duration_result <- run_duration(songs_clean)
  cat("Duration part completed.\n")

  dance_result <- run_danceability(songs_clean)
  cat("Danceability part completed.\n")

  explicit_result <- run_explicit(songs_clean)
  cat("Explicit part completed.\n")

  popularity_result <- run_popularity(songs_clean)
  cat("Popularity part completed.\n")

  print_coef_table(
    "Duration regression coefficients, HC1 SE",
    duration_result$coefficients %>%
      select(model, term, estimate, robust_se, statistic, p_value),
    se_col = "robust_se"
  )
  print_table("Duration model quality", duration_result$gof)
  print_table("Duration tests", duration_result$tests %>%
    transmute(test, q, statistic = fmt_num(statistic, 3), p_value = fmt_p(p_value)))

  print_coef_table(
    "Danceability regression coefficients, HC1 SE",
    dance_result$coefficients %>%
      select(model, term, estimate, robust_se, statistic, p_value),
    se_col = "robust_se"
  )
  print_table("Danceability model quality", dance_result$gof)
  print_table("Danceability tests", dance_result$tests %>%
    transmute(test, q, statistic = fmt_num(statistic, 3), p_value = fmt_p(p_value)))

  print_coef_table(
    "Explicit Logit coefficients",
    explicit_result$logit_coefficients %>%
      select(model, term, estimate, std_error, statistic, p_value),
    se_col = "std_error"
  )
  print_coef_table(
    "Explicit Probit coefficients",
    explicit_result$probit_coefficients %>%
      select(model, term, estimate, std_error, statistic, p_value),
    se_col = "std_error"
  )
  print_coef_table(
    "Explicit Logit AME",
    explicit_result$logit_ames %>%
      select(any_of(c("model", "term", "estimate", "std_error", "statistic", "p_value", "note"))),
    se_col = "std_error"
  )
  print_table("Explicit tests", explicit_result$tests %>%
    transmute(test, q, statistic = fmt_num(statistic, 3), p_value = fmt_p(p_value)))

  print_table(
    "Popularity main regression table, coefficients with clustered SE in parentheses",
    popularity_result$regression_table %>%
      select(label, M0, M1, M2, M3, M4) %>%
      rename(Term = label)
  )
  print_table("Popularity Wald tests", popularity_result$walds %>%
    transmute(test, q, statistic = fmt_num(statistic, 3), p_value = fmt_p(p_value)))
  print_table("Popularity model quality", popularity_result$gof)

  cat("\nDone.\n")
  cat("Vlad output tables:", VLAD_OUT, "\n")
  cat("Popularity output tables and figures:", POP_OUT, "\n")
}

run_all()
