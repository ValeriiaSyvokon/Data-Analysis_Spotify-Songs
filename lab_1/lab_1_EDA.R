library(dplyr)
library(ggplot2)
library(hexbin)
library(scales)
library(tidyr)
library(ggridges)
library(fmsb)
library(readr)
library(corrplot)
library(reshape2)
library(tidyverse)
library(forcats)
library(ggcorrplot)
library(tidyverse)

# 1. Завантаження даних
songs_df_clean <- readRDS("songs_clean.rds")

#Розподіл Speechiness (з логарифмічною шкалою Y)
ggplot(songs_df_clean, aes(x = speechiness)) +
  geom_histogram(bins = 60, fill = "#9B59B6", color = "black", alpha = 0.8) +
  
  scale_y_log10(labels = scales::comma_format()) + 
  
  #офіційні пороги Spotify
  geom_vline(xintercept = 0.33, linetype = "dashed", color = "#E45756", linewidth = 1) +
  geom_vline(xintercept = 0.66, linetype = "dashed", color = "#E45756", linewidth = 1) +
  
  annotate("text", x = 0.15, y = 100000, label = "Звичайна музика", size = 4) +
  annotate("text", x = 0.5, y = 100000, label = "Музика з великою кількістю\nтексту (переважно реп)", size = 4) +
  annotate("text", x = 0.85, y = 100000, label = "Подкасти / Вірші", size = 4) +
  
  labs(
    title = "Розподіл розмовної мови (Speechiness)",
    x = "Speechiness (0.0 - Музика, 1.0 - Розмовна мова)",
    y = "Кількість пісень (Log-шкала)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))



#Розподіл Acousticness, Instrumentalness, Liveness (з логарифмічною шкалою Y)
skewed_acoustics <- songs_df_clean %>%
  select(acousticness, instrumentalness, liveness) %>%
  drop_na() %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = str_to_title(Metric))

ggplot(skewed_acoustics, aes(x = Value, fill = Metric)) +
  geom_histogram(bins = 40, color = "black", alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
  
  scale_y_log10(labels = scales::comma_format()) +
  
  scale_fill_viridis_d(option = "mako") +
  labs(
    title = "Розподіл специфічних акустичних характеристик",
    subtitle = "Вісь Y логарифмована",
    x = "Значення (0.0 - 1.0)",
    y = "Кількість пісень (Log10 шкала)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14))



#Розподіл Valence
p_valence <- songs_df_clean %>%
  filter(!is.na(valence)) %>%
  ggplot(aes(x = valence)) +
  
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#DDA0DD", color = "black", alpha = 0.8) +
  
  geom_density(color = "#8B008B", linewidth = 1.2) +
  
  labs(
    title = "Розподіл емоційної окраси пісень (Valence)",
    x = "Valence (0.0 - Сумно, 1.0 - Ейфорійно)",
    y = "Щільність розподілу"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#555555")
  )

print(p_valence)



#Розподіл Темпу (Tempo)
p_tempo <- songs_df_clean %>%
  ggplot(aes(x = tempo)) +
  
  geom_histogram(binwidth = 5, fill = "#F39C12", color = "black", alpha = 0.85) +
  
  #маркери для найпопулярніших темпів
  geom_vline(xintercept = 120, linetype = "dashed", color = "#2C3E50", linewidth = 1) +
  geom_vline(xintercept = 95, linetype = "dashed", color = "#2C3E50", linewidth = 1) +
  geom_vline(xintercept = 140, linetype = "dashed", color = "#2C3E50", linewidth = 1) +
  
  geom_vline(aes(xintercept = median(tempo, na.rm = TRUE), color = "Медіана"), 
             linetype = "dashed", linewidth = 1.2) +
  
  geom_vline(aes(xintercept = mean(tempo, na.rm = TRUE), color = "Середнє"), 
             linetype = "dashed", linewidth = 1.2) +
  
  annotate("text", x = 119, y = 30000, label = "Поп (120 BPM)", angle = 90, vjust = -1) +
  annotate("text", x = 94, y = 30000, label = "Хіп-Хоп (95 BPM)", angle = 90, vjust = -1) +
  annotate("text", x = 150, y = 30000, label = "Дабстеп (140 BPM)", angle = 90, vjust = -1) +
  
  scale_x_continuous(breaks = seq(40, 240, by = 20)) +
  
  scale_color_manual(
    name = "Статистичні показники", 
    values = c("Медіана" = "#E45756", "Середнє" = "#30a20b")
  ) +
  
  labs(
    title = "Розподіл темпу музики (BPM)",
    subtitle = "Мультимодальний розподіл: піки відповідають стандартам популярних жанрів",
    x = "Темп (Ударів на хвилину / BPM)",
    y = "Кількість пісень"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top" 
  )

print(p_tempo)



#Розподіл Тривалості пісень (До 10 хвилин)
p_duration <- songs_df_clean %>%
  mutate(duration_min = duration_ms / 60000) %>%
  ggplot(aes(x = duration_min)) +
  
  #binwidth = 0.5 #означає, що кожен стовпчик це 30 секунд
geom_histogram(binwidth = 0.5, fill = "#fc9ef1", color = "black", alpha = 0.8) +
  
  geom_vline(aes(xintercept = median(duration_min, na.rm = TRUE), color = "Медіана"), 
             linetype = "dashed", linewidth = 1.2) +
  
  geom_vline(aes(xintercept = mean(duration_min, na.rm = TRUE), color = "Середнє"), 
             linetype = "dashed", linewidth = 1.2) +
  
  coord_cartesian(xlim = c(0, 10)) +
  scale_x_continuous(breaks = 0:10) +
  
  labs(
    title = "Розподіл тривалості пісень",
    subtitle = "Графік наближено до 10 хвилин (відкинуто екстремальні викиди)",
    x = "Тривалість (у хвилинах)",
    y = "Кількість пісень"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p_duration)



#Розподіл року випуску
p_year <- songs_df_clean %>%
  filter(!is.na(year), year > 1900, year <= 2023) %>%
  
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  
  geom_col(fill = "#2980B9", color = "black", alpha = 0.8) +
  
  scale_y_log10(labels = scales::comma_format()) +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  
  labs(
    title = "Історичний розподіл музики на Spotify",
    subtitle = "Логарифмічна шкала (Y) демонструє експоненційне зростання каталогу",
    x = "Рік випуску",
    y = "Кількість пісень (Log-шкала)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_year)


#Розподіл total_artist_followers
unique_artists <- songs_df_clean %>%
  distinct(artists, .keep_all = TRUE)
total_artist_followers_distribution <- ggplot(data = unique_artists, aes(x = pmax(total_artist_followers, 1))) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.8) +
  
  geom_vline(aes(xintercept = median(total_artist_followers, na.rm = TRUE), color = "Медіана"), 
             linetype = "dashed", linewidth = 1.2) +
  
  geom_vline(aes(xintercept = mean(total_artist_followers, na.rm = TRUE), color = "Середнє"), 
             linetype = "dashed", linewidth = 1.2) +
  
  labs(
    title = "Розподіл загальної кількості підписників артистів",
    x = "Загальна кількість підписників (логарифмічна шкала)",
    y = "Кількість унікальних артистів"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  scale_x_log10(
    breaks = 10^(0:10),             
    labels = scales::comma          
  ) +
  scale_color_manual(name = "Показники", values = c("Медіана" = "#E45756", "Середнє" = "#30a20b"))

print(total_artist_followers_distribution)




#Розподіл Середньої популярності артистів
p_avg_pop <- songs_df_clean %>%
  filter(!is.na(avg_artist_popularity)) %>%
  ggplot(aes(x = avg_artist_popularity)) +
  
  geom_histogram(aes(y = after_stat(density)), bins = 35, fill = "#d08cd6", color = "black", alpha = 0.8) +
  
  geom_vline(aes(xintercept = median(avg_artist_popularity, na.rm = TRUE), color = "Медіана"), 
             linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = mean(avg_artist_popularity, na.rm = TRUE), color = "Середнє"), 
             linetype = "dashed", linewidth = 1.2) +
  
  labs(
    title = "Розподіл середньої популярності артистів",
    x = "Середня популярність (0 - 100)",
    y = "Щільність розподілу"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p_avg_pop)



#Розподіл популярності (без нулів)
zero_pct <- songs_df_clean %>%
  filter(!is.na(popularity)) %>%
  summarise(pct = mean(popularity == 0)) %>%
  pull(pct)

zero_text <- scales::percent(zero_pct, accuracy = 0.1)

p_pop_filtered <- songs_df_clean %>%
  filter(popularity > 0) %>%
  ggplot(aes(x = popularity)) +
  
  geom_histogram(binwidth = 2, fill = "#3498DB", color = "black", alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  
  labs(
    title = "Розподіл популярності треків",
    subtitle = paste0("Важливо: ", zero_text, " усіх треків у датасеті мають популярність 0 і виключені з графіка."),
    x = "Популярність (1 - 100)",
    y = "Кількість пісень"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#C0392B", face = "italic", size = 11) 
  )

print(p_pop_filtered)




#Розподіл Danceability
p_dance <- songs_df_clean %>%
  filter(!is.na(danceability)) %>%
  ggplot(aes(x = danceability)) +
  
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "#5DADE2", color = "black", alpha = 0.7) +
  
  geom_density(color = "#1B4F72", linewidth = 1.2) +
  
  #маркер медіани і середнього
  geom_vline(aes(xintercept = median(danceability, na.rm = TRUE)), 
             linetype = "dashed", color = "#C0392B", linewidth = 1.2) +
  
  geom_vline(aes(xintercept = mean(danceability, na.rm = TRUE)), 
             linetype = "dashed", color = "#30a20b", linewidth = 1.2) +
  
  labs(
    title = "Розподіл танцювальності пісень (Danceability)",
    subtitle = "Нормальний розподіл. Червоний пунктир вказує на медіану, зелений - на середнє",
    x = "Танцювальність (0.0 - Складно танцювати, 1.0 - Легко танцювати)",
    y = "Щільність розподілу"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#555555")
  )

print(p_dance)



#Розподіл енергійності (Energy)
p_energy <- songs_df_clean %>%
  filter(!is.na(energy)) %>%
  ggplot(aes(x = energy)) +
  
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "#F39C12", color = "black", alpha = 0.7) +
  
  geom_density(color = "#D35400", linewidth = 1.2) +
  
  #маркер медіани і середнього
  geom_vline(aes(xintercept = median(energy, na.rm = TRUE)), 
             linetype = "dashed", color = "#2C3E50", linewidth = 1.2) +
  
  geom_vline(aes(xintercept = mean(energy, na.rm = TRUE)), 
             linetype = "dashed", color = "#30a20b", linewidth = 1.2) +
  
  labs(
    title = "Розподіл енергійності пісень (Energy)",
    subtitle = "Лівостороння асиметрія. Позначено медіану сірим та середнє зеленим.",
    x = "Енергійність (0.0 - Спокійно, 1.0 - Інтенсивно)",
    y = "Щільність розподілу"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#555555")
  )

print(p_energy)



#Розподіл тональностей (Key)
p_key <- songs_df_clean %>%
  filter(!is.na(key)) %>%
  ggplot(aes(x = key)) +
  
  geom_bar(fill = "#8E44AD", color = "black", alpha = 0.8) +
  
  labs(
    title = "Розподіл музичних тональностей пісень (Key)",
    subtitle = "Найпопулярнішими є зручні для гри G (Соль), C (До) та D (Ре)",
    x = "Тональність (Музична нота)",
    y = "Кількість пісень"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#555555"),
    panel.grid.major.x = element_blank() 
  )

print(p_key)




#Розподіл гучності (Loudness)
p_loudness <- songs_df_clean %>%
  filter(!is.na(loudness)) %>%
  ggplot(aes(x = loudness)) +
  
  geom_histogram(binwidth = 1, fill = "#4C78A8", color = "black", alpha = 0.85) +
  coord_cartesian(xlim = c(-25, 0)) +
  scale_x_continuous(breaks = seq(-25, 0, by = 5)) +
  
  geom_vline(aes(xintercept = median(loudness, na.rm = TRUE)), 
             linetype = "dashed", color = "#E45756", linewidth = 1.2) +
  
  geom_vline(aes(xintercept = mean(loudness, na.rm = TRUE)), 
             linetype = "dashed", color = "#30a20b", linewidth = 1.2) +
  
  labs(
    title = "Розподіл гучності треків (Loudness)",
    subtitle = "Графік наближено до діапазону [-25, 0] dB. Червоний пунктир — медіана; зелений — середнє",
    x = "Гучність (dB)",
    y = "Кількість пісень"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#555555")
  )

print(p_loudness)



#Розподіл за ладом (Mode)
p_mode <- songs_df_clean %>%
  filter(!is.na(mode)) %>%
  ggplot(aes(x = mode, fill = mode)) +
  geom_bar(color = "black", alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = c("Major" = "#F1C40F", "Minor" = "#8E44AD")) +
  
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 4) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), labels = scales::comma) +
  labs(
    title = "Розподіл пісень за музичним ладом (Mode)",
    x = "Музичний лад",
    y = "Кількість пісень"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p_mode)



#Нецензурна лексика (Explicit)
p_explicit <- songs_df_clean %>%
  filter(!is.na(explicit)) %>%
  ggplot(aes(x = explicit, fill = explicit)) +
  geom_bar(color = "black", alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = c("Clean" = "#2ECC71", "Explicit" = "#E74C3C")) + 
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), labels = scales::comma) +
  labs(
    title = "Наявність нецензурної лексики (Explicit Content)",
    y = "Кількість пісень"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p_explicit)




#Розподіл пісень за основними жанрами
p_genre <- songs_df_clean %>%
  filter(!is.na(genre)) %>%
  count(genre) %>%
  
  ggplot(aes(x = reorder(genre, n), y = n)) +
  geom_col(fill = "#9B59B6", color = "black", alpha = 0.8) +
  
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 4) +
  
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = scales::comma) +
  
  labs(
    title = "Представленість музичних жанрів у датасеті",
    x = "Узагальнений жанр (Genre)",
    y = "Кількість пісень"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 11, face = "bold")
  )

print(p_genre)




#Топ-15 нішевих жанрів за explicit-контентом
top_explicit_niches <- songs_df_clean %>%
  filter(!is.na(niche_genres), !is.na(explicit), !is.na(genre)) %>%
  
  group_by(niche_genres, genre) %>%
  summarise(
    total_tracks = n(),
    explicit_count = sum(explicit == "Explicit"), 
    explicit_pct = explicit_count / total_tracks,
    .groups = 'drop'
  ) %>%
  filter(total_tracks >= 50) %>%
  arrange(desc(explicit_pct)) %>%
  slice_head(n = 15) %>%
  
  mutate(label_full = paste0(niche_genres, " (", genre, ")"))

labels_order <- top_explicit_niches %>% pull(label_full)

niches_to_keep <- top_explicit_niches %>% pull(niche_genres)

p_top_explicit <- songs_df_clean %>%
  filter(niche_genres %in% niches_to_keep) %>%
  
  mutate(label_full = paste0(niche_genres, " (", genre, ")")) %>%
  
  mutate(label_full = factor(label_full, levels = rev(labels_order))) %>%
  
  filter(!is.na(label_full)) %>%
  
  ggplot(aes(y = label_full, fill = explicit)) +
  geom_bar(position = "fill", color = "black", alpha = 0.8) +
  
  scale_x_continuous(labels = scales::percent_format()) +
  
  scale_fill_manual(
    values = c("Clean" = "#4C78A8", "Explicit" = "#E45756"),
    name = "Контент"
  ) +
  
  labs(
    title = "Топ-15 нішевих жанрів за найвищою часткою Explicit-треків",
    subtitle = "Враховано лише репрезентативні жанри (мінімум 50 треків у каталозі)",
    x = "Відсоток треків",
    y = "Нішевий жанр (Загальний напрямок)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 11),
    panel.grid.major.y = element_blank() 
  )

print(p_top_explicit)

#Нішеві жанри: Танцювальність vs Енергійність
top_niches_data <- songs_df_clean %>%
  filter(!is.na(niche_genres)) %>%
  group_by(niche_genres) %>%
  summarise(
    count = n(),
    mean_dance = mean(danceability, na.rm = TRUE),
    mean_energy = mean(energy, na.rm = TRUE),
    mean_pop = mean(popularity, na.rm = TRUE),
    main_genre = as.character(genre[which.max(table(genre))])
  ) %>%
  filter(count > 500) %>%
  slice_max(order_by = count, n = 15) %>%
  mutate(label_text = paste0(niche_genres, "\n(", main_genre, ")"))


p1 <- ggplot(top_niches_data, aes(x = mean_dance, y = mean_energy)) +
  
  geom_point(aes(color = mean_pop), size = 3, alpha = 0.8) +
  scale_color_gradient(low = "#00ffd0", high = "#ef0004") +
  
  geom_text(aes(label = label_text), vjust = -0.5, size = 3, color = "black", show.legend = FALSE) +
  
  labs(
    title = "Танцювальність та Енергійність: Топ-15 специфічних жанрів",
    subtitle = "У дужках вказано загальний макро-жанр",
    x = "Середня танцювальність (Danceability)",
    y = "Середня енергійність (Energy)",
    color = "Сер. Популярність"
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.15))) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
  )

print(p1)


#Відсоток Explicit-контенту
#знаходимо топ-15 жанрів за кількістю пісень
top_15_niches <- songs_df_clean %>%
  filter(!is.na(niche_genres)) %>%
  count(niche_genres, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(niche_genres)

p2 <- songs_df_clean %>%
  filter(niche_genres %in% top_15_niches, !is.na(explicit)) %>%
  
  mutate(niche_genres = fct_reorder(niche_genres, explicit == "Explicit", .fun = mean)) %>%
  
  ggplot(aes(x = niche_genres, fill = explicit)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  scale_fill_manual(values = c("Clean" = "#4C78A8", "Explicit" = "#E45756")) +
  labs(
    title = "Частка Explicit-треків у наймасовіших нішевих жанрах",
    x = "Нішевий жанр",
    y = "Відсоток треків",
    fill = "Контент"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
  )

print(p2)

#Розподіл популярності всередині топ-10 альбомів
#беремо альбоми, де є хоча б 8 треків, і обираємо 10 найпопулярніших
top_albums_data <- songs_df_clean %>%
  filter(!is.na(album_name)) %>%
  group_by(album_name) %>%
  mutate(track_count = n(), avg_album_pop = mean(popularity, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(track_count >= 8) %>%
  filter(dense_rank(desc(avg_album_pop)) <= 10) 

p3 <- ggplot(top_albums_data, aes(x = reorder(album_name, popularity, FUN = median), y = popularity)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey", alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.6, color = "#2CA02C", size = 2) +
  coord_flip() +
  labs(
    title = "Розподіл популярності окремих пісень усередині Топ-10 альбомів",
    x = "Назва альбому",
    y = "Популярність треку"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
  )

print(p3)

#Синдром "Одного хіта"
#шукаємо альбоми з найбільшим розривом між хітом і рештою пісень
one_hit_albums_data <- songs_df_clean %>%
  filter(!is.na(album_name)) %>%
  group_by(album_name) %>%
  summarise(
    track_count = n(),
    max_pop = max(popularity, na.rm = TRUE),    #популярність найвідомішого треку
    median_pop = median(popularity, na.rm = TRUE), #популярність "типового" треку в альбомі
    pop_gap = max_pop - median_pop,             #різниця (головний критерій)
    .groups = 'drop'
  ) %>%
  filter(track_count >= 8) %>%
  filter(max_pop >= 75) %>%
  arrange(desc(pop_gap)) %>%
  slice_head(n = 10) %>%
  pull(album_name)

p4 <- songs_df_clean %>%
  filter(album_name %in% one_hit_albums_data) %>%
  mutate(album_name = factor(album_name, levels = rev(one_hit_albums_data))) %>%
  
  ggplot(aes(x = album_name, y = popularity)) +
  geom_boxplot(outlier.shape = NA, fill = "#f0f0f0", color = "#333333", alpha = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.7, color = "#E45756", size = 2.5) +
  coord_flip() +
  labs(
    title = "Синдром «Одного Хіта»",
    subtitle = "Альбоми з найбільшим розривом між супер-хітом та рештою треків",
    x = "Назва альбому",
    y = "Популярність треку"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10)
  )

print(p4)


#Співвідношення бестселерів та альбомів одного хіта
#Класифікація альбомів
album_classification <- songs_df_clean %>%
  filter(!is.na(album_name)) %>%
  group_by(album_name) %>%
  summarise(
    track_count = n(),
    max_pop = max(popularity, na.rm = TRUE),    #Найпопулярніша пісня
    median_pop = median(popularity, na.rm = TRUE), #"Типова" пісня
    .groups = 'drop'
  ) %>%
  #Беремо повноцінні альбоми, де є хоча б один хіт
  filter(track_count >= 6, max_pop >= 75) %>% 
  
  #Категорії
mutate(
  album_type = case_when(
    median_pop >= 55 ~ "Альбоми-бестселери\n(більшість пісень популярні)",
    median_pop < 30 ~ "Один хіт\n(решта альбому невідома)",
    TRUE ~ "Середина\n(хіт + кілька відомих пісень)"
  )
)


ratio_data <- album_classification %>%
  count(album_type) %>%
  mutate(
    percentage = n / sum(n),
    label_text = paste0(scales::percent(percentage, accuracy = 1), " (", n, ")")
  )

p5 <- ggplot(ratio_data, aes(x = reorder(album_type, percentage), y = percentage, fill = album_type)) +
  geom_col(color = "white", alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = label_text), hjust = -0.1, size = 4, fontface = "bold", color = "#333333") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = c(
    "Альбоми-бестселери\n(більшість пісень популярні)" = "#2CA02C",
    "Середина\n(хіт + кілька відомих пісень)" = "#FF7F0E",
    "Один хіт\n(решта альбому невідома)" = "#E45756"
  )) +
  labs(
    title = "Альбоми-бестселери VS. Один хіт",
    subtitle = "Аналіз альбомів, які мають хоча б одну пісню з популярністю > 75",
    x = "Тип альбому",
    y = "Частка від загальної кількості успішних альбомів"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 11, face = "bold")
  )

print(p5)


#Розподіл Valence (Щастя) залежно від Ладу (Major/Minor)
p6_valence_mode <- songs_df_clean %>%
  filter(!is.na(mode), !is.na(valence)) %>%
  ggplot(aes(x = valence, fill = mode)) +
  geom_density(alpha = 0.6, color = "white", linewidth = 0.5) +
  scale_fill_manual(values = c("Minor" = "#5D6D7E", "Major" = "#F4D03F")) +
  labs(
    title = "Мінор сумніший за Мажор",
    subtitle = "Розподіл показника Valence (позитивності) за музичним ладом",
    x = "Валентність (0 = Сумно, 1 = Ейфорійно)",
    y = "Щільність розподілу",
    fill = "Музичний лад"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p6_valence_mode)


#Еволюція аудіо-характеристик з часом
p7 <- songs_df_clean %>%
  filter(year >= 1960, year <= 2023) %>%
  group_by(year) %>%
  summarise(
    avg_acousticness = mean(acousticness, na.rm = TRUE),
    #Нормалізування гучністі від 0 до 1
    avg_loudness_norm = (mean(loudness, na.rm = TRUE) - min(loudness, na.rm = TRUE)) / 
      (max(loudness, na.rm = TRUE) - min(loudness, na.rm = TRUE))
  ) %>%
  pivot_longer(cols = c(avg_acousticness, avg_loudness_norm), 
               names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = year, y = Value, color = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(
    values = c("avg_acousticness" = "#8E44AD", "avg_loudness_norm" = "#E67E22"),
    labels = c("Акустичність", "Гучність (Нормалізована)")
  ) +
  labs(
    title = "Еволюція музики: 1960 - 2023",
    subtitle = "Занепад живої акустики та зростання загальної гучності",
    x = "Рік випуску",
    y = "Середнє значення (0-1)",
    color = "Показник"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p7)





#Розподіл популярності нішевих жанрів (Classical) з кількістю треків
classical_data <- songs_df_clean %>%
  filter(genre == "Classical", !is.na(niche_genres), !is.na(popularity)) %>%
  group_by(niche_genres) %>%
  mutate(n_tracks = n()) %>%
  ungroup() %>%
  filter(n_tracks >= 20) %>%
  mutate(niche_genres = fct_reorder(niche_genres, popularity, .fun = median))

labels_data <- classical_data %>%
  group_by(niche_genres) %>%
  summarise(n_tracks = first(n_tracks))

p_classical_niches <- ggplot(classical_data, aes(x = niche_genres, y = popularity)) +
  
  geom_boxplot(aes(fill = niche_genres), color = "black", alpha = 0.8, outlier.alpha = 0.4, outlier.size = 1.5) +
  
  geom_text(
    data = labels_data, 
    aes(x = niche_genres, y = 90, label = paste0("n=", n_tracks)), 
    hjust = 0, size = 3.5, fontface = "italic", color = "black"
  ) +
  
  coord_flip() +
  
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 80, by = 20)) +
  
  scale_fill_viridis_d(option = "mako", guide = "none") +
  
  labs(
    title = "Розподіл популярності нішевих жанрів (Classical)",
    subtitle = "Відсортовано за медіаною. Справа вказано розмір вибірки (n) для кожної ніші",
    x = "Нішевий жанр",
    y = "Популярність (0 - 100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_classical_niches)





#Повна матриця кореляцій
vars <- c(
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "tempo",
  "duration_ms",
  "year",
  "popularity",
  "total_artist_followers",
  "avg_artist_popularity"
)

df_corr <- songs_df_clean %>%
  select(all_of(vars))

corr_matrix <- cor(df_corr, method = "spearman", use = "complete.obs")

corr_plot <- corr_matrix
corr_plot[upper.tri(corr_plot, diag = TRUE)] <- NA

corr_df <- reshape2::melt(corr_plot, na.rm = TRUE)
colnames(corr_df) <- c("Var1", "Var2", "Correlation")

corr_df$Var1 <- factor(corr_df$Var1, levels = rev(vars))
corr_df$Var2 <- factor(corr_df$Var2, levels = vars)

# підписи на протилежній діагоналі
diag_labels <- data.frame(
  Var2 = factor(vars, levels = vars),
  Var1 = factor(vars, levels = rev(vars)),
  label = vars
)

p <- ggplot(corr_df, aes(x = Var2, y = Var1, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3.8) +
  geom_text(
    data = diag_labels,
    aes(x = Var2, y = Var1, label = label),
    inherit.aes = FALSE,
    hjust = 0.1,
    vjust = 0.8,
    size = 4.5
  ) +
  scale_fill_gradient2(
    low = "#3B4CC0",
    mid = "white",
    high = "#B40426",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Кореляція Спірмена"
  ) +
  coord_fixed(clip = "off") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(20, 40, 20, 20)
  )

print(p)

ggsave(
  "spearman_triangular_corrplot_opposite_diagonal.png",
  plot = p,
  width = 12,
  height = 9,
  dpi = 300,
  bg = "white"
)
###################################
#1)	Як розподіляється популярність треків у різних жанрах?
###################################

popularity_by_genre_boxplot <- songs_df_clean %>%
  group_by(genre) %>%
  mutate(n_label = paste0(genre, "\n(n=", n(), ")")) %>%
  ungroup() %>%
  mutate(n_label = fct_reorder(n_label, popularity, .fun=median, .desc=TRUE)) %>%
  
  ggplot(aes(x = n_label, y = popularity, fill = genre)) +
  
  geom_boxplot(
    alpha = 0.8,                
    color = "grey10",           
    outlier.alpha = 0.3,        
    outlier.size = 1.2,         
    outlier.color = "grey50"    
  ) +
  
  labs(
    title = "Розподіл популярності за жанрами",
    x = "Жанр (кількість пісень)", 
    y = "Популярність (0 - 100)"
  ) + 
  
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("%.1f", after_stat(y))),
    vjust = -0.8,
    fontface = "bold",
    size = 3.5,
    color = "black"
  ) +
  
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#2C3E50", margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 11, lineheight = 0.8, color = "#2C3E50"), # lineheight зближує рядки
    panel.grid.major.x = element_blank(), # Прибираємо вертикальну сітку
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
print(popularity_by_genre_boxplot)

#####################################################

popularity_by_genre_violin <- songs_df_clean %>%
  group_by(genre) %>%
  mutate(n_label = paste0(genre, "\n(n=", n(), ")")) %>%
  ungroup() %>%
  mutate(n_label = fct_reorder(n_label, popularity, .fun=median, .desc=TRUE)) %>%
  ggplot(aes(x = n_label, y = popularity, fill = genre)) +
  
  geom_violin(
    scale = "area",
    trim = TRUE,
    alpha = 0.75,
    color = "grey40",
    linewidth = 0.4
  ) +
  
  labs(
    title = "Розподіл популярності за жанрами",
    subtitle = "Чорною лінією позначено медіану",
    x = "Жанр (кількість пісень)", 
    y = "Популярність (0 - 100)"
  ) + 
  
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = after_stat(y)),
    vjust = -0.7,
    fontface = "bold",
    size = 3,
    color = "black"
  ) +
  
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.2,           
    color = "black",       
    linewidth = 0.2        
  ) +
  
  
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#2C3E50", margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 11, lineheight = 0.8, color = "#2C3E50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

print(popularity_by_genre_violin)

##############################################

genre_more_than_50_count_bar_plot <- songs_df_clean %>%
  filter(popularity >= 50) %>%
  group_by(genre) %>%
  
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  
  mutate(genre = fct_reorder(genre, count, .desc = TRUE)) %>%
  
  ggplot(aes(x = genre, y = count, fill = genre)) +
  
  geom_col(color = "black", alpha = 0.8) + 
  
  geom_text(aes(label = count), vjust = -0.5, size = 4, fontface = "bold") +
  
  labs(
    title = "Кількість пісень з популярністю 50+ за жанрами",
    subtitle = "Скільки треків у кожному жанрі подолали позначку популярності у 50 балів",
    x = "Жанр", 
    y = "Кількість пісень"
  ) + 
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 11), 
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10) 
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(genre_more_than_50_count_bar_plot)

##################################################

popularity_by_genre_more_than_50_violin_plot <- songs_df_clean %>%
  
  filter(popularity >= 50) %>%
  
  group_by(genre) %>%
  mutate(n_label = paste0(genre, "\n(n=", n(), ")")) %>%
  ungroup() %>%
  mutate(n_label = fct_reorder(n_label, popularity, .fun=median, .desc=TRUE)) %>%
  ggplot(aes(x=n_label, y=popularity, fill = genre)) +
  geom_violin(
    scale = "area",
    trim = TRUE,
    alpha = 0.7,
    color = "grey30",
    linewidth = 0.3
  ) +
  labs(
    title = "Кількість пісень з популярністю 50+ за жанрами",
    subtitle = "Чорною лінією позначено медіану",
    x="Жанр (кількість пісень)", 
    y="Популярність"
  ) + 
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.6,           
    color = "black",       
    linewidth = 0.2        
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = after_stat(round(y, 1))),
    vjust = -0.7,
    fontface = "bold",
    size = 3.5
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "#C0392B", face = "italic", size = 11)
  )
print(popularity_by_genre_more_than_50_violin_plot)

##################################################

popularity_by_genre_more_than_50_boxplot <- songs_df_clean %>%
  
  filter(popularity >= 50) %>%
  
  group_by(genre) %>%
  mutate(n_label = paste0(genre, "\n(n=", n(), ")")) %>%
  ungroup() %>%
  mutate(n_label = fct_reorder(n_label, popularity, .fun=median, .desc=TRUE)) %>%
  ggplot(aes(x=n_label, y=popularity, fill = genre)) +
  geom_boxplot(outlier.alpha = 0.8) +
  labs(
    title = "Кількість пісень з популярністю 50+ за жанрами",
    x="Жанр (кількість пісень)", 
    y="Популярність"
  ) + 
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.6,           
    color = "black",       
    linewidth = 0.2,
    alpha = 0.6
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = after_stat(round(y, 1))),
    vjust = -0.7,
    fontface = "bold",
    size = 3.5
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "#C0392B", face = "italic", size = 11)
  )

yearly_median_pop <- songs_df_clean %>%
  
  group_by(year, genre) %>%
  summarise(median_popularity = median(popularity, na.rm = TRUE), .groups = 'drop')

print(popularity_by_genre_more_than_50_boxplot)

##################################################

facet_popularity_trend_plot <- ggplot(yearly_median_pop, aes(x = year, y = median_popularity, fill = genre, color = genre)) +
  
  geom_line(alpha = 0.4, linewidth = 0.8) +
  
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.5) + 
  facet_wrap(~ genre, ncol = 5) + # Розбиваємо на 2 рядки по 5 колонок
  labs(
    title = "Еволюція медіанної популярності за жанрами(з 1955 року)",
    subtitle = "Чорна лінія відображає загальний тренд (LOESS)",
    x = "Рік випуску",
    y = "Медіанна популярність"
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(xlim = c(1955, 2023)) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 5))
  )
print(facet_popularity_trend_plot)

##################################################
#2) Як розподіляється тривалість треків у різних жанрах?
##################################################

duration_violin_graph_less_than_10_min <- songs_df_clean %>%
  
  filter(duration_ms <= 600000) %>%
  
  mutate(duration_min = duration_ms / 60000) %>%
  
  mutate(genre = fct_reorder(genre, duration_min, .fun = median, .desc = TRUE)) %>%
  
  ggplot(aes(x = genre, y = duration_min, fill = genre)) +
  
  geom_violin(
    scale = "area",
    trim = TRUE,
    alpha = 0.75,
    color = "grey40",
    linewidth = 0.4
  ) +
  
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.6,           
    color = "black",       
    linewidth = 0.2        
  ) +
  
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = after_stat(sprintf("%.2f", y))), 
    vjust = -0.7,          
    fontface = "bold",
    size = 3.5,
    color = "grey10"       
  ) +
  
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  
  labs(
    title = "Розподіл тривалості пісень за жанрами",
    subtitle = "Чорною лінією позначено медіану",
    x = "Жанр", 
    y = "Тривалість (у хвилинах)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    # Заголовки
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = "#7F8C8D", margin = margin(b = 15)),
    
    # Осі
    axis.title.x = element_text(size = 13, face = "bold", color = "#34495E", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", color = "#34495E", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", color = "#2C3E50"),
    axis.text.y = element_text(color = "#2C3E50"),
    
    # Очищення фону графіка
    panel.grid.major.x = element_blank(), # Видаляємо вертикальні лінії сітки (вони заважають під скрипками)
    panel.grid.minor = element_blank()    # Видаляємо дрібні проміжні лінії сітки
  )

print(duration_violin_graph_less_than_10_min)

##################################################
#3) Чи впливає explicit-контент на популярність треків?
##################################################

explicit_popularity_small_boxplot <- songs_df_clean %>%
  group_by(explicit) %>%
  mutate(n_label = paste0(explicit, "\n(n=", n(), ")")) %>%
  ungroup() %>%
  ggplot(aes(x=n_label, y=popularity, fill=explicit)) +
  geom_boxplot(outlier.alpha = 0.5) +
  labs(
    title = "Розподіл популярності за explicit content",
    x="Explicit-content (кількість пісень)", 
    y="Популярність"
  ) + 
  
  scale_fill_manual(
    values = c(
      "Explicit" = "indianred1",
      "Clean" = "dodgerblue2"   
    )
  ) +
  
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = after_stat(round(y, 1))),
    vjust = -0.7,
    fontface = "bold",
    size = 3.5
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"), # Заголовок (великий і жирний)
    axis.title.x = element_text(size = 14),              # Підпис осі X ("Жанр")
    axis.title.y = element_text(size = 14),              # Підпис осі Y ("Середня енергійність")
    axis.text.x = element_text(size = 12),               # Самі назви жанрів під віссю
    axis.text.y = element_text(size = 12)                # Цифри на осі Y
  )
print(explicit_popularity_small_boxplot)

##################################################

explicit_popularityl_boxplot  <- songs_df_clean %>%
  
  mutate(explicit_cat = ifelse(explicit == "Explicit", "З матами", "Без матів")) %>%
  
  group_by(genre) %>%
  mutate(
    sum_of_medians = sum(
      median(popularity[explicit_cat == "З матами"]),
      median(popularity[explicit_cat == "Без матів"])
    )
  ) %>%
  ungroup() %>%
  
  mutate(genre = fct_reorder(genre, sum_of_medians, .desc = TRUE)) %>%
  
  ggplot(aes(x = genre, y = popularity, fill = explicit_cat)) +
  
  geom_boxplot(position = position_dodge(width = 0.75), outlier.alpha = 0.2) +
  
  scale_fill_manual(
    values = c(
      "З матами" = "indianred1",
      "Без матів" = "dodgerblue1"   
    )
  ) +
  
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = after_stat(round(y, 1))),
    position = position_dodge(width = 0.75),
    vjust = -0.7,
    fontface = "bold",
    size = 3.5
  ) +
  
  stat_summary(
    fun.data = function(x) {
      data.frame(
        y = median(x, na.rm = TRUE),           
        label = paste0("n=", length(x))
      )
    },
    geom = "text",
    position = position_dodge(width = 0.75), 
    vjust = 1.5,      
    fontface = "bold",
    size = 2.5,         
    color = "grey20"
  ) +
  
  labs(
    title = "Розподіл популярності за жанрами та наявністю матів",
    x = "Жанр", 
    y = "Популярність",
    fill = "" # Підпис для легенди
  ) + 
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0), # Якщо назви налізуть одна на одну, змініть angle на 45
    axis.text.y = element_text(size = 12),
    legend.position = "top", # Переносимо легенду нагору, щоб не забирала місце збоку
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  )
print(explicit_popularityl_boxplot)

##################################################
#Які жанри мають вищу середню energy, danceability та loudness?
##################################################

mean_energy_graph <- songs_df_clean %>%
  group_by(genre) %>%
  summarise(mean_energy = mean(energy, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = reorder(genre, -mean_energy), y = mean_energy)) +
  
  geom_col(fill = "#F39C12", color = "black", alpha = 0.85, width = 0.75) + 
  
  # Охайні підписи з 2 знаками після коми
  geom_text(
    aes(label = sprintf("%.2f", mean_energy)), 
    vjust = -0.8, 
    fontface = "bold", 
    size = 4,
    color = "grey10"
  ) +
  
  labs(
    title = "Середня енергійність за жанрами", 
    x = "Жанр", 
    y = "Середня енергійність (0 - 1.0)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
    panel.grid.major.x = element_blank() # Прибираємо зайві вертикальні лінії
  ) +
  # Даємо +15% вільного простору зверху, щоб текст не обрізався
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(mean_energy_graph)

##################################################

mean_danceability_graph <- songs_df_clean %>%
  group_by(genre) %>%
  summarise(mean_danceability = mean(danceability, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = reorder(genre, -mean_danceability), y = mean_danceability)) +
  
  geom_col(fill = "#2980B9", color = "black", alpha = 0.85, width = 0.75) + 
  
  geom_text(
    aes(label = sprintf("%.2f", mean_danceability)), 
    vjust = -0.8, 
    fontface = "bold", 
    size = 4,
    color = "grey10"
  ) +
  
  labs(
    title = "Середня танцювальність за жанрами", 
    x = "Жанр", 
    y = "Середня danceability (0 - 1.0)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(mean_danceability_graph)

##################################################

mean_loudness_graph <- songs_df_clean %>%
  group_by(genre) %>%
  summarise(mean_loudness = mean(loudness, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = reorder(genre, -mean_loudness), y = mean_loudness)) +
  
  geom_col(fill = "#C0392B", color = "black", alpha = 0.85, width = 0.75) + 
  
  geom_text(
    aes(label = sprintf("%.2f dB", mean_loudness)), 
    vjust = 1.5,
    fontface = "bold", 
    size = 3.5,
    color = "grey10"
  ) +
  
  labs(
    title = "Середня гучність за жанрами",
    subtitle = "Чим меньше децибел тим тихіша пісня",
    x = "Жанр", 
    y = "Середня гучність (dB)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 3)),
    plot.subtitle = element_text(size = 14, color = "#7F8C8D", margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
    panel.grid.major.x = element_blank()
  ) +
  # Даємо простір ЗНИЗУ (бо значення від'ємні), щоб текст не зрізався
  scale_y_continuous(expand = expansion(mult = c(0.15, 0)))
print(mean_loudness_graph)

##################################################

#Питання 4: Які жанри мають найбільш контрастні аудіо-профілі?

#################################################
audio_features <- c(
  "danceability", "energy", "loudness",
  "speechiness", "acousticness",
  "instrumentalness", "liveness", "valence", "tempo"
)

# 2. Обчислюємо медіани по жанрах
df_genre_profile <- songs_df_clean %>%
  group_by(genre) %>%
  summarise(
    across(all_of(audio_features), median),
    .groups = "drop"
  )

# 3. Переводимо в long format
df_genre_long <- df_genre_profile %>%
  pivot_longer(
    cols = -genre,
    names_to = "feature",
    values_to = "value"
  ) %>%
  group_by(feature) %>%
  mutate(
    z_value = as.numeric(scale(value))
  ) %>%
  ungroup()

# 4. Heatmap
ggplot(df_genre_long, aes(x = feature, y = genre, fill = z_value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "firebrick",
    midpoint = 0
  ) +
  labs(
    title = "Жанрові аудіо-профілі за ключовими характеристиками",
    subtitle = "Використанні медіани аудіо-характеристик",
    x = "Аудіо-характеристика",
    y = "Жанр"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = "none")

#####################################


top_genres <- songs_df_clean %>%
  filter(!is.na(genre)) %>%
  group_by(genre) %>%
  summarise(
    median_popularity = median(popularity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_popularity)) %>%
  slice_head(n = 5) %>%
  pull(genre)

# 2. Обираємо характеристики
selected_features <- c(
  "danceability",
  "energy",
  "acousticness",
  "valence",
  "loudness"
)

# 3. Рахуємо медіани по всіх жанрах
df_radar <- songs_df_clean %>%
  filter(genre %in% top_genres) %>%
  group_by(genre) %>%
  summarise(
    across(all_of(selected_features), median, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Нормалізація до [0,1]
df_scaled <- df_radar
df_scaled[selected_features] <- lapply(df_scaled[selected_features], function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) {
    rep(0, length(x))
  } else {
    (x - min(x, na.rm = TRUE)) / rng
  }
})

# 5. Формат для fmsb:
# перший рядок = max, другий = min
radar_data <- rbind(
  rep(1, length(selected_features)),
  rep(0, length(selected_features)),
  df_scaled[selected_features]
)

rownames(radar_data) <- c("max", "min", as.character(df_scaled$genre))

# 6. Автоматичні кольори для будь-якої кількості жанрів
n_genres <- nrow(df_scaled)
colors_border <- grDevices::rainbow(n_genres, alpha = 0.9)
colors_fill <- grDevices::adjustcolor(colors_border, alpha.f = 0.2)

# 7. Експорт у PNG
png("genre_radar_chart_all_genres.png", width = 2200, height = 1800, res = 220)

radarchart(
  radar_data,
  axistype = 1,
  pcol = colors_border,
  pfcol = colors_fill,
  plwd = 1.5,
  plty = 1,
  cglcol = "grey70",
  cglty = 1,
  cglwd = 0.8,
  axislabcol = "grey30",
  vlcex = 1
)

legend(
  "topright",
  legend = as.character(df_scaled$genre),
  bty = "n",
  pch = 15,
  col = colors_border,
  text.col = "black",
  cex = 0.7
)

title("Порівняння топ-5 популярних жанрів за аудіо-профілями")

dev.off()

###################################

audio_features <- c(
  "danceability", "energy", "loudness", "speechiness",
  "acousticness", "instrumentalness", "liveness",
  "valence", "tempo"
)

# 1. Профілі жанрів: медіани
genre_profiles <- songs_df_clean %>%
  group_by(genre) %>%
  summarise(
    across(all_of(audio_features), median),
    .groups = "drop"
  )

# 2. Матриця жанрів
genre_matrix <- as.data.frame(genre_profiles)
rownames(genre_matrix) <- as.character(genre_matrix$genre)
genre_matrix$genre <- NULL

# 3. Стандартизація
genre_scaled <- scale(genre_matrix)

# 4. Матриця відстаней
dist_matrix <- as.matrix(dist(genre_scaled, method = "euclidean"))

# 5. Перетворення в long format
dist_df <- as.data.frame(as.table(dist_matrix)) %>%
  rename(
    genre_1 = Var1,
    genre_2 = Var2,
    distance = Freq
  ) %>%
  mutate(
    genre_1 = as.character(genre_1),
    genre_2 = as.character(genre_2)
  )

# 6. Для таблиці найбільш різних пар:
genre_names <- rownames(dist_matrix)

dist_half <- expand.grid(
  genre_1 = genre_names,
  genre_2 = genre_names,
  stringsAsFactors = FALSE
) %>%
  mutate(
    distance = as.vector(dist_matrix),
    i = match(genre_1, genre_names),
    j = match(genre_2, genre_names)
  ) %>%
  filter(i < j)

ggplot(dist_half, aes(x = genre_1, y = genre_2, fill = distance)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(distance, 2)), size = 3) +
  scale_fill_gradient(
    low = "white",
    high = "darkred",
    name = "Відстань"
  ) +
  labs(
    title = "Відстані між жанрами за аудіо-профілями",
    x = "Жанр",
    y = "Жанр"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#################################################

features <- c(
  "danceability", "energy", "loudness", "speechiness",
  "acousticness", "instrumentalness", "liveness",
  "valence", "tempo"
)

genre_pairs <- tibble(
  genre_a = c("Rock", "Jazz", "Hip-Hop"),
  genre_b = c("Jazz", "Electronic", "Classical")
)

# тільки потрібні жанри
df_pairs <- songs_df_clean %>%
  filter(genre %in% unique(c(genre_pairs$genre_a, genre_pairs$genre_b))) %>%
  mutate(
    pop_decile = ntile(popularity, 10)
  )

# медіанні профілі по жанру в кожному децилі popularity
profiles <- df_pairs %>%
  group_by(genre, pop_decile) %>%
  summarise(
    across(all_of(features), median),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(features),
    names_to = "feature",
    values_to = "value"
  )

# стандартизація всередині кожної ознаки
profiles_z <- profiles %>%
  group_by(feature) %>%
  mutate(z = as.numeric(scale(value))) %>%
  ungroup()

# обчислення відстаней для кожної пари
dist_by_pair <- genre_pairs %>%
  rowwise() %>%
  do({
    a <- .$genre_a
    b <- .$genre_b
    
    prof_a <- profiles_z %>%
      filter(genre == a) %>%
      select(pop_decile, feature, z) %>%
      rename(z_a = z)
    
    prof_b <- profiles_z %>%
      filter(genre == b) %>%
      select(pop_decile, feature, z) %>%
      rename(z_b = z)
    
    prof_a %>%
      inner_join(prof_b, by = c("pop_decile", "feature")) %>%
      group_by(pop_decile) %>%
      summarise(
        distance = sqrt(sum((z_a - z_b)^2, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(pair = paste(a, "vs", b))
  }) %>%
  ungroup()

print(dist_by_pair)

# окремі графіки для кожної пари
pairs <- unique(dist_by_pair$pair)

for (pname in pairs) {
  df_plot <- dist_by_pair %>%
    filter(pair == pname)
  
  p <- ggplot(df_plot, aes(x = pop_decile, y = distance)) +
    geom_line(linewidth = 1.2, color = "darkred") +
    geom_point(size = 3, color = "darkred") +
    scale_x_continuous(breaks = 1:10) +
    labs(
      title = pname,
      x = "Дециль popularity",
      y = "Відстань між аудіо-профілями"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  print(p)
  
  
  ggsave(
    filename = paste0("distance_", pname, ".png"),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}
#################################################

#Питання 5Які аудіо-характеристики найбільше пов’язані з популярністю у різних жанрах 
#і чи правдиві стереотипи жанрової музики

#################################################

selected_genres <- sort(unique(as.character(na.omit(songs_df_clean$genre))))

audio_features <- c(
  "danceability", "energy", "loudness", "speechiness",
  "acousticness", "instrumentalness", "liveness",
  "valence", "tempo"
)

cor_genre_feature <- songs_df_clean %>%
  filter(genre %in% selected_genres) %>%
  group_by(genre) %>%
  summarise(
    across(
      all_of(audio_features),
      ~ cor(.x, popularity, method = "spearman")
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -genre,
    names_to = "feature",
    values_to = "r"
  )

ggplot(cor_genre_feature, aes(x = feature, y = genre, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r, 2)), size = 4) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "firebrick",
    midpoint = 0,
    name = "Кореляція\nСпірмена"
  ) +
  labs(
    title = "Зв’язок аудіо-характеристик з popularity у різних жанрах",
    x = "Аудіо-характеристика",
    y = "Жанр"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

############################################

genres <- sort(unique(as.character(na.omit(songs_df_clean$genre))))

for (g in genres) {
  df_plot <- songs_df_clean %>%
    filter(genre == g) %>%
    select(danceability, energy, popularity)
  
  if (nrow(df_plot) == 0) next
  
  x_med <- median(df_plot$danceability)
  y_med <- median(df_plot$energy)
  
  df_plot <- df_plot %>%
    mutate(
      quadrant = case_when(
        danceability < x_med & energy >= y_med ~ "Q1",
        danceability >= x_med & energy >= y_med ~ "Q2",
        danceability < x_med & energy < y_med ~ "Q3",
        TRUE ~ "Q4"
      )
    )
  
  quad_stats <- df_plot %>%
    group_by(quadrant) %>%
    summarise(
      n = n(),
      median_popularity = median(popularity),
      .groups = "drop"
    )
  
  all_quads <- tibble(
    quadrant = c("Q1", "Q2", "Q3", "Q4"),
    x = c(x_med * 0.28, x_med + (1 - x_med) * 0.72, x_med * 0.28, x_med + (1 - x_med) * 0.72),
    y = c(y_med + (1 - y_med) * 0.80, y_med + (1 - y_med) * 0.80, y_med * 0.20, y_med * 0.20),
    dance_lab = c("low", "high", "low", "high"),
    energy_lab = c("high", "high", "low", "low")
  )
  
  quad_stats <- all_quads %>%
    left_join(quad_stats, by = "quadrant") %>%
    mutate(
      n = ifelse(is.na(n), 0, n),
      median_popularity = ifelse(is.na(median_popularity), 0, median_popularity)
    )
  
  box_w <- 0.3
  box_h <- 0.15
  
  quad_stats <- quad_stats %>%
    mutate(
      xmin = x - box_w / 2,
      xmax = x + box_w / 2,
      ymin = y - box_h / 2,
      ymax = y + box_h / 2
    )
  
  p <- ggplot(df_plot, aes(x = danceability, y = energy)) +
    geom_point(
      aes(size = popularity),
      alpha = 0.08,
      color = "steelblue"
    ) +
    geom_vline(
      aes(xintercept = x_med, linetype = "Медіана"),
      linewidth = 0.9,
      color = "red"
    ) +
    geom_hline(
      aes(yintercept = y_med, linetype = "Медіана"),
      linewidth = 0.9,
      color = "red"
    ) +
    geom_rect(
      data = quad_stats,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = scales::alpha("white", 0.68),
      color = "black",
      linewidth = 0.4
    ) +
    geom_text(
      data = quad_stats,
      aes(x = x, y = y + 0.05, label = paste0("dance: ", dance_lab), color = dance_lab),
      inherit.aes = FALSE,
      size = 3.8,
      fontface = "bold",
      show.legend = FALSE
    ) +
    geom_text(
      data = quad_stats,
      aes(x = x, y = y + 0.02, label = paste0("energy: ", energy_lab), color = energy_lab),
      inherit.aes = FALSE,
      size = 3.8,
      fontface = "bold",
      show.legend = FALSE
    ) +
    geom_text(
      data = quad_stats,
      aes(x = x, y = y - 0.01, label = paste0("n = ", n)),
      inherit.aes = FALSE,
      size = 3.7,
      color = "black"
    ) +
    geom_text(
      data = quad_stats,
      aes(x = x, y = y - 0.04, label = paste0("median popularity = ", round(median_popularity, 1))),
      inherit.aes = FALSE,
      size = 3.5,
      color = "black"
    ) +
    scale_color_manual(
      values = c("high" = "red", "low" = "darkgreen")
    ) +
    scale_linetype_manual(
      name = "",
      values = c("Медіана" = "dashed")
    ) +
    scale_size_continuous(
      range = c(0.4, 4.5),
      name = "Популярність"
    ) +
    scale_x_continuous(
      breaks = sort(unique(c(seq(0, 1, 0.25), x_med))),
      labels = function(x) ifelse(abs(x - x_med) < 1e-8, paste0(round(x_med, 2)), round(x, 2))
    ) +
    scale_y_continuous(
      breaks = sort(unique(c(seq(0, 1, 0.25), y_med))),
      labels = function(y) ifelse(abs(y - y_med) < 1e-8, paste0(round(y_med, 2)), round(y, 2))
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    labs(
      title = g,
      x = "Танцювальність (danceability)",
      y = "Енергійність (energy)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.margin = margin(20, 30, 20, 30)
    )
  
  print(p)
  
  ggsave(
    filename = paste0("quadrants_points_", g, ".png"),
    plot = p,
    width = 8.5,
    height = 6.5,
    dpi = 300
  )
}

#ПИТАННЯ 7
#Еволюція музики: Як змінювалися ключові аудіо-характеристики  з роками?
#################################################
df_heat_year <- songs_df_clean %>%
  filter(!is.na(year), !is.na(mode), year > 1923, year < 2024) %>%
  group_by(year) %>%
  summarise(
    acousticness = median(acousticness),
    energy = median(energy),
    liveness = median(liveness),
    instrumentalness = median(instrumentalness),
    danceability = median(danceability),
    valence = median(valence),
    speechiness = median(speechiness),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(acousticness, energy, liveness, danceability, valence, instrumentalness, speechiness),
    names_to = "feature",
    values_to = "value"
  )

ggplot(df_heat_year, aes(x = year, y = feature, fill = value)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white",
    high = "darkblue",
    name = "Медіана"
  ) +
  labs(
    title = "Зміна аудіо-характеристик з роками",
    x = "Рік",
    y = "Характеристика"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

###############################

df_ridge <- songs_df_clean %>%
  filter(year != 1900, year %% 5 == 0) %>%
  select(year, energy)

p <- ggplot(df_ridge, aes(x = energy, y = factor(year), fill = after_stat(x))) +
  geom_density_ridges_gradient(
    scale = 1.8,
    rel_min_height = 0.01,
    size = 0.25
  ) +
  scale_fill_gradientn(
    colours = c("#5B2A86", "#A23EBA", "#E457A1", "#F58549", "#F3B61F"),
    name = NULL
  ) +
  labs(
    title = "Як змінювався розподіл energy з роками",
    x = "Energy",
    y = "Рік"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

print(p)

ggsave(
  filename = "energy_ridge_plot.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300
)


###############################

#Еволюція використання ладу (Mode: Major vs Minor) з плином часу
p_mode_time <- songs_df_clean %>%
  filter(!is.na(mode), !is.na(year), year >= 1950, year <= 2023) %>%
  count(year, mode) %>%
  
  ggplot(aes(x = year, y = n, fill = mode)) +
  geom_area(position = "fill", alpha = 0.85, color = "black", linewidth = 0.2) +
  
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  
  scale_fill_manual(
    values = c("Major" = "#F1C40F", "Minor" = "#8E44AD"),
    name = "Музичний лад"
  ) +
  
  labs(
    title = "Еволюція музичного ладу: Мажор проти Мінору (1950 - 2023)",
    subtitle = "Спостерігається поступовий тренд до збільшення частки мінорних композицій",
    x = "Рік випуску",
    y = "Відсоток від усіх пісень"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank() 
  )

print(p_mode_time)
##############################################

#Еволюція використання тональностей з плином часу (Key over Time)
p_keys_time <- songs_df_clean %>%
  filter(!is.na(key), !is.na(year), year >= 1950, year <= 2023) %>%
  count(year, key) %>%
  
  ggplot(aes(x = year, y = n, fill = key)) +
  geom_area(position = "fill", alpha = 0.9, color = "black", linewidth = 0.1) +
  
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  
  scale_fill_viridis_d(option = "turbo") + 
  
  labs(
    title = "Еволюція популярності музичних тональностей (1950 - 2023)",
    subtitle = "Частка кожної тональності в загальному обсязі релізів за рік",
    x = "Рік випуску",
    y = "Відсоток від усіх пісень",
    fill = "Тональність"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )

print(p_keys_time)

###########################################

songs_df_original <- read_csv("songs.csv")

# 2. Перевірка кількості пісень за 2024 і 2025
songs_df_original %>%
  filter(year %in% c(2024, 2025)) %>%
  count(year)

# 3. Виведення самих пісень та їх ID
songs_df_original %>%
  filter(year %in% c(2024, 2025)) %>%
  select(year, id, name, artists, album_name) %>%
  arrange(desc(year)) %>%
  print(n = Inf)

###################################

#Питання8
#Зменшення впливу фан-бази після певного порогу: Чи однаково зростає популярність треків у малих і великих артистів?

################################### 


# 1. Підготовка даних
df_ridge_followers <- songs_df_clean %>%
  select(total_artist_followers, popularity) %>%
  filter(!is.na(total_artist_followers), !is.na(popularity)) %>%
  mutate(
    log_followers = log10(total_artist_followers + 1),
    followers_group = cut(
      log_followers,
      breaks = c(0, 2, 4, 5, 6, 7, 8, 9),
      include.lowest = TRUE,
      right = TRUE
    )
  )

# 2. Нормальні підписи для інтервалів
group_labels <- c(
  "[0,2]" = "1 – 100",
  "(2,4]" = "101 – 10 тис.",
  "(4,5]" = "10 тис. – 100 тис.",
  "(5,6]" = "100 тис. – 1 млн",
  "(6,7]" = "1 млн – 10 млн",
  "(7,8]" = "10 млн – 100 млн",
  "(8,9]" = "100 млн – 1 млрд"
)

df_ridge_followers <- df_ridge_followers %>%
  mutate(
    followers_group_label = recode(as.character(followers_group), !!!group_labels),
    followers_group_label = factor(
      followers_group_label,
      levels = c(
        "1 – 100",
        "101 – 10 тис.",
        "10 тис. – 100 тис.",
        "100 тис. – 1 млн",
        "1 млн – 10 млн",
        "10 млн – 100 млн",
        "100 млн – 1 млрд"
      )
    )
  )

# 3. Побудова графіка
p <- ggplot(
  df_ridge_followers,
  aes(x = popularity, y = followers_group_label, fill = after_stat(x))
) +
  geom_density_ridges_gradient(
    scale = 1.8,
    rel_min_height = 0.01,
    size = 0.25
  ) +
  scale_fill_gradientn(
    colours = c("#5B2A86", "#A23EBA", "#E457A1", "#F58549", "#F3B61F"),
    name = NULL
  ) +
  labs(
    title = "Розподіл popularity у групах розміру фан-бази",
    x = "Популярність треку",
    y = "Кількість підписників артиста"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

print(p)

# 4. Експорт у PNG
ggsave(
  filename = "ridge_popularity_by_followers_groups.png",
  plot = p,
  width = 12,
  height = 7,
  dpi = 300
)

########################################

df_q <- songs_df_clean %>%
  select(total_artist_followers, popularity) %>%
  filter(
    !is.na(total_artist_followers),
    !is.na(popularity),
    total_artist_followers <= 100000000
  )

p <- ggplot(df_q, aes(x = total_artist_followers, y = popularity)) +
  geom_hex(bins = 45) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    color = "red",
    linewidth = 1.2,
    se = FALSE
  ) +
  scale_fill_gradient(
    low = "white",
    high = "darkblue",
    trans = "log10",
    breaks = c(1, 100, 10000),
    labels = c("1", "100", "10 000"),
    name = "Кількість треків\n(лог. шкала)"
  ) +
  scale_x_continuous(
    limits = c(0, 100000000),
    breaks = c(0, 25000000, 50000000, 75000000, 100000000),
    labels = scales::label_number(big.mark = " ", accuracy = 1)
  ) +
  labs(
    title = "Чи зменшується вплив фан-бази після певного порогу?",
    subtitle = "Показано лише артистів із кількістю підписників до 100 млн",
    x = "Кількість підписників артиста",
    y = "Популярність треку"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

print(p)

ggsave(
  filename = "threshold_fanbase_hex_smooth_100m.png",
  plot = p,
  width = 12,
  height = 7,
  dpi = 300
)