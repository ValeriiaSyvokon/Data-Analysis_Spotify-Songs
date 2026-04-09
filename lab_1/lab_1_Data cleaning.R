library(tidyverse)
library(knitr)


cat("ЕТАП 0: ЗАВАНТАЖЕННЯ ДАНИХ\n")
songs_df_original <- read_csv("songs.csv", show_col_types = FALSE)

#Структура оригінального датасету
str(songs_df_original, give.attr = FALSE)

cat("ЕТАП 3: ДЕСКРИПТИВНІ СТАТИСТИКИ\n")
desc_stats_lol <- songs_df_original %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "Змінна", values_to = "Значення") %>%
  group_by(Змінна) %>%
  summarise(
    `Мінімум` = round(min(Значення, na.rm = TRUE), 3),
    `Медіана` = round(median(Значення, na.rm = TRUE), 3),
    `Середнє` = round(mean(Значення, na.rm = TRUE), 3),
    `Максимум` = round(max(Значення, na.rm = TRUE), 3),
    `Ст. відхилення` = round(sd(Значення, na.rm = TRUE), 3)
  )

print(kable(desc_stats_lol, format = "markdown"))







cat("ЕТАП 1: ВИЯВЛЕННЯ ПОМИЛОК\n")
na_counts <- colSums(is.na(songs_df_original))
na_df <- tibble(Змінна = names(na_counts), Пропуски = na_counts) %>% filter(Пропуски > 0)
cat("\nКількість пропущених значень (NA) в оригіналі:\n")
print(kable(na_df, format = "markdown"))


duplicate_count <- songs_df_original %>% count(name, artists) %>% filter(n > 1) %>% nrow()
cat(sprintf("\nЗнайдено потенційних дублікатів: %d\n", duplicate_count))


cat("ЕТАП 2: ОЧИЩЕННЯ ТА ДОДАВАННЯ EXPLICIT\n")
#Словник для пошуку explicit-контенту
bad_words_regex <- "(?i)\\b(assholes?|bastards?|bitch\\w*|bullshit\\w*|cock(s|ed|ing)?|cunts?|damn\\w*|dick(s|head(s)?|ish|wad|face|ing|ed)?|douchebags?|dumbass(es)?|faggots?|freak\\w*|fuck\\w*|goon\\w*|jerk\\w*|morons?|motherfuckers?|nigg(a|er)s?|puss(y|ies)|retard\\w*|shit\\w*|slut\\w*|wankers?|whore\\w*)\\b"

songs_df_clean <- songs_df_original %>%
  distinct(name, artists, .keep_all = TRUE) %>%
  filter(duration_ms > 0) %>%
  select(-id, -artist_ids) %>%
  
  #Обробка пропусків
  mutate(across(where(is.character), ~ str_trim(.))) %>% 
  mutate(across(where(is.character), ~ na_if(., ""))) %>% 
  
  #Витягуємо перше значення зі списків
  mutate(
    artists = str_extract(artists, "(?<=['\"]).*?(?=['\"])"),
    niche_genres = str_extract(niche_genres, "(?<=['\"]).*?(?=['\"])")
  ) %>%
  
    #Створення стовпця explicit
  mutate(
    explicit = if_else(str_detect(lyrics, bad_words_regex), "Explicit", "Clean")
  ) %>%
  
  drop_na(genre, popularity) %>% 
  
  #Перетворення у фактори
  mutate(
    genre = as.factor(genre),
    niche_genres = as.factor(niche_genres),
    key = factor(key, levels = 0:11, labels = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")),
    mode = factor(mode, levels = c(0, 1), labels = c("Minor", "Major")),
    explicit = as.factor(explicit)
  )

cat(sprintf("Рядків до очищення: %d\n", nrow(songs_df_original)))
cat(sprintf("Рядків після очищення: %d\n", nrow(songs_df_clean)))


cat("ЕТАП 3: ДЕСКРИПТИВНІ СТАТИСТИКИ\n")
desc_stats <- songs_df_clean %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "Змінна", values_to = "Значення") %>%
  group_by(Змінна) %>%
  summarise(
    `Мінімум` = round(min(Значення, na.rm = TRUE), 3),
    `Медіана` = round(median(Значення, na.rm = TRUE), 3),
    `Середнє` = round(mean(Значення, na.rm = TRUE), 3),
    `Максимум` = round(max(Значення, na.rm = TRUE), 3),
    `Ст. відхилення` = round(sd(Значення, na.rm = TRUE), 3)
  )

print(kable(desc_stats, format = "markdown"))


cat("ЕТАП 4: ЗБЕРЕЖЕННЯ РЕЗУЛЬТАТІВ\n")
#зберігаємо у CSV для звіту
write_csv(songs_df_clean, "songs_clean.csv")

#зберігаємо у RDS для себе (щоб не втратити фактори у другому скрипті)
write_rds(songs_df_clean, "songs_clean.rds")

cat("\nОчищений датасет збережено у 'songs_clean.csv' та 'songs_clean.rds'.\n")



#Структура охайного датасету
str(songs_df_clean, give.attr = FALSE)