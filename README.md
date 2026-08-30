# Data Analysis — Spotify Songs

## About the Project

This project presents a comprehensive statistical analysis of a large music dataset containing Spotify track metadata, audio characteristics, lyrics, genre information, artist metrics, and track popularity.

The analysis explores how musical characteristics, genre, artist audience size, and other factors are associated with track popularity, danceability, duration, and explicit content. The project progresses from data cleaning and exploratory analysis to statistical inference, regression modelling, nonparametric methods, and principal component analysis.

## Dataset

The analysis is based on the [550K Spotify Songs — Audio, Lyrics and Genres](https://www.kaggle.com/datasets/serkantysz/550k-spotify-songs-audio-lyrics-and-genres) dataset published on Kaggle.

The dataset contains approximately **550,000 English-language tracks** and combines several types of information:

* **Track metadata:** title, album, artists, and release year;
* **Audio characteristics:** `danceability`, `energy`, `key`, `loudness`, `mode`, `speechiness`, `acousticness`, `instrumentalness`, `liveness`, `valence`, and `tempo`;
* **Track information:** duration, lyrics, genre, and popularity;
* **Artist-level metrics:** total followers and average artist popularity;
* **Genre information:** main genres and more specific niche genres.

According to the dataset description, the source dataset was reconstructed using Spotify IDs and enriched with additional artist-level information and lyrics. The tracks were categorized into ten main genres:

**Rock, Pop, Electronic, Folk, Country, Hip-Hop, R&B, Jazz, Blues, and Classical.**

For the laboratory analyses, the data was further cleaned and transformed as described in the individual laboratory sections below.

## Laboratory Work

### Lab 1 — Data Cleaning and Exploratory Data Analysis

The first laboratory focused on preparing a large music dataset for analysis and exploring its main structural and audio characteristics.

#### Data Cleaning

The original dataset contained **550,622 tracks and 24 variables**, including metadata, audio features, lyrics, release years, genres, and popularity metrics. The data-cleaning stage included:

* removing duplicate track–artist combinations;
* checking for logical anomalies in track duration and tempo;
* removing system identifiers that were not required for statistical analysis;
* standardizing missing values and text fields;
* transforming nested artist and genre fields into a tidy format;
* creating an `explicit` feature based on lyrics;
* converting categorical variables to appropriate factor types.

After cleaning, the dataset contained **477,565 observations and 23 variables**.

#### Exploratory Data Analysis

The cleaned dataset was then explored to investigate trends in modern music, relationships between artists and track popularity, and the distribution of audio characteristics across genres.

#### Key Findings

* Modern music shows a shift toward **higher energy, louder production, and lower acousticness**, reflecting the increasing prevalence of electronic production.
* As tracks become more commercially successful, differences between genres in their audio profiles tend to decrease, suggesting a degree of **sonic homogenization in mainstream music**.
* Musical mode alone does not determine whether a song sounds positive or negative: although minor-key tracks tend to have lower valence, the distributions overlap substantially.
* Artist fan base is associated with track popularity in a **non-linear** way, with the strongest increase occurring at smaller and medium audience sizes.
* No single audio feature provides a reliable recipe for a hit. Track popularity has no strong direct correlation with any individual audio characteristic.
* Explicit tracks represent approximately **21% of the dataset** and have a higher median popularity than clean tracks, although this association should not be interpreted as causal.
* The dataset also reveals limitations of broad genre labels, including unexpected genre assignments and a strong representation of Rock, which can influence aggregate statistics.

---

### Lab 2 — Statistical Inference

The second laboratory focused on **statistical inference**, extending the exploratory analysis from Lab 1 by statistically validating the patterns observed in the dataset. The observed relationships were formulated as research hypotheses and tested using **Wald tests, confidence intervals, and bootstrap methods**.

The analysis examined several aspects of music popularity and audio characteristics, including:

* the relationship between explicit content and track popularity, both overall and across individual genres;
* the convergence of audio profiles among the most popular tracks;
* differences between selected genre pairs;
* relationships between `energy`, `loudness`, and `acousticness`;
* the association between an artist's fan base and track popularity;
* the prevalence of tracks with `popularity = 0` among highly followed artists;
* changes in acoustic characteristics and energy across release years;
* differences in `valence` distributions between Major and Minor tracks;
* the relationship between explicit content and the probability of becoming a super-hit.

For the analysis, the dataset was cleaned again using a similar procedure, including removal of duplicates, invalid track durations, technical identifiers, redundant whitespace, and empty strings. List-based fields were transformed into usable categorical values, an `explicit` feature was constructed from lyrics, and categorical variables were converted to factors. After cleaning, the dataset contained **516,129 observations**.

#### Key Findings

* Explicit tracks tend to have higher popularity than clean tracks in many genres, with the strongest effects observed in **Hip-Hop and R&B**. The difference was not statistically significant for some genres, such as Jazz and Blues.
* Audio profiles become more similar across genres among the most popular tracks, providing statistical evidence for **audio homogenization**, although genre-specific differences remain.
* `Energy` has a strong positive relationship with `loudness` and a strong negative relationship with `acousticness`.
* No individual audio feature has a strong direct relationship with track popularity, supporting the idea that musical success is **multifactorial**.
* Artist fan base is positively associated with track popularity for both smaller artists and megastars, but the relationship is statistically stronger among artists with smaller audiences.
* Even highly followed artists can have a substantial number of tracks with `popularity = 0`, indicating that a large fan base does not guarantee visibility for every release.
* Confidence intervals were generally narrow across large subsamples, reflecting high statistical precision. However, statistical significance should not be interpreted as evidence of causation, particularly given the very large sample size.
* The convergence of genre audio profiles was not universal across all genre pairs: for some comparisons, the reduction in between-genre distance was not statistically confirmed.

Overall, the laboratory demonstrated how **statistical inference can be used to assess the reliability and robustness of patterns discovered through exploratory data analysis**, rather than merely describing them.

---

### Lab 3 — Regression Analysis

The third laboratory, **Regression Analysis**, extended the previous univariate and bivariate analyses to a multivariate setting. Regression models were used to examine relationships between track characteristics while controlling for other relevant factors (*ceteris paribus*).

The analysis employed **OLS, Logit, and Probit models**, with a focus on track duration, danceability, explicit status, and popularity. The models incorporated non-linear terms, genre indicators, interaction effects, and robust standard errors where appropriate.

#### Key Findings

* Track duration follows a **non-linear historical trend**: tracks tended to become longer in earlier periods, while modern streaming-era releases show a tendency toward shorter durations. The magnitude of this trend varies across genres.
* `Tempo` has a non-linear relationship with track duration, meaning that the same increase in BPM can have different effects for slower and faster tracks.
* `Tempo` and `valence` have important non-linear relationships with `danceability`. After accounting for multicollinearity between `energy` and `loudness`, higher valence is associated with greater danceability, although the effect becomes weaker as valence increases.
* Genre remains an important determinant of danceability: **Hip-Hop and Electronic** tracks tend to have higher baseline danceability, while less rhythm-oriented genres tend to have lower values.
* Logit and Probit models produced consistent results for `explicit` status. **Genre is a major determinant** of the probability that a track is explicit, with particularly strong effects for Hip-Hop. Higher `energy` and `danceability` are also associated with a greater probability of explicit content, while `valence` shows the opposite relationship.
* Artist fan base remains positively associated with track popularity after controlling for genre, decade, and audio characteristics.
* Explicit content retains a statistically significant positive association with popularity after accounting for other factors.
* The initial hypothesis of diminishing marginal returns from an artist's fan base was **not supported** by the final popularity specification: the estimated marginal effect of `log_followers` was larger at the upper end of the fan-base distribution.

#### Limitations

Since the dataset is observational, the estimated relationships should primarily be interpreted as **conditional associations rather than definitive causal effects**. Factors such as marketing, label support, playlist placement, audience demographics, cultural context, and detailed streaming history were not available in the dataset.

The analysis also demonstrated the importance of functional specification. Logarithmic transformations, centering, quadratic terms, interaction effects, choice of reference categories, and robust standard errors can substantially affect the interpretation of regression results. Due to correlations between audio features, individual coefficients should not automatically be interpreted as independent causal effects.

Overall, the laboratory demonstrated the transition from descriptive and bivariate analysis to **multivariate statistical modelling**, showing the importance of controlling for confounding factors and accounting for non-linearity, interactions, multicollinearity, and model-specific uncertainty when analysing music data.

---

### Lab 4 — Nonparametric Regression and Principal Component Analysis

The fourth laboratory extended the previous regression analysis by introducing **nonparametric and semiparametric methods** to investigate whether relationships between music characteristics are genuinely linear or are better described by flexible functions.

The analysis combined **Principal Component Analysis (PCA)** with Generalized Additive Models (GAM), partially linear models, and kernel regression. PCA was used to describe the underlying structure of correlated audio features, while flexible regression methods were applied to `danceability`, `popularity`, and `explicit` status.

#### Principal Component Analysis

PCA revealed a meaningful **multidimensional structure of audio characteristics**. The principal components captured distinct dimensions such as energetic and loud versus acoustic sound, danceability and positive valence, as well as recording characteristics such as liveness and speechiness.

This showed that the audio characteristics of a track cannot be reduced to a single simple measure of quality or popularity. PCA therefore served not merely as a dimensionality-reduction technique, but as an interpretable way of describing the latent structure of the dataset and dealing with correlations between audio features.

#### Key Findings

* `Tempo` has a **non-linear, dome-shaped relationship** with danceability: danceability tends to increase toward a middle BPM range and decrease for both very slow and very fast tracks.
* `Valence` is generally positively associated with danceability, although the strength and stability of the relationship vary across the feature range. Wider confidence bands at the edges of the distributions indicate greater uncertainty where fewer observations are available.
* The relationship between artist audience size (`log_followers`) and track popularity is also **non-linear**. The association becomes more pronounced at higher levels of the artist's audience.
* The GAM using the original audio controls achieved the best validation performance in the popularity analysis, suggesting that the full flexible specification described the data better than a simple linear model or a model replacing all audio variables with PCA components.
* For `explicit` status, GAM and other flexible models revealed non-linear relationships with `danceability`, `energy`, and `valence`. In particular, the probability of explicit content can increase more rapidly at higher levels of danceability.
* Kernel methods provided a local, flexible validation of the estimated relationships without imposing a global functional form. However, their estimates are more sensitive to data density, bandwidth selection, and dimensionality, making PCA and partially constrained models useful practical compromises.
* The results should be interpreted as **descriptive model-based relationships rather than causal effects**, since important factors such as marketing, playlist placement, audience demographics, label activity, and time since release were not available in the dataset.

Overall, the laboratory demonstrated how **nonparametric and semiparametric methods can uncover complex relationships that may be hidden by conventional linear models**, while PCA provided a compact and interpretable representation of the underlying audio-feature structure.

## Technologies

* **R**
* **tidyverse** — data manipulation and visualization
* **dplyr / tidyr / readr / stringr / forcats** — data cleaning and transformation
* **ggplot2** — data visualization
* **ggcorrplot / corrplot / hexbin / ggridges / fmsb** — specialized visualizations and exploratory analysis
* **boot** — bootstrap-based statistical inference
* **FactoMineR / factoextra** — principal component analysis
* **gratia** — analysis and visualization of generalized additive models
* **knitr** — report generation

## Project Structure

```text
Data-Analysis_Spotify-Songs/
│
├── lab_1/
│   ├── lab_1_Data cleaning.R
│   ├── lab_1_EDA.R
│   ├── АД лаб. №1 звіт.pdf
│   └── АД лаб. №1 презентація.pdf
│
├── lab_2/
│   ├── lab_2_final_merged.R
│   ├── АД лаб. №2 звіт.pdf
│   └── АД лаб. №2 презентація.pdf
│
├── lab_3/
│   ├── lab3_run_all.R
│   ├── АД лаб. №3 звіт.pdf
│   └── Регресійний аналіз.pdf
│
└── lab_4/
    ├── lab_4.R
    ├── АД лаб. №4 звіт.pdf
    └── Непараметрична Регресія.pdf
```

Each laboratory folder contains the corresponding R code, written report, and presentation.

## Team

* [Valeriia Syvokon](https://github.com/ValeriiaSyvokon)
* [Vladyslav Susiak](https://github.com/SusiakVladyslav)
* [ytkachenkokm33](https://github.com/ytkachenkokm33)

```
```
