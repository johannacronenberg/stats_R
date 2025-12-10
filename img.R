library(gridExtra)
library(magrittr)
library(tidyverse)

theme <- theme_light(base_size = 14) +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.margin = margin(t = -5),
        legend.key.width = unit(1.5, "cm"),
        legend.title = element_text(size = 14), 
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(color = "black"),
        strip.text.y = element_text(color = "black"))

##### 01_einführung_inferenzstatistik / 01_intro_statistics #####

# normalverteilung (810 x 600 png)
ggplot(data.frame(x = c(-10, 10))) + aes(x) + xlab("") + ylab("") +
  stat_function(fun = dnorm, linewidth = 1.2, color = "black") + 
  stat_function(fun = dnorm, args = list(mean = 3, sd = 2), linewidth = 1.2, color = "royalblue3") + 
  stat_function(fun = dnorm, args = list(mean = -2, sd = 1.5), linewidth = 1.2, color = "springgreen4") + 
  # ylab("Wahrscheinlichkeitsdichte") + 
  ylab("Probability Density") +
  theme

normal <- rnorm(200)
skew_right <- c(normal[normal > 0] * 2.5, normal[normal < 0])
skew_left <- c(normal[normal < 0] * 2.5, normal[normal > 0])
bimodal <- c(rnorm(100, -3, .25), rnorm(100, 3, .25))
uniform <- runif(200, min = -4, max = 4)

df <- data.frame(skew_right, skew_left, bimodal, uniform) %>% 
  pivot_longer(cols = c("skew_right", "skew_left", "bimodal", "uniform"), 
               names_to = "distribution", values_to = "samples")

# dists (800 x 750 png)
df %>% 
  # mutate(distribution = case_when(distribution == "skew_left" ~ "skew links",
  #                                 distribution == "skew_right" ~ "skew rechts",
  #                                 .default = distribution)) %>%
  mutate(distribution = case_when(distribution == "skew_left" ~ "skew left",
                                  distribution == "skew_right" ~ "skew right",
                                  .default = distribution)) %>%
  ggplot() + aes(x = samples) + 
  geom_density(linewidth = 1.2) + 
  facet_wrap(~distribution) + 
  xlab("") + 
  # ylab("Wahrscheinlichkeitsdichte") + 
  ylab("Probability Density") +
  theme

library(plyr)
library(dplyr)
grid <- seq(min(df$samples), max(df$samples), length = 100)
normaldens <- ddply(df, "distribution", function(d) {
  data.frame( 
    samples = grid,
    density = dnorm(grid, mean(d$samples), sd(d$samples))
  )
})

# dists_norm (800 x 750 png)
df %>% 
  mutate(distribution = case_when(distribution == "skew_left" ~ "skew links",
                                  distribution == "skew_right" ~ "skew rechts",
                                  .default = distribution)) %>%
  # mutate(distribution = case_when(distribution == "skew_left" ~ "skew left",
  #                                 distribution == "skew_right" ~ "skew right",
  #                                 .default = distribution)) %>%
  ggplot() + aes(x = samples) + 
  geom_density(linewidth = 1.2) + 
  geom_line(data = normaldens %>% 
              mutate(distribution = case_when(distribution == "skew_left" ~ "skew links",
                                              distribution == "skew_right" ~ "skew rechts",
                                              .default = distribution)), # %>%
              # mutate(distribution = case_when(distribution == "skew_left" ~ "skew left",
              #                                 distribution == "skew_right" ~ "skew right",
              #                                 .default = distribution)), 
            aes(y = density), linewidth = 1.2, color = "royalblue3") + 
  facet_wrap(~distribution) + 
  xlab("") + 
  ylab("Wahrscheinlichkeitsdichte") +
  # ylab("Probability Density") +
  theme

# dists_qq (800 x 750 png)
df %>% 
  # mutate(distribution = case_when(distribution == "skew_left" ~ "skew links",
  #                                 distribution == "skew_right" ~ "skew rechts",
  #                                 .default = distribution)) %>%
  mutate(distribution = case_when(distribution == "skew_left" ~ "skew left",
                                  distribution == "skew_right" ~ "skew right",
                                  .default = distribution)) %>%
  ggplot() + aes(sample = samples) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~distribution) +
  ylab("samples") + xlab("theoretical quantiles") +
  theme

# area_norm (810 x 600 png)
ggplot() +
  xlim(-4, 4) + 
  # xlab("Standardabweichung") + ylab("Wahrscheinlichkeitsdichte") + 
  xlab("Standard Deviation") + ylab("Probability Density") + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                inherit.aes = F) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(-1, 1), fill = "cornflowerblue", alpha = 0.5) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(-2, -1), fill = "violetred4", alpha = 0.5) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(1, 2), fill = "violetred4", alpha = 0.5) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(-3, -2), fill = "goldenrod", alpha = 0.5) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(2, 3), fill = "goldenrod", alpha = 0.5) + 
  annotate(geom = "text", x = 0.5, y = 0.2, label = "0.34") + 
  annotate(geom = "text", x = -0.5, y = 0.2, label = "0.34") + 
  annotate(geom = "text", x = 1.5, y = 0.05, label = "0.135") + 
  annotate(geom = "text", x = -1.5, y = 0.05, label = "0.135") + 
  annotate(geom = "text", x = 2.3, y = 0.01, label = "0.02") + 
  annotate(geom = "text", x = -2.3, y = 0.01, label = "0.02") + 
  geom_vline(xintercept = 0, lty = "dashed") + 
  theme

url <- "http://www.phonetik.uni-muenchen.de/~jmh/lehre/Rdf"
df <- read.table(file.path(url, "vdata.txt")) %>% as_tibble() %>% 
  dplyr::rename(vokal = V, spannung = Tense, konsonant = Cons, tempo = Rate, subject = Subj) %>% 
  mutate(dauer = log(dur)) %>% 
  select(-c(X, Y))
mu <- mean(df$dauer)
SE <- sd(df$dauer)

# norm_area1 (810 x 600 png)
ggplot(df) +
  aes(x = dauer) + xlim(3.5, 6.5) + 
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE)) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(3.5, 4.5), fill = "cornflowerblue") +
  # xlab("Log. Dauer") + ylab("Wahrscheinlichkeitsdichte") +
  xlab("Log. Duration") + ylab("Probability Density") +
  theme

# norm_area2 (810 x 600 png)
ggplot(df) +
  aes(x = dauer) + xlim(3.5, 6.5) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE)) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(5.1, 6), fill = "cornflowerblue") +
  # xlab("Log. Dauer") + ylab("Wahrscheinlichkeitsdichte") +
  xlab("Log. Duration") + ylab("Probability Density") +
  theme

# norm_area3 (810 x 600 png)
ggplot(df) +
  aes(x = dauer) + xlim(3.5, 6.5) + 
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE)) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(4.9, 5.5), fill = "cornflowerblue") +
  # xlab("Log. Dauer") + ylab("Wahrscheinlichkeitsdichte") +
  xlab("Log. Duration") + ylab("Probability Density") +
  theme

# norm_area4 (810 x 600 png)
ggplot(df) +
  aes(x = dauer) + xlim(3.5, 6.5) + 
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE)) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(3.5, 4.35), fill = "cornflowerblue") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(5.47, 6.5), fill = "cornflowerblue") +
  annotate(geom="text", x=4.9, y=0.5, label="Fläche = 0.95") +
  annotate(geom="text", x=3.9, y=0.1, label="Fläche = 0.025", color = "cornflowerblue") +
  annotate(geom="text", x=5.9, y=0.1, label="Fläche = 0.025", color = "cornflowerblue") +
  # xlab("Log. Dauer") + ylab("Wahrscheinlichkeitsdichte") +
  xlab("Log. Duration") + ylab("Probability Density") +
  theme

##### 02_einfacheLineareRegression / 02_simpleLinearRegression #####

queen <- read.table(file.path(url, "queen.txt")) %>% as_tibble()
queen.lm <- lm(f0 ~ Alter, data = queen)

alter <- queen %>% filter(Alter > 30 & Alter < 41) %>% pull(Alter)
queen_fitted <- data.frame(Alter = alter,
                           f0 = predict(queen.lm, data.frame(Alter = alter)))

# resid (680 x 570 png)
queen %>% filter(Alter > 30 & Alter < 41) %>% 
  ggplot() +
  aes(x = Alter, y = f0) + 
  geom_point(size = 4) + 
  # xlab("Alter") + ylab("f0") + 
  xlab("Age") + ylab("f0") +
  geom_abline(slope = coef(queen.lm)[2], intercept = coef(queen.lm)[1], color = "blue") + 
  geom_point(data = queen_fitted, color = "red", size = 4) + 
  geom_segment(aes(x = 31, y = 261.5, xend = 31, yend = 254.9), lty = "dashed") + 
  geom_segment(aes(x = 32, y = 259.6, xend = 32, yend = 253.8), lty = "dashed") + 
  geom_segment(aes(x = 33, y = 248.9, xend = 33, yend = 252.8), lty = "dashed") + 
  geom_segment(aes(x = 34, y = 257.0, xend = 34, yend = 251.7), lty = "dashed") + 
  geom_segment(aes(x = 35, y = 243.0, xend = 35, yend = 250.6), lty = "dashed") + 
  geom_segment(aes(x = 37, y = 235.5, xend = 37, yend = 248.5), lty = "dashed") + 
  geom_segment(aes(x = 38, y = 224.8, xend = 38, yend = 247.4), lty = "dashed") + 
  geom_segment(aes(x = 39, y = 237.6, xend = 39, yend = 246.3), lty = "dashed") + 
  geom_segment(aes(x = 40, y = 228.7, xend = 40, yend = 245.2), lty = "dashed") + 
  scale_x_continuous(breaks = seq(30, 40, by = 2)) +
  theme

# resid2 (680 x 570 png)
queen %>% filter(Alter > 30 & Alter < 41) %>% 
  ggplot() +
  aes(x = Alter, y = f0) + 
  geom_point(size = 4) + 
  xlab("Alter") + ylab("f0") +
  # xlab("Age") + ylab("f0") +
  geom_abline(slope = coef(queen.lm)[2], intercept = coef(queen.lm)[1], color = "blue") + 
  geom_abline(slope = 0, intercept = mean(queen$f0), color = "orange", size = 1.2) + 
  geom_point(data = queen_fitted, color = "red", size = 4) + 
  geom_segment(aes(x = 31, y = 261.5, xend = 31, yend = 254.9), lty = "dashed") + 
  geom_segment(aes(x = 32, y = 259.6, xend = 32, yend = 253.8), lty = "dashed") + 
  geom_segment(aes(x = 33, y = 248.9, xend = 33, yend = 252.8), lty = "dashed") + 
  geom_segment(aes(x = 34, y = 257.0, xend = 34, yend = 251.7), lty = "dashed") + 
  geom_segment(aes(x = 35, y = 243.0, xend = 35, yend = 250.6), lty = "dashed") + 
  geom_segment(aes(x = 37, y = 235.5, xend = 37, yend = 248.5), lty = "dashed") + 
  geom_segment(aes(x = 38, y = 224.8, xend = 38, yend = 247.4), lty = "dashed") + 
  geom_segment(aes(x = 39, y = 237.6, xend = 39, yend = 246.3), lty = "dashed") + 
  geom_segment(aes(x = 40, y = 228.7, xend = 40, yend = 245.2), lty = "dashed") + 
  scale_x_continuous(breaks = seq(30, 40, by = 2)) + 
  theme

##### 03_multipleLineareRegression / 03_multipleLinearRegression #####

url <- "http://www.phonetik.uni-muenchen.de/~jmh/lehre/Rdf"
faux <- read.table(file.path(url, "faux.txt"), stringsAsFactors = T, header = T) %>% 
  as_tibble()

lm1 <- lm(f0 ~ dB + dur, data = faux)
k <- lm1 %>% tidy() %>% filter(term == "(Intercept)") %>% pull(estimate)
b_dB <- lm1 %>% tidy() %>% filter(term == "dB") %>% pull(estimate)
b_dur <- lm1 %>% tidy() %>% filter(term == "dur") %>% pull(estimate)
high_dur <- 450
low_dur <- 150
intercept_high_dur <- k + b_dB * 0 + b_dur * high_dur
intercept_high_dur
intercept_low_dur <- k + b_dB * 0 + b_dur * low_dur
intercept_low_dur
slope <- b_dB
slope

p1 <- ggplot(faux) + 
  aes(x = dB, y = f0, col = dur) + 
  geom_point() + 
  xlim(0, 95) +
  ylim(0, 500) +
  geom_abline(slope = slope, intercept = intercept_high_dur, color = "#56B1F7", linewidth = 1.2) +
  geom_abline(slope = slope, intercept = intercept_low_dur, color = "#132B43", linewidth = 1.2) +
  geom_vline(xintercept = 0, lty = "dashed") +
  theme

lm2 <- lm(f0 ~ dB * dur, data = faux)
k <- lm2 %>% tidy() %>% filter(term == "(Intercept)") %>% pull(estimate)
b_dB <- lm2 %>% tidy() %>% filter(term == "dB") %>% pull(estimate)
b_dur <- lm2 %>% tidy() %>% filter(term == "dur") %>% pull(estimate)
b_interaction <- lm2 %>% tidy() %>% filter(term == "dB:dur") %>% pull(estimate)
intercept_low_dur <- k + b_dB * 0 + b_dur * low_dur + b_interaction * (0 * low_dur)
intercept_low_dur
intercept_high_dur <- k + b_dB * 0 + b_dur * high_dur + b_interaction * (0 * high_dur)
intercept_high_dur
slope_low_dur <- b_dB + b_interaction * low_dur
slope_low_dur
slope_high_dur <- b_dB + b_interaction * high_dur
slope_high_dur

p2 <- ggplot(faux) + 
  aes(x = dB, y = f0, col = dur) + 
  geom_point() + 
  xlim(0, 95) +
  ylim(0, 500) +
  geom_abline(slope = slope_low_dur, intercept = intercept_low_dur, color = "#132B43", linewidth = 1.2) +
  geom_abline(slope = slope_high_dur, intercept = intercept_high_dur, color = "#56B1F7", linewidth = 1.2) +
  geom_vline(xintercept = 0, lty = "dashed") + 
  theme

# interaction (1100 x 430 png)
grid.arrange(p1, p2, nrow = 1)

##### 05_logistischeRegression / 05_logisticRegression #####

scurve <- function(x, k = 0, b = 1) {
  y <- exp(b * x + k) / (1 + exp(b * x + k))
  return(y)
}

sig_df <- expand.grid(x = seq(-10, 10, by = 1),
                      b = c(0, 1, 0.5, 0.25),
                      k = c(0, 1, -1)) %>% 
  as_tibble() %>% 
  arrange(b, k, x) %>% 
  mutate(y = scurve(x, k, b)) %>%
  mutate(b = factor(b, levels = c(0, 1, 0.5, 0.25)),
         k = factor(k, levels = c(0, 1, -1)))

# sigmoid_b (680 x 500 png)
sig_df %>% 
  filter(k == 0 & b != 0) %>% 
  ggplot() + 
  aes(x, y, col = b) + 
  geom_line(linewidth = 1.2) + 
  scale_colour_manual(values = c("black", "tomato", "limegreen")) +
  ylim(0, 1) +
  theme

# sigmoid_k (680 x 500 png)
sig_df %>% 
  filter(b == 0) %>% 
  ggplot() + 
  aes(x, y, col = k) + 
  geom_line(linewidth = 1.2) + 
  scale_colour_manual(values = c("black", "tomato", "limegreen")) +
  ylim(0, 1) +
  theme

# sigmoid (680 x 500 png)
tibble(x = seq(-10, 10, by = 1),
       b = 0.8,
       k = 4) %>% 
  mutate(y = scurve(x, k, b)) %>% 
  ggplot() + 
  aes(x, y) + 
  geom_line(linewidth = 1.2) + 
  geom_hline(mapping = aes(yintercept = 0.5)) + 
  geom_vline(mapping = aes(xintercept = -5), linetype = "dashed") +
  ylim(0, 1) +
  theme
