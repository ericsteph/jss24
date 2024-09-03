rm(list=ls())

library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(openxlsx)
library(readxl)
library(cowplot)

library(themeustat)
palustat <- themeustat::pal_ustat


# Carico i dati

# d1: dati IPC

t0 <- 1984
ultimo_mese <- 7

d1_ <- read_excel("dati/su-e-05.02.67.xlsx", 
                  sheet = "VAR_m-12", skip = 3,
                  n_max = 1)

d1 <- t(d1_)

k <- nrow(d1)
d1 <- as.data.frame(d1[28:k, ])

rownames(d1) <- 1:nrow(d1)
colnames(d1) <- "value"

d1$value <- as.double(d1$value)

tmp <- seq(as.yearmon(t0), as.yearmon(2024 + (ultimo_mese - 1)/12), 1/12)
d1$data <- tmp

d1$stat <- "IPC"

d1 <- d1 %>%
  select(data, stat, value)


# d2: dati della Seco

t0 <- 2023
ultimo_mese <- 7

d2_ <- readRDS(file = "rda/IFC_sper_data.rds")

d2 <- d2_ %>%
  filter(structure == "ks_s22_1_price_exp_med_m") %>%
  select(value)


tmp <- seq(as.yearmon(t0), as.yearmon(2024 + (ultimo_mese - 1)/12), 1/12)
d2$data <- tmp

d2$stat <- "Icc"

d2 <- d2 %>%
  select(data, stat, value)


# d3: dati della Deloitte
d3 <- read_excel("dati/Deloitte_vs_IFCseco.xlsx", 
                 sheet = "Deloitte")

colnames(d3)[2] <- "value"

d3$value <- as.double(d3$value)
d3$value <- d3$value * 100

d3$data <- as.yearmon(d3$Data_i) + .45

d3$stat <- "Deloitte"

d3 <- d3 %>%
  select(data, stat, value)


tmp <- rbind(d2, d3)


# Preparazione per la figura
# dal 2017

tmp2 <- d1 %>%
  filter(data >= 2017) %>%
  select(-stat)


col2 <- RColorBrewer::brewer.pal(8, "Set2")
col2 <- c(pal_ustat[3:9], col2[8])

temp <- c(2022.5 - 1/12, 2023.5 - 1/12, 2024.25 - 1/12)

a <- tmp2 %>%
  filter(data %in% temp) %>%
  select(value) %>%
  as.vector()

a <- as.double(t(a))


# F.1: previsione IFC nel prossimo anno

renames1 <- c("Icc" = " \nPrix dans les 12 prochaines mois, Enquête\nsur le climat de consommation, Seco",
              "Deloitte" = " \nCroissance de l'IPC dans 2 ans, CFO\nSurvey, Deloitte Suisse")

p <- tmp %>%
  filter(data >= 2017) %>%
  ggplot(aes(x = data, y = value, colour = stat)) +
  geom_path(data = tmp2, aes(x = data, y = value, colour = ""), colour = col2[8], linewidth = .8) +
  geom_segment(data = tmp2, aes(x = temp[1], xend = temp[1], y = -1.5, yend = a[1]), 
               linetype = "dotted", colour = col2[2], linewidth = .25) +
  geom_segment(data = tmp2, aes(x = temp[2], xend = temp[2], y = -1.5, yend = a[2]), 
               linetype = "dotted", colour = col2[2], linewidth = .25) +
  geom_segment(data = tmp2, aes(x = temp[3], xend = temp[3], y = -1.5, yend = a[3]),
               linetype = "dotted", colour = col2[1], linewidth = .25) + 
  geom_text(aes(x = temp[1], y = -.25, label = "1ère hausse du tx dir."),
            colour = col2[2], vjust = 2, size = 3.5, angle = 90, family = "trade_gothic") +
  geom_text(aes(x = temp[2], y = -.25, label = "Dernière hausse du tx dir."),
            colour = col2[2], vjust = -1, size = 3.5, angle = 90, family = "trade_gothic") +
  geom_text(aes(x = temp[3], y = -.25, label = "1ère baisse du tx dir."),
            colour = col2[1], vjust = 2, size = 3.5, angle = 90, family = "trade_gothic") +
  geom_line(aes(group = stat), linewidth = 1.25) +
  scale_colour_manual(values = pal_ustat) +
  scale_y_continuous(breaks = c(0, 2, 4), labels = c("0,0%", "2,0%", "4,0%"), limits = c(-1.5, 5.8)) +
  facet_wrap(vars(stat), labeller = as_labeller(renames1)) +
  guides(colour = "none") +
  labs(title = "Indice de crédibilité de la politique monétaire",
       subtitle = "Prévisions de croissance des prix vs taux de croissance de l'IPC (en %)\n ",
       caption = "Source: Expected price, Deloitte Suisse CFO survey, Zürich; Expected price, ICC, Seco, Berne (ks_s22_1_price_exp_med_m);\nIndice des prix à la consommation, OFS, Neuchâtel",
       x = "", y = "") + 
  theme_ustat() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = col2[8], size = 18),
    plot.caption = element_text(color = col2[8], size = 12),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 18)
  )

p


# # F.2: previsione IFC tasso di crescita tra 5 anni

d2 <- d2_ %>%
  filter(structure == "ks_s23_1_price_exp_5_med_m") %>%
  select(value)

tmp <- seq(as.yearmon(t0), as.yearmon(2024 + (ultimo_mese - 1)/12), 1/12)
d2$data <- tmp

d2$stat <- "Icc"

d2 <- d2 %>%
  select(data, stat, value)

tmp <- rbind(d2, d3)

renames2 <- c("Icc" = " \nCroissance de l'IPC dans 5 ans, Enquête\nsur le climat de consommation, Seco",
              "Deloitte" = " \nCroissance de l'IPC dans 2 ans, CFO\nSurvey, Deloitte Suisse")

p2 <- tmp %>%
  filter(data >= 2017) %>%
  ggplot(aes(x = data, y = value, colour = stat)) +
  geom_path(data = tmp2, aes(x = data, y = value, colour = ""), colour = col2[8], linewidth = .8) +
  geom_segment(data = tmp2, aes(x = temp[1], xend = temp[1], y = -1.5, yend = a[1]), 
               linetype = "dotted", colour = col2[2], linewidth = .25) +
  geom_segment(data = tmp2, aes(x = temp[2], xend = temp[2], y = -1.5, yend = a[2]), 
               linetype = "dotted", colour = col2[2], linewidth = .25) +
  geom_segment(data = tmp2, aes(x = temp[3], xend = temp[3], y = -1.5, yend = a[3]),
               linetype = "dotted", colour = col2[1], linewidth = .25) + 
  geom_text(aes(x = temp[1], y = -.25, label = "1ère hausse du tx dir."),
            colour = col2[2], vjust = 2, size = 3.5, angle = 90, family = "trade_gothic") +
  geom_text(aes(x = temp[2], y = -.25, label = "Dernière hausse du tx dir."),
            colour = col2[2], vjust = -1, size = 3.5, angle = 90, family = "trade_gothic") +
  geom_text(aes(x = temp[3], y = -.25, label = "1ère baisse du tx dir."),
            colour = col2[1], vjust = 2, size = 3.5, angle = 90, family = "trade_gothic") +
  geom_line(aes(group = stat), linewidth = 1.25) +
  scale_colour_manual(values = pal_ustat) +
  scale_y_continuous(breaks = c(0, 2, 4), labels = c("0,0%", "2,0%", "4,0%"), limits = c(-1.5, 5.8)) +
  facet_wrap(vars(stat), labeller = as_labeller(renames2)) +
  guides(colour = "none") +
  labs(title = "Indice de crédibilité de la politique monétaire (2)",
       subtitle = "Prévisions de croissance des prix vs taux de croissance de l'IPC (en %)\n  ",
       caption = "Source: Expected price, Deloitte Suisse CFO survey, Zürich; Expected price, ICC, Seco, Berne (ks_s23_1_price_exp_5_med_m);\nIndice des prix à la consommation, OFS, Neuchâtel",
       x = "", y = "") + 
  theme_ustat() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = col2[8], size = 18),
    plot.caption = element_text(color = col2[8], size = 12),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 18)
  )


