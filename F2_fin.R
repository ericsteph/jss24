rm(list=ls())

library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(openxlsx)
library(readxl)
library(cowplot)
library(patchwork)
library(reshape2)


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
  mutate(
    mese = format(data, "%m"),
    anno = format(data, "%y"),
    m12 = rollmean(value, k = 12, fill = NA, align = "right")) %>%
  select(data, mese, anno,
         stat, value, m12) %>%
  melt(id.vars = c("data", "mese", "anno", "stat"), variable.name = "var")

tmp1 <- d1 %>%
  filter(var == "value")

tmp2 <- d1 %>%
  filter(mese %in% c("01", "04", "07", "10"),
         var == "m12")

d1 <- rbind(tmp1, tmp2)


# d2: dati della Seco, serie lunga (trimestrale)

url <- "https://www.seco.admin.ch/dam/seco/fr/dokumente/Wirtschaft/Wirtschaftslage/Konsumentenstimmung/ks_q.csv.download.csv/ks_q.csv"

d2_ <- read_csv(url)

d2 <- d2_ %>%
  filter(structure == "ks_i21_price_hist_q",
         type == "index",
         seas_adj == "csa") %>%
  select(date, value)

t0 <- 1973 - 3/12
ultimo_mese <- 7

tmp <- seq(as.yearmon(t0), as.yearmon(2024 + (ultimo_mese - 1)/12), 3/12)
d2$data <- tmp

d2$stat <- "Icc"

d2 <- d2 %>%
  select(data, stat, value)




## Figure

col2 <- RColorBrewer::brewer.pal(8, "Set2")
col2 <- c(pal_ustat[3:9], col2[8])

k <- 1/12

temp0 <- c(1987, 1989 + 6/12, 1990 + 9/12, 1992 + 9/12)
temp0 <- as.yearmon(temp0)

a0 <- tmp2 %>%
  filter(data %in%  temp0) %>%
  select(value) %>%
  as.vector()

a0 <- as.double(t(a0))


p1_ <- d1 %>%
  filter(data >= 1985,
         data < 1995) %>%
  ggplot(aes(x = data, y = value, group = var, colour = var
             # , linewidth = var
  )) +
  geom_rect(aes(xmin = temp0[2] - k, xmax = temp0[2] + k, ymin = -Inf, ymax = a0[2]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_rect(aes(xmin = temp0[4] - k, xmax = temp0[4] + k, ymin = -Inf, ymax = a0[4]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_segment(aes(x = temp0[2], xend = temp0[2], y = -Inf, yend = a0[2]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp0[4], xend = temp0[4], y = -Inf, yend = a0[4]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp0[1], xend = temp0[1], y = -Inf, yend = a0[1]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_segment(aes(x = temp0[3], xend = temp0[3], y = -Inf, yend = a0[3]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_line(aes(linewidth = var)) +
  scale_color_manual(values = c(pal_ustat[7], pal_ustat[2])) +
  scale_linewidth_manual(values = c(.5, 2)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6), labels = c("0,0%", "2,0%", "4,0%", "6,0%"),
                     limits = c(-1.5, 6.67)) +
  labs(title = "Indice des prix à la consommation (IPC)",
       subtitle = "Taux de croissance (y-to-y, en %)\n   ",
       caption = "Source: Indice des prix à la consommation, OFS, Neuchâtel",
       x = "", y = "") +
  theme_ustat() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = col2[8], size = 18),
    plot.caption = element_text(color = col2[8], size = 12),
    axis.text = element_text(size = 12)
  ) +
  guides(colour = "none", linewidth = "none")


b0 <- d2 %>%
  filter(data %in% temp0) %>%
  select(value) %>%
  as.vector()

b0 <- as.double(t(b0))

p2_ <- d2 %>%
  filter(data >= 1985,
         data < 1995) %>%
  ggplot(aes(x = data, y = value, colour = stat)) +
  geom_rect(aes(xmin = temp0[2] - k, xmax = temp0[2] + k, ymin = -Inf, ymax = b0[2]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_rect(aes(xmin = temp0[4] - k, xmax = temp0[4] + k, ymin = -Inf, ymax = b0[4]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_segment(aes(x = temp0[2], xend = temp0[2], y = -Inf, yend = b0[2]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp0[4], xend = temp0[4], y = -Inf, yend = b0[4]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp0[1], xend = temp0[1], y = -Inf, yend = b0[1]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_segment(aes(x = temp0[3], xend = temp0[3], y = -Inf, yend = b0[3]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_line(color = pal_ustat[1], linewidth = 1.5) +
  scale_y_continuous(breaks = c(0, 60, 120, 180), labels = c("0", "60", "120", "180"),
                     limits = c(-45, 201)) +
  labs(title = "Perception de l'évolution des prix",
       subtitle = "Qu: Comment les prix ont-ils évolué au cours\ndes 12 derniers mois?",
       caption = "Source: Past price situation, ICC, Seco, Berne (ks_i21_price_hist_q)",
       x = "", y = "") +
  theme_ustat() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = col2[8], size = 18),
    plot.caption = element_text(color = col2[8], size = 12),
    axis.text = element_text(size = 12)
  ) +
  guides(colour = "none", linewidth = "none")


p0 <- p2_ + p1_



###


temp <- c(2015 + 6/12, 2018 + 9/12, 2021, 2023 + 9/12)
temp <- as.yearmon(temp)

a <- tmp2 %>%
  filter(data %in%  temp) %>%
  select(value) %>%
  as.vector()

a <- as.double(t(a))


p1 <- d1 %>%
  filter(data >= 2014) %>%
  ggplot(aes(x = data, y = value, group = var, colour = var
             # , linewidth = var
             )) +
  geom_rect(aes(xmin = temp[2] - k, xmax = temp[2] + k, ymin = -Inf, ymax = a[2]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_rect(aes(xmin = temp[4] - k, xmax = temp[4] + k, ymin = -Inf, ymax = a[4]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_segment(aes(x = temp[2], xend = temp[2], y = -Inf, yend = a[2]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp[4], xend = temp[4], y = -Inf, yend = a[4]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp[1], xend = temp[1], y = -Inf, yend = a[1]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_segment(aes(x = temp[3], xend = temp[3], y = -Inf, yend = a[3]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_line(aes(linewidth = var)) +
  scale_color_manual(values = c(pal_ustat[7], pal_ustat[2])) +
  scale_linewidth_manual(values = c(.5, 1.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6), labels = c("0,0%", "2,0%", "4,0%", "6,0%"),
                     limits = c(-1.5, 6.7)) +
  labs(title = "Indice des prix à la consommation (IPC)",
       subtitle = "Taux de croissance (y-to-y, en %)\n   ",
       caption = "Source: Indice des prix à la consommation, OFS, Neuchâtel",
       x = "", y = "") +
  theme_ustat() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = col2[8], size = 18),
    plot.caption = element_text(color = col2[8], size = 12),
    axis.text = element_text(size = 12)
  ) +
  guides(colour = "none", linewidth = "none")



b <- d2 %>%
  filter(data %in% temp) %>%
  select(value) %>%
  as.vector()

b <- as.double(t(b))

p2 <- d2 %>%
  filter(data >= 2014) %>%
  ggplot(aes(x = data, y = value, colour = stat)) +
  geom_rect(aes(xmin = temp[2] - k, xmax = temp[2] + k, ymin = -Inf, ymax = b[2]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_rect(aes(xmin = temp[4] - k, xmax = temp[4] + k, ymin = -Inf, ymax = b[4]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_segment(aes(x = temp[2], xend = temp[2], y = -Inf, yend = b[2]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp[4], xend = temp[4], y = -Inf, yend = b[4]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp[1], xend = temp[1], y = -Inf, yend = b[1]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_segment(aes(x = temp[3], xend = temp[3], y = -Inf, yend = b[3]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_line(color = pal_ustat[1], size = 1.5) +
  scale_y_continuous(breaks = c(0, 60, 120, 180), labels = c("0", "60", "120", "180"),
                     limits = c(-45, 201)) +
  labs(title = "Perception de l'évolution des prix",
       subtitle = "Qu: Comment les prix ont-ils évolué au cours\ndes 12 derniers mois?",
       caption = "Source: Past price situation, ICC, Seco, Berne (ks_i21_price_hist_q)",
       x = "", y = "") +
  theme_ustat() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = col2[8], size = 18),
    plot.caption = element_text(color = col2[8], size = 12),
    axis.text = element_text(size = 12)
  ) +
  guides(colour = "none", linewidth = "none")



g <- p2 + p1



## Ho fatto le figure anche con le attese sui prezzi per i prossimi mesi, ma sono meno interessanti

# d3 <- d2_ %>%
#   filter(structure == "ks_i22_price_exp_q",
#          type == "index",
#          seas_adj == "csa") %>%
#   select(value)
# 
# t0 <- 1973 - 1/12
# ultimo_mese <- 6
# 
# tmp <- seq(as.yearmon(t0), as.yearmon(2024 + (ultimo_mese - 1)/12), 3/12)
# d3$data <- tmp
# 
# d3$stat <- "Icc"
# 
# d3 <- d3 %>%
#   select(data, stat, value)

