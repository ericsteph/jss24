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


## fare il grafico coi dati IFC: sicurezza del posto di lavoro vs crescita tx diso (iin p.p.)
# dal 1991


###############################

# 3.1 Sécurité de l’emploi

######################################

# Question : Selon vous, comment la sécurité de l’emploi a-t-elle évolué au cours des 12 der-niers mois ? 
# Elle… Réponses : est nettement plus sûre (+2) /
# est un peu plus sûre (+1) / est restée à peu près inchangée (0) /
# est un peu plus incertaine (−1) / est nettement plus incertaine (−2)



# Carico i dati

# d1: dati diso Seco

t0 <- 2002
ultimo_mese <- 7

d1 <- readRDS(file = "dati/10_disoSECO_mesi.rds", refhook = TRUE)

d1 <- d1 %>%
  dplyr::filter(Misura_spec == "Tasso di disoccupazione Seco, val. mensili (in percentuale)",
                Grande.regione == "Svizzera") %>%
  distinct() %>%
  select(Mese, value)

rownames(d1) <- 1:nrow(d1)

d1$stat <- "disoSECO"

tmp1 <- data.frame(2024 + 5/12, 2.3, "disoSECO")
tmp2 <- data.frame(2024 + 6/12, 2.3, "disoSECO")

tmp1[1] <- as.yearmon(tmp1[1], format = "%m %Y")
tmp2[1] <- as.yearmon(tmp2[1], format = "%m %Y")

colnames(tmp1) <- colnames(tmp2) <- colnames(d1)

d1 <- rbind(d1, tmp1, tmp2)

# d1 <- d1 %>%
#   mutate(tmp = lag(value, n = 4L),
#          value = value - tmp)

d1 <- d1 %>%
  mutate(m12 = rollmean(value, k = 12, fill = NA, align = "right"),
         d12 = m12 - lag(m12, n = 1L),
         d12_ = value - lag(value, n = 1L),
         cat = if_else(m12 >= 3.375, "1",
                       if_else(value < 2.625, "2", "0")),
         cat2 = if_else(d12 > .15, "1",
                        if_else(d12 < -.15, "2", "0")))

tmp <- seq(as.yearmon(t0), as.yearmon(2024 + (ultimo_mese - 1)/12), 1/12)
d1$data <- tmp

d1 <- d1 %>%
  mutate(
    mese = format(data, "%m"),
    anno = format(data, "%y")) %>%
  select(data, mese, anno, stat, cat, cat2, value, m12, d12, d12_) %>%
  melt(id.vars = c("data", "mese", "anno", "stat"), variable.name = "var")

d1$value <- as.double(d1$value)

# d2: dati della Seco, serie lunga (trimestrale)

url <- "https://www.seco.admin.ch/dam/seco/fr/dokumente/Wirtschaft/Wirtschaftslage/Konsumentenstimmung/ks_q.csv.download.csv/ks_q.csv"
d2_ <- read_csv(url)

d2 <- d2_ %>%
  filter(structure %in% c("ks_i32_unemp_exp_q", "ks_i31_job_secure_q"),
         type == "index",
         seas_adj == "csa") %>%
  mutate(data = as.yearmon(date),
         mese = format(data, "%m"),
         anno = format(data, "%y"),
         stat = "icc") %>%
  select(data, mese, anno, structure, stat, value)


tmp1 <- d2 %>%
  filter(structure == "ks_i31_job_secure_q") %>%
  mutate(value = -value)

tmp2 <- d2 %>%
  filter(structure != "ks_i31_job_secure_q") 

d2 <- rbind(tmp1, tmp2)



## Figure

  col2 <- RColorBrewer::brewer.pal(8, "Set2")
  col2 <- c(pal_ustat[3:9], col2[8])
  
  # temp <- c(2005 + 3/12, 2010 + 3/12, 2016 + 3/12, 2021 + 3/12, 2020, )
  temp <- c(2016 + 3/12, 2019, 2021, 2023)
  temp <- as.yearmon(temp)
  
  a <- d1 %>%
    filter(data %in%  temp, var == "m12") %>%
    select(value) %>%
    as.vector()
  
  a <- as.double(t(a))
  
  
  p1 <- d1 %>%
    filter(data >= 2015,
           var %in% c("m12", "value")) %>%
    ggplot(aes(x = data, y = value, group = var, colour = var)) +
    geom_rect(aes(xmin = temp[1] - 1/12, xmax = temp[1] + 1/12, ymin = -Inf, ymax = a[1]),
              fill = col2[8], alpha = 0.05, colour = col2[8]) +
    geom_rect(aes(xmin = temp[3] - 1/12, xmax = temp[3] + 1/12, ymin = -Inf, ymax = a[3]),
              fill = col2[8], alpha = 0.05, colour = col2[8]) +
    geom_segment(aes(x = temp[1], xend = temp[1], y = -Inf, yend = a[1]),
                 linetype = "dotted", colour = "white", linewidth = .75) +
    geom_segment(aes(x = temp[3], xend = temp[3], y = -Inf, yend = a[3]),
                 linetype = "dotted", colour = "white", linewidth = .75) +
    geom_segment(aes(x = temp[2], xend = temp[2], y = -Inf, yend = a[2]),
                 linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
    geom_segment(aes(x = temp[4], xend = temp[4], y = -Inf, yend = a[4]),
                 linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
    geom_line(aes(linewidth = var)) +
    scale_color_manual(values = c(pal_ustat[7], pal_ustat[2])) +
    scale_linewidth_manual(values = c(.5, 1.5)) +
    scale_y_continuous(breaks = c(1, 2, 3),
                       labels = c("1.0%", "2.0%", "3.0%"),
                       limits = c((1 - 5/6), (3 + 5/6))) +
    labs(title = "Taux de chômage (SECO)",
         subtitle = "Moyenne mobile (12 dernières mois, en %)\n   ",
         caption = "Source: Statistique du chômage des inscripts aux ORC, SECO, Berne",
         x = "", y = "") +
    theme_ustat() +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(color = col2[8], size = 18),
      plot.caption = element_text(color = col2[8], size = 12),
      axis.text = element_text(size = 12)
    ) +
    guides(colour = "none", linewidth = "none")
  
  
  a2 <- d1 %>%
    filter(data %in%  temp, var == "d12") %>%
    select(value) %>%
    as.vector()
  
  a2 <- as.double(t(a2))
  
  p1_ <- d1 %>%
    filter(data >= 2015,
           var %in% c("d12", "d12_")) %>%
    ggplot(aes(x = data, y = value, group = var, colour = var)) +
    geom_rect(aes(xmin = temp[1] - 1/12, xmax = temp[1] + 1/12, ymin = -Inf, ymax = a2[1]),
              fill = col2[8], alpha = 0.05, colour = col2[8]) +
    geom_rect(aes(xmin = temp[3] - 1/12, xmax = temp[3] + 1/12, ymin = -Inf, ymax = a2[3]),
              fill = col2[8], alpha = 0.05, colour = col2[8]) +
    geom_segment(aes(x = temp[1], xend = temp[1], y = -Inf, yend = a2[1]),
                 linetype = "dotted", colour = "white", linewidth = .75) +
    geom_segment(aes(x = temp[3], xend = temp[3], y = -Inf, yend = a2[3]),
                 linetype = "dotted", colour = "white", linewidth = .75) +
    geom_segment(aes(x = temp[2], xend = temp[2], y = -Inf, yend = a2[2]),
                 linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
    geom_segment(aes(x = temp[4], xend = temp[4], y = -Inf, yend = a2[4]),
                 linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
    geom_line(aes(linewidth = var)) +
    scale_color_manual(values = c(pal_ustat[2], pal_ustat[7])) +
    scale_linewidth_manual(values = c(1.5, .5)) +
    scale_y_continuous(breaks = c(-.5, 0, .5),
                       labels = c("-0.5 p.p.", "0.0 p.p.", "+0.5 p.p."),
                       limits = c((-.5 - 5/6 * .1), (.5 + 5/6 * .1))) +
    labs(title = "Taux de chômage de la SECO",
         subtitle = "Delta moyenne mobile (m-to-m, en p.p.)",
         caption = "Source: Statistique du chômage des inscripts aux ORC, SECO, Berne",
         x = "", y = "") +
    theme_ustat() +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(color = col2[8], size = 18),
      plot.caption = element_text(color = col2[8], size = 12),
      axis.text = element_text(size = 12)
    ) +
    guides(colour = "none", linewidth = "none")

b <- tmp1 %>%
  filter(data %in% temp) %>%
  select(value) %>%
  as.vector()

b <- as.double(t(b))

p2 <- tmp1 %>%
  filter(data >= 2015) %>%
  ggplot(aes(x = data, y = value, group = structure)) +
  geom_rect(aes(xmin = temp[1] - 1/12, xmax = temp[1] + 1/12, ymin = -Inf, ymax = b[1]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_rect(aes(xmin = temp[3] - 1/12, xmax = temp[3] + 1/12, ymin = -Inf, ymax = b[3]),
            fill = col2[8], alpha = 0.05, colour = col2[8]) +
  geom_segment(aes(x = temp[1], xend = temp[1], y = -Inf, yend = b[1]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp[3], xend = temp[3], y = -Inf, yend = b[3]),
               linetype = "dotted", colour = "white", linewidth = .75) +
  geom_segment(aes(x = temp[2], xend = temp[2], y = -Inf, yend = b[2]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_segment(aes(x = temp[4], xend = temp[4], y = -Inf, yend = b[4]),
               linetype = "dotted", colour = pal_ustat[3], linewidth = .75) +
  geom_line(aes(colour = stat), linewidth = 1.5) +
  scale_color_manual(values = pal_ustat[1]) +
  scale_y_continuous(breaks = c(0, 60, 120), labels = c("0", "60", "120"),
                     limits = c(-50, 170)) +
  labs(title = "Sécurité de l'emploi",
       subtitle = "Qu: comment la sécurité de l'emploi a-t-elle évolué\nau cours des 12 derniers mois",
       caption = "Source: Job security situation, ICC, Seco, Berne (ks_i31_job_secure_q)",
       x = "", y = "") +
  theme_ustat() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = col2[8], size = 18),
    plot.caption = element_text(color = col2[8], size = 12),
    axis.text = element_text(size = 12)
  ) +
  guides(colour = "none")

v <- p2 + p1
v2 <- p2 + p1_


###