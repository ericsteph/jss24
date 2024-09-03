
library(kableExtra)
library(magrittr)
library(webshot)
library(themeustat)

r0 <- c("Salaires", "Prix", "Chômage")
rownames <- c(" ", "Tableaux", "Cubes de données", "Articles")

r1 <- c("4", "4", "3 (9)")
r2 <- c("3", "-", "-")
r3 <- c("7", "3", "1")

t <- rbind(r0, r1, r2, r3)


# colnames(t) <- colnames
rownames(t) <- rownames

t <- as.data.frame(t(t))

t1 <- knitr::kable(t,
                   align = c("l", "c", "c", "c"),
                   booktabs = FALSE,
                   escape = FALSE,
                   family = "trade_gothic",
                   caption = "T1. Offre USTAT (2020-2024), selon différentes thèmes"
                   ) |>
  kableExtra::kable_classic(full_width = T
                            # ,
                            # html_font = "Calibri"
                            ) |>
  kableExtra::kable_styling(position = "left") |>
  kableExtra::pack_rows(index = c("Thème" = 3)) |>
  kableExtra::column_spec(1, italic = T
  ) 

  # as_image(file = "immagini/t1.jpg")
  # save_kable(file = "immagini/t1.png", zoom = 3)


r1 <- c("< 30", "<1000**", "< 300 (< 150)***")
r2 <- c("< 30", "-", "-")
r3 <- c("< 100*", "30", "-")

t2 <- rbind(r0, r1, r2, r3)
rownames(t2) <- rownames

t2 <- as.data.frame(t(t2))

t2 <- knitr::kable(t2,
                   align = c("l", "c", "c", "c"),
                   booktabs = FALSE,
                   escape = FALSE,
                   family = "trade_gothic",
                   caption = "T2. Visualisation (ou téléchargement | depuis juillet 2023)"
                   ) |>
  kableExtra::kable_classic(full_width = T
                            # ,
                            # html_font = "Calibri"
                            ) |>
  kableExtra::kable_styling(position = "left") |>
  kableExtra::pack_rows(index = c("Thème" = 3)) |>
  kableExtra::column_spec(1, italic = T
  ) |>
  kableExtra::footnote(
                       general_title = "Remarques",
                       general = c("*  Articles salaires: maximum de ~30 dwnl. / jour",
                                   "** Tableaux prix: moyenne de ~60 dwnl. / mois",
                                   "*** Tableux chômage: résultats au sens de l'ILO (vs résultats Seco)"
                                   
                                   )
                       )


