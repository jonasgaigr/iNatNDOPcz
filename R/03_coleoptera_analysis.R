# Coleoptera - HDII species ----
inat_col_sitmap0 <- 
  data_col %>%
  dplyr::group_by(
    DRUH, 
    SITMAP
  ) %>%
  dplyr::reframe(
    pocet_n = n(),
    pocet_inat = sum(str_detect(ZDROJ, "iNat")),
    proc_inat = pocet_inat/pocet_n
  ) %>%
  dplyr::group_by(DRUH) %>%
  dplyr::reframe(
    celkem_sitmap_0 = n(),
    sitmap_0_plne_inat = sum(proc_inat == 1),
    procento_0_plne_inat = sitmap_0_plne_inat / celkem_sitmap_0 * 100
  ) %>%
  dplyr::arrange(
    desc(procento_0_plne_inat)
  ) 

inat_col_sitmap1 <- 
  data_col %>%
  dplyr::group_by(
    DRUH, 
    SITMAP_1
  ) %>%
  dplyr::reframe(
    pocet_n = n(),
    pocet_inat = sum(str_detect(ZDROJ, "iNat")),
    proc_inat = pocet_inat/pocet_n
  ) %>%
  dplyr::group_by(DRUH) %>%
  dplyr::reframe(
    celkem_1_sitmap = n(),
    sitmap_1_plne_inat = sum(proc_inat == 1),
    procento_1_plne_inat = sitmap_1_plne_inat / celkem_1_sitmap * 100
  ) %>%
  dplyr::arrange(
    desc(procento_1_plne_inat)
  ) 

inat_col_result <- 
  dplyr::left_join(
    inat_col_sitmap0,
    inat_col_sitmap1
  ) %>%
  dplyr::arrange(
    desc(procento_1_plne_inat),
    desc(sitmap_1_plne_inat),
    desc(celkem_1_sitmap)
  ) %>%
  dplyr::left_join(
    data_hym %>%
      dplyr::select(
        DRUH,
        REDLIST
      ) %>%
      sf::st_drop_geometry()
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct()

# Write results ----
write_csv2_win1250(
  inat_col_result,
  "Outputs/Data/inat_col_result.csv"
)

# Prepare your table
tab <- inat_col_result %>%
  # Optionally rename columns for clarity in the table
  dplyr::rename(
    Druh = DRUH,
    `Počet SITMAP 0` = celkem_sitmap_0,
    `Plně iNat (SITMAP 0)` = sitmap_0_plne_inat,
    `% plně iNat (SITMAP 0)` = procento_0_plne_inat,
    `Počet SITMAP 1` = celkem_1_sitmap,
    `Plně iNat (SITMAP 1)` = sitmap_1_plne_inat,
    `% plně iNat (SITMAP 1)` = procento_1_plne_inat,
    `Červený seznam` = REDLIST
  ) %>%
  flextable() %>%
  # Styling
  autofit() %>%
  theme_zebra() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(
    Druh = "Druh",
    `Červený seznam` = "Červený seznam"
  )

# Export to Word
doc <- read_docx() %>%
  body_add_par("Výsledky Coleoptera – iNat pokrytí", style = "heading 1") %>%
  body_add_flextable(tab) %>%
  body_add_par("", style = "Normal")

print(doc, target = "Outputs/Tables/inat_col_result.docx")
