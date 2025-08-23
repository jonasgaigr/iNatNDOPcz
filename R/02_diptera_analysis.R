# Hymenoptera - all species ----
inat_dip_sitmap0 <- 
  data_dip %>%
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

inat_dip_sitmap1 <- 
  data_dip %>%
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

inat_dip_result <- 
  dplyr::left_join(
    inat_dip_sitmap0,
    inat_dip_sitmap1
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
  inat_dip_result,
  "Outputs/Data/inat_dip_result.csv"
)
