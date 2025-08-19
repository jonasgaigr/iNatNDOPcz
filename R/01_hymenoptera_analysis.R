data_hym %>%
  dplyr::group_by(DRUH, SITMAP) %>%
  dplyr::reframe(pocet_n = n(),
                 pocet_inat = sum(str_detect(ZDROJ, "iNat")),
                 proc_inat = pocet_inat/pocet_n) %>%
  dplyr::group_by(DRUH) %>%
  dplyr::reframe(
    celkem_sitmap = n(),
    sitmap_plne_inat = sum(proc_inat == 1),
    procento_plne_inat = sitmap_plne_inat / celkem_sitmap
  ) %>%
  dplyr::arrange(-procento_plne_inat) 

data_hym_rl %>%
  dplyr::group_by(DRUH, SITMAP) %>%
  dplyr::reframe(
    pocet_n = n(),
    pocet_inat = sum(str_detect(ZDROJ, "iNat")),
    proc_inat = pocet_inat/pocet_n
    ) %>%
  dplyr::group_by(DRUH) %>%
  dplyr::reframe(
    celkem_sitmap = n(),
    sitmap_plne_inat = sum(proc_inat == 1),
    procento_plne_inat = sitmap_plne_inat / celkem_sitmap
  ) %>%
dplyr::arrange(-procento_plne_inat) 

