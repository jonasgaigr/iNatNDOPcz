# Data - all species ----
inat_sitmap0 <- 
  data %>%
  dplyr::group_by(
    KAT_TAX,
    DRUH, 
    SITMAP
  ) %>%
  dplyr::reframe(
    pocet_n = n(),
    pocet_inat = sum(str_detect(ZDROJ, "iNat")),
    proc_inat = pocet_inat/pocet_n
    ) %>%
  dplyr::group_by(
    DRUH
    ) %>%
  dplyr::reframe(
    celkem_sitmap_0 = n(),
    sitmap_0_plne_inat = sum(proc_inat == 1),
    procento_0_plne_inat = sitmap_0_plne_inat / celkem_sitmap_0 * 100
  ) %>%
  dplyr::arrange(
    desc(procento_0_plne_inat)
  ) 

inat_sitmap1 <- 
  data %>%
  dplyr::group_by(
    KAT_TAX,
    DRUH, 
    SITMAP_1
    ) %>%
  dplyr::reframe(
    pocet_n = n(),
    pocet_inat = sum(str_detect(ZDROJ, "iNat")),
    proc_inat = pocet_inat/pocet_n
    ) %>%
  dplyr::group_by(
    DRUH
    ) %>%
  dplyr::reframe(
    celkem_1_sitmap = n(),
    sitmap_1_plne_inat = sum(proc_inat == 1),
    procento_1_plne_inat = sitmap_1_plne_inat / celkem_1_sitmap * 100
  ) %>%
  dplyr::arrange(
    desc(procento_1_plne_inat)
    ) 

inat_result <- 
  dplyr::left_join(
    inat_sitmap0,
    inat_sitmap1
  ) %>%
  dplyr::arrange(
    desc(procento_1_plne_inat),
    desc(sitmap_1_plne_inat),
    desc(celkem_1_sitmap)
  ) %>%
  dplyr::left_join(
    data %>%
      dplyr::select(
        DRUH,
        KAT_TAX,
        REDLIST
      ) %>%
      sf::st_drop_geometry()
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct()

# Data - Red list species ----
inat_rl_sitmap0 <-
  data %>%
  dplyr::filter(
    is.na(REDLIST) == FALSE
  ) %>%
  dplyr::group_by(
    KAT_TAX,
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

inat_rl_sitmap1 <-
  data %>%
  dplyr::filter(
    is.na(REDLIST) == FALSE
  ) %>%
  dplyr::group_by(
    KAT_TAX,
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

inat_rl_result <- 
  dplyr::left_join(
  inat_rl_sitmap0,
  inat_rl_sitmap1
) %>%
  dplyr::arrange(
    desc(procento_1_plne_inat)
  ) %>%
  dplyr::left_join(
    data %>%
      dplyr::select(
        DRUH,
        KAT_TAX,
        REDLIST
      ) %>%
      sf::st_drop_geometry()
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct()

# Compare Red List and LC/NE taxa
ggplot(
  data = inat_result, 
  aes(
    x = KAT_TAX, 
    y = procento_1_plne_inat,
    colour = REDLIST)
  ) +
  geom_boxplot()
  

# Write results ----
write_csv2_win1250(
  inat_hym_result,
  "Outputs/Data/inat_hym_result.csv"
)

write_csv2_win1250(
  inat_hym_rl_result,
  "Outputs/Data/inat_hym_rl_result.csv"
)

