# Compare Red List and LC/NE taxa
ggplot(
  data = inat_result, 
  aes(
    x = REDLIST, 
    y = procento_1_plne_inat,
    colour = KAT_TAX)
) +
  geom_boxplot()


inat_result %>%
  dplyr::select(
    DRUH, 
    REDLIST,
    procento_0_plne_inat
    ) %>%
  dplyr::filter(
    procento_0_plne_inat > 0
  ) %>%
  pivot_longer(
    cols = starts_with("procento"),
    names_to = "level",
    values_to = "percent"
  ) %>%
  mutate(
    level = ifelse(level == "procento_0_plne_inat", "SITMAP")
    ) %>%
  ggplot(aes(x = reorder(DRUH, percent), y = percent, fill = REDLIST)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    x = "Species",
    y = "% of SITMAP fully iNat",
    fill = "Level"
  ) +
  theme_minimal()

ggplot(inat_result, aes(x = procento_0_plne_inat, y = procento_1_plne_inat)) +
  geom_point(aes(size = celkem_1_sitmap, color = KAT_TAX), alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "% fully iNat (SITMAP)",
    y = "% fully iNat (SITMAP_1)",
    color = "Taxon",
    size = "N SITMAP_1"
  ) +
  theme_minimal()

