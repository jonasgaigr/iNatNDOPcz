# Compare Red List and LC/NE taxa
ggplot(
  data = inat_result, 
  aes(
    x = KAT_TAX, 
    y = procento_1_plne_inat,
    colour = REDLIST)
) +
  geom_boxplot()