pacman::p_load(ggplot2)

results <- results |>
  mutate(TIPOPROCESOSELECCION_CAT = factor(TIPOPROCESOSELECCION,
                                           levels = c("Adjudicación Simplificada",
                                                      "Comparación de Precios",
                                                      "Contratación Directa",
                                                      "Contratación Internacional",
                                                      "Licitación Pública",
                                                      "Subasta Inversa Electrónica"),
                                          labels = c("Simplified Selection",
                                                     "Price Comparison",
                                                     "Direct Contract",
                                                     "International Contract",
                                                     "Public Bidding",
                                                     "Electronic Reverse Auction")))
table(results$grupo_oncologico)

results <- results |>
  mutate(moquegua = case_when(str_detect(ENTIDADCONTRATANTE, "MOQUEGUA") ~ 1,
                              TRUE ~ 0))

results |>
  filter(moquegua == 1) |>
  View()

results |>
  filter(grupo_oncologico != "Resto no oncológico") |>
  ggplot(aes(x=TIPOPROCESOSELECCION_CAT,y=DIAS_HASTA_CONTRATO)) + 
  geom_boxplot(fill='steelblue') +
  ylab("Days until contract signing") +
  xlab("") +
  coord_flip()

results |>
  filter(grupo_diabetes == 1) |>
  filter(moquegua == 1) |> 
  ggplot(aes(x=TIPOPROCESOSELECCION_CAT,y=DIAS_HASTA_CONTRATO)) + 
  geom_boxplot(fill='steelblue') +
  ylab("Days until contract signing") +
  xlab("") +
  coord_flip()

results |>
  ggplot(aes(x = DIAS_HASTA_FIN, y = MONTOCONTRATADOITEM)) +
  geom_point()

results |>
  filter(grupo_diabetes == 1) |>
  ggplot(aes(x=TIPOPROCESOSELECCION_CAT,y=DIAS_HASTA_FIN)) + 
  geom_boxplot(fill='steelblue') +
  ylab("Days until contract effective end date") +
  xlab("") +
  coord_flip()


results |>
  mutate(MONTOCONTRATADOITEM = as.numeric(MONTOCONTRATADOITEM)) |>
  filter(grupo_oncologico != "Resto no oncológico") |>
  ggplot(aes(x = MONTOCONTRATADOITEM, y = DIAS_HASTA_CONTRATO)) +
  geom_point()



