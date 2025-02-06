

by <- join_by(CODIGOCONVOCATORIA, NITEM == NUMITEM)
results <- dt_conv_busq |>
  filter(GRUPO == "Medicamentos y productos farmacéuticos") |>
  filter(str_detect(ITEMCUBSO, "VACUNA", negate = TRUE)) |>
  # filter(grupo_diabetes == 1 | grupo_oncologico != "Resto no oncológico") |>
  # filter(grupo_oncologico != "Resto no oncológico") |>
  filter(ULTIMO == "ULTIMO") |>
  select(CODIGOCONVOCATORIA, ENTIDAD, NITEM, MONTOREFERENCIALITEM, ITEMCUBSO, DESCRIPCIONITEM, TIPOPROCESOSELECCION, TIPOCOMPRA, TIPOENTIDAD, TIPOPROVEEDOR, GRUPO, FECHACONVOCATORIA, grupo_diabetes, grupo_oncologico) |>
  left_join(dt_contratos_clean |>
              select(CODIGOCONVOCATORIA, RUCENTIDAD, NUMITEM, ENTIDADCONTRATANTE, MONTOREFERENCIALTOTAL, MONTOCONTRATADOTOTAL, MONTOCONTRATADOITEM, FECHASUSCRIPCIONCONTRATO,FECHAVIGENCIAINICIAL,FECHAVIGENCIAFINAL,FECHAVIGENCIAFINACTUALIZADA), by,
            relationship = "many-to-many") |>
  filter(is.na(RUCENTIDAD) == F) |>
  mutate(ENTIDAD_COMPRA = case_when(str_detect(ENTIDAD, "CENTRO NACIONAL DE ABASTECIMIENTO") ~ "CENARES",
                                    str_detect(ENTIDAD, "REGIONAL") ~ "INSTITUTO/HOSPITAL - GR",
                                    str_detect(ENTIDAD, "HOSPITAL") |
                                      str_detect(ENTIDAD, "INST") |
                                      str_detect(ENTIDAD, "REDES INTEGRADAS") |
                                      str_detect(ENTIDAD, "MINISTERIO DE SALUD") ~ "INSTITUTO/HOSPITAL - GN",
                                    str_detect(ENTIDAD, "SEGURO SOCIAL") ~ "ESSALUD",
                                    str_detect(ENTIDAD, "POLICÍA") |
                                      str_detect(ENTIDAD, "EJERCITO") |
                                      str_detect(ENTIDAD, "FUERZA AEREA") |
                                      str_detect(ENTIDAD, "MARINA") ~ "SANIDAD PNP",
                                    TRUE ~ "OTROS"
  ),
  ANO_CONVOCATORIA = format(FECHACONVOCATORIA, "%Y"),
  ANO_CONTRATO = format(FECHASUSCRIPCIONCONTRATO, "%Y"),
  ANO_VIGENCIAFIN = format(FECHAVIGENCIAFINACTUALIZADA, "%Y"),
  DIAS_HASTA_CONTRATO = as.integer(difftime(FECHASUSCRIPCIONCONTRATO,FECHACONVOCATORIA, units = "days")),
  DIAS_HASTA_INICIO = as.integer(difftime(FECHAVIGENCIAINICIAL,FECHACONVOCATORIA, units = "days")),
  DIAS_HASTA_F = as.integer(difftime(FECHAVIGENCIAFINAL,FECHACONVOCATORIA, units = "days")),
  DIAS_HASTA_FIN = as.integer(difftime(FECHAVIGENCIAFINACTUALIZADA,FECHACONVOCATORIA, units = "days")),
  across(starts_with("DIAS_HASTA"), 
         ~ case_when(. < 0 ~ 0,
                     TRUE ~ .)),
  PLAZO_HASTA_INICIO = DIAS_HASTA_INICIO - DIAS_HASTA_CONTRATO,
  PLAZO_HASTA_F = DIAS_HASTA_F - DIAS_HASTA_INICIO,
  PLAZO_HASTA_FIN = DIAS_HASTA_FIN - DIAS_HASTA_F,
  across(starts_with("PLAZO_HASTA"), 
         ~ case_when(. < 0 ~ 0,
                     TRUE ~ .))
  ) |>
  filter(str_detect(ENTIDAD_COMPRA, "ESSALUD", negate = TRUE))


write.table(results, file = here::here("output","export.txt"), sep = "|", row.names = FALSE, )


results |>
  filter(ANO_CONVOCATORIA == 2022) |>
  filter(ENTIDAD_COMPRA == "OTROS") |>
  group_by(ENTIDAD) |>
  summarise(sum = sum(MONTOCONTRATADOITEM)) |>
  View()


  

