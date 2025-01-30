

by <- join_by(CODIGOCONVOCATORIA, NITEM == NUMITEM)
results <- dt_conv_busq |>
  filter(grupo_diabetes == 1 | grupo_oncologico != "Resto no oncológico") |>
  # filter(grupo_oncologico != "Resto no oncológico") |>
  filter(ULTIMO == "ULTIMO") |>
  select(CODIGOCONVOCATORIA, ENTIDAD, NITEM, MONTOREFERENCIALITEM, ITEMCUBSO, DESCRIPCIONITEM, TIPOPROCESOSELECCION, TIPOCOMPRA, TIPOENTIDAD, TIPOPROVEEDOR, GRUPO, FECHACONVOCATORIA, grupo_diabetes, grupo_oncologico) |>
  left_join(dt_contratos_clean |>
              select(CODIGOCONVOCATORIA, RUCENTIDAD, NUMITEM, ENTIDADCONTRATANTE, MONTOREFERENCIALTOTAL, MONTOCONTRATADOTOTAL, MONTOCONTRATADOITEM, FECHASUSCRIPCIONCONTRATO,FECHAVIGENCIAFINACTUALIZADA), by) |>
  filter(is.na(RUCENTIDAD) == F) |>
  mutate(ENTIDAD_COMPRA = case_when(str_detect(ENTIDAD, "CENTRO NACIONAL DE ABASTECIMIENTO") ~ "CENARES",
                                    str_detect(ENTIDAD, "REGIONAL") ~ "INSTITUTO/HOSPITAL - GR",
                                    str_detect(ENTIDAD, "HOSPITAL") |
                                      str_detect(ENTIDAD, "INST") ~ "INSTITUTO/HOSPITAL - GN",
                                    str_detect(ENTIDAD, "SEGURO SOCIAL") ~ "ESSALUD",
                                    str_detect(ENTIDAD, "POLICÍA") ~ "SANIDAD PNP",
                                    TRUE ~ "OTROS"
  ),
  ANO_CONVOCATORIA = format(FECHACONVOCATORIA, "%Y"),
  ANO_CONTRATO = format(FECHASUSCRIPCIONCONTRATO, "%Y"),
  ANO_VIGENCIAFIN = format(FECHAVIGENCIAFINACTUALIZADA, "%Y"),
  DIAS_HASTA_CONTRATO = as.integer(difftime(FECHASUSCRIPCIONCONTRATO,FECHACONVOCATORIA, units = "days")),
  DIAS_HASTA_FIN = as.integer(difftime(FECHAVIGENCIAFINACTUALIZADA,FECHACONVOCATORIA, units = "days")),
  across(starts_with("DIAS_HASTA"), 
         ~ case_when(. < 0 ~ 0,
                     TRUE ~ .))
  )

table(results$DIAS_HASTA_FIN, exclude = NULL)
write.table(results, file = here::here("output","export.txt"), sep = "|", row.names = FALSE)



  

