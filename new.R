pacman::p_load(stringr,
               gtsummary,dplyr)

dt_conv_busq <- dt_convocatorias |>
  mutate(grupo_oncologico = case_when(str_detect(ITEMCUBSO, "CISPLATINO") ~ "QUIMIOTERAPEÚTICO",
                                    str_detect(ITEMCUBSO, "DOXORR?UBICINA") ~ "QUIMIOTERAPEÚTICO",
                                    str_detect(ITEMCUBSO, "PACLITAXEL") ~ "QUIMIOTERAPEÚTICO",
                                    str_detect(ITEMCUBSO, "DOCETAXEL") ~ "QUIMIOTERAPEÚTICO",
                                    str_detect(ITEMCUBSO, "METOTREXATO") ~ "QUIMIOTERAPEÚTICO",
                                    str_detect(ITEMCUBSO, "5-FL?U") ~ "QUIMIOTERAPEÚTICO",
                                    str_detect(ITEMCUBSO, "GEMCITABINA") ~ "QUIMIOTERAPEÚTICO",
                                    str_detect(ITEMCUBSO, "TRASTUZUMAB") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "HERCEPTIN") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "ERLOTINIB") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "TARCVEA") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "IMATINIB") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "GLEEVEC") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "BEVACIZUMAB") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "AVASTIN") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "RITUXIMAB") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "RITUXAN") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "LAPATINIB") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "TYKERB") ~ "TERAPIADIRIGIDA",
                                    str_detect(ITEMCUBSO, "PEMBROLIZUMAB") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "KEYTRUDA") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "NIVOLUMAB") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "OPDIVO") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "IPILIMUMAB") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "YERVOY") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "ATEZOLIZUMAB") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "TECENTRIQ") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "DURVALUMAB") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "IMFINZI") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "AVELUMAB") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "BAVENCIO") ~ "INMUNOTERAPIA",
                                    str_detect(ITEMCUBSO, "TAMOXIFENO") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "ANASTROZOL") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "ARIMIDEX") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "LETROZOL") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "FEMARA") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "LEUPROLIDA") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "LUPRON") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "FULVESTRANT") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "FASLODEX") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "ABIRATERONA") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "ZYTIGA") ~ "HORMONOTERAPIA",
                                    str_detect(ITEMCUBSO, "VEMURAFENIB") ~ "INHIBIDORBRAF",
                                    str_detect(ITEMCUBSO, "ZELBORAF") ~ "INHIBIDORBRAF",
                                    str_detect(ITEMCUBSO, "DABRAFENIB") ~ "INHIBIDORBRAF",
                                    str_detect(ITEMCUBSO, "TAFINLAR") ~ "INHIBIDORBRAF",
                                    str_detect(ITEMCUBSO, "ENCORAFENIB") ~ "INHIBIDORBRAF",
                                    str_detect(ITEMCUBSO, "BRAFTOVI") ~ "INHIBIDORBRAF",
                                    str_detect(ITEMCUBSO, "CETUXIMAB") ~ "AMONOCLONAL",
                                    str_detect(ITEMCUBSO, "OBINUTUZUMAB") ~ "AMONOCLONAL",
                                    str_detect(ITEMCUBSO, "GAZYVA") ~ "AMONOCLONAL",
                                    str_detect(ITEMCUBSO, "ALEMTUZUMAB") ~ "AMONOCLONAL",
                                    str_detect(ITEMCUBSO, "CAMPATH") ~ "AMONOCLONAL",
                                    str_detect(ITEMCUBSO, "PERTUZUMAB") ~ "AMONOCLONAL",
                                    str_detect(ITEMCUBSO, "PERJETA") ~ "AMONOCLONAL",
                                    str_detect(ITEMCUBSO, "IFOSFAMIDA") ~ "AALQUILANTE",
                                    str_detect(ITEMCUBSO, "MELPHALAN") ~ "AALQUILANTE",
                                    str_detect(ITEMCUBSO, "BUSULFAN") ~ "AALQUILANTE",
                                    str_detect(ITEMCUBSO, "VINCRISTINA") ~ "AANTIMT",
                                    str_detect(ITEMCUBSO, "VINBLASTINA") ~ "AANTIMT",
                                    str_detect(ITEMCUBSO, "PACLITAXEL") ~ "AANTIMT",
                                    str_detect(ITEMCUBSO, "DOCETAXEL") ~ "AANTIMT",
                                    str_detect(ITEMCUBSO, "PEMETREXED") ~ "ANTIMETABOLITO",
                                    str_detect(ITEMCUBSO, "ALIMTA") ~ "ANTIMETABOLITO",
                                    str_detect(ITEMCUBSO, "CAPECITABINA") ~ "ANTIMETABOLITO",
                                    str_detect(ITEMCUBSO, "XELODA") ~ "ANTIMETABOLITO",
                                    str_detect(ITEMCUBSO, "MERCAPTOPURINA") ~ "ANTIMETABOLITO",
                                    str_detect(ITEMCUBSO, "OLAPARIB") ~ "INHIBIDORPARP",
                                    str_detect(ITEMCUBSO, "LYNPARZA") ~ "INHIBIDORPARP",
                                    str_detect(ITEMCUBSO, "RUCAPARIB") ~ "INHIBIDORPARP",
                                    str_detect(ITEMCUBSO, "NIRAPARIB") ~ "INHIBIDORPARP",
                                    str_detect(ITEMCUBSO, "ZEJULA") ~ "INHIBIDORPARP",
                                    TRUE ~ "Resto no oncológico"
                                    ),
         grupo_diabetes = dplyr::case_when(str_detect(ITEMCUBSO, "GLIBENCLAMIDA") ~ 1,
                                           str_detect(ITEMCUBSO, "GLICLAZIDA") ~ 1,
                                           str_detect(ITEMCUBSO, "INSULINA") ~ 1,
                                           str_detect(ITEMCUBSO, "METFORMINA") ~ 1,
                                           str_detect(ITEMCUBSO, "DESMOPRESINA") ~ 1,
                                           TRUE ~ 0
                                           ))



#onco
result_2 <- dt_conv_busq |>
  filter(grupo_oncologico != "Resto no oncológico") |>
  filter(ULTIMO == "ULTIMO") |>
  select(CODIGOCONVOCATORIA, ENTIDAD, NITEM, MONTOREFERENCIALITEM, ITEMCUBSO, DESCRIPCIONITEM, TIPOPROCESOSELECCION, TIPOCOMPRA, TIPOENTIDAD, TIPOPROVEEDOR, GRUPO, FECHACONVOCATORIA) |>
  left_join(dt_contratos |>
              select(CODIGOCONVOCATORIA, NUMITEM_NUM, MONTOREFERENCIALTOTAL, MONTOCONTRATADOTOTAL, MONTOCONTRATADOITEM, FECHASUSCRIPCIONCONTRATO,FECHAVIGENCIAFINACTUALIZADA), by) |>
  mutate(ENTIDAD_COMPRA = case_when(str_detect(ENTIDAD, "CENTRO NACIONAL DE ABASTECIMIENTO") ~ "CENARES",
                                    str_detect(ENTIDAD, "REGIONAL") ~ "INSTITUTO/HOSPITAL - GR",
                                    str_detect(ENTIDAD, "HOSPITAL") |
                                      str_detect(ENTIDAD, "INST") ~ "INSTITUTO/HOSPITAL - GN",
                                    str_detect(ENTIDAD, "SEGURO SOCIAL") ~ "ESSALUD",
                                    str_detect(ENTIDAD, "POLICÍA") ~ "SANIDAD PNP",
                                    TRUE ~ "OTROS"
                                    ))

#antidb
result_t <- dt_conv_busq |>
  filter(grupo_diabetes == 1 | grupo_oncologico != "Resto no oncológico") |>
  filter(ULTIMO == "ULTIMO") |>
  select(CODIGOCONVOCATORIA, ENTIDAD, NITEM, MONTOREFERENCIALITEM, ITEMCUBSO, DESCRIPCIONITEM, TIPOPROCESOSELECCION, TIPOCOMPRA, TIPOENTIDAD, TIPOPROVEEDOR, GRUPO, FECHACONVOCATORIA) |>
  left_join(dt_contratos |>
              select(CODIGOCONVOCATORIA, NUMITEM_NUM, MONTOREFERENCIALTOTAL, MONTOCONTRATADOTOTAL, MONTOCONTRATADOITEM, FECHASUSCRIPCIONCONTRATO,FECHAVIGENCIAFINAL), by) |>
  mutate(ENTIDAD_COMPRA = case_when(str_detect(ENTIDAD, "CENTRO NACIONAL DE ABASTECIMIENTO") ~ "CENARES",
                                    str_detect(ENTIDAD, "REGIONAL") ~ "INSTITUTO/HOSPITAL - GR",
                                    str_detect(ENTIDAD, "HOSPITAL") |
                                      str_detect(ENTIDAD, "INST") ~ "INSTITUTO/HOSPITAL - GN",
                                    str_detect(ENTIDAD, "SEGURO SOCIAL") ~ "ESSALUD",
                                    str_detect(ENTIDAD, "POLICÍA") ~ "SANIDAD PNP",
                                    TRUE ~ ENTIDAD
  )) |>
  View()


#table(dt_conv_busq$TIPOCOMPRA) #Compra Corporativa Facultativa  Por encargo a Entidad Pública                 Por la Entidad 
# table(dt_conv_busq$TIPOPROVEEDOR) #Consorcio       Persona Juridica        Persona Natural Persona No Domiciliada 


# str_detect(ITEMCUBSO, "BRAFTOVI") ~ "INHIBIDORBRAF",dt_conv_busq |>
#   filter(!is.na(med_priorizado)) |>
#   select(med_priorizado, ITEMCUBSO, MONTOREFERENCIALITEMSOLES) |>
#     tbl_summary(by = med_priorizado)
# 
# ANHO = format(FECHASUSCRIPCIONCONTRATO, "%Y"))

export <- dt_conv_busq |>
  filter(!is.na(med_priorizado)) |>
  filter(ULTIMO == "ULTIMO") |>
  mutate(ANHO = format(FECHABUENAPRO, "%Y")) |>
  select(med_priorizado, MONTOREFERENCIALITEMSOLES, ANHO, TIPOPROCESOSELECCION, ENTIDAD) |> 
  write.table(file = "export.txt", sep = "|", row.names = FALSE)  

# dt_conv_busq |>
#   filter(!is.na(med_priorizado)) |>
#   mutate(ANHO = format(FECHABUENAPRO, "%Y")) |>
#   select(med_priorizado, MONTOREFERENCIALITEMSOLES, ANHO, TIPOPROCESOSELECCION, ENTIDAD) |> 
# tbl_summary(by = ANHO,
#               statistic = list(all_continuous() ~ "{sum}"),
#               missing_text = "(Missing)"
#               ) |>
#   add_overall(last = T, col_label = "**Total**  \nN = {style_number(N)}")
#   aggregate(MONTOREFERENCIALITEMSOLES ~ med_priorizado, sum)


  str_count()