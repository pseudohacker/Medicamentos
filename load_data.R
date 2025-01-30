pacman::p_load(dplyr, stringr)

#convocatorias
dt_convocatorias <- readRDS(here::here("input","dt_convocatorias.Rds")) |>
  mutate(NITEM = as.integer(NITEM))

#contratos
data_path <- "input/contratos"   # path to the data
files <- dir(here::here("input","contratos"), pattern = ".xlsx$") # get file names
dt_contratos_xls <- files %>%
  # read in all the files, appending the path before the filename
  purrr::map(~ readxl::read_xlsx(file.path(data_path, .))) %>%
  purrr::reduce(rbind)


files <- dir(here::here("input","contratos"), pattern = "contratos 2023.csv") # get file names
dt_contratos_csv <- files %>%
  # read in all the files, appending the path before the filename
  purrr::map(~ data.table::fread(file.path(data_path, .), encoding = "Latin-1", fill = T)) %>%
  purrr::reduce(rbind)

#filtro problemas
dt_contratos_csv <- dt_contratos_csv |>
  mutate(red_flag = case_when(!(DESCRIPCIONCONTRATO != "" &
                                  V28 == "" &
                                  V29 == "" &
                                  V30 == "" &
                                  V31 == "" &
                                  V32 == "") ~ 1,
                              TRUE ~ 0))

#une la base de contratos excluyendo problemas
dt_contratos_clean <- dt_contratos_csv |>
  filter(red_flag == 0) %>%
  # as.Date(.$FECHAPUBLICACIONCONTRATO, format = "%d/%m/%y")
  mutate(across(starts_with("FECHA"), ~ as.POSIXct(.x, format = "%d/%m/%y")),
         MONTOCONTRATADOTOTAL = as.numeric(MONTOCONTRATADOTOTAL),
         RUCENTIDAD = as.character(RUCENTIDAD),
         RUCENTIDADCONTRATANTE = as.character(RUCENTIDADCONTRATANTE)) |>
  select(CODIGOCONVOCATORIA, RUCENTIDAD, ENTIDADCONTRATANTE, NUMITEM, MONTOREFERENCIALTOTAL, MONTOCONTRATADOTOTAL, MONTOCONTRATADOITEM, FECHASUSCRIPCIONCONTRATO,FECHAVIGENCIAFINACTUALIZADA) |>
  rbind(dt_contratos_xls |>
          select(CODIGOCONVOCATORIA, RUCENTIDAD, ENTIDADCONTRATANTE, NUMITEM, MONTOREFERENCIALTOTAL, MONTOCONTRATADOTOTAL, MONTOCONTRATADOITEM, FECHASUSCRIPCIONCONTRATO,FECHAVIGENCIAFINACTUALIZADA), fill = TRUE)




#búsqueda de casos problemas
by <- join_by(CODIGOCONVOCATORIA)
result_flag <- dt_conv_busq |>
  filter(grupo_diabetes == 1 | grupo_oncologico != "Resto no oncológico") |>
  # filter(grupo_oncologico != "Resto no oncológico") |>
  filter(ULTIMO == "ULTIMO") |>
  select(CODIGOCONVOCATORIA, ENTIDAD, NITEM, MONTOREFERENCIALITEM, ITEMCUBSO, DESCRIPCIONITEM, TIPOPROCESOSELECCION, TIPOCOMPRA, TIPOENTIDAD, TIPOPROVEEDOR, GRUPO, FECHACONVOCATORIA) |>
  left_join(dt_contratos_csv |>
              filter(red_flag == 1), by) |>
  filter(is.na(RUCENTIDAD)==F) |>
  select(CODIGOCONVOCATORIA, NITEM, MONTOREFERENCIALITEM) |>
  unique() |>
  arrange(desc(MONTOREFERENCIALITEM))

            
             
  

