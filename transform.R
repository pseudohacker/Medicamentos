test <- dt_contratos_csv |>
  filter(red_flag == 1) |>
  filter(RUCCODIGOGANADOR == 0 | RUCCODIGOGANADOR == 1) |>
  mutate(MONTOREFERENCIALTOTAL = MONTOREFERENCIALTOTAL + NUMITEM/10^nchar(as.character(NUMITEM)),
         NUMITEM = ESCONSORCIO,
         ESCONSORCIO = RUCCODIGOGANADOR,
         RUCCODIGOGANADOR = GANADOR,
         GANADOR = FECHAPUBLICACIONCONTRATO,
         FECHAPUBLICACIONCONTRATO = FECHASUSCRIPCIONCONTRATO,
         FECHASUSCRIPCIONCONTRATO = FECHAVIGENCIAINICIAL,
         FECHAVIGENCIAINICIAL = FECHAVIGENCIAFINAL,
         FECHAVIGENCIAFINAL = FECHAVIGENCIAFINACTUALIZADA,
         FECHAVIGENCIAFINACTUALIZADA = CODIGOCONTRATO,
         CODIGOCONTRATO = RUCDESTINATARIOPAGO,
         RUCDESTINATARIOPAGO = NOMBREDESTINATARIOPAGO,
         NOMBREDESTINATARIOPAGO = NUMCONTRATO,
         NUMCONTRATO = MONTOCONTRATADOTOTAL,
         MONTOCONTRATADOTOTAL = as.character(MONTOCONTRATADOITEM),
         MONTOCONTRATADOITEM = MONTOADICIONAL,
         MONTOADICIONAL = MONTOREDUCCION,
         MONTOREDUCCION = MONTOPRORROGA,
         MONTOPRORROGA = MONTOCOMPLEMENTARIO,
         MONTOCOMPLEMENTARIO = DESCRIPCIONCONTRATO,
         DESCRIPCIONCONTRATO = V28,
         V28 = V29,
         V29 = V30,
         V30 = V31,
         V31 = V32,
         V32 = "")

dt_contratos_csv <- dt_contratos_csv |>
  mutate(MONTOREFERENCIALTOTAL = as.double(MONTOREFERENCIALTOTAL),
         ESCONSORCIO = as.character(ESCONSORCIO),
         MONTOCOMPLEMENTARIO = as.character(MONTOCOMPLEMENTARIO))

dt_contratos_csv <- dt_contratos_csv |>
  rows_update(test, by = "ID") 

dt_contratos_csv <- dt_contratos_csv |>
  mutate(red_flag = case_when(!(DESCRIPCIONCONTRATO != "" &
                                  (V28 == "" | is.na(V28 == T)) &
                                  (V29 == "" | is.na(V29 == T)) &
                                  (V30 == "" | is.na(V30 == T)) &
                                  (V31 == "" | is.na(V31 == T)) &
                                  (V32 == "" | is.na(V32 == T))) ~ 1,
                              TRUE ~ 0))
 table(dt_contratos_csv$red_flag)

 ###29700
 
 test <- dt_contratos_csv |>
  filter(red_flag == 1) |>
  filter(V29 != "" & V30 == "" & V31 == "" & V32 == "") |>
  mutate(MONTOCONTRATADOTOTAL = as.character(as.numeric(MONTOCONTRATADOTOTAL) + MONTOCONTRATADOITEM/10^nchar(as.character(MONTOCONTRATADOITEM))),
         MONTOCONTRATADOITEM = MONTOADICIONAL + MONTOREDUCCION/10^nchar(as.character(MONTOREDUCCION)),
         MONTOADICIONAL = MONTOPRORROGA,
         MONTOREDUCCION = MONTOCOMPLEMENTARIO,
         MONTOPRORROGA = DESCRIPCIONCONTRATO,
         MONTOCOMPLEMENTARIO = V28,
         DESCRIPCIONCONTRATO = V29,
         V28 = V30,
         V29 = V31,
         V30 = "",
         V31 = "")

dt_contratos_csv <- dt_contratos_csv |>
  mutate(MONTOREDUCCION = as.character(MONTOREDUCCION),
         MONTOPRORROGA = as.character(MONTOPRORROGA))

  dt_contratos_csv <- dt_contratos_csv |>
    rows_update(test, by = "ID") 

  dt_contratos_csv <- dt_contratos_csv |>
    mutate(red_flag = case_when(!(DESCRIPCIONCONTRATO != "" &
                                    (V28 == "" | is.na(V28 == T)) &
                                    (V29 == "" | is.na(V29 == T)) &
                                    (V30 == "" | is.na(V30 == T)) &
                                    (V31 == "" | is.na(V31 == T)) &
                                    (V32 == "" | is.na(V32 == T))) ~ 1,
                                TRUE ~ 0))
  
  table(dt_contratos_csv$red_flag)  

  #4660 casos
  
  test <- dt_contratos_csv |>
    filter(red_flag == 1) |>
    filter(V30 != "" & V31 == "" & V32 == "" &
            !(MONTOCONTRATADOTOTAL == "" | is.na(MONTOCONTRATADOTOTAL) == T) &
            !(MONTOCONTRATADOITEM == "" | is.na(MONTOCONTRATADOITEM) == T) &
            !(MONTOADICIONAL == "" | is.na(MONTOADICIONAL) == T) &
            !(MONTOREDUCCION == "" | is.na(MONTOREDUCCION) == T) &
            !(MONTOPRORROGA == "" | is.na(MONTOPRORROGA) == T) &
             !(MONTOCOMPLEMENTARIO == "" | is.na(MONTOCOMPLEMENTARIO) == T) &
             (DESCRIPCIONCONTRATO == "" | is.na(DESCRIPCIONCONTRATO) == T)
           ) |>
    mutate(MONTOCONTRATADOTOTAL = as.character(as.numeric(MONTOCONTRATADOTOTAL) + MONTOCONTRATADOITEM/10^nchar(as.character(MONTOCONTRATADOITEM))),
           MONTOCONTRATADOITEM = MONTOADICIONAL + as.numeric(MONTOREDUCCION)/10^nchar(MONTOREDUCCION),
           MONTOADICIONAL = as.numeric(MONTOPRORROGA) + as.numeric(MONTOCOMPLEMENTARIO)/10^nchar(as.character(MONTOCOMPLEMENTARIO)),
           MONTOREDUCCION = DESCRIPCIONCONTRATO,
           MONTOPRORROGA = V28,
           MONTOCOMPLEMENTARIO = V29,
           DESCRIPCIONCONTRATO = V30,
           V28 = "",
           V29 = "",
           V30 = "",
           V31 = "")  
  
  dt_contratos_csv <- dt_contratos_csv |>
    rows_update(test, by = "ID") 
  
  dt_contratos_csv <- dt_contratos_csv |>
    mutate(red_flag = case_when(!(DESCRIPCIONCONTRATO != "" &
                                    (V28 == "" | is.na(V28 == T)) &
                                    (V29 == "" | is.na(V29 == T)) &
                                    (V30 == "" | is.na(V30 == T)) &
                                    (V31 == "" | is.na(V31 == T)) &
                                    (V32 == "" | is.na(V32 == T))) ~ 1,
                                TRUE ~ 0))
  
  table(dt_contratos_csv$red_flag)    

  #3947
  
  test <- dt_contratos_csv |>
    filter(red_flag == 1) |>
    filter(V28 != "" & V29 == "" & V30 == "" & V31 == "" & V32 == "") |>
    filter(!(MONTOREDUCCION == "" | is.na(MONTOREDUCCION) == T) &
             !(MONTOADICIONAL == "" | is.na(MONTOADICIONAL) == T) &
             (MONTOPRORROGA == "" | is.na(MONTOPRORROGA) == T) &
             (MONTOCOMPLEMENTARIO == "" | is.na(MONTOCOMPLEMENTARIO) == T)) |>
    mutate(MONTOADICIONAL = MONTOADICIONAL + as.numeric(MONTOREDUCCION)/10^nchar(MONTOREDUCCION),
           MONTOREDUCCION = MONTOPRORROGA,
           MONTOPRORROGA = MONTOCOMPLEMENTARIO,
           MONTOCOMPLEMENTARIO = DESCRIPCIONCONTRATO,
           DESCRIPCIONCONTRATO = V28,
           V28 = ""
           )  

  dt_contratos_csv <- dt_contratos_csv |>
    rows_update(test, by = "ID") 
  
  dt_contratos_csv <- dt_contratos_csv |>
    mutate(red_flag = case_when(!(DESCRIPCIONCONTRATO != "" &
                                    (V28 == "" | is.na(V28 == T)) &
                                    (V29 == "" | is.na(V29 == T)) &
                                    (V30 == "" | is.na(V30 == T)) &
                                    (V31 == "" | is.na(V31 == T)) &
                                    (V32 == "" | is.na(V32 == T))) ~ 1,
                                TRUE ~ 0))
  
  table(dt_contratos_csv$red_flag)      
#3643
  
  test <- dt_contratos_csv |>
    filter(red_flag == 1) |>
    filter(as.integer(MONTOCONTRATADOTOTAL) == MONTOADICIONAL) |>
    mutate(MONTOCONTRATADOTOTAL = as.character(as.numeric(MONTOCONTRATADOTOTAL) + MONTOCONTRATADOITEM/10^nchar(as.character(MONTOCONTRATADOITEM))),
           MONTOCONTRATADOITEM = MONTOADICIONAL + as.numeric(MONTOREDUCCION)/10^nchar(MONTOREDUCCION),
           MONTOADICIONAL = as.numeric(MONTOPRORROGA),
           MONTOREDUCCION = MONTOCOMPLEMENTARIO,
           MONTOPRORROGA = DESCRIPCIONCONTRATO,
           MONTOCOMPLEMENTARIO =  V28,
           DESCRIPCIONCONTRATO = V29,
           V28 = V30,
           V29 = V31,
           V30 = "",
           V31 = ""
    )  
    
  dt_contratos_csv <- dt_contratos_csv |>
    rows_update(test, by = "ID")    

  dt_contratos_csv <- dt_contratos_csv |>
    mutate(red_flag = case_when(!(DESCRIPCIONCONTRATO != "" &
                                    (V28 == "" | is.na(V28 == T)) &
                                    (V29 == "" | is.na(V29 == T)) &
                                    (V30 == "" | is.na(V30 == T)) &
                                    (V31 == "" | is.na(V31 == T)) &
                                    (V32 == "" | is.na(V32 == T))) ~ 1,
                                TRUE ~ 0))
  
  table(dt_contratos_csv$red_flag)      

##3643  
problems <- c(917859,
              923121,
              881523,
              921617,
              919113,
              913856,
              977072,
              909587,
              919113,
              908275,
              886661,
              908057,
              943024)
  test <- dt_contratos_csv |>
    filter(red_flag == 1) |>
    filter(CODIGOCONVOCATORIA %in% problems) |>
    filter((MONTOREDUCCION == "" | is.na(MONTOREDUCCION) == T) &
             !(MONTOADICIONAL == "" | is.na(MONTOADICIONAL) == T) &
             (MONTOPRORROGA == "" | is.na(MONTOPRORROGA) == T) &
             (MONTOCOMPLEMENTARIO == "" | is.na(MONTOCOMPLEMENTARIO) == T)) |>
    mutate(MONTOCONTRATADOTOTAL = as.character(as.numeric(MONTOCONTRATADOTOTAL) + MONTOCONTRATADOITEM/10^nchar(as.character(MONTOCONTRATADOITEM))),
          MONTOCONTRATADOITEM = MONTOADICIONAL + as.numeric(MONTOREDUCCION)/10^nchar(MONTOREDUCCION),
          MONTOADICIONAL = as.integer(MONTOREDUCCION),
           MONTOREDUCCION = MONTOPRORROGA,
           MONTOPRORROGA = MONTOCOMPLEMENTARIO,
           MONTOCOMPLEMENTARIO = DESCRIPCIONCONTRATO,
           DESCRIPCIONCONTRATO = V28,
           V28 = ""
    )    

  dt_contratos_csv <- dt_contratos_csv |>
    rows_update(test, by = "ID")    
  dt_contratos_csv <- dt_contratos_csv |>
    mutate(red_flag = case_when(!(DESCRIPCIONCONTRATO != "" &
                                    (V28 == "" | is.na(V28 == T)) &
                                    (V29 == "" | is.na(V29 == T)) &
                                    (V30 == "" | is.na(V30 == T)) &
                                    (V31 == "" | is.na(V31 == T)) &
                                    (V32 == "" | is.na(V32 == T))) ~ 1,
                                TRUE ~ 0))
  
  table(dt_contratos_csv$red_flag)   
  
  #1919
  
  problems <- c(916106)

  test <- dt_contratos_csv |>
    filter(red_flag == 1) |>
    filter(CODIGOCONVOCATORIA %in% problems) |>
    mutate(MONTOADICIONAL = as.integer(MONTOADICIONAL + as.numeric(MONTOREDUCCION)/10^nchar(MONTOREDUCCION)),
           MONTOREDUCCION = as.character(as.integer(MONTOPRORROGA) + as.numeric(MONTOCOMPLEMENTARIO)/10^nchar(MONTOCOMPLEMENTARIO)),
           MONTOPRORROGA = DESCRIPCIONCONTRATO,
           MONTOCOMPLEMENTARIO = V28,
           DESCRIPCIONCONTRATO = V29,
           V29 = ""
    )    
  
  
  dt_contratos_csv <- dt_contratos_csv |>
    rows_update(test, by = "ID")    
  

  problems <- c(881778)
  
  test <- dt_contratos_csv |>
    filter(red_flag == 1) |>
    filter(CODIGOCONVOCATORIA %in% problems) |>
    mutate(MONTOCONTRATADOTOTAL = as.character(as.numeric(MONTOCONTRATADOTOTAL) + MONTOCONTRATADOITEM/10^nchar(as.character(MONTOCONTRATADOITEM))),
           MONTOCONTRATADOITEM = MONTOADICIONAL + as.numeric(MONTOREDUCCION)/10^nchar(MONTOREDUCCION),
           MONTOREDUCCION = as.character(as.numeric(MONTOCOMPLEMENTARIO) + as.numeric(DESCRIPCIONCONTRATO)/10^nchar(DESCRIPCIONCONTRATO)),
           MONTOADICIONAL = as.integer(MONTOPRORROGA),
           MONTOPRORROGA = V28,
           MONTOCOMPLEMENTARIO = V29,
           DESCRIPCIONCONTRATO = V30,
           V30 = ""
    )    
  
  
  dt_contratos_csv <- dt_contratos_csv |>
    rows_update(test, by = "ID") 
  
  
  
  problems <- c(881767)
  
  test <- dt_contratos_csv |>
    filter(red_flag == 1) |>
    filter(CODIGOCONVOCATORIA %in% problems) |>
    mutate(MONTOREDUCCION = as.character(as.numeric(MONTOREDUCCION) + as.numeric(MONTOPRORROGA)/10^nchar(MONTOPRORROGA)),
           MONTOPRORROGA = MONTOCOMPLEMENTARIO,
           MONTOCOMPLEMENTARIO = DESCRIPCIONCONTRATO,
           DESCRIPCIONCONTRATO = V28,
           V28 = ""
    )    
  
  
  dt_contratos_csv <- dt_contratos_csv |>
    rows_update(test, by = "ID") 
# dt_contratos_csv |>
#   # filter(red_flag == 1) |>
#   # filter(as.integer(MONTOCONTRATADOTOTAL) == MONTOADICIONAL) |>
#   filter(CODIGOCONVOCATORIA == 908275) |>
#   # filter(V28 != "" & V29 == "" & V30 == "" & V31 == "" & V32 == "") |>
#   # filter((MONTOREDUCCION == "" | is.na(MONTOREDUCCION) == T) &
#   #          !(MONTOADICIONAL == "" | is.na(MONTOADICIONAL) == T) &
#   #          (MONTOPRORROGA == "" | is.na(MONTOPRORROGA) == T) &
#   #          (MONTOCOMPLEMENTARIO == "" | is.na(MONTOCOMPLEMENTARIO) == T)) |>
#   View()




