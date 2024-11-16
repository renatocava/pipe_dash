# Columnas acumuladas por region

# Load libraries ----------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

data_upch <- REDCapR::redcap_report(redcap_uri = "https://redcap.upch.edu.pe/api/", token = Sys.getenv("token_upch"), report_id = 781L)$data
data_ivi <- REDCapR::redcap_report(redcap_uri = "https://ivirdc.ivi.int/api/", token = Sys.getenv("token_ivi"), report_id = 1098L)$data

googlesheets4::gs4_auth(email = "renato.cava@upch.pe")

# Calc indicators TOTAL ---------------------------------------------------------

# data_ivi %>%
#   filter(
#     !str_detect(hh_id, "P[A-Z]{1}-[0-9]{2}-[0-9]{4}-[0-9]{2}")
#   ) %>%
#   View()
# 
# data_upch %>%
#   filter(
#     !str_detect(vivienda_id2, "P[A-Z]{1}-[0-9]{2}-[0-9]{4}-[0-9]{2}")
#   ) %>%
#   View()

## Working days ----
data_upch %>% 
  # count(formulario_id) %>% 
  filter(
    !is.na(vivienda_id2),
    # str_starts(vivienda_id2, "PI-")
    lubridate::ymd(fecha_entrevista2) < lubridate::today()
  ) %>% 
  # count(fecha_entrevista2) %>%
  # View()
  distinct(vivienda_original, existe_vivienda2, vivienda_id2, .keep_all = T) %>%
  # View()
  mutate(
    week = lubridate::epiweek(fecha_entrevista2),
    fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1),
    region = str_sub(vivienda_id2, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa"))
  ) %>% 
  # count(region) %>% 
  # View()
  count(region, fecha_entrevista2) %>%
  # View()
  janitor::tabyl(region) %>%
  # View()
  janitor::adorn_totals() %>% 
  select(-percent) %>% 
  pivot_wider(names_from = region, values_from = n) -> tbl_wk_days
# View()

## total_number_hh_visit ----
data_upch %>% 
  # count(formulario_id) %>% 
  filter(
    !is.na(vivienda_id2),
    lubridate::ymd(fecha_entrevista2) < lubridate::today()
    # str_starts(vivienda_id2, "PI-")
  ) %>% 
  distinct(vivienda_original, existe_vivienda2, vivienda_id2, .keep_all = T) %>%
  distinct(vivienda_id2, .keep_all = T) %>% 
  mutate(
    week = lubridate::epiweek(fecha_entrevista2),
    fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1),
    region = str_sub(vivienda_id2, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa"))
  ) %>% 
  # View()
  janitor::tabyl(region) %>%
  janitor::adorn_totals() %>% 
  select(-percent) %>% 
  pivot_wider(names_from = region, values_from = n) -> tbl_n_hh_vst
# View()

## hhs reasigned code ----
data_upch %>% 
  filter(
    !is.na(vivienda_id2),
    lubridate::ymd(fecha_entrevista2) < lubridate::today()
    # str_starts(vivienda_id2, "PI-")
  ) %>% 
  # add_count(vivienda_id2) %>% 
  # filter(
  #   n > 1
  # ) %>% 
  filter(
    existe_vivienda2 == 2
  ) %>% 
  mutate(
    week = lubridate::epiweek(fecha_entrevista2),
    fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1),
    region = str_sub(vivienda_id2, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa"))
  ) %>%
  # View()
  janitor::tabyl(region) %>%
  janitor::adorn_totals() %>% 
  select(-percent) %>% 
  pivot_wider(names_from = region, values_from = n) -> tbl_hh_rsgd
# View()

## total number of hh not enrolled and reason why ----
data_upch %>% 
  filter(
    enrolamiento2 == 2,
    !no_enrolamiento_motivo2 %in% c(4,5),
    lubridate::ymd(fecha_entrevista2) < lubridate::today()
    # str_starts(vivienda_id2, "PI-")
  ) %>% 
  distinct(vivienda_original, existe_vivienda2, vivienda_id2, .keep_all = T) %>%
  mutate(
    no_enrolamiento_motivo2 = case_when(
      no_enrolamiento_motivo2 == 1 ~ "  HH vacant",
      no_enrolamiento_motivo2 == 2 ~ "  Absence of HH head or representative",
      no_enrolamiento_motivo2 == 3 ~ "  Refused",
      # no_enrolamiento_motivo2 == 4 ~ "Other",
      # no_enrolamiento_motivo2 == 5 ~ "Es restaurante, tienda, centro educativo, comercio, oficina o puesto de salud",
      T ~ NA_character_
    ),
    no_enrolamiento_motivo2 = factor(no_enrolamiento_motivo2,  levels = c("  HH vacant", "  Absence of HH head or representative", "  Refused")),
    week = lubridate::epiweek(fecha_entrevista2),
    fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1),
    region = str_sub(vivienda_id2, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa"))
  ) %>% 
  janitor::tabyl(no_enrolamiento_motivo2, region, show_missing_levels = TRUE) %>%
  janitor::adorn_totals(where = c("row", "col")) %>% 
  mutate(
    no_enrolamiento_motivo2 = case_when(
      no_enrolamiento_motivo2 == "Total" ~ "Total number of HHs not enrolled and reason why",
      T ~ no_enrolamiento_motivo2
    )
  ) %>% 
  # View()
  slice(4, 1:3) %>% 
  # View()
  rename("TOTAL" = no_enrolamiento_motivo2) -> tbl_hh_nt_enroll
# View()
# select(-percent) %>% 
# pivot_wider(names_from = fecha_inicio, values_from = n) -> tbl_hh_nt_enroll
# View()

bind_rows(
  tbl_hh_nt_enroll,
  tbl_n_hh_vst
) %>% 
  # View()
  mutate(
    # `LIMA (EA + EB)` = case_when(
    #   is.na(`LIMA (EA + EB)`) ~ "Total HH Visit",
    #   T ~ `LIMA (EA + EB)`
    # ),
    
    # Total = as.character(Total),
    
    across(
      2:Total, 
      ~ case_when(
        `TOTAL` == "Total number of HHs not enrolled and reason why" ~ as.character(paste(., "(", janitor::round_half_up(./lag(., n = 7, default = last(.))*100,2), ")")),
        T ~ as.character(.)
      )
    ),
    
    # Total = case_when(
    #   `LIMA (EA + EB)` == "Total number of HHs not enrolled and reason why" ~ as.character(paste(Total, "(", janitor::round_half_up(Total/lag(Total, n = 7, default = last(Total))*100,2), ")")),
    #   T ~ as.character(Total)
    # )
  ) %>% 
  filter(
    !is.na(`TOTAL`)
  ) -> tbl_hh_nt_enroll

### HH vacant ----
# data_upch %>% 
#   filter(
#     enrolamiento2 == 2,
#     no_enrolamiento_motivo2 == 1
#   ) %>% 
#   distinct(vivienda_original, existe_vivienda2, vivienda_id2, .keep_all = T) %>%
#   mutate(
#     week = lubridate::epiweek(fecha_entrevista2),
#     fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1)
#   ) %>% 
#   count(fecha_inicio) %>% 
#   View()
### Absence of HH head or representative ----
# data_upch %>% 
#   filter(
#     enrolamiento2 == 2,
#     no_enrolamiento_motivo2 == 2
#   ) %>% 
#   distinct(vivienda_original, existe_vivienda2, vivienda_id2, .keep_all = T) %>%
#   mutate(
#     week = lubridate::epiweek(fecha_entrevista2),
#     fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1)
#   ) %>% 
#   count(fecha_inicio) %>% 
#   View()

### Refused ----
# data_upch %>% 
#   filter(
#     enrolamiento2 == 2,
#     no_enrolamiento_motivo2 == 3
#   ) %>% 
#   distinct(vivienda_original, existe_vivienda2, vivienda_id2, .keep_all = T) %>%
#   mutate(
#     week = lubridate::epiweek(fecha_entrevista2),
#     fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1)
#   ) %>% 
#   count(fecha_inicio) %>% 
#   View()

### Other ----
# data_upch %>% 
#   filter(
#     enrolamiento2 == 2,
#     no_enrolamiento_motivo2 == 4
#   ) %>% 
#   distinct(vivienda_original, existe_vivienda2, vivienda_id2, .keep_all = T) %>%
#   mutate(
#     week = lubridate::epiweek(fecha_entrevista2),
#     fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1)
#   ) %>% 
#   count(fecha_inicio) %>% 
#   View()



## Total number of HHs agreed to participate ----
data_upch %>% 
  filter(
    enrolamiento2 == 1,
    lubridate::ymd(fecha_entrevista2) < lubridate::today()
    # str_starts(vivienda_id2, "PI-")
  ) %>% 
  mutate(
    week = lubridate::epiweek(fecha_entrevista2),
    fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1),
    region = str_sub(vivienda_id2, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa"))
  ) %>% 
  janitor::tabyl(region) %>%
  janitor::adorn_totals() %>% 
  select(-percent) %>% 
  pivot_wider(names_from = region, values_from = n) -> tbl_hh_agrd

bind_rows(
  tbl_hh_agrd,
  tbl_n_hh_vst
) %>% 
  mutate(
    
    across(
      everything(), 
      ~ as.character(paste(., "(", janitor::round_half_up(./lag(., n = 7, default = last(.))*100,2), ")"))
    ),
    
  ) %>% 
  slice(1) -> tbl_hh_agrd

## Cantidad de viviendas que aceptaron participar segun numero de habitantes ----
# data_ivi %>% 
#   filter(
#     # is.na(redcap_repeat_instrument),
#     !is.na(hh_ppl_live)
#   ) %>%
#   # View()
#   mutate(
#     hh_ppl_live = paste("  Viviendas con ", hh_ppl_live, " habitante", sep = ""),
#     week = lubridate::epiweek(hh_intv_dt),
#     fecha_inicio = lubridate::floor_date(hh_intv_dt, "week", week_start = 1)
#   ) %>% 
#   # count(hh_ppl_live) %>%
#   janitor::tabyl(hh_ppl_live, fecha_inicio, show_missing_levels = TRUE) %>%
#   janitor::adorn_totals(where = c("col")) %>% 
#   rename("LIMA (EA + EB)" = hh_ppl_live)-> tbl_n_hh_living

## Total number of members living in enrolled HHs ----
data_upch %>% 
  filter(
    enrolamiento2 == 1,
    lubridate::ymd(fecha_entrevista2) < lubridate::today()
    # str_starts(vivienda_id2, "PI-")
  ) %>% 
  mutate(
    week = lubridate::epiweek(fecha_entrevista2),
    fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1),
    region = str_sub(vivienda_id2, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa"))
  ) %>% 
  group_by(region) %>% 
  summarise(
    total_liv = sum(cuantos_miembros_hogar2, na.rm = T)
  ) %>% 
  # View()
  pivot_wider(names_from = region, values_from = total_liv) %>% 
  mutate(
    Total = rowSums(across(everything()))
  ) -> tbl_mm_liv

## Total number of HHs members enrolled ----
data_upch %>% 
  filter(
    enrolamiento2 == 1,
    lubridate::ymd(fecha_entrevista2) < lubridate::today()
    # str_starts(vivienda_id2, "PI-")
  ) %>% 
  mutate(
    week = lubridate::epiweek(fecha_entrevista2),
    fecha_inicio = lubridate::floor_date(fecha_entrevista2, "week", week_start = 1),
    region = str_sub(vivienda_id2, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa"))
  ) %>% 
  group_by(region) %>% 
  summarise(
    total_liv = sum(cuantos_miembros_hogar2, na.rm = T),
    total_enrol = sum(cuantos_miembros_participan2, na.rm = T),
    # total = total_enrol/total_liv,
    # Total = rowSums(across(c()))
  ) %>% 
  ungroup() %>% 
  janitor::as_tabyl() %>% 
  janitor::adorn_totals(where = "row") %>% 
  mutate(
    p = janitor::round_half_up(total_enrol/total_liv*100, 2),
    `n (%)` = paste(total_enrol, "(", p, ")")
  )  %>% 
  select(region, `n (%)`) %>% 
  # View()
  pivot_wider(names_from = region, values_from = `n (%)`) -> tbl_mm_enroll
  # View()
  # mutate(
  #   Total = rowSums(across(everything()))
  # ) -> tbl_mm_enroll

## Total number of HHs per members enrolled----
data_ivi %>%
  # View()
  filter(
    redcap_repeat_instrument == "individual_form",
    lubridate::ymd(mm_intv_dt) < lubridate::today()
    # str_starts(mm_hh_id, "PI-")
  ) %>% 
  # count(mm_intv_dt) %>% 
  # View()
  mutate(
    week = lubridate::epiweek(mm_intv_dt),
    fecha_inicio = lubridate::floor_date(mm_intv_dt, "week", week_start = 1),
    region = str_sub(mm_hh_id, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa"))
  ) %>%
  # count(region) %>% 
  # View()
  # group_by(hh_id) %>%
  # count(hh_id_serial, mm_hh_id) %>%
  # count(hh_id_serial) %>% 
  add_count(mm_hh_id) %>%
  distinct(mm_hh_id, .keep_all = T) %>%
  mutate(
    n = paste("  Viviendas con ", n, " enrolado", sep = "")
  ) %>% 
  janitor::tabyl(n, region, show_missing_levels = TRUE) %>%
  janitor::adorn_totals(where = c("col")) %>% 
  rename("TOTAL" = n) -> tbl_n_hh_enrolled

## Average of enrolled participants per household” ----
bind_rows(
  data_ivi %>%
    filter(
      redcap_repeat_instrument == "individual_form",
      lubridate::ymd(mm_intv_dt) < lubridate::today(),
    ) %>% 
    mutate(
      week = lubridate::epiweek(mm_intv_dt),
      fecha_inicio = lubridate::floor_date(mm_intv_dt, "week", week_start = 1),
      region = str_sub(mm_hh_id, 1, 2),
      region = case_when(
        region == "PI" ~ "Lima",
        region == "PA" ~ "Arequipa",
        region == "PO" ~ "Loreto",
        region == "PU" ~ "Ucayali",
        region == "PT" ~ "Tumbes",
        T ~ NA_character_
      ),
    ) %>%
    add_count(mm_hh_id) %>%
    group_by(region) %>% 
    summarise(
      mean_part_hh = janitor::round_half_up(mean(n, na.rm = T), 1)
    ),
  
  data_ivi %>%
    filter(
      redcap_repeat_instrument == "individual_form",
      lubridate::ymd(mm_intv_dt) < lubridate::today(),
    ) %>% 
    mutate(
      week = lubridate::epiweek(mm_intv_dt),
      fecha_inicio = lubridate::floor_date(mm_intv_dt, "week", week_start = 1),
      region = str_sub(mm_hh_id, 1, 2),
      region = case_when(
        region == "PI" ~ "Lima",
        region == "PA" ~ "Arequipa",
        region == "PO" ~ "Loreto",
        region == "PU" ~ "Ucayali",
        region == "PT" ~ "Tumbes",
        T ~ NA_character_
      ),
    ) %>%
    add_count(mm_hh_id) %>%
    summarise(
      mean_part_hh = janitor::round_half_up(mean(n, na.rm = T), 1)
    )
) %>% 
  pivot_wider(names_from = region, values_from = mean_part_hh) %>%
  rename("Total"=`NA`) -> tbl_mean_mm_enrolled


## Age group of enrolled HH members ----
data_ivi %>% 
  filter(
    !is.na(mm_hh_id),
    lubridate::ymd(mm_intv_dt) < lubridate::today()
    # str_starts(mm_hh_id, "PI-")
  ) %>% 
  # View()
  mutate(
    week = lubridate::epiweek(mm_entdt_now),
    fecha_inicio = lubridate::floor_date(mm_entdt_now, "week", week_start = 1),
    
    region = str_sub(mm_hh_id, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa")),
    
    # < 1y
    # 1-4y
    # 5-9y
    # 10-19y
    # 20-29y
    # 30-39y
    # 40-49y
    # 50-59y
    # 60y or above
    
    age_grp = case_when(
      mm_cal_age < 1 ~ "  < 1y",
      mm_cal_age >= 1 & mm_cal_age <= 9 ~ "  1-4y",
      mm_cal_age >= 1 & mm_cal_age <= 9 ~ "  5-9y",
      mm_cal_age >= 10 & mm_cal_age <= 19 ~ "  10-19y",
      mm_cal_age >= 20 & mm_cal_age <= 29 ~ "  20-29y",
      mm_cal_age >= 30 & mm_cal_age <= 39 ~ "  30-39y",
      mm_cal_age >= 40 & mm_cal_age <= 49 ~ "  40-49y",
      mm_cal_age >= 50 & mm_cal_age <= 59 ~ "  50-59y",
      mm_cal_age >= 60 ~ "  60y or above",
      T ~ NA_character_
    ),
    age_grp = factor(age_grp, c("  < 1y", "  1-4y", "  5-9y", "  10-19y", "  20-29y", "  30-39y", "  40-49y", "  50-59y", "  60y or above"))
  ) %>% 
  # filter(is.na(age_grp)) %>% 
  # View()
  # count(fecha_inicio) %>%
  janitor::tabyl(age_grp, region) %>% 
  janitor::adorn_totals(where = c("row", "col")) %>% 
  mutate(
    age_grp = case_when(
      age_grp == "Total" ~ "Age group of enrolled HH members",
      T ~ age_grp
    )
  ) %>% 
  # View()
  slice(10, 1:9) %>% 
  # View()
  rename("TOTAL" = age_grp) -> tbl_age_grp


## Total number of DBS samples collected ----
data_ivi %>% 
  filter(
    !is.na(mm_hh_id),
    lubridate::ymd(mm_intv_dt) < lubridate::today(),
    # str_starts(mm_hh_id, "PI-"),
    mm_dbs == 1
  ) %>% 
  mutate(
    week = lubridate::epiweek(mm_entdt_now),
    fecha_inicio = lubridate::floor_date(mm_entdt_now, "week", week_start = 1),
    region = str_sub(mm_hh_id, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa")),
  ) %>% 
  janitor::tabyl(region) %>%
  janitor::adorn_totals() %>% 
  select(-percent) %>% 
  pivot_wider(names_from = region, values_from = n) -> tbl_dbs_clltd


## Incomplete/inadecuate DBS collections ----

data_ivi %>% 
  filter(
    !is.na(mm_hh_id),
    # str_starts(mm_hh_id, "PI-"),
    lubridate::ymd(mm_intv_dt) < lubridate::today(),
    mm_dbs_succ == 2
  ) %>%
  # View()
  mutate(
    week = lubridate::epiweek(mm_entdt_now),
    fecha_inicio = lubridate::floor_date(mm_entdt_now, "week", week_start = 1),
    region = str_sub(mm_hh_id, 1, 2),
    region = case_when(
      region == "PI" ~ "Lima",
      region == "PA" ~ "Arequipa",
      region == "PO" ~ "Loreto",
      region == "PU" ~ "Ucayali",
      region == "PT" ~ "Tumbes",
      T ~ NA_character_
    ),
    region = forcats::fct_relevel(forcats::as_factor(region), c("Lima", "Arequipa")),
  ) %>% 
  # View()
  janitor::tabyl(region) %>%
  janitor::adorn_totals() %>% 
  select(-percent) %>% 
  pivot_wider(names_from = region, values_from = n) -> tbl_dbs_incmpltd

## Integrate all ----
bind_rows(
  tbl_wk_days %>% 
    mutate(
      across(everything(), ~as.character(.))
    ) %>% 
    add_column("TOTAL"="Working days", .before = 1),
  tbl_n_hh_vst %>% 
    mutate(
      across(everything(), ~as.character(.))
    ) %>% 
    add_column("TOTAL"="Total number of HHs visit", .before = 1),
  tbl_hh_rsgd %>% 
    mutate(
      across(everything(), ~as.character(.))
    ) %>% 
    add_column("TOTAL"="HHs reasigned code", .before = 1),
  tbl_hh_nt_enroll %>% 
    mutate(
      across(everything(), ~as.character(.))
    ),
  tbl_hh_agrd %>%
    mutate(
      across(everything(), ~as.character(.))
    ) %>% 
    add_column("TOTAL"="Total number of HHs agreed to participate", .before = 1),
  # tbl_n_hh_living,
  tbl_n_hh_enrolled %>% 
    mutate(
      across(everything(), ~as.character(.))
    ),
  tbl_mean_mm_enrolled %>% 
    mutate(
      across(everything(), ~ as.character(.))
    ) %>% 
    add_column("TOTAL"="Average of enrolled participants per household", .before = 1),
  tbl_mm_liv %>%
    mutate(
      across(everything(), ~as.character(.))
    ) %>% 
    add_column("TOTAL"="Total number of members living in enrolled HHs", .before = 1),
  tbl_mm_enroll %>%
    mutate(
      across(everything(), ~as.character(.))
    ) %>% 
    add_column("TOTAL"="Total number of HHs members enrolled", .before = 1),
  # tbl_n_hh_enrolled,
  tbl_age_grp %>% 
    mutate(
      across(everything(), ~as.character(.))
    ),
  tbl_dbs_clltd %>% 
    mutate(
      across(everything(), ~ as.character(.))
    ) %>% 
    add_column("TOTAL"="Total number of DBS samples collected", .before = 1),
  tbl_dbs_incmpltd %>% 
    mutate(
      across(everything(), ~as.character(.))
    ) %>% 
    add_column("TOTAL"="Incomplete/inadecuate DBS collections", .before = 1)
) %>% 
  # View()
  relocate(Total, .after = 1) %>% 
  # View()
  mutate(
    across(everything(), ~ replace_na(., "0"))
  ) %>% 
  # View()
  add_row() %>% 
  # View()
  add_row(
    `TOTAL` = "Goal of enrollment in Total",
    Total = "4803"
  ) %>%
  add_row(
    `TOTAL` = "Total number of individuals enrolled",
    Total = tbl_mm_enroll %>% pull(Total) %>% str_sub(.,1,4)
  ) %>%
  # View()
  add_row(
    `TOTAL` = "Percentage (%) of individuals enrolled",
    Total = as.character(janitor::round_half_up(tbl_mm_enroll %>% pull(Total) %>% str_sub(.,1,4) %>% as.numeric()/4803*100, 2))
  ) %>%
  # View()
  googlesheets4::write_sheet(ss = "https://docs.google.com/spreadsheets/d/1VvPH6FZ7d6-npHQeYvMGkogQ7-GGb0JkO6dI5agCDLg/edit?usp=sharing", sheet = "TEST Weekly Report IVI (DM)")

# rm(list = ls()[grep("tbl_", ls())])
