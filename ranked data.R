


fallas <- vroom::vroom("issues-raw.csv")

fallas$FECHA_FIN <- as_datetime(dmy_hm(fallas$FECHA_FIN))
fallas$FECHA_INICIO <- as_datetime(dmy_hm(fallas$FECHA_INICIO))

fallas <- 
  fallas |> 
  janitor::clean_names() |> 
  mutate(
    horario = case_when(
      hour(fecha_fin) < 12 & hour(fecha_fin) >5~"Mañana",
      hour(fecha_fin) >= 12 & hour(fecha_fin) < 19~"Tarde",
      hour(fecha_fin) >= 19 & hour(fecha_fin) < 23 ~"Noche",
      TRUE ~ "Madrugada"
    )
  )  
x <- 
  fallas |> 
  group_by(atm_id, horario) |> 
  summarise(
    Total_fallas = n()
  ) 

x <- 
  x |> 
  mutate(
    score = Total_fallas/max(x$Total_fallas, na.rm = TRUE),
    riesgo_fallo = case_when(
      score < 0.007 ~ "Muy bajo",
      score > 0.007 & score <= 0.05 ~ "Bajo",
      score > 0.05 & score <= 0.15 ~ "Moderado",
      score > 0.15 & score <= 0.5 ~ "Alto",
      score > 0.5 ~ "Muy alto",
    )
  ) 

write.csv(x, file = "final_data.csv")









