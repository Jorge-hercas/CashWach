
library(shiny)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(echarts4r)
library(lubridate)
library(reactable)
library(sever)
library(mapboxer)
library(bslib)
library(shinyWidgets)
library(shiny)
library(ggmap)
library(shinybusy)
library(waiter)
library(shinyalert)

if (hour(Sys.time()) < 12 & hour(Sys.time())>5 ){
  hour <- "Mañana"
}else if (hour(Sys.time()) >= 12 & hour(Sys.time())  < 19){
  hour <- "Tarde"
}else if (hour(Sys.time()) >= 19 & hour(Sys.time())  < 23){
  hour <- "Noche"
}else{hour <- "Madrugada"}

data <- vroom::vroom("data/atms-raw.csv")
data <- 
data |>
  janitor::clean_names() |> 
  rename(
    lat = latitud,
    lng = longitud
  ) 

riesgo_fallos <- vroom::vroom("final_data.csv")
data <- 
data |> 
  left_join(
    riesgo_fallos |> 
      filter(
        horario == hour
          #"Tarde"
      ),
    by = c("atm"="atm_id")
  ) |> 
  mutate(
    color = case_when(
      riesgo_fallo == "Muy bajo" ~"#6ab871",
      riesgo_fallo == "Bajo" ~"#a0bd6f",
      riesgo_fallo == "Moderado" ~"#adbd6f",
      riesgo_fallo == "Alto" ~"#d68718",
      riesgo_fallo == "Muy alto" ~"#ba3e38",
      TRUE ~"#6ab871"
    )
  )

fallas <- vroom::vroom("data/issues-raw.csv")

fallas$FECHA_FIN <- as_datetime(dmy_hm(fallas$FECHA_FIN))
fallas$FECHA_INICIO <- as_datetime(dmy_hm(fallas$FECHA_INICIO))

key <- "AIzaSyC-L1JqEFQyWuwMiflGyA-8HRMi8K31noM"
register_google(key)

theme <- bs_theme(
  bg = "#000000", fg = "#B8BCC2",
  "input-border-color" = "#a6a6a6"
)

opts <- list(
  `actions-box` = TRUE,
  `live-search`=TRUE)


x <- 
  tibble(
    com =c("A","B"),
    lng = c(-99.1050779445615,-99.1050779445615),
    lat = c(19.4189957949528, 19.4189957949528)
  )

y <- osrm::osrmTrip(x)[[1]]$trip[1,]

disconnected <- tagList(
  h1("Oh no :(..."),
  p("Te has desconectado del servidor"),
  reload_button("Recargar página", class = "warning")
)

waiting_screen <- tagList(
  img(src = "CashWatch Hackathon.png", style="position:top;margin:auto;",height="50%", width="50%"),
  br(),br(),br(),
  spin_folding_cube(),
  h3("Cargando aplicación...")
) 


source("js code.R")




