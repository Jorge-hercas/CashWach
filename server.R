
function(input, output, session){
  
  waiter_show(html = waiting_screen, color = "black"
  )
  Sys.sleep(3) 
  waiter_hide()
  
  sever(html = disconnected, color = "black")
  
  observeEvent(input$mail,{
    shinyalert(
      title = "¿Quieres enviar estos datos a tu correo?",
      text = "¡Introdúcelo aquí!",
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "input",
      inputType = "text",
      inputValue = "",
      inputPlaceholder = "micorreo@ejemplo.com",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#99CEFF",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  output$map <- renderMapboxer({
    as_mapbox_source(data) |> 
      mapboxer(
        style = "mapbox://styles/mapbox/light-v10",
        token = "pk.eyJ1IjoiYWFyb242NjYiLCJhIjoiY2o2M2NmZmJ4MWc0ZDJxbnI3dmZ5OXB2MSJ9.T3trJ5Tu66Kw-w-1ELpzBA"
      ) |> 
      set_view_state( -105.133208, 21.4326077, 5,pitch = 40) |> 
      add_circle_layer(circle_color = c("get","color")) |> 
      add_line_layer(
        source = as_mapbox_source(y),
        line_color = "#b36969",
        id = "line",
        line_cap = "round",
        line_width = 4
      )
    
  })
  
  output$recom <- renderText({
    req(seleccion_cajero())
    
    
    paste0("Te recomendamos ir al cajero ubicado en ",tolower(head(seleccion_cajero()$sitio,1)), ", ",
           tolower(head(seleccion_cajero()$calle,1))
           )
    
  }) |> 
    bindEvent(input$ruta)
  
  output$ult_inc <- renderText({
    req(seleccion_cajero())
    
    x <- 
      fallas |> 
      janitor::clean_names() |> 
      filter(
        atm_id == seleccion_cajero()$atm
      ) 
    
    
    paste0(
      "Último incidente reportado en el cajero: hace ",
      round(as.numeric(difftime(Sys.time(), tail(x$fecha_fin,1), units = "days"))),
      " días"
    )
    
    
  }) |> 
    bindEvent(input$ruta)
  
  
  
  
  map_rendered <- reactiveVal(TRUE)
  
  observeEvent(input$ruta, {
    data <- 
      tibble(
        west =ggmap::geocode(input$my_address)$lon-0.01,
        south =ggmap::geocode(input$my_address)$lat-0.01,
        east =ggmap::geocode(input$my_address)$lon+0.01,
        north =ggmap::geocode(input$my_address)$lat+0.01
      )
    data2 <- 
      tibble(
        west =input$long-0.01,
        south =input$lat-0.01,
        east =input$long+0.01,
        north =input$lat+0.01
      )
    
    
    req(map_rendered())
    if (input$perro == TRUE & nchar(input$my_address) == 0){
      
      mapboxer_proxy("map") |> 
        fit_bounds(
          bounds = c(data2$west, data2$south,
                     data2$east, data2$north
          )
        ) |> 
        update_mapboxer()
      
    }else{
      
      mapboxer_proxy("map") |> 
        fit_bounds(
          bounds = c(data$west, data$south,
                     data$east, data$north
          )
        ) |> 
        update_mapboxer()
      
    }
  }) 
  
  points <- reactive({
    #validate(
     # need(input$my_address, message = FALSE)
    #)
    lon <-ggmap::geocode(input$my_address)$lon
    lat <- ggmap::geocode(input$my_address)$lat
   data |> 
     bind_rows(
       tibble(
         lng =c(lon),
         lat =c(lat),
         color = c("#2f5b87")
       )
     )
    
  })
  
  data2 <- reactive({
    validate(
      need(input$lat, message = FALSE),
      need(input$long, message = FALSE)
    )
    
    lat <- input$lat
    lng <- input$long
    
    data |> 
      bind_rows(
        tibble(
          lat = lat,
          lng = lng,
          color = c("#2f5b87")
        )
      )
  })
  
  
  
  
  observeEvent(input$perro,{
    
    output$actual_dir <- renderUI(tags$script(script))
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$ruta,{
    
    req(map_rendered())
    req(points())
    req(data2())
    
    if (input$perro == TRUE & nchar(input$my_address) == 0 ){
      
      mapboxer_proxy("map") |> 
        set_data(data2()) |> 
        update_mapboxer()
      
    }else if (input$perro == FALSE & nchar(input$my_address) >= 0 ){
      
      mapboxer_proxy("map") |> 
        set_data(points()) |> 
        update_mapboxer()
      
    }else{
      
      mapboxer_proxy("map") |> 
        set_data(points()) |> 
        update_mapboxer()
      
    }
    
  })
  
  observe(
    if (nchar(input$my_address) > 0 ){
      updateCheckboxInput("perro", value = FALSE, session = session)
    }
  )
  
  observe(
    if (input$perro == TRUE ){
      updateTextInput(session, "my_address", value = "")
    }
  )
  
  seleccion_cajero <- reactive({
    
    
    
    if (input$perro == TRUE & nchar(input$my_address) == 0 ){
      validate(
        need(input$lat, message = FALSE),
        need(input$long, message = FALSE)
      )
      
      lat_r <- input$lat
      lng_r <- input$long
      
      x <- 
        data |> 
        filter(riesgo_fallo %in% c("Muy bajo","Bajo","Moderado")) |> 
        mutate(
          lng_final = lng_r,
          lat_final = lat_r,
          dist = geosphere::distHaversine(
            cbind(lng, lat), cbind(lng_final, lat_final)
          )
        ) 
      
      x |> 
        filter(
          dist == min(x$dist, na.rm = TRUE)
        )
      
      
    }else if (input$perro == FALSE & nchar(input$my_address) >= 0 ){
      lon_r <-ggmap::geocode(input$my_address)$lon
      lat_r <- ggmap::geocode(input$my_address)$lat
      
      x <- 
        data |> 
        filter(riesgo_fallo %in% c("Muy bajo","Bajo","Moderado")) |> 
        mutate(
          lng_final = lon_r,
          lat_final = lat_r,
          dist = geosphere::distHaversine(
            cbind(lng, lat), cbind(lng_final, lat_final)
          )
        ) 
      
      x |> 
        filter(
          dist == min(x$dist, na.rm = TRUE)
        )
      
      
    }else{
      lon_r <-ggmap::geocode(input$my_address)$lon
      lat_r <- ggmap::geocode(input$my_address)$lat
     
      x <- 
        data |> 
        filter(riesgo_fallo %in% c("Muy bajo","Bajo","Moderado")) |> 
        mutate(
          lng_final = lon_r,
          lat_final = lat_r,
          dist = geosphere::distHaversine(
            cbind(lng, lat), cbind(lng_final, lat_final)
          )
        ) 
      x <-
      x |> 
        filter(
          dist == min(x$dist, na.rm = TRUE)
        )
      
    }
    
    
  })
  
  
  ruta_fin <- reactive({
    
    req(seleccion_cajero())
    
    x <- 
      tibble(
        com =c("A","B"),
        lng = c(seleccion_cajero()$lng[1],seleccion_cajero()$lng_final[1]),
        lat = c(seleccion_cajero()$lat[1], seleccion_cajero()$lat_final[1])
      )
    
    osrm::osrmTrip(x,overview = "full",osrm.profile = input$transporte)[[1]]$trip[1,]
    
  })
  
  
  observeEvent(input$ruta,{
    
    req(ruta_fin())
    
    mapboxer_proxy("map") |> 
      set_data(ruta_fin(), source_id ="line") |> 
      update_mapboxer()
    
    
  })
  
  
  output$tabla_indicador <- renderReactable({
    
    
    tibble(
      `Riesgo de fallo del cajero` = c(
        "Muy bajo",
        "Bajo",
        "Moderado",
        "Alto",
        "Muy alto"
      )
    ) |> 
      reactable(
        bordered = TRUE, 
        highlight = TRUE, 
        theme = reactablefmtr::espn(background_color = "transparent"),
        columns = list(
          `Riesgo de fallo del cajero` = colDef(
            cell = JS("function(cellInfo) {
        let color
        switch (cellInfo.value) {
          case 'Muy bajo':
            color = '#6ab871'
            break
          case 'Bajo':
            color = '#a0bd6f'
            break
          case 'Moderado':
            color = '#adbd6f'
            break
          case 'Alto':
            color = '#d68718'
            break
          case 'Muy alto':
            color = '#ba3e38'
            break  
        }
        const badgeStyle = 'display: inline-block; margin-right: 8px; width: 9px; height: 9px;' +
          'background-color: ' + color + '; border-radius: 50%'
        const badge = '<span style=\"' + badgeStyle + '\"></span>'
        return badge + cellInfo.value
      }"),
            html = TRUE
          )
        )
      ) 
    
    
    
  })
  
  
  
  output$grafico_fallas <- renderEcharts4r({
    
    req(seleccion_cajero())
    
    if (length(seleccion_cajero()$atm[1]) > 0){
      fallas |> 
        janitor::clean_names() |> 
        filter(atm_id %in% seleccion_cajero()$atm ) |> 
        mutate(
          tiempo = difftime(fecha_fin,fecha_inicio, units = "mins"),
          hora = as.factor(hour(fecha_inicio))
        ) |> 
        filter(tiempo < 1000) |> 
        group_by(
          hora
        ) |> 
        summarise(
          tiempo = round(as.numeric(mean(tiempo, na.rm = TRUE)))
        ) |> 
        e_charts(
          hora, dispose = FALSE
        ) |> 
        e_bar(tiempo, name = "Minutos") |> 
        e_tooltip(trigger = "axis",
                  confine = TRUE,
                  textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |> 
        e_legend(bottom = 20, textStyle = list(fontFamily = "Roboto Condensed", 
                                               color = "gray",
                                               fontSize = 12)) |>
        e_theme("auritus") |> 
        e_color(color ="#ad5151", background = "transparent" ) |> 
        e_title("Tiempo promedio de falla en cajeros por hora del día", 
                "Desde la hora en la que comenzaron a fallar",
                left = "center",
                textStyle = list(
                  color = "#000000",
                  fontFamily = "Roboto Condensed",
                  fontSize = 14
                )
        )
      
    }else{
      
      fallas |> 
        janitor::clean_names() |> 
        mutate(
          tiempo = difftime(fecha_fin,fecha_inicio, units = "mins"),
          hora = as.factor(hour(fecha_inicio))
        ) |> 
        filter(tiempo < 1000) |> 
        group_by(
          hora
        ) |> 
        summarise(
          tiempo = round(as.numeric(mean(tiempo, na.rm = TRUE)))
        ) |> 
        e_charts(
          hora, dispose = FALSE
        ) |> 
        e_bar(tiempo, name = "Minutos") |> 
        e_tooltip(trigger = "axis",
                  confine = TRUE,
                  textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |> 
        e_legend(bottom = 20, textStyle = list(fontFamily = "Roboto Condensed", 
                                               color = "gray",
                                               fontSize = 12)) |>
        e_theme("auritus") |> 
        e_color(color ="#ad5151", background = "transparent" ) |> 
        e_title("Tiempo promedio de falla en cajeros por hora del día", 
                "Desde la hora en la que comenzaron a fallar",
                left = "center",
                textStyle = list(
                  color = "#000000",
                  fontFamily = "Roboto Condensed",
                  fontSize = 14
                )
        )
      
    } 
    
    
    
  }) |> 
    bindEvent(input$ruta)
  
  
  output$top_cajeros <- renderReactable({
    
    
    data |> 
      tidyr::replace_na(
        list(
          score = 0,
          riesgo_fallo = "Muy bajo"
        )
      ) |> 
      select(sitio,riesgo_fallo,score) |> 
      arrange(desc(score)) |> 
      mutate(score = round(score, digits = 3)) |> 
      head(50) |> 
      reactable(
        compact = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        style = list(fontSize =10),
        theme = reactableTheme(backgroundColor = "transparent"),
        #theme = reactablefmtr::nytimes(background_color = "transparent"),
        columns = list(
          riesgo_fallo = colDef(name = "Riesgo de fallo"),
          sitio = colDef(name = "Zona"),
          score = colDef(
            minWidth = 150,
            cell = reactablefmtr::data_bars(
              data = data,
              text_position = 'none',
              box_shadow = TRUE,
              round_edges = TRUE,
              fill_color = rev(MetBrewer::met.brewer('Troy')),
              bias = 1.5,
              icon = 'skull-crossbones',
              background = 'transparent',
              bar_height = 4,
              max_value = 1
            )
          )
        )
      )
    
  })
  
  
}


