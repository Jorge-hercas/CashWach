

bootstrapPage(
  useSever(),
  useWaiter(),
  useShinyalert(),
  add_busy_spinner(spin = "breeding-rhombus" ),
  absolutePanel(
    top = 10, left = 50, style = "z-index:500; text-align: right;",
    tags$h2("CashWach: ¿A qué cajero acudo hoy?")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  mapboxerOutput("map", width = "100%", height = "100%"),
  shinymanager::fab_button(
    animation = "slidein-spring",
    actionButton(
      inputId = "mail",
      label = "¿Necesitas esta información en tu correo?",
      icon = icon("fa-regular fa-envelope")
    ),
    inputId = "fab"
  ),
  absolutePanel(
    top = 100, left = 50, style = "z-index:500; text-align: left;",
    draggable = TRUE,
    tags$head(
      tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
     .pac-container:after {

    background-image: none !important;
    height: 0px;}"))
    ),
    
    div(
      textInput(inputId = "my_address", label = "Introduce una dirección")    
      ,textOutput(outputId = "full_address")
      ,HTML(paste0(" <script> 
                function initAutocomplete() {

                 var autocomplete =   new google.maps.places.Autocomplete(document.getElementById('my_address'),{types: ['geocode']});
                 autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                 autocomplete.addListener('place_changed', function() {
                 var place = autocomplete.getPlace();
                 if (!place.geometry) {
                 return;
                 }

                 var addressPretty = place.formatted_address;
                 var address = '';
                 if (place.address_components) {
                 address = [
                 (place.address_components[0] && place.address_components[0].short_name || ''),
                 (place.address_components[1] && place.address_components[1].short_name || ''),
                 (place.address_components[2] && place.address_components[2].short_name || ''),
                 (place.address_components[3] && place.address_components[3].short_name || ''),
                 (place.address_components[4] && place.address_components[4].short_name || ''),
                 (place.address_components[5] && place.address_components[5].short_name || ''),
                 (place.address_components[6] && place.address_components[6].short_name || ''),
                 (place.address_components[7] && place.address_components[7].short_name || '')
                 ].join(' ');
                 }
                 var address_number =''
                 address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
                 var coords = place.geometry.location;
                 //console.log(address);
                 Shiny.onInputChange('jsValue', address);
                 Shiny.onInputChange('jsValueAddressNumber', address_number);
                 Shiny.onInputChange('jsValuePretty', addressPretty);
                 Shiny.onInputChange('jsValueCoords', coords);});}
                 </script> 
                 <script src='https://maps.googleapis.com/maps/api/js?key=", key,"&libraries=places&callback=initAutocomplete' async defer></script>")
      ),
      checkboxInput(
        inputId = "perro",
        label = "O utiliza tu localización actual...", 
        value = FALSE
      ),
      awesomeRadio(
       "transporte",
       "¿De qué forma te gustaría ir al cajero?",
       status = "success",
       selected = "foot",
       inline = TRUE,
       choices = c("A pie" = "foot", "En bicicleta" = "bicycle", "En auto" = "car")
     ),
      actionButton(
        inputId = "ruta",
        label = "Generar ruta",
        icon = icon("bars")
      ),
     br(),br(),
     strong(textOutput("recom")),
     strong(textOutput("ult_inc")),
     br(),br(),
     echarts4rOutput("grafico_fallas", width = 400, height = 300),
      uiOutput("actual_dir")
    ),
    
    
    
  ),
  absolutePanel(
    bottom = 100, left = 50, style = "z-index:500; text-align: right;",
    reactableOutput("tabla_indicador", width = 200)
    
    ),
  absolutePanel(
    top = 100, right = 50, style = "z-index:500; text-align: right;",
    reactableOutput("top_cajeros")
  )
  
  
  
)



