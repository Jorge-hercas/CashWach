
#NNGEO
start=Sys.time()
optimizacion=st_nn(geolocalizacion, cajeros, k = 5, parallel = 2, returnDist = TRUE)
Sys.time()-start
cajeros_nn=cajeros[unlist(optimizacion$nn),]

ggplot()+
  geom_sf(data=cajeros_nn)+
  geom_sf(data=geolocalizacion, color="Red")

##MAPSAPI
doc = mp_matrix(
  origins = geolocalizacion,
  destinations = cajeros,
  mode = mode_api,
  key = key,
  quiet = TRUE
)

# Menor distancia (en segundos)
t= mp_get_matrix(doc, value = "duration_s")
colnames(t) = cajeros$ATM
rownames(t) = "usuario"
which(t == min(t), arr.ind=FALSE)
# Esto resuelve el problema: lo quito para evitar tiempo de computación
## cajeros[ which(t == min(t), arr.ind=FALSE),] #Aquí me falta verificar si siempre se acomodan en orden...

# Rutas y mapa
doc = mp_directions(
  origin = geolocalizacion,
  destination = cajeros[ which(t == min(t), arr.ind=FALSE),],
  alternatives = FALSE,
  mode=mode_api,
  key = key,
  quiet = TRUE
)

r = mp_get_routes(doc)
r


