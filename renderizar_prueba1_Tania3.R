# Librerías a utilizar:
library("rmarkdown") # Tener a la vista el siguiente documento: https://bookdown.org/yihui/rmarkdown-cookbook/
library(readxl)
library(dplyr)
library(lubridate)

# Generar información de prueba sobre la que se correrá el loop

ruta    <- "C:/Users/51985/OneDrive - INDECOPI/EDUARDO/17. Git/Galaxy/KoboLineamientosAnalysis-Tania3"

input  <- file.path(ruta, "input")
output <- file.path(ruta, "output")
tmp    <- file.path(ruta, "tmp")

file <- "FICHA_DE_EVALUACIÓN_-_all_versions_-_labels_-_2023-10-13-19-32-29.xlsx"

# Leer los datos desde el archivo
fichas <- read_xlsx(file.path(input, file), sheet = "FICHA DE EVALUACIÓN", skip = 0)
hechos <- read_xlsx(file.path(input, file), sheet = "hechos", skip = 0)
names  <- read_xlsx(file.path(input, file), sheet = "names", skip = 0)

#fichas <- read_xlsx("C:\\Users\\tperez\\Documents\\Equipo de soporte económico\\GITHUB\\KoboLineamientosAnalysis\\input\\FICHA_DE_EVALUACIÓN_-_all_versions_-_labels_-_2023-10-11-15-24-47.xlsx", sheet = "FICHA DE EVALUACIÓN", skip=0) # Genera un objeto en R con la información del archivo en excel
#hechos <- read_xlsx("C:\\Users\\tperez\\Documents\\Equipo de soporte económico\\GITHUB\\KoboLineamientosAnalysis\\input\\FICHA_DE_EVALUACIÓN_-_all_versions_-_labels_-_2023-10-11-15-24-47.xlsx", sheet = "hechos", skip=0) # Genera un objeto en R con la información del archivo en excel
#names  <- read_xlsx("C:\\Users\\tperez\\Documents\\Equipo de soporte económico\\GITHUB\\KoboLineamientosAnalysis\\input\\FICHA_DE_EVALUACIÓN_-_all_versions_-_labels_-_2023-10-11-15-24-47.xlsx", sheet = "names", skip=0) # Genera un objeto en R con la información del archivo en exce

as.data.frame(fichas) # Convierte el objeto en un dataframe
as.data.frame(hechos)
as.data.frame(names) 

fichas <- fichas %>% rename("ID" = "_id") #renombrando
fichas$ID <- as.integer(fichas$ID) # convierte el número a un formato entero

fichas <- fichas %>% rename("responsable" = "Indicar el nombre del/de la responsable:")
fichas <- fichas %>% rename("cargo_responsable" = "Indicar su cargo:")
fichas <- fichas %>% rename("equipoacargo" = "Seleccionar el equipo legal a cargo del análisis:")
fichas <- fichas %>% rename("nro_registro" = "Indicar el Nro de registro que identifica la carpeta en donde se encuentra evidencia del reporte:")
fichas <- fichas %>% rename("fecha_reporte" = "Fecha del reporte:")
fichas <- fichas %>% rename("autor_reporte" = "Autor del reporte:")
fichas <- fichas %>% rename("autor_organo" = "Indicar el Órgano del Indecopi que remite:")
fichas <- fichas %>% rename("autor_otro" = "Indicar Otro:...14")
fichas <- fichas %>% rename("medio" = "Medio de recepción:")
fichas <- fichas %>% rename("medio_otro" = "Indicar Otro:...16")
fichas <- fichas %>% rename("hecho" = "Hecho reportado:")
fichas <- fichas %>% rename("nhechos" = "¿Cuántos hechos reportados son?")
fichas <- fichas %>% rename("sinoreq" = "¿Fue necesario complementar la información del reporte a través de requerimientos o actos indagatorios?")
fichas <- fichas %>% rename("numinhh" = "¿De cuántas fuentes se solicitó información?")

names <- names %>% rename("ID" = "_submission__id")
names <- names %>% rename("name" = "Destinatario del requerimiento #${namenumber}")
names <- names %>% rename("fecha_reporte" = "Fecha del requerimiento #${namenumber}:")
names <- names %>% rename("sino_reitera" = "¿Fue reiterado? (para el requerimiento #${namenumber})")
names <- names %>% rename("sino_rpta" = "¿Recibió respuesta? (para el requerimiento #${namenumber})")

hechos <- hechos %>% rename("ID" = "_submission__id")
hechos <- hechos %>% rename("sino1" = "¿La fiscalización del bien o servicio y hecho reportado se encuentra prevista en el PAF?")
hechos <- hechos %>% rename("sino2" = "¿Ya se ha realizado la selección de proveedores con respecto al bien o servicio reportado?")
hechos <- hechos %>% rename("sino3" = "¿El proveedor vinculado al reporte ha sido considerado como sujeto de fiscalización?")
hechos <- hechos %>% rename("sino4" = "¿Los hechos reportados ya han sido o se encuentran siendo fiscalizados por el Indecopi? *")
hechos <- hechos %>% rename("sino5" = "¿La fiscalización de los hechos reportados es competencia del Indecopi?")
hechos <- hechos %>% rename("sino6" = "¿La competencia respecto de la fiscalización de los hechos reportados recae en las Secretarías Técnicas de CC3, ILN o CCD?")
hechos <- hechos %>% rename("competencia" = "¿De quién es la competencia?...8")
hechos <- hechos %>% rename("sinorecurtri" = "¿El equipo cuenta con capacidad para programar acciones de fiscalización en el trimestre en curso?")
hechos <- hechos %>% rename("sinorecuranu" = "¿El equipo cuenta con capacidad para programar acciones de fiscalización durante el año en curso?")
hechos <- hechos %>% rename("sinoriesgo" = "¿El bien o servicio reportado fue incluido como parte del diagnóstico para materia de protección al consumidor y competencia desleal del PAF vigente?")
hechos <- hechos %>% rename("division" = "Seleccione la división del bien o servicio reportado")
hechos <- hechos %>% rename("bienservicio1" = "Seleccione el bien o servicio reportado...13")
hechos <- hechos %>% rename("bienservicio2" = "Seleccione el bien o servicio reportado...14")
hechos <- hechos %>% rename("bienservicio3" = "Seleccione el bien o servicio reportado...15")
hechos <- hechos %>% rename("bienservicio4" = "Seleccione el bien o servicio reportado...16")
hechos <- hechos %>% rename("bienservicio5" = "Seleccione el bien o servicio reportado...17")
hechos <- hechos %>% rename("bienservicio6" = "Seleccione el bien o servicio reportado...18")
hechos <- hechos %>% rename("bienservicio7" = "Seleccione el bien o servicio reportado...19")
hechos <- hechos %>% rename("bienservicio8" = "Seleccione el bien o servicio reportado...20")
hechos <- hechos %>% rename("bienservicio9" = "Seleccione el bien o servicio reportado...21")
hechos <- hechos %>% rename("bienservicio10" = "Seleccione el bien o servicio reportado...22")
hechos <- hechos %>% rename("bienservicio11" = "Seleccione el bien o servicio reportado...23")
hechos <- hechos %>% rename("name" = "Detalle la conducta del proveedor analizada para considerar los atenuantes y agravantes:")

##############
#OJO AQUÍ
#############
hechos <- hechos %>% rename("atenuantes1" = "Indique si, de acuerdo con la información del reporte u otra obtenida de las indagaciones o requerimientos de información realizados, se identifica la existencia de atenuantes del riesgo:/Medidas para mitigar el impacto sobre los consumidores y/o el mercado.")
hechos <- hechos %>% rename("atenuantes2" = "Indique si, de acuerdo con la información del reporte u otra obtenida de las indagaciones o requerimientos de información realizados, se identifica la existencia de atenuantes del riesgo:/Medidas para evitar nuevos casos.")
hechos <- hechos %>% rename("atenuantes3" = "Indique si, de acuerdo con la información del reporte u otra obtenida de las indagaciones o requerimientos de información realizados, se identifica la existencia de atenuantes del riesgo:/Colaboración con la autoridad durante la investigación (asiste a reuniones, proporciona información).")
hechos <- hechos %>% rename("atenuantes4" = "Indique si, de acuerdo con la información del reporte u otra obtenida de las indagaciones o requerimientos de información realizados, se identifica la existencia de atenuantes del riesgo:/Otro")

hechos$atenuantes1 <- as.character(hechos$atenuantes1)
hechos$atenuantes1 <- ifelse(hechos$atenuantes1 == "0", "No", hechos$atenuantes1)
hechos$atenuantes1 <- ifelse(hechos$atenuantes1 == "1", "Si", hechos$atenuantes1)

hechos$atenuantes2 <- as.character(hechos$atenuantes2)
hechos$atenuantes2 <- ifelse(hechos$atenuantes2 == "0", "No", hechos$atenuantes2)
hechos$atenuantes2 <- ifelse(hechos$atenuantes2 == "1", "Si", hechos$atenuantes2)

hechos$atenuantes3 <- as.character(hechos$atenuantes3)
hechos$atenuantes3 <- ifelse(hechos$atenuantes3 == "0", "No", hechos$atenuantes3)
hechos$atenuantes3 <- ifelse(hechos$atenuantes3 == "1", "Si", hechos$atenuantes3)

hechos$atenuantes4 <- as.character(hechos$atenuantes4)
hechos$atenuantes4 <- ifelse(hechos$atenuantes4 == "0", "No", hechos$atenuantes4)
hechos$atenuantes4 <- ifelse(hechos$atenuantes4 == "1", "Si", hechos$atenuantes4)

hechos <- hechos %>% rename("agravantes1" = "Indique si se identifica la existencia de agravantes del riesgo:/Ha generado daño a la vida, salud o integridad física de consumidores.")
hechos <- hechos %>% rename("agravantes2" = "Indique si se identifica la existencia de agravantes del riesgo:/Presenta antecedentes de conductas similares.")
hechos <- hechos %>% rename("agravantes3" = "Indique si se identifica la existencia de agravantes del riesgo:/Falta de colaboración con la autoridad ​(por ejemplo: no asiste a reuniones, no responde a los pedidos de información).")
hechos <- hechos %>% rename("agravantes4" = "Indique si se identifica la existencia de agravantes del riesgo:/Otro")

hechos$agravantes1 <- as.character(hechos$agravantes1)
hechos$agravantes1 <- ifelse(hechos$agravantes1 == "0", "No", hechos$agravantes1)
hechos$agravantes1 <- ifelse(hechos$agravantes1 == "1", "Si", hechos$agravantes1)

hechos$agravantes2 <- as.character(hechos$agravantes2)
hechos$agravantes2 <- ifelse(hechos$agravantes2 == "0", "No", hechos$agravantes2)
hechos$agravantes2 <- ifelse(hechos$agravantes2 == "1", "Si", hechos$agravantes2)

hechos$agravantes3 <- as.character(hechos$agravantes3)
hechos$agravantes3 <- ifelse(hechos$agravantes3 == "0", "No", hechos$agravantes3)
hechos$agravantes3 <- ifelse(hechos$agravantes3 == "1", "Si", hechos$agravantes3)

hechos$agravantes4 <- as.character(hechos$agravantes4)
hechos$agravantes4 <- ifelse(hechos$agravantes4 == "0", "No", hechos$agravantes4)
hechos$agravantes4 <- ifelse(hechos$agravantes4 == "1", "Si", hechos$agravantes4)

##############
#############

hechos <- hechos %>% rename("riesgomenor" = "¿Se han programado acciones no contempladas en el PAF respecto de hechos con una valoración de riesgo menor?")
hechos <- hechos %>% rename("sinoacciones" = "Atendiendo al análisis de riesgo y a los recursos disponibles ¿se recomienda realizar acciones?")

colnames(hechos)
#"277078390", "277079847","277245934", "277818264", "277877372"
IDs_a_eliminar <- c("277148528")

# Lista de data frames
data_frames <- list(fichas, names, hechos)

# Aplicar la operación de filtrado a cada data frame
data_frames <- lapply(data_frames, function(df) df[!df$ID %in% IDs_a_eliminar, ])

# Asignar los data frames filtrados nuevamente a sus variables originales
fichas <- data_frames[[1]]
names <- data_frames[[2]]
hechos <- data_frames[[3]]


# Nombre del jefe de equipo
fichas <- fichas %>%
  mutate(
    jefe = case_when(
      equipoacargo == "A" ~ "Kellyn Angelica Agurto Zapata",
      equipoacargo == "B" ~ "Ivette Yesenia Sanchez Yarleque",
      equipoacargo == "C" ~ "Kattya del Rosario Palacios Herrera",
      equipoacargo == "D" ~ "Steffani Gisella Sancarranco Estela",
      TRUE ~ NA_character_
    )
  )

fichas <- fichas %>%
  mutate(
    cargojefe = case_when(
      jefe == "Kellyn Angelica Agurto Zapata" ~"Jefa de Equipo Legal",
      jefe == "Ivette Yesenia Sanchez Yarleque" ~"Jefa de Equipo Legal",
      jefe == "Kattya del Rosario Palacios Herrera" ~"Jefa de Equipo Legal",
      jefe == "Steffani Gisella Sancarranco Estela" ~"Jefa de Equipo Legal",
      TRUE ~ NA_character_
    )
  )

#Autor del reporte: Con este código, autor_est tomará el valor de autor_organo si este no es NA. Si autor_organo es NA, tomará el valor de autor_otro. Si ambos, autor_organo y autor_otro, son NA, tomará el valor de autor_reporte.
fichas <- fichas %>%
  mutate(
    autor_est = coalesce(autor_organo, autor_otro, autor_reporte)
  )

#Medio de recepción
fichas <- fichas %>%
  mutate(
    medio_est = coalesce(medio_otro, medio)
  )

#Bien o servicio reportado
hechos <- hechos %>%
  mutate(
    bienservicio_est = coalesce(bienservicio1,
                                bienservicio2,
                                bienservicio3,
                                bienservicio4,
                                bienservicio5,
                                bienservicio6,
                                bienservicio7,
                                bienservicio8,
                                bienservicio9,
                                bienservicio10,
                                bienservicio11
    )
  )

#Puntaje de riesgo
hechos <- hechos %>%
  mutate(
    valor_bienservicio_est = coalesce(valor_bienservicio1,
                                valor_bienservicio2,
                                valor_bienservicio3,
                                valor_bienservicio4,
                                valor_bienservicio5,
                                valor_bienservicio6,
                                valor_bienservicio7,
                                valor_bienservicio8,
                                valor_bienservicio9,
                                valor_bienservicio10,
                                valor_bienservicio11
    )
  )

#Nivel de riesgo
hechos <- hechos %>%
  mutate(
    nivel_riesgo_est = coalesce(nivel_riesgo1,
                                      nivel_riesgo2,
                                      nivel_riesgo3,
                                      nivel_riesgo4,
                                      nivel_riesgo5,
                                      nivel_riesgo6,
                                      nivel_riesgo7,
                                      nivel_riesgo8,
                                      nivel_riesgo9,
                                      nivel_riesgo10,
                                      nivel_riesgo11
    )
  )

#Selección de acciones

hechos <- hechos %>%
  mutate_at(vars(txt_accion9,txt_accion10, txt_accion13, txt_accion14, txt_accion17, txt_accion19, txt_accion21), ~ifelse(. == "-", NA, .))

hechos <- hechos %>%
  mutate(
    acciones_est = coalesce(txt_accion9,
                            txt_accion10,
                            txt_accion13,
                            txt_accion14,
                            txt_accion17,
                            txt_accion19,
                            txt_accion21
    )
  )

# Formatos de fechas
fichas$date_only <- as.Date(ymd_hms(fichas$endtime)) #endtime contiene fechas en formato "YYYY-MM-DD HH:MM:SS" y se desea extraer solo la parte de la fecha
date_obj <- ymd(fichas$date_only) # Convertir la cadena a un objeto Date
fichas$date_only <- format(date_obj, "%d-%m-%Y") # Reformatar a "DD-MM-YYYY"

date_obj2 <- ymd(fichas$fecha_reporte) # Convertir la cadena a un objeto Date
fichas$fecha_reporte <- format(date_obj2, "%d-%m-%Y") # Reformatar a "DD-MM-YYYY"

(fichas)
colnames(fichas)

# Limpiando data
fichas$responsable <- gsub("_", " ", fichas$responsable)

#############################################################################
#############################################################################
#############################################################################

# Correr el loop que renderizar los archivos
for (i in 1:nrow(fichas)) {
  j <- fichas[i, ] # Introduce la información de la fila que corre en el loop en una variable
  
  rmarkdown::render(
    'Prueba1_Tania3.Rmd', # Nombre del archivo R Markdown que utilizaremos como plantilla
    params = j, # Pasa variables al archivo R Markdown
    intermediates_dir = tmp, # Carpeta donde se generan y eliminar los archivos intermedios
    output_dir = output, # Carpeta donde aparecerán los archivos generados
    output_file = paste0(j$ID, '.pdf') # Nombre de los archivos generados
    
    #params = j, # Pasa variables al archivo R Markdown
    #intermediates_dir = here::here('tmp'), # Carpeta donde se generan y eliminar los archivos intermedios
    #output_dir = here::here('output'), # Carpeta donde aparecerán los archivos generados
    #output_file = paste0(j$ID, '.pdf') # Nombre de los archivos generados
  )
}
  
# Eliminar los archivos .log generados (opcional)
for (i in 1:nrow(fichas)){
  j <- fichas[i, ]
  file.remove(paste0(j$ID, '.log'))
}
