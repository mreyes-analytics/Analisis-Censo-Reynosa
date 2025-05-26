📊 Análisis Demográfico y Construcción de Indicadores a Nivel AGEB (Reynosa, INEGI Censo 2020)
Requisitos y dependencias
R >= 4.0

Paquetes: tidyverse, sf, leaflet, RColorBrewer, ggrepel, viridis, forcats

Datos: INEGI Censo de Población y Vivienda 2020 (AGEB urbana Reynosa)

1. Descripción general
Este proyecto realiza un análisis exploratorio-demográfico de Reynosa, Tamaulipas, utilizando datos abiertos del Censo de Población y Vivienda 2020 del INEGI.
El análisis se centra en el nivel AGEB (Área Geoestadística Básica), la unidad mínima de división territorial empleada por el INEGI, que agrupa manzanas urbanas con características sociodemográficas similares.

Objetivo principal:
Identificar zonas potenciales dentro de Reynosa para:

Evaluación y planeación de futuras aperturas de negocios y estudios de mercado.

Para ello se construyen y visualizan indicadores clave (demográficos, socioeconómicos y de vivienda) a nivel AGEB, facilitando la toma de decisiones basada en evidencia territorial.

2. Pasos y lógica del análisis
A. Obtención y carga de datos
Fuente: INEGI – Censo de Población y Vivienda 2020 (AGEB urbana).

Herramientas: R, tidyverse.

Carga inicial:

r
Copiar
Editar
library(tidyverse)
# ...otros paquetes...

setwd("C:/Users/Miguel Reyes/Desktop/Supply Chain Data Science Project")
reynx_datos <- read_csv("conjunto_de_datos_ageb_urbana_28_cpv2020.csv")

reynx_datos <- reynx_datos %>% filter(NOM_MUN == "Reynosa")
B. Análisis y transformación por bloques temáticos

1. Edades y estructura demográfica
   
Selección de columnas clave de edades.

Limpieza de valores erróneos/faltantes (asteriscos a cero).

Construcción de tibble con estructura poblacional por AGEB.

¿Por qué? Permite conocer la pirámide poblacional y la estructura por edades a nivel territorial.

2. Estado civil
Segmentación de población 12+ por estado civil (solteros, casados, separados/divorciados/viudos).

Validación cruzada con el total de población 12+.

¿Por qué? Para validar integridad y analizar estructura familiar por AGEB.

3. Agregaciones de interés
Cálculo de población total, infantil, juvenil, adulta y adulta mayor por AGEB.

Transformaciones para obtener sub-segmentos (mujeres, hombres, grupos de edad clave).

¿Por qué? Permite análisis comparativos y diagnósticos territoriales.

4. Hogares y jefatura femenina/masculina
Cálculo de hogares por AGEB distinguiendo entre jefatura femenina y masculina.

¿Por qué? Ayuda a identificar vulnerabilidades y patrones de estructura familiar relevantes para intervención social o enfoque de mercado.

5. Vivienda y creación de índices sintéticos

Se analizaron características de vivienda (acceso a servicios, bienes, espacio, movilidad y tecnología).

Creación de 5 índices sintéticos (IIB, IEH, IBD, IM, ITE), cada uno representa una proporción respecto a viviendas particulares habitadas.

Importancia:
Estos índices permiten comparar condiciones materiales y de servicios entre AGEBs de forma estandarizada, y sirven de base para una clasificación socioeconómica propia usando percentiles (25, 50, 75).
![](output/01_distribucion_nivel_socioeconomico.png)

6. Validación y visualización de índices
Se implementaron boxplots por nivel socioeconómico para validar la capacidad discriminante de los índices.

Ejemplo visual: el índice de tecnología muestra desigualdades claras entre clases altas y bajas.

¿Por qué?
Los índices permiten análisis focalizados para políticas públicas o segmentación de mercado.

⚠️ Nota: Los índices pueden superar 1, ya que suman proporciones de varias características (por ejemplo, muchas viviendas con computadora, internet y TV paga suman >1).

⚠️ Nota sobre el Índice de Potencial de Mejoras:
  Interpretación de valores negativos:
  El índice de potencial de mejoras puede tomar valores negativos en algunos AGEBs. Esto ocurre porque el índice da mayor peso a las carencias de infraestructura y espacio habitacional: si un AGEB tiene valores muy altos en estos componentes (es decir, ya cuenta con excelente infraestructura y espacio), la resta (1 - IIB) o (1 - IEH) resulta negativa.

¿Qué significa?
  
Valores negativos: El AGEB tiene un nivel de infraestructura y espacio superior al estándar y, por lo tanto, no es prioritario para intervenciones o mejoras (tiene “potencial de mejora” menor a cero).

Valores cercanos a cero: El AGEB cumple el estándar básico; prioridad baja a media.

Valores altos y positivos: El AGEB presenta carencias, por lo que sí es prioritario para acciones de mejora o inversión.

7. Desocupación de vivienda y hacinamiento
Desocupación: Se clasificó cada AGEB en Baja, Media, Alta o Muy Alta según el porcentaje de viviendas deshabitadas.

Hacinamiento:

Promedio de ocupantes por vivienda habitada

Densidad de habitantes

Tasa de ocupación de viviendas

Visualización: Gráfico de dispersión ocupantes promedio vs. densidad poblacional, coloreado por tasa de ocupación.

¿Por qué?
Identifica áreas con posible hacinamiento o infrautilización, útil para focalizar políticas de vivienda o estrategias comerciales.

8. Análisis económico poblacional
Cálculo de PEA, tasas de participación y desempleo por AGEB, diferenciadas por sexo.

Integración de todos los indicadores demográficos y económicos en un solo dataframe por AGEB (reynx_analisis_completo).

9. Score de potencial territorial (construcción de índice compuesto)

Se sintetizan todos los indicadores relevantes en un solo "Score_Total" para cada AGEB.

¿Cómo?

Indicadores clave se normalizan (0-100).

Se ponderan según relevancia (ver abajo).

Se crea un índice de potencial de mejoras habitacionales (más alto = más necesidad y oportunidad de mejora).

r
Copiar
Editar
Indice_Potencial_Mejoras = (
  (1 - IIB) * 0.40 +
  (1 - IEH) * 0.30 +
  IBD * 0.10 +
  IM * 0.10 +
  ITE * 0.10
)

Score_Total = (
  Score_Poblacion * 0.15 +
  Score_PEA * 0.18 +
  Score_Ocupacion * 0.12 +
  Score_Viviendas * 0.12 +
  Score_Ocupantes * 0.08 +
  Score_NSE * 0.20 +
  Score_Potencial_Mejoras * 0.15
)
Clasificación de AGEBs en 5 niveles (quintiles):
Muy alto potencial, Alto, Medio, Bajo, Muy bajo.

6. Visualización geoespacial interactiva
Se utiliza leaflet para crear mapas interactivos.

Cada AGEB se colorea por categoría de potencial y muestra popup con información relevante.

r
Copiar
Editar
library(leaflet)
# ... chunk esencial de leaflet ...
¿Por qué?
Permite identificar visualmente zonas de alto y bajo potencial, validando el análisis estadístico en el territorio real.

📍 Preparación de los Datos Espaciales
Para poder visualizar los resultados de potencial territorial por AGEB en un mapa interactivo, fue necesario obtener el shapefile de AGEBs urbanos de Reynosa a partir de los datos abiertos de INEGI, y asegurarse de que las coordenadas estuvieran en el sistema adecuado para Leaflet (WGS84, EPSG:4326).

Pasos clave:

Descargar el archivo shp de AGEBs urbanos de Reynosa desde INEGI.

Cargarlo en R usando sf.

Convertir las coordenadas al sistema WGS84 (EPSG:4326), que es el estándar para visualización web y mapas interactivos.

Ejemplo de Código
r
Copiar
Editar
library(sf)

# 1. Cargar el shapefile de AGEBs urbanos de Reynosa
mapa_reyno <- st_read("reynosa_map.shp")

# 2. Transformar las coordenadas al sistema WGS84
mapa_reyno_transformado <- st_transform(mapa_reyno, crs = 4326)

# 3. Enriquecer con información y crear popups
mapa_reyno_leaflet <- mapa_reyno_transformado %>%
  left_join(reyno_ageb_categorizado, by = c("CVE_AGEB" = "AGEB")) %>%
  mutate(
    popup_info = paste(
      "<strong>Información del AGEB:</strong><br>",
      "<strong>AGEB:</strong>", CVE_AGEB, "<br>",
      "<strong>Categoría de Potencial:</strong>", Categoria_Potencial, "<br>",
      "<strong>Score Total:</strong>", round(Score_Total, 2), "<br>",
      "<hr>",
      "<strong>Población Total:</strong>", Pob_Tot, "<br>",
      "<strong>Densidad de habitantes:</strong>", round(densidad_habitantes,2), "<br>",
      "<strong>Total Viviendas:</strong>", Total_Viviendas, "<br>",
      "<strong>Nivel Socioeconómico:</strong>", Nivel_Socioeconomico, "<br>",
      "<hr>",
      "<strong>Indicadores:</strong><br>",
      "<strong>Proporción PEA:</strong>", round(Prop_PEA,2), "<br>",
      "Tasa de Ocupación de Vivienda: ", round(tasa_ocupacion, 2), "%", "<br>",
      "Potencial de Mejoras: ", round(Indice_Potencial_Mejoras, 2)
    )
  )
¿Por qué transformar a WGS84?
La mayoría de los mapas interactivos en la web (incluyendo Leaflet y OpenStreetMap) requieren que los datos estén en el sistema de coordenadas WGS84 (EPSG:4326), que usa latitud y longitud en grados decimales. Si el shapefile original está en otro sistema de referencia (como Lambert, UTM, etc.), los polígonos no se mostrarán correctamente o aparecerán en otro lugar del mundo.


7. Reflexión personal y valor profesional
Aunque originalmente no dominaba la visualización geoespacial ni todo el stack de R, este proyecto evidencia mi capacidad de:

Aprender rápidamente nuevas herramientas y explorar el uso de nuevos paquetes.

Integrar recursos de IA y documentación oficial de fuentes secundarias de información.

Llevar un análisis de datos desde la recolección, transformación, carga y eso hasta la visualización avanzada e interpretación para toma de decisiones.

8. Consideraciones y utilidad
La documentación y scripts pueden ser útiles para planners, analistas y cualquier persona interesada en el análisis urbano, social y de mercado en México.

La metodología es replicable para otras ciudades.

9. Referencias y agradecimientos
INEGI (2020). Censo de Población y Vivienda 2020.

Comunidad R y recursos de AI por la ayuda en visualización avanzada.

Última actualización: Mayo 2025
