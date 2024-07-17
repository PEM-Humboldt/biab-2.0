---
title: "Guía para Crear y Agregar un Single Script"
author: 
  - name: "Rincon-Parra VJ"
    email: "rincon-v@javeriana.edu.co"
affiliation: "Instituto de Investigación de Recursos Biológicos Alexander von Humboldt - IAvH"
output: 
  #md_document:
  github_document:
    md_extension: +gfm_auto_identifiers
    preserve_yaml: true
    toc: true
    toc_depth: 6
---

Guía para Crear y Agregar un Single Script
================

Este tutorial describe los pasos necesarios para crear un
`Single Script` y agregarlo a la plataforma Bon in a Box. Estos códigos
corresponden a bloques de código diseñados para realizar tareas
específicas de principio a fin. Cada `Single Script` está diseñado para
recibir ciertos parámetros y argumentos como entradas, procesar estas
entradas mediante el código interno y luego generar salidas específicas
basadas en el procesamiento realizado.

- [Componentes del script](#componentes-del-script)
- [Archivos YML](#archivos-yml)
  - [Creación y Estructura de Archivos
    YML](#creación-y-estructura-de-archivos-yml)
- [Bloque de Código - Script](#bloque-de-código---script)
  - [Componentes minimos de código](#componentes-minimos-de-código)
    - [Parámetros de Sesión](#parámetros-de-sesión)
      - [Instalar Librerías](#instalar-librerías)
      - [Cargar Librerías](#cargar-librerías)
      - [Establecer Entorno de Trabajo](#establecer-entorno-de-trabajo)
    - [Ejecucion del código](#ejecucion-del-código)
    - [Exportar resultados](#exportar-resultados)
    - [Probar y Validar el Script](#probar-y-validar-el-script)

## Componentes del script

Los `Single Script` constan de dos componentes principales: un bloque de
código y un archivo en formato YML asociado.

- Bloque de código: Contiene el código y las instrucciones de ejecución
  del algoritmo y análisis requerido.
- Archivo YAML: Define la configuración de entradas y salidas de ese
  código, así como los parámetros necesarios para su ejecución. Este
  archivo actúa como un puente que permite a los usuarios configurar el
  código a través de una interfaz amigable, asegurando que los datos
  sean correctamente interpretados y utilizados por el código
  subyacente.

## Archivos YML

Los archivos YML son un formato de serialización de datos que se utiliza
para la configuración de archivos. En Bon in a Box, los archivos YML
definen la configuración de entradas y salidas del código, así como los
parámetros necesarios para ejecutar un script. Actúan como un puente
entre la interfaz de usuario y el código subyacente.

### Creación y Estructura de Archivos YML

Aunque los archivos YML pueden generarse desde cualquier editor de
texto, se recomienda organizarlos en [Visual Studio Code]() para
facilitar su organización. La correcta indentación y los saltos de línea
son cruciales, ya que errores en el formato pueden impedir su inclusión
en la plataforma.

En Bon in a Box, los archivos YML siguen una estructura específica,
detallada en la [documentación oficial de bon in a
box](https://github.com/GEO-BON/bon-in-a-box-pipeline-engine/blob/main/README-user.md#describing-a-script),
que debe incluir como minimo los siguientes elementos principales:

- `script`: Nombre del código subyacente al YML, incluyendo su extensión
  (ej. `myScript.R`). Debe ubicarse en el mismo folder que el YML para
  una correcta conexión.
- `name`: Etiqueta con la que aparecerá el script en la interfaz de Bon
  in a Box.
- `description`: Breve descripción del objetivo y procesamiento del
  código.
- Lista de `input`: Argumentos de entrada que el script necesita. Los
  inputs tienen una línea example donde se definen los valores de
  ejemplo por defecto.
- Lista de `output`: Resultados que el script genera como salida.

<!-- -->

    script: # script file with extension, such as "myScript.py".
    name: # short name, such as My Script
    description: # Targetted to those who will interpret pipeline results and edit pipelines.
    author: # 1 to many
      - name: # Full name
        email: # Optional, email address of the author. This will be publicly available.
        identifier: # Optional, full URL of a unique digital identifier, such as an ORCID.
    license: # Optional. If unspecified, the project's MIT license will apply.
    external_link: # Optional, link to a separate project, github repo, etc.
    timeout: # Optional, in minutes. By defaults steps time out after 1h to avoid hung process to consume resources. It can be made longer for heavy processes.

    inputs: # 0 to many
      key: # replace the word "key" by a snake case identifier for this input
        label: # Human-readable version of the name
        description: # Targetted to those who will interpret pipeline results and edit pipelines.
        type: # see below
        example: # will also be used as default value, can be null

    outputs: # 1 to many
      key:
        label:
        description:
        type:
        example: # optional, for documentation purpose only

    references: # 0 to many
      - text: # plain text reference
        doi: # link

Cada input y output comienza nombrándose con una `key` cuyo valor
asignado será el mismo que se usará en el código para referenciarla.
Esto es crucial, ya que cambios en estos parámetros pueden hacer que el
código no interprete correctamente los inputs y outputs definidos.
Además, estos elementos incluyen parámetros como label, description y
type. Los tipos de archivo admitidos están descritos en la sección input
and output types de la [documentación oficial de bon in a
box](https://github.com/GEO-BON/bon-in-a-box-pipeline-engine/blob/main/README-user.md#describing-a-script)
y corresponden a [MIME media
types](https://www.iana.org/assignments/media-types/media-types.xhtml).

## Bloque de Código - Script

La definición del YML permite la visualización de sus parámetros en la
plataforma de Bon in a Box, donde pueden ejecutarse ya sea como
`Single Script` o como `pipeline` integrado con otros códigos. Sin
embargo, para que la ejecución se realice de forma efectiva, el código
asociado debe estructurarse lógicamente.

El código debe guardarse con el mismo nombre y extensión especificados
en el parámetro script del YML. Cuando se oprime `run` en Bon in a Box,
el servidor crea una carpeta con el nombre del código dentro de la ruta
~/output del servidor. En esa carpeta, para cada ejecución, se genera
una carpeta con nombre temporal en la que se crea un archivo input.json
con las entradas especificadas. El código leerá ese archivo y lo usará
para la ejecución.

### Componentes minimos de código

Para este ejemplo, utilizaremos un código en R, pero la misma lógica
aplica para códigos en Python o Julia.

#### Parámetros de Sesión

##### Instalar Librerías

La ejecución de códigos se respalda en el uso de librerías o paquetes.
Estas deben estar instaladas en el servidor del repositorio para la
correcta ejecución del código. Es recomendable listar las librerías
necesarias en packagesNeed y validar cuáles no están instaladas para
proceder a instalarlas.

``` r
# Install necessary libraries - packages  
packagesPrev<- installed.packages()[,"Package"] # Check and get a list of installed packages in this machine and R version
packagesNeed<- c("magrittr", "dplyr", "plyr", "ggplot2", "tibble", "pbapply", "rredlist", "plyr", "reshape2") # Define the list of required packages to run the script
new.packages <- packagesNeed[!(packagesNeed %in% packagesPrev)]; if(length(new.packages)) {install.packages(new.packages, binary=T, force=T, dependencies = F, repos= "https://packagemanager.posit.co/cran/__linux__/jammy/latest")} # Check and install required packages that are not previously installed
```

##### Cargar Librerías

Una vez instaladas, las librerías deben cargarse. Es recomendable llamar
las funciones específicas de las librerías directamente mediante :: para
no sobrecargar el servidor.

``` r
# Load libraries
packagesList<-list("magrittr", "rredlist", "ggplot2") # Explicitly list the required packages throughout the entire routine. Explicitly listing the required packages throughout the routine ensures that only the necessary packages are listed. Unlike 'packagesNeed', this list includes packages with functions that cannot be directly called using the '::' syntax. By using '::', specific functions or objects from a package can be accessed directly without loading the entire package. Loading an entire package involves loading all the functions and objects 
lapply(packagesList, library, character.only = TRUE)  # Load libraries - packages  
```

##### Establecer Entorno de Trabajo

Para completar el puente entre el código y el YML, es necesario que el
código lea el archivo `input.json` generado por el YML subyacente. Para
esto, el servidor usa la función Sys.setenv hacia la ruta del folder
`output` donde se almacena. Esto crea en el entorno del código en el
servidor una variable outputFolder con la ruta completa del folder
`output` desde donde leer `input.json`. Además, sobre esa ruta se deben
guardar todos los resultados para su visualización y descarga.

Sin embargo, para facilitar la depuración y pruebas del código, se
añaden unas líneas de lectura del outputFolder fuera del servidor. Esta
línea tiene un condicional que solo se ejecuta fuera del servidor, y lo
que hace es completar las rutas del repositorio al entorno de la máquina
local para pruebas.

``` r
Sys.setenv(outputFolder = "/path/to/output/folder")

# Option 2: Recommended for debugging purposes to be used as a testing environment. This is designed to facilitate script testing and correction
if ( (!exists("outputFolder"))  ) {
  outputFolder<- {x<- this.path::this.path();  file_prev<-  paste0(gsub("/scripts.*", "/output", x), gsub("^.*/scripts", "", x)  ); options<- tools::file_path_sans_ext(file_prev) %>% {c(., paste0(., ".R"), paste0(., "_R"))}; folder_out<- options %>% {.[file.exists(.)]} %>% {.[which.max(sapply(., function(info) file.info(info)$mtime))]}; folder_final<- list.files(folder_out, full.names = T) %>% {.[which.max(sapply(., function(info) file.info(info)$mtime))]} }
}
```

Con outputFolder definido, las siguientes líneas cargan `input.json` con
las entradas y hacen algunos ajustes para su correcta lectura posterior.

``` r
# Set the 'input' environment variables. The 'input' environment contains the specified inputs from the ByB platform.
# The input file 'input.json' is generated by executing the 'Run Script' command in the ByB platform.
input <- rjson::fromJSON(file=file.path(outputFolder, "input.json")) # Load input file

# This section adjusts the input values based on specific conditions to rectify and prevent errors in the input paths
input<- lapply(input, function(x) { if (!is.null(x) && length(x) > 0 && grepl("/", x) && !grepl("http://", x)  ) { 
  sub("/output/.*", "/output", outputFolder) %>% dirname() %>%  file.path(x) %>% {gsub("//+", "/", .)}  } else{x} }) 
```

#### Ejecucion del código

Con el entorno de trabajo establecido, ya se tienen todas las entradas
necesarias para la ejecución del código. El archivo `input.json` se
convierte en una lista llamada `input` dentro del entorno de R. Como
cualquier lista en este entorno, puede citarse simplemente usando \$ o
nombrando el elemento directamente como input\[\[elemento\]\].

Con esto establecido, se debe ejecutar el código normalmente. Es
importante que el código esté bien documentado con comentarios internos
para facilitar su entendimiento y mantenimiento. Algunos consejos de
buenas prácticas de documentación están detallados en [Buneas practicas
de docucumentación y comentarios en R]()

#### Exportar resultados

Una vez se obtengan los resultados del código, estos deben exportarse de
la manera preferida. Por ejemplo, un archivo CSV se exporta con la
función write.csv o cualquier otra función equivalente que genere un
archivo de ese tipo o extensión. Se recomienda que en el código se
especifique explícitamente la ruta completa del archivo exportado y que
se guarde en la ruta establecida como elemento outputFolder, para así
facilitar su referenciación posterior.

``` r
## Write results ####  
IUCN_historyAssesment_data_path<- file.path(outputFolder, paste0("IUCN_historyAssesment_data", ".csv")) # Define the file path 
write.csv(IUCN_historyAssesment_data, IUCN_historyAssesment_data_path, row.names = F) # write result
```

Todas estas rutas deben incorporarse en una lista output cuyos nombres
deben ser equivalentes a los elementos nombrados en la lista output del
YML. Esto es necesario porque Bon in a Box espera esos argumentos para
disponer la visualización y posibilidad de descarga de esos archivos.
Con la lista generada, el último paso es exportarla como un archivo JSON
en el outputFolder para completar el proceso.

``` r
# Define final output list
output<- list(IUCN_historyAssesment_data= IUCN_historyAssesment_data_path)

#### Outputing result to JSON ####

# Write the output list to the 'output.json' file in JSON format
setwd(outputFolder)
jsonlite::write_json(output, "output.json", auto_unbox = TRUE, pretty = TRUE)
```

#### Probar y Validar el Script