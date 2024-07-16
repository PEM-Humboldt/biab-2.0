---
title: "Lanzar Bon in a Box"
author: 
  - name: "Rincon-Parra VJ"
    email: "rincon-v@javeriana.edu.co"
affiliation: "Instituto de Investigación de Recursos Biológicos Alexander von Humboldt - IAvH"
output: 
  #md_document:
  github_document:
    md_extension: +gfm_auto_identifiers
    preserve_yaml: true
    toc: false
    toc_depth: 6
---

Lanzar Bon in a Box
================
true

Para utilizar la plataforma Bon in a Box en tu computadora, es necesario
utilizar las herramientas
[GitHub](https://docs.github.com/en/desktop/installing-and-authenticating-to-github-desktop/installing-github-desktop)
y [Docker](https://www.docker.com/products/docker-desktop/). Si es la
primera vez que utilizas la plataforma, por favor consulta el [tutorial
de Instalación Bon in a Box](../install_ByB) que describe los requisitos
y el paso a paso para instalación y lanzamiento por primera vez.

Para iniciar el servidor de Bon in a Box, asegúrate de tener Docker
abierto y ejecuta el comando `./server-up.sh` desde una consola de
código linux. Se recomienda abrir la consola de código desde GitHub
Desktop para que se configure automáticamente con la dirección del
repositorio. Para ejecutar el comando, simplemente escribe
`./server-up.sh` en la consola y presiona Enter.

![](README_figures/launch_ByB.png)

La primera ejecución puede llevar más tiempo, ya que descargará los
micro-servicios necesarios para lanzar el contenedor Docker. Las
siguientes ejecuciones serán más rápidas o inmediatas, dependiendo de
los cambios realizados. Este comando ejecutará todas las acciones
definidas en el archivo server-up.sh, ubicado en el directorio clonado,
iniciando así el servidor de Bon in a Box.

Una vez que el servidor esté en funcionamiento, simplemente escribe la
dirección <http://localhost/> en tu navegador para acceder a la
plataforma Bon in a Box. ![](README_figures/localhostByB.png)