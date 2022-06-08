# BON in a Box 2.0

Mapping Post-2020 Global Biodiversity Framework indicators and their uncertainty.

A Geo BON project, born from a collaboration between Microsoft, McGill, Humbolt institue, Université de Sherbrooke, Université Concordia and Université de Montréal.

## Running the servers locally
Prerequisites : 
 - Git
 - Linux: Docker with Docker Compose installed
 - Windows/Mac: Docker Desktop
 - At least 6 GB of free space (this includes the installation of Docker Desktop)

To run:
1. Clone repository (Windows users: do not clone this in a folder under OneDrive.)
2. Using a terminal, navigate to top-level folder.
3. `docker compose build` (this needs to be re-run everytime the server code changes, or when using git pull if you are not certain.)
4. `docker compose up -d`
5. In browser:
    - http://localhost/ shows the UI
6. `docker compose down` (to stop the server when done)

Servers do not need to be restarted when modifying scripts in the /scripts folder:
- When modifying an existing script, simply re-run the script from the UI and the new version will be executed.
- When adding/renaming/removing scripts, refresh the browser page.

## Scripts
The scripts perform the actual work behind the scenes. They are located in [/scripts folder](/scripts)

Currently supported : 
 - R version 4.1.2
 - Julia version 1.7.1
 - Python3
 - sh

Script lifecycle:
1. Script launched with output folder as a parameter.
2. Script reads input.json to get execution parameters (ex. species, area, data source, etc.)
3. Script performs its task
4. Script generates output.json, containing links to result files, or native values (number, string, etc.)

### Describing a script
The script description is in a .yml file next to the script. It describes
- The filename of the script to run
- Inputs
- Outputs
- Description
- External link (optional)
- References

See [example](/scripts/HelloWorld/HelloR.yml)

Each input and output must declare a type, *in lowercase.* The following are accepted:
| "type" attribute               | UI rendering                 |
|--------------------------------|------------------------------|
| application/dbf                | Link to the shape file       |
| application/geopackage+sqlite3 | Link to the geopackage file  |
| boolean                        | Plain text                   |
| float, float[]                 | Plain text                   |
| image/jpg                      | \<img> tag                   |
| image/tiff;application=geotiff | Map widget (leaflet)         |
| int, int[]                     | Plain text                   |
| text, text[]                   | Plain text                   |
| text/csv                       | HTML table (partial content) |
| text/plain                     | Plain text file              |
| text/tab-separated-values      | HTML table (partial content) |
| (any unknown type)             | Plain text                   |

#### Reporting problems
The output keys `warning` and `error` can be used to report problems in script execution. They do not need to be described in the `outputs` section of the description. Both will be displayed specially in the UI.

## Pipelines
Each script becomes a pipeline step. Pipelines support the same input and output types and UI rendering as individual scripts.

To create or edit pipelines, see the [pipeline editor documentation](/docs/pipeline-editor.md).

## Developer documentation
The linked content is intended for those developing the microservice infrastructure supporting the pipelines.

[Developer documentation](/docs/dev.md)

