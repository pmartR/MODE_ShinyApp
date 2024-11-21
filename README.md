## MODE:  An application to visualize omics data in trelliscope.
****

Create shareable and interactive trelliscope displays for visualizing omics data and statistics results. 

****

## How to Run

#### Locally in RStudio

Clone the git repo, open the global.R, ui.R, or server.R file in RStudio, and click the "Run App" button.

#### Within our Website

Go to our [application website](https://map.emsl.pnnl.gov/app/mode-classic)

#### Using the Docker Container

First build the base docker file using Dockerfile-base, and then build the MODE dockerfile using Dockerfile. Make
sure to update the dockerfiles with whatever you named your containers.

`docker build -f Dockerfile-base --no-cache -t <name>`

`docker build -t Dockerfile --no-cache -t <name>`