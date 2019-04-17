FROM rocker/r-base

MAINTAINER Brandon Istenes "bistenes@gmail.com"

# Install requisite R packages
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'dplyr', 'readxl', 'lubridate', 'ggplot2', 'reshape2'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/app
COPY . /root/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app')"]
