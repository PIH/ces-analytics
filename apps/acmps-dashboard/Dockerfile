FROM rocker/r-base

MAINTAINER Brandon Istenes "bistenes@gmail.com"

# Install requisite R packages
RUN apt-get update \
        && apt-get install -y \
            libssl-dev libcurl4-openssl-dev \
        && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'rmarkdown', 'DT', 'dplyr', 'readxl', 'lubridate', 'ggplot2', 'plotly', 'reshape2', 'scales'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/app
COPY . /root/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app')"]
