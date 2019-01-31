FROM rocker/r-base

MAINTAINER Brandon Istenes "bistenes@gmail.com"

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/helloapp
COPY helloapp /root/helloapp

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/helloapp')"]
