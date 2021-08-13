FROM openanalytics/r-base

LABEL maintainer "Tobias Verbeke <tobias.verbeke@openanalytics.eu>"

# system libraries of general use
RUN apt-get update && apt-get install -y software-properties-common
RUN apt-get upgrade -y
#RUN add-apt-repository ppa:cran/jq
RUN add-apt-repository ppa:ubuntugis/ppa
RUN apt-get -y --no-install-recommends install \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libxml2-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    cmake \
    libfreetype6-dev \
    libfontconfig1-dev \
    xclip \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    ibgeos++-dev \
    libprotobuf-dev \
    libv8-dev \
    libjq-dev \
    libprotoc-dev \
    valgrind \
    libpq-dev \
    netcdf-bin \
    protobuf-compiler \
    && . /etc/environment



  

RUN R -e "install.packages(c('shiny', 'rmarkdown','flexdashboard','knitr','rgeos','rgdal','leaflet', 'tidyverse','geojsonio','sf','plotly','ggiraph', 'rlang', 'lazyeval', 'viridis', 'hrbrthemes', 'dummies','DT', 'feather'),dependencies=TRUE, repos='https://cloud.r-project.org/')"


# copy the app to the image
RUN mkdir -p /root/projects/panel/scripts
#COPY app/panel.Rmd /root/projects/panel/
#COPY scripts/GA_Script.html /root/projects/panel/

#COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "rmarkdown::run('/data/panel.Rmd', shiny_args = list(host = '0.0.0.0', port = 3838))"]


#CMD ["R", "-e", "rmarkdown::run('/root/projects/panel/panel.Rmd')"]
#CMD ["R", "-e", "rmarkdown::run('/root/projects/panel/panel.Rmd', shiny_args = list(port = 3838, host = '0.0.0.0'))"]
