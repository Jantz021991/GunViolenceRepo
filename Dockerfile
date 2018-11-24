FROM r-base:latest

MAINTAINER Walt Wells <walt.p.wells@gmail.com>

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libxml2-dev \
    libssl-dev \
    gsl-bin \
    libgsl0-dev

RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinythemes', 'xts', 'dplyr', 'leaflet'), repos='http://cran.rstudio.com/')"

RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
rm -f version.txt ss-latest.deb

COPY app.R /srv/shiny-server/
COPY Assets/styles.css /srv/shiny-server/Assets/styles.css
COPY Assets/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY Data/GunsGeo.rds /srv/shiny-server/Data/GunsGeo.rds

RUN touch /srv/shiny-server/runlog.log

WORKDIR /srv/shiny-server

EXPOSE 80

RUN echo "local({options(shiny.port = 80, shiny.host = '0.0.0.0')})" >> /usr/lib/R/etc/Rprofile.site

COPY Assets/shiny-server.sh /usr/bin/shiny-server.sh
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
CMD /usr/bin/shiny-server.sh