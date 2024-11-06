ARG R_VERSION
FROM rocker/r-base:4.3.1

RUN apt-get update && \
    apt-get install --yes libssl-dev libsasl2-dev libcurl4-openssl-dev libxml2-dev \
                          pandoc libfontconfig1-dev openssh-server sudo libv8-dev \
                          libharfbuzz-dev libfribidi-dev libfreetype-dev libpng-dev \
                          libtiff5-dev libjpeg-dev r-cran-igraph libfreetype-dev \
                          libfreetype6 libfreetype6-dev pkg-config libbz2-dev libbz2-dev \
                          zlib1g-dev && \
    apt-get clean

RUN R -e ' \
    install.packages(c("remotes","treemap")); \
    require(remotes); \
    install_version("extrafont", version = "0.18"); \
    install_version("float", version = "0.3.0"); \
    install_version("here", version = "1.0.1"); \
    install_version("systemfonts", version = "1.0.6"); \
    install_version("svglite", version = "2.1.3"); \
    install_version("kableExtra", version = "1.3.4"); \
    install_version("knitr", version = "1.41"); \
    install_version("maditr", version = "0.8.3"); \
    install_version("RColorBrewer", version = "1.1.3"); \
    install_version("rmarkdown", version = "2.19"); \
    install_version("stringr", version = "1.5.0"); \
    install_version("tidyverse", version = "1.3.2"); \
    install_version("tinytex", version = "0.43"); \
    install_version("jsonlite", version = "1.8.8"); \
    install_version("RCurl", version = "1.98-1.16"); '

RUN R -e ' \
    require(tinytex); \
    deps <- c("fancyhdr","tabu","float","lipsum","multirow","wrapfig","colortbl","pdflscape","threeparttable","threeparttablex","environ","ulem","makecell","caption","bookmark"); \
        install_tinytex(repository="https://ftp.tu-chemnitz.de/pub/tug/historic/systems/texlive/2022/tlnet-final",version="2022.12",force=TRUE, extra_packages = deps); \
    library(tinytex); \
    options(tinytex.tlmgr.options = "--no-auto-install")'
EXPOSE 22




