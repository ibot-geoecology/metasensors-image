FROM r-base

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'aws.s3', 'DT', 'stringr', 'lubridate', 'purrr', 'optparse', 'dplyr', 'shinymanager'), repos='https://cran.rstudio.com/')"

RUN useradd -ms /bin/bash shiny
RUN mkdir -p /usr/local/src/app
RUN chown -R shiny:shiny /usr/local/src/app

COPY ./app /usr/local/src/app
WORKDIR /usr/local/src/app

USER shiny