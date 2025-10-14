FROM r-base

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'aws.s3', 'DT', 'stringr', 'lubridate', 'purrr', 'optparse', 'dplyr', 'shinymanager'), repos='https://cran.rstudio.com/')"

RUN groupadd --gid 1001 group && \
    useradd --gid 1001 --uid 1001 --no-create-home --shell /bin/bash user
RUN mkdir -p /usr/local/src/app
RUN chown -R user:group /usr/local/src/app

COPY ./app /usr/local/src/app
WORKDIR /usr/local/src/app

USER user