FROM r-base

# r-base mixes Debian testing and sid; dev headers must come from sid to match the preinstalled libs
RUN apt-get update && apt-get install -y -t unstable \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "pkgs <- c('shiny', 'aws.s3', 'DT', 'stringr', 'lubridate', 'purrr', 'optparse', 'dplyr', 'shinymanager'); \
    install.packages(pkgs, repos='https://cran.rstudio.com/'); \
    missing <- pkgs[!sapply(pkgs, requireNamespace, quietly=TRUE)]; \
    if (length(missing)) stop('Failed to install: ', paste(missing, collapse=', '))"

RUN groupadd --gid 1001 group && \
    useradd --gid 1001 --uid 1001 --no-create-home --shell /bin/bash user
RUN mkdir -p /usr/local/src/app
RUN chown -R user:group /usr/local/src/app

COPY ./app /usr/local/src/app
WORKDIR /usr/local/src/app

USER user