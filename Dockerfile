FROM rocker/tidyverse:4.4

LABEL org.opencontainers.image.title="diseasenowcasting"
LABEL org.opencontainers.image.description="Epidemic nowcasting via censored regression"
LABEL org.opencontainers.image.source="https://github.com/RodrigoZepeda/diseasenowcasting"

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      libgsl-dev \
      libhdf5-dev && \
    rm -rf /var/lib/apt/lists/*

RUN echo 'options(repos = c( \
    CRAN = "https://cloud.r-project.org", \
    RTMB = "https://kaskr.r-universe.dev", \
    EPINOWCAST = "https://epinowcast.r-universe.dev"))' \
  >> /usr/local/lib/R/etc/Rprofile.site

RUN Rscript -e "install.packages('pak', N_cpus = max(parallel::detectCores(logical = TRUE) - 1, 1))"

RUN Rscript -e "pak::pkg_install(c( \
    'RTMB',\
    'RTMBdist',\
    'RTMBode',\
    'scoringutils',\
    'future',\
    'RodrigoZepeda/tbl.now',\
    'RodrigoZepeda/diseasenowcasting'\
  ))"

WORKDIR /work
CMD ["R", "--no-save"]