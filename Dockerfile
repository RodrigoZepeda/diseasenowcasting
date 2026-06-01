# dcast3 Docker image
# Base: rocker/tidyverse (R + tidyverse pre-installed)
# Adds: RTMB, RTMBdist, tbl.now (GitHub), and dcast3 itself.
#
# Build:
#   docker build -t dcast3 .
#
# Run (interactive R session):
#   docker run --rm -it dcast3 R
#
# Run (Rscript):
#   docker run --rm -v $(pwd):/work dcast3 Rscript /work/my_analysis.R

FROM rocker/tidyverse:4.4

LABEL org.opencontainers.image.title="dcast3"
LABEL org.opencontainers.image.description="Epidemic nowcasting via censored regression (RTMB engine)"
LABEL org.opencontainers.image.source="https://github.com/RodrigoZepeda/dcast3"

# System dependencies required by RTMB / Matrix / scoringutils
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgsl-dev \
    liblapack-dev \
    libblas-dev \
    libhdf5-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install CRAN dependencies
# (tidyverse is already installed in the base image)
RUN Rscript -e "\
  options(repos = c(CRAN = 'https://cloud.r-project.org')); \
  install.packages(c( \
    'RTMB', 'RTMBdist', \
    'S7', 'cli', 'Matrix', \
    'scoringutils', 'doFuture', 'foreach', 'future', \
    'remotes' \
  ), dependencies = TRUE)"

# Install tbl.now from GitHub
RUN Rscript -e "remotes::install_github('RodrigoZepeda/tbl.now', upgrade = 'never')"

# Copy the dcast3 package source and install it
COPY . /dcast3/
RUN Rscript -e "remotes::install_local('/dcast3', dependencies = TRUE, upgrade = 'never')"

# Clean up build artefacts
RUN rm -rf /dcast3

# Default working directory for user scripts
WORKDIR /work

# Default command: interactive R
CMD ["R", "--no-save"]
