FROM rocker/r-ver:4.4.0

ENV DEFAULT_USER=rstudio
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN mkdir /project

ENV RENV_PATHS_ROOT=/renv_cache
ENV RENV_PATHS_LIBRARY=/project-library
ENV RENV_VERSION=1.0.7
ENV repo=https://p3m.dev/cran/__linux__/jammy/latest
RUN mkdir ${RENV_PATHS_LIBRARY}

COPY ops/create_user.sh /rocker_scripts/create_user.sh
RUN /rocker_scripts/create_user.sh rstudio $USER_UID $USER_GID
RUN chown -R $DEFAULT_USER:$DEFAULT_USER \
     /project $RENV_PATHS_LIBRARY

RUN apt update && apt install -y \
    libssl-dev libcurl4-openssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    libxml2-dev \
    sudo git

WORKDIR /project

# Install renv
RUN <<EOR
R --vanilla -e "install.packages('remotes', repos='${repo}')
remotes::install_version('renv', version = '${RENV_VERSION}', repos='${repo}')"
EOR

# Copy renv files and dirs
COPY renv.lock /project/renv.lock
COPY .renv_cache $RENV_PATHS_ROOT
COPY .Rprofile /project/.Rprofile
COPY renv /project/renv

# Install renv in the project library
RUN <<EOR
R --vanilla -e "dir.create(renv::paths\$library(), recursive=TRUE)
file.copy(find.package('renv'), renv::paths\$library(), recursive = TRUE)"
EOR

# Restore the project library
RUN R -e "renv::restore()" || true
