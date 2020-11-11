FROM rocker/verse
LABEL maintainer='Joao Martins'

ENV USER rstudio

COPY ./README.md /home/$USER/
COPY ./DESCRIPTION /home/$USER/
COPY ./LICENSE /home/$USER/
COPY ./data /home/$USER/data
COPY ./R /home/$USER/R
# COPY ./notebooks /home/$USER/notebooks
# COPY ./docs /home/$USER/docs

RUN wget https://github.com/zambujo/editorials/raw/main/DESCRIPTION && \
R -e "install.packages('remotes'); \
remotes::install_deps()"
