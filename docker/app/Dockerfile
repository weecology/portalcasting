# base image is weecology/portalcasting, which includes all required environment content

FROM weecology/portalcasting:latest

# Setup production directory

RUN R -e "portalcasting::setup_production( )"

