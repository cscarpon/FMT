# Use the shiny-verse base image which includes Shiny server, Shiny, and tidyverse packages
FROM rocker/shiny-verse:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libudunits2-dev \
    libglu1-mesa-dev \
    python3-pip

# Install Python packages using pip
RUN pip3 install numpy

# Install R packages
RUN install2.r --error \
    rgl \
    terra \
    lidR \
    sf \
    scales \
    leaflet \
    concaveman \
    reticulate

# Copy the app to the image
COPY ./fmt /srv/shiny-server/
COPY ./r /srv/shiny-server/r

# Make all app files readable (solves potential issues when the app runs)
RUN chmod -R +r /srv/shiny-server/

# Set the working directory to the app directory
WORKDIR /srv/shiny-server/

# Expose port 3838 to make the Shiny app available
EXPOSE 3838

# Set the default command to run the app
CMD ["shiny-server"]
