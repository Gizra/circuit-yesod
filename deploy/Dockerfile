FROM ubuntu:16.04
RUN apt-get -y update && apt-get install -y libpq5
WORKDIR /var/www
ADD . /var/www
CMD sleep 10 && cd /var/www/dist/ && /var/www/dist/circuit-yesod
