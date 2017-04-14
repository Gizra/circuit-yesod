# Circuit App deployment guide

## Initial deployment

Spin up a server with SSH root access (on-metal, VPS, etc) with Ubuntu LTS (tested with Xenial Xerus).
Install Docker and Docker Compose with the officially supported way:
 - https://docs.docker.com/engine/installation/linux/ubuntu/
 - https://docs.docker.com/compose/install/

Prepare a `deploy` user on the server with Docker access.

Prepare the package by:
 - switch to <repository root>
 - `deploy/package.sh`

Copy to the server:
 - rsync -avzhe ssh . deploy@<server ip>:~/circuit

Login to the server and start the system:

 - `ssh deploy@<server ip>`
 - `cd circuit/deploy`
 - `docker build -t circuit .`
 - To avoid race conditions while creating the database, initially comment out all the circuit instances in `docker-compose.yml`, but one, start it, then restore the YML file, `docker-compose stop`, then proceed to the next step. After the database exist, this hack is not needed anymore.
 - `docker-compose up`

## Releasing a new version

This no-downtime procedure is usable when there's no database change is needed and it's acceptable that some users use the old version, some users use the new version in parallel for a short period.

 - Refresh the `circuit` folder on the server
 - `ssh deploy@<server ip>`
 - `cd circuit/deploy`
 - `docker build -t circuit .`
 - `docker-compose stop circuit_1`
 - `docker-compose stop circuit_2`
 - `docker-compose up -d --no-deps circuit_1`
 - `docker-compose up -d --no-deps circuit_2`
 - `docker-compose stop circuit_0`
 - `docker-compose up -d --no-deps circuit_0`
