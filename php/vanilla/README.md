# Vanilla PHP - Neurelo API

### Prerequisites:

- [Docker](https://docs.docker.com/get-docker/) to start the development environment
- [Guzzle : guzzlehttp/guzzle](https://github.com/guzzle/guzzle)

> :warning: **Note**: If you are not using Docker, you can use PHP, Composer and Nginx to run the application. Simply include the `data` directory in your Nginx configuration. You can take a look at `./nginx` for a configuration.

### Install dependencies:

```sh
# Install dependencies
cd data && composer install
```

### Run the application:

- Docker compose file reads environment variables from `.env` file.
- Let's create a `.env` file and set the environment variables. You can use the `.env.example` file as a reference.

```sh
# Fill in the environment variables in the .env file
cp .env.example .env

# Start the development environment
docker-compose up -d
```

- Open the browser and navigate to [http://localhost:8229](http://localhost:8229)

```sh
# Stop the development environment
docker-compose down
```

> :warning: **Note**: All docker related commands should be run from the root of project where `docker-compose.yml` file is located.
