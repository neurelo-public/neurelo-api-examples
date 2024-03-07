## Haskell Backend App

A Yesod app using neurelo as a data source. It uses the openapi [haskell-http-client](https://openapi-generator.tech/docs/generators/haskell-http-client/) generator to generate the Neurelo API.

### Directory Structure

- [neurelo](neurelo): The Neurelo API definition. You will need to import the API definition into Neurelo to test this example.
- [openapi](openapi): The OpenAPI spec.
- [neurelo_api](neurelo_api): The generated API bindings.
- [server](server): Our example app.

### Starting server

```bash
cd server

# Fill in .env with your neurelo credentials
cp .env.example .env

# This will expose the API on http://localhost:12345
stack run
```

### Using the example API

The API itself exposes 3 endpoints:
- `GET /user/list`: List existing users.
- `POST /user`: Create a new user.
- `PUT /user/update`: Update an existing user.

There is a sample script excercising this API in `./testCli.hs`.

```
$ ./testCli.hs
Enter email:
somebody@gmail.com
Enter password:
adsf
Enter name:
John Doe
Created user:
User {email = "somebody@gmail.com", name = "John Doe", id = "65ea1a5cd59f8efe7755af2d"}
Users with the same domain:
[User {email = "test@gmail.com", name = "The Name", id = "65ea18e2d59f8efe7755af2b"},User {email = "somebody@gmail.com", name = "John Doe", id = "65ea1a5cd59f8efe7755af2d"}]
Enter new email:
somebody@otherdomain.com
Enter new name:
Some other name
Updated user:
User {email = "somebody@otherdomain.com", name = "Some other name", id = "65ea1a5cd59f8efe7755af2d"}
```

### Using your own API

If you want to use your own Neurelo API, you should follow these steps:

#### 1. Download the OpenAPI spec from Neurelo

You can find the download under Definitions->Docs->OpenAPI Specification.

#### 2. Copy the spec and openapi-generator into your Haskell project

```bash
$ mkdir openapi
$ cp path/to/downloaded/file.json openapi/schema.json
```

#### 2.1 Copy OpenAPI-Generator from this repository

Currently you will need to use a the openapi-generator in [openapi/openapi-generator-cli.jar](./openapi/openapi-generator-cli.jar). 
You can find the relevant changes in https://github.com/OpenAPITools/openapi-generator/pull/18047.

```bash
$ cp path/to/this/repo/openapi/openapi-generator-cli.jar openapi
```

#### 3. Generate code

```bash
$ java -jar openapi/openapi-generator-cli.jar generate -i openapi/schema.json -g haskell-http-client -o neurelo-api --additional-properties=generateLenses=false
[main] INFO  o.o.codegen.DefaultGenerator - Generating with dryRun=false
[main] INFO  o.o.codegen.DefaultGenerator - OpenAPI Generator: haskell-http-client (client)
[main] INFO  o.o.codegen.DefaultGenerator - Generator 'haskell-http-client' is considered stable.
...
```