# Vanilla PHP - Neurelo API

### Prerequisites:

- [Direnv](https://direnv.net/docs/installation.html) to set environment variables 
- [Rust](https://www.rust-lang.org/tools/install)
- A Neurelo project with the data definition located at `rust-rest-api/neurelo/schema.json` applied
- A Neurelo API key for a running environment
- A Neurelo Gateway URL for a running environment


### Run the application:

- The application reads environment variables from `.env` file.
- Let's create a `.envrc` file and set the environment variables. You can use the `.envrc.example` file as a reference.

```sh
# Copy in the environment variables in the .envrc file
cp .envrc.example .envrc && echo "Please edit the variables in .envrc file"

# After editing the .envrc file, ensure it is loaded
direnv allow

# Print the help menu
cargo run help
```