mod cli;
mod create;
mod http;
mod model;

use std::process::exit;

use crate::model::Model;

use self::{
    cli::{Cli, Command},
    http::Client,
    model::{Data, Error, Response},
};
use anyhow::Result;
use clap::Parser;

fn handle_response(response: Response) {
    match response {
        Response::Data(Data::One(model)) => {
            println!("result: {:#?}", model.inner_debug());
        }
        Response::Data(Data::Many(models)) => {
            println!(
                "results: {:#?}",
                models
                    .iter()
                    .flatten()
                    .map(Model::inner_debug)
                    .collect::<Vec<_>>()
            );
        }
        Response::Data(_) => {
            println!("operation successful");
        }
        Response::Errors(errors) => {
            for Error { error } in errors {
                eprintln!("{error}");
            }

            exit(1);
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let neurelo_env_url = std::env::var("NEURELO_ENV_URL")?;
    let api_key = std::env::var("X_API_KEY")?;

    let cli = Cli::parse();
    let client = Client::new(neurelo_env_url, api_key)?;

    let response = match cli.command {
        Command::Create(object) => client.create_object(object).await?,
        Command::Get(args) => client.get_object(args).await?,
        Command::Set(object) => client.set_object(object).await?,
        Command::Delete(object) => client.delete_object(object).await?,
    };

    handle_response(response);

    Ok(())
}
