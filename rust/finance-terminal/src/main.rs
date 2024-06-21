mod api;
mod app;
mod cli;
mod ct;
mod http;
mod model;
mod sample;
mod ui;

use std::{process::exit, time::Duration};

use crate::model::Model;

use self::{
    cli::{Cli, SingleCommand},
    http::Client,
    model::{Data, Error, Response},
};
use anyhow::Result;
use clap::Parser;
use cli::TerminalObject;

fn handle_response(response: Response) {
    match response {
        Response::Data(Data::One(model)) => {
            println!("{}", serde_json::to_string_pretty(&model).unwrap());
        }
        Response::Data(Data::Many(models)) => {
            println!(
                "{:#?}",
                models.iter().map(Model::inner_debug).collect::<Vec<_>>()
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

    if let cli::Command::Run(command) = cli.command {
        let _ = handle_single_command(client, command).await;
        return Ok(());
    }
    if let cli::Command::Terminal(terminal) = cli.command {
        let _: Result<_, _> = handle_interactive(Cli::parse(), client, terminal).await;
        return Ok(());
    }

    Ok(())
}

async fn handle_single_command(client: Client, command: SingleCommand) -> Result<()> {
    let response = match command {
        SingleCommand::Create(object) => client.create_object(object).await?,
        SingleCommand::Get(args) => client.get_object(args).await?,
        SingleCommand::Delete(object) => client.delete_object(object).await?,
        SingleCommand::DeleteAll(object) => client.delete_objects(object).await?,
    };
    handle_response(response);
    Ok(())
}

async fn handle_interactive(
    cli: Cli,
    client: Client,
    terminal: TerminalObject,
) -> Result<(), Box<dyn std::error::Error>> {
    let tick_rate = Duration::from_millis(cli.tick_rate);
    crate::ct::run(tick_rate, client, cli.enhanced_graphics).await?;
    Ok(())
}
