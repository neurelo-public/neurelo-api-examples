use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

use crate::api::{CreatePortfolio, CreateSecurity, CreateTrade};
use strum_macros::Display;

#[derive(Debug, Parser)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,

    #[arg(long, default_value = "250")]
    pub tick_rate: u64,

    #[arg(long, default_value = "true")]
    pub enhanced_graphics: bool,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    #[command(subcommand)]
    Run(SingleCommand),

    Terminal(TerminalObject),
}

#[derive(Debug, Args, Clone)]
pub struct TerminalObject {
    #[arg(long, value_name = "file.json")]
    pub config: Option<PathBuf>,
}

#[derive(Debug, Subcommand)]
pub enum SingleCommand {
    /// Insert a single object into the database
    #[command(subcommand)]
    Create(CreateObject),

    /// Retrieve one or more objects from the database
    #[command(subcommand)]
    Get(GetObject),

    /// Delete an object
    #[command(subcommand)]
    Delete(DeleteObject),

    /// Delete objects
    #[command(subcommand)]
    DeleteAll(DeleteAllObject),
}

#[derive(Debug, Subcommand, Display)]
#[strum(serialize_all = "lowercase")]
pub enum CreateObject {
    /// Insert a portfolio into the database.    
    Portfolio(CreatePortfolio),

    /// Insert an account into the database.    
    Security(CreateSecurity),

    /// Insert a trade into the database.    
    Trade(CreateTrade),
}

#[derive(Debug, Subcommand, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum GetObject {
    /// Retrieve one or more portfolio from the database.
    Portfolio(GetArgs),

    /// Retrieve one or more portfolio from the database.
    Security(GetArgs),

    /// Retrieve one or more portfolio from the database.
    Trade(GetArgs),
}

#[derive(Debug, Args)]
pub struct GetArgs {
    /// Retrieve only up to a certain number of objects.
    #[clap(long)]
    pub limit: Option<u8>,

    /// Retrieve only the objects matching the specified id(s).
    #[clap(long)]
    pub id: Option<Vec<String>>,
}

#[derive(Debug, Subcommand)]
pub enum DeleteObject {
    /// Delete one or more portfolio from the database.
    Portfolio(DeleteArgs),

    /// Delete one or more securities from the database.
    Security(DeleteArgs),

    /// Delete one or more trade from the database.
    Trade(DeleteArgs),
}

#[derive(Debug, Subcommand)]
pub enum DeleteAllObject {
    Portfolio,
    Security,
    Trade,
}

impl DeleteObject {
    pub fn model_name(&self) -> &str {
        match self {
            DeleteObject::Portfolio(..) => "portfolio",
            DeleteObject::Security(..) => "security",
            DeleteObject::Trade(..) => "trade",
        }
    }

    pub fn model_id(&self) -> &str {
        match self {
            DeleteObject::Portfolio(args)
            | DeleteObject::Security(args)
            | DeleteObject::Trade(args) => &args.id,
        }
    }
}

impl DeleteAllObject {
    pub fn model_name(&self) -> &str {
        match self {
            DeleteAllObject::Portfolio => "portfolio",
            DeleteAllObject::Security => "security",
            DeleteAllObject::Trade => "trade",
        }
    }
}

#[derive(Debug, Args)]
pub struct DeleteArgs {
    /// Delete the objects matching the specified id(s).
    #[clap(long)]
    pub id: String,
}
