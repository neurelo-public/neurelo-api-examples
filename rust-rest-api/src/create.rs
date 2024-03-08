use clap::Args;
use serde::Serialize;

#[derive(Debug, Serialize, Args)]
pub struct CreatePet {
    #[clap(long)]
    pub name: String,

    #[clap(long)]
    pub species: String,

    #[clap(long)]
    pub age: u8,
}

#[derive(Debug, Serialize, Args)]
pub struct CreateDoctor {
    #[clap(long)]
    pub first_name: String,
    #[clap(long)]
    pub last_name: String,
}

#[derive(Debug, Serialize, Args)]
pub struct CreateClinic {
    #[clap(long)]
    pub name: String,
}
