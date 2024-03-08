use std::fmt::Debug;

use clap::Args;
use serde::{de::IgnoredAny, Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Args)]
pub struct Pet {
    #[clap(long)]
    pub id: String,

    #[clap(long)]
    pub name: String,

    #[clap(long)]
    pub species: String,

    #[clap(long)]
    pub age: u8,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Doctor {
    pub id: String,
    pub first_name: String,
    pub last_name: String,
    #[serde(skip_serializing_if = "Option::is_none", rename = "clinics_ref")]
    pub work_location: Option<VetClinic>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct VetClinic {
    pub id: String,

    pub name: String,

    #[serde(default, rename = "pets_ref")]
    pub patients: Vec<Pet>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Response {
    Data(Data),
    Errors(Vec<Error>),
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Data {
    One(Model),
    Many(Option<Vec<Model>>),
    Other(IgnoredAny),
}
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Model {
    Pet(Pet),
    Doctor(Doctor),
    Clinic(VetClinic),
}

impl Model {
    pub fn inner_debug(&self) -> &dyn Debug {
        match self {
            Model::Pet(pet) => pet,
            Model::Doctor(doctor) => doctor,
            Model::Clinic(clinic) => clinic,
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct Error {
    pub error: String,
}
