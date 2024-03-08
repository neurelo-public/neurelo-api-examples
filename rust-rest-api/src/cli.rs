use clap::{Args, Parser, Subcommand};

use crate::create::{CreateClinic, CreateDoctor, CreatePet};

#[derive(Debug, Parser)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Insert a single object into the database
    #[command(subcommand)]
    Create(CreateObject),

    /// Retrieve one or more objects from the database
    #[command(subcommand)]
    Get(GetObject),

    /// Update an object
    #[command(subcommand)]
    Set(SetObject),

    /// Delete an object
    #[command(subcommand)]
    Delete(DeleteObject),
}

#[derive(Debug, Subcommand)]
pub enum CreateObject {
    /// Insert a pet into the database.
    Pet(CreatePet),

    /// Insert a clinic into the database.
    Clinic(CreateClinic),

    /// Insert a doctor into the database.    
    Doctor(CreateDoctor),
}

#[derive(Debug, Subcommand)]
pub enum GetObject {
    /// Retrieve one or more pets from the database.
    Pet(GetArgs),

    /// Retrieve one or more clinics from the database.
    Clinic(GetArgs),

    /// Retrieve one or more doctors from the database.
    Doctor(GetArgs),
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
pub enum SetObject {
    /// Set the clinic where a doctor works
    WorkLocation(WorkLocation),

    /// Set the clinic where a pet is a patient
    PatientAt(PatientAt),
}

impl SetObject {
    pub fn model_name(&self) -> &str {
        match self {
            SetObject::WorkLocation(..) => "doctors",
            SetObject::PatientAt(..) => "clinics",
        }
    }

    pub fn model_id(&self) -> &str {
        match self {
            Self::WorkLocation(WorkLocation { doctor_id, .. }) => &doctor_id,
            Self::PatientAt(PatientAt { clinic_id, .. }) => &clinic_id,
        }
    }

    pub fn ref_name(&self) -> &str {
        match self {
            Self::WorkLocation(..) => "clinics_ref",
            Self::PatientAt(..) => "pets_ref",
        }
    }

    pub fn ref_id(&self) -> &str {
        match self {
            SetObject::WorkLocation(WorkLocation { clinic_id, .. }) => &clinic_id,
            SetObject::PatientAt(PatientAt { pet_id, .. }) => &pet_id,
        }
    }
}

#[derive(Debug, Args)]
pub struct WorkLocation {
    #[clap(long)]
    pub doctor_id: String,

    #[clap(long)]
    pub clinic_id: String,
}

#[derive(Debug, Args)]
pub struct PatientAt {
    #[clap(long)]
    pub pet_id: String,

    #[clap(long)]
    pub clinic_id: String,
}

#[derive(Debug, Subcommand)]
pub enum DeleteObject {
    /// Delete one or more pets from the database.
    Pet(DeleteArgs),

    /// Delete one or more clinics from the database.
    Clinic(DeleteArgs),

    /// Delete one or more doctors from the database.
    Doctor(DeleteArgs),
}

impl DeleteObject {
    pub fn model_name(&self) -> &str {
        match self {
            DeleteObject::Pet(..) => "pets",
            DeleteObject::Clinic(..) => "clinics",
            DeleteObject::Doctor(..) => "doctors",
        }
    }

    pub fn model_id(&self) -> &str {
        match self {
            DeleteObject::Pet(args) | DeleteObject::Clinic(args) | DeleteObject::Doctor(args) => {
                &args.id
            }
        }
    }
}

#[derive(Debug, Args)]
pub struct DeleteArgs {
    /// Delete the objects matching the specified id(s).
    #[clap(long)]
    pub id: String,
}
