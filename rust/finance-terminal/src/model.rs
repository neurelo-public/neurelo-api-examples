use std::fmt::Debug;

use chrono::{DateTime, Utc};
use clap::Args;
use serde::{de::IgnoredAny, Deserialize, Serialize};

use crate::api::Action;

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Portfolio {
    pub id: String,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none", rename = "trade_ref")]
    pub trades: Option<Vec<Trade>>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Security {
    pub id: String,
    pub ticker_symbol: String,
    pub company_name: String,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Trade {
    pub id: String,
    pub quantity: i32,
    pub price: f32,
    pub date: DateTime<Utc>,
    pub portfolio_id: String,
    pub security_id: String,
    pub action: Action,
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
    Many(Vec<Model>),
    Other(IgnoredAny),
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Model {
    Portfolio(Portfolio),
    Security(Security),
    Trade(Trade),
}

impl Model {
    pub fn inner_debug(&self) -> &dyn Debug {
        match self {
            Model::Portfolio(portfolio) => portfolio,
            Model::Security(security) => security,
            Model::Trade(trade) => trade,
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct Error {
    pub error: String,
}
