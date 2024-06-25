use clap::Args;
use serde::{Deserialize, Serialize};
use strum::EnumString;

#[derive(Debug, Serialize, Args)]
pub struct CreatePortfolio {
    #[clap(long)]
    pub name: String,
}

#[derive(Debug, Serialize, Args)]
pub struct CreateSecurity {
    #[clap(long)]
    pub ticker_symbol: String,

    #[clap(long)]
    pub company_name: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, EnumString, strum_macros::Display)]
#[strum(ascii_case_insensitive)]
pub enum Action {
    Buy,
    Sell,
}

#[derive(Debug, Serialize, Deserialize, Args)]
pub struct CreateTrade {
    #[clap(long)]
    pub quantity: i32,

    #[clap(long)]
    pub price: f32,

    #[clap(long)]
    pub action: Action,

    #[clap(long)]
    pub portfolio_id: String,

    #[clap(long)]
    pub security_id: String,
}

impl CreateTrade {
    pub fn to_json_body(&self) -> serde_json::Value {
        serde_json::json!({
            "quantity": self.quantity,
            "price": self.price,
            "action": self.action,
            "portfolio_ref" : {
                "connect": {
                    "id": self.portfolio_id,
                }
            },
            "security_ref" : {
                "connect": {
                    "id": self.security_id,
                }
            }
        })
    }
}
