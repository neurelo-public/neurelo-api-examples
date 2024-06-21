use serde::Deserialize;

#[derive(Debug, Deserialize, Clone)]
pub struct MockPortfolio {
    pub name: String,
}

#[derive(Debug, Deserialize, Clone)]
pub struct MockSecurity {
    pub company_name: String,
    pub ticker_symbol: String,
    pub share_price: f32,
    pub volatility: i32,
    pub trend: f32,
}
