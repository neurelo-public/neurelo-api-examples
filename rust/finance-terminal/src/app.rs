use anyhow::Result;
use futures::future::join_all;
use rand::{
    distributions::{Distribution, Uniform},
    rngs::ThreadRng,
    Rng,
};
use ratatui::widgets::ListState;

use crate::{
    api::{Action, CreatePortfolio, CreateSecurity, CreateTrade},
    cli::{CreateObject, GetArgs, GetObject},
    http::Client,
    model::{Portfolio, Security, Trade},
    sample::{MockPortfolio, MockSecurity},
    Data, Model, Response,
};
use std::{error::Error, fs::File, io::BufReader};

const EVENTS: [(&str, u64); 24] = [
    ("B1", 9),
    ("B2", 12),
    ("B3", 5),
    ("B4", 8),
    ("B5", 2),
    ("B6", 4),
    ("B7", 5),
    ("B8", 9),
    ("B9", 14),
    ("B10", 15),
    ("B11", 1),
    ("B12", 0),
    ("B13", 4),
    ("B14", 6),
    ("B15", 4),
    ("B16", 6),
    ("B17", 4),
    ("B18", 7),
    ("B19", 13),
    ("B20", 8),
    ("B21", 11),
    ("B22", 9),
    ("B23", 3),
    ("B24", 5),
];

#[derive(Clone)]
pub struct RandomSignal {
    distribution: Uniform<u64>,
    rng: ThreadRng,
}

impl RandomSignal {
    pub fn new(lower: u64, upper: u64) -> Self {
        Self {
            distribution: Uniform::new(lower, upper),
            rng: rand::thread_rng(),
        }
    }
}

impl Iterator for RandomSignal {
    type Item = u64;
    fn next(&mut self) -> Option<u64> {
        Some(self.distribution.sample(&mut self.rng))
    }
}

#[derive(Clone)]
pub struct SinSignal {
    x: f64,
    interval: f64,
    period: f64,
    scale: f64,
}

impl SinSignal {
    pub const fn new(interval: f64, period: f64, scale: f64) -> Self {
        Self {
            x: 0.0,
            interval,
            period,
            scale,
        }
    }
}

impl Iterator for SinSignal {
    type Item = (f64, f64);
    fn next(&mut self) -> Option<Self::Item> {
        let point = (self.x, (self.x * 1.0 / self.period).sin() * self.scale);
        self.x += self.interval;
        Some(point)
    }
}

pub struct TabsState<'a> {
    pub titles: Vec<&'a str>,
    pub index: usize,
}

impl<'a> TabsState<'a> {
    pub fn new(titles: Vec<&'a str>) -> TabsState {
        TabsState { titles, index: 0 }
    }
    pub fn next(&mut self) {
        self.index = (self.index + 1) % self.titles.len();
    }

    pub fn previous(&mut self) {
        if self.index > 0 {
            self.index -= 1;
        } else {
            self.index = self.titles.len() - 1;
        }
    }
}

pub struct StatefulList<T> {
    pub state: ListState,
    pub items: Vec<T>,
}

impl<T> StatefulList<T> {
    pub fn with_items(items: Vec<T>) -> Self {
        Self {
            state: ListState::default(),
            items,
        }
    }

    pub fn next(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i >= self.items.len() - 1 {
                    0
                } else {
                    i + 1
                }
            }
            None => 0,
        };
        self.state.select(Some(i));
    }

    pub fn previous(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i == 0 {
                    self.items.len() - 1
                } else {
                    i - 1
                }
            }
            None => 0,
        };
        self.state.select(Some(i));
    }
}

pub struct Signal<S: Iterator> {
    source: S,
    pub points: Vec<S::Item>,
    tick_rate: usize,
}

impl<S> Signal<S>
where
    S: Iterator,
{
    fn on_tick(&mut self) {
        self.points.drain(0..self.tick_rate);
        self.points
            .extend(self.source.by_ref().take(self.tick_rate));
    }
}

pub struct Signals {
    pub sin1: Signal<SinSignal>,
    pub sin2: Signal<SinSignal>,
    pub window: [f64; 2],
}

impl Signals {
    fn on_tick(&mut self) {
        self.sin1.on_tick();
        self.sin2.on_tick();
        self.window[0] += 1.0;
        self.window[1] += 1.0;
    }
}

pub struct Server<'a> {
    pub name: &'a str,
    pub location: &'a str,
    pub coords: (f64, f64),
    pub status: &'a str,
}

pub struct App<'a> {
    pub title: &'a str,
    pub ticks: i32,
    pub client: Client,
    pub should_quit: bool,
    pub tabs: TabsState<'a>,
    pub show_chart: bool,
    pub progress: f64,
    pub sparkline: Signal<RandomSignal>,
    pub securities: Vec<Security>,
    pub portfolios: StatefulList<Portfolio>,
    pub logs: StatefulList<Trade>,
    pub signals: Signals,
    pub barchart: Vec<(&'a str, u64)>,
    pub servers: Vec<Server<'a>>,
    pub enhanced_graphics: bool,
}

async fn init_portfolios(client: &Client) -> Result<Vec<Portfolio>> {
    let config_path = "./sample/portfolios.json";
    let file = File::open(config_path)?;
    let reader = BufReader::new(file);
    let portfolios: Vec<MockPortfolio> = serde_json::from_reader(reader)?;

    let mut model_portfolios: Vec<Portfolio> = Default::default();
    for mock in portfolios {
        let co = CreateObject::Portfolio(CreatePortfolio { name: mock.name });
        let response = client.create_object(co).await?;
        match response {
            Response::Data(Data::One(Model::Portfolio(portfolio))) => {
                model_portfolios.push(portfolio);
            }
            other => {
                eprintln!("got non-one response: {other:?}");
            }
        }
    }
    Ok(model_portfolios)
}

async fn init_securities(client: &Client) -> Result<Vec<Security>> {
    let mock_securities = get_mock_securities()?;
    let mut model_securities: Vec<Security> = Default::default();
    for mock in mock_securities {
        let co = CreateObject::Security(CreateSecurity {
            ticker_symbol: mock.ticker_symbol,
            company_name: mock.company_name,
        });
        let response = client.create_object(co).await?;
        match response {
            Response::Data(Data::One(Model::Security(security))) => {
                model_securities.push(security);
            }
            other => {
                eprintln!("got non-one response: {other:?}");
            }
        }
    }
    Ok(model_securities)
}

fn get_mock_securities() -> Result<Vec<MockSecurity>> {
    let config_path = "./sample/securities.json";
    let file = File::open(config_path)?;
    let reader = BufReader::new(file);
    let securities: Vec<MockSecurity> = serde_json::from_reader(reader)?;
    Ok(securities)
}

fn determine_price(company: MockSecurity, tick: i32) -> f32 {
    let mut rng = rand::thread_rng();
    let random_factor: f32 = rng.gen_range(-company.volatility as f32..=company.volatility as f32);
    let current_price = company.share_price + random_factor * (company.trend * tick as f32);
    //(current_price * 100.0).round() / 100.0
    f32::trunc(current_price * 100.0) / 100.0
}

impl<'a> App<'a> {
    async fn refresh_portfolios(&mut self) -> Result<()> {
        let ids = self.portfolios.items.iter().map(|p| p.id.clone()).collect();
        let getobj = GetObject::Portfolio(GetArgs {
            limit: None,
            id: Some(ids),
        });
        let response = self.client.get_object(getobj).await?;
        match response {
            Response::Data(Data::One(Model::Portfolio(portfolio))) => {
                self.portfolios.items.clear();
                self.portfolios.items.push(portfolio)
            }
            Response::Data(Data::Many(models)) => {
                self.portfolios.items.clear();
                for model in models {
                    match model {
                        Model::Portfolio(p) => {
                            self.portfolios.items.push(p);
                        }
                        _ => (),
                    }
                }
            }
            other => {
                eprintln!("got non-one response: {other:?}");
            }
        }
        Ok(())
    }

    pub async fn make_trades(&self) -> Result<()> {
        let mock_securities = get_mock_securities()?;
        let mut rng = rand::thread_rng();
        let num_securities = self.securities.len();
        let futs: Vec<_> = self
            .portfolios
            .items
            .iter()
            .map(|port| {
                let rand_security_idx = rng.gen_range(0..num_securities);
                let rand_mock_security = mock_securities[rand_security_idx].clone();
                let rand_model_security = self
                    .securities
                    .iter()
                    .filter(|s| s.ticker_symbol == rand_mock_security.ticker_symbol)
                    .last()
                    .unwrap();
                let price = determine_price(rand_mock_security, self.ticks);
                let quantity = rng.gen_range(0..10);
                let action = match rng.gen_bool(1.0 / 2.0) {
                    true => Action::Buy,
                    false => Action::Sell,
                };
                let create_args = CreateObject::Trade(CreateTrade {
                    security_id: rand_model_security.id.clone(),
                    portfolio_id: port.id.clone(),
                    quantity,
                    price,
                    action,
                });
                return self.client.create_object(create_args);
            })
            .collect();
        let _results = join_all(futs).await;

        Ok(())
    }

    pub async fn new(title: &'a str, client: Client, enhanced_graphics: bool) -> Result<Self> {
        let mut rand_signal = RandomSignal::new(0, 100);
        let sparkline_points = rand_signal.by_ref().take(300).collect();
        let mut sin_signal = SinSignal::new(0.2, 3.0, 18.0);
        let sin1_points = sin_signal.by_ref().take(100).collect();
        let mut sin_signal2 = SinSignal::new(0.1, 2.0, 10.0);
        let sin2_points = sin_signal2.by_ref().take(200).collect();
        let portfolios: Vec<Portfolio> = init_portfolios(&client).await?;
        let securities: Vec<Security> = init_securities(&client).await?;
        let app = App {
            title,
            ticks: 0,
            client,
            should_quit: false,
            tabs: TabsState::new(vec!["Tab0", "Tab1", "Tab2"]),
            show_chart: true,
            progress: 0.0,
            sparkline: Signal {
                source: rand_signal,
                points: sparkline_points,
                tick_rate: 1,
            },
            securities,
            portfolios: StatefulList::with_items(portfolios),
            logs: StatefulList::with_items(Vec::new()),
            signals: Signals {
                sin1: Signal {
                    source: sin_signal,
                    points: sin1_points,
                    tick_rate: 5,
                },
                sin2: Signal {
                    source: sin_signal2,
                    points: sin2_points,
                    tick_rate: 10,
                },
                window: [0.0, 20.0],
            },
            barchart: EVENTS.to_vec(),
            servers: vec![
                Server {
                    name: "NorthAmerica-1",
                    location: "New York City",
                    coords: (40.71, -74.00),
                    status: "Up",
                },
                Server {
                    name: "Europe-1",
                    location: "Paris",
                    coords: (48.85, 2.35),
                    status: "Failure",
                },
                Server {
                    name: "SouthAmerica-1",
                    location: "São Paulo",
                    coords: (-23.54, -46.62),
                    status: "Up",
                },
                Server {
                    name: "Asia-1",
                    location: "Singapore",
                    coords: (1.35, 103.86),
                    status: "Up",
                },
            ],
            enhanced_graphics,
        };
        Ok(app)
    }

    pub fn on_up(&mut self) {
        self.portfolios.previous();
    }

    pub fn on_down(&mut self) {
        self.portfolios.next();
    }

    pub fn on_right(&mut self) {
        self.tabs.next();
    }

    pub fn on_left(&mut self) {
        self.tabs.previous();
    }

    pub fn on_key(&mut self, c: char) {
        match c {
            'q' => {
                self.should_quit = true;
            }
            't' => {
                self.show_chart = !self.show_chart;
            }
            _ => {}
        }
    }

    pub async fn on_tick(&mut self) {
        self.ticks += 1;
        // Update progress
        self.progress += 0.001;
        if self.progress > 1.0 {
            self.progress = 0.0;
        }

        self.sparkline.on_tick();
        self.signals.on_tick();

        self.make_trades().await;
        self.refresh_portfolios().await;

        let p_idx = self
            .portfolios
            .state
            .selected()
            .unwrap_or(self.portfolios.state.offset());
        let trades = self.portfolios.items[p_idx]
            .clone()
            .trades
            .unwrap_or_default();
        self.logs.items = trades;

        let event = self.barchart.pop().unwrap();
        self.barchart.insert(0, event);
    }
}
