use anyhow::{Ok, Result};
use reqwest::{
    header::{HeaderName, HeaderValue},
    ClientBuilder, RequestBuilder,
};
use serde::Serialize;
use serde_json::json;

use crate::{
    cli::{CreateObject, DeleteAllObject, DeleteObject, GetArgs, GetObject},
    model::Response,
};

const API_KEY: HeaderName = HeaderName::from_static("x-api-key");

#[derive(Debug)]
pub struct Client {
    url: String,
    http_client: reqwest::Client,
}

impl Client {
    pub fn new(url: String, api_key: String) -> Result<Self> {
        let api_key = HeaderValue::try_from(api_key)?;
        let default_headers = std::iter::once((API_KEY, api_key)).collect();

        let inner = ClientBuilder::new()
            .default_headers(default_headers)
            .https_only(true)
            .use_rustls_tls()
            .build()?;

        Ok(Self {
            url,
            http_client: inner,
        })
    }

    fn add_ref_param(&self, builder: RequestBuilder) -> RequestBuilder {
        let k = String::from("select");
        let v = json!({
            "$scalars": true,
            "$related": true
        })
        .to_string();
        builder.query(&[(k, v)])
    }

    fn add_id_filter_param(&self, builder: RequestBuilder, ids: &Vec<String>) -> RequestBuilder {
        let k = String::from("filter");
        let v = json!({
            "id": {
                "in": ids
            }
        })
        .to_string();
        builder.query(&[(k, v)])
    }

    pub async fn get_object(&self, object: GetObject) -> Result<Response> {
        let object_string = object.to_string();
        match object {
            GetObject::Portfolio(args) => self.get(&object_string, args, true).await,
            GetObject::Security(args) => self.get(&object_string, args, false).await,
            GetObject::Trade(args) => self.get(&object_string, args, false).await,
        }
    }

    pub async fn create_object(&self, object: CreateObject) -> Result<Response> {
        let object_string = object.to_string();
        match object {
            CreateObject::Portfolio(portfolio) => self.create(&object_string, portfolio).await,
            CreateObject::Security(security) => self.create(&object_string, security).await,
            CreateObject::Trade(trade) => self.create(&object_string, trade.to_json_body()).await,
        }
    }

    async fn get(&self, name: &str, args: GetArgs, needs_related: bool) -> Result<Response> {
        let base_url = format!("{}/rest/{name}", self.url);
        let is_multiple_ids = args.id.as_ref().unwrap().iter().len() > 1;
        let builder = match is_multiple_ids {
            false => {
                let id: &String = args.id.as_ref().unwrap().iter().last().unwrap();
                let url = format!("{base_url}/{id}");
                let builder = self.http_client.get(url);
                match needs_related {
                    true => self.add_ref_param(builder),
                    false => builder,
                }
            }
            true => {
                let ids = args.id.as_ref().unwrap();
                let mut builder = self.http_client.get(base_url);
                builder = self.add_id_filter_param(builder, ids);
                match needs_related {
                    true => self.add_ref_param(builder),
                    false => builder,
                }
            }
        };
        let response = builder.send().await?.json().await?;
        Ok(response)
    }

    async fn create(&self, name: &str, value: impl Serialize) -> Result<Response> {
        let url = format!("{}/rest/{name}/__one", self.url);
        let response = self
            .http_client
            .post(url)
            .json(&value)
            .send()
            .await?
            .json()
            .await?;
        Ok(response)
    }

    pub async fn delete_object(&self, object: DeleteObject) -> Result<Response> {
        let url = format!(
            "{}/rest/{}/{}",
            self.url,
            object.model_name(),
            object.model_id()
        );
        let response = self.http_client.delete(url).send().await?.json().await?;
        Ok(response)
    }

    pub async fn delete_objects(&self, object: DeleteAllObject) -> Result<Response> {
        let url = format!("{}/rest/{}", self.url, object.model_name(),);
        let response = self.http_client.delete(url).send().await?.json().await?;
        Ok(response)
    }
}
