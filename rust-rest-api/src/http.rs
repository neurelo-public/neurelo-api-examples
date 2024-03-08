use anyhow::{Ok, Result};
use reqwest::{
    header::{HeaderName, HeaderValue},
    ClientBuilder,
};
use serde::Serialize;

use crate::{
    cli::{CreateObject, DeleteObject, GetArgs, GetObject, SetObject},
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

    pub async fn get_object(&self, object: GetObject) -> Result<Response> {
        match object {
            GetObject::Pet(args) => self.get("pet", args, false, None).await,
            GetObject::Clinic(args) => self.get("clinic", args, true, None).await,
            GetObject::Doctor(args) => self.get("doctor", args, true, Some("clinics_ref")).await,
        }
    }

    pub async fn create_object(&self, object: CreateObject) -> Result<Response> {
        match object {
            CreateObject::Pet(pet) => self.create("pets", pet).await,
            CreateObject::Clinic(clinic) => self.create("clinics", clinic).await,
            CreateObject::Doctor(doctor) => self.create("doctors", doctor).await,
        }
    }

    async fn get(
        &self,
        name: &str,
        args: GetArgs,
        needs_related: bool,
        nested_relation: Option<&str>,
    ) -> Result<Response> {
        let mut url = format!("{}/rest/{name}s?", self.url);

        // If we're fetching an object with related objects, we need to indicate to retrieve the
        // embedded objects in the `select` query parameter.
        if needs_related {
            url.push_str(r#"select={ "$scalars": true, "$related": true "#);

            // If the embedded object also has related objects, we similarly need to indicate that
            // those objects are needed as well.
            if let Some(nested_relation) = nested_relation {
                url.push_str(r#",""#);
                url.push_str(nested_relation);
                url.push_str(r#"": { "$scalars": true, "$related": true }"#);
            }

            url.push_str("}");
        }

        // If we're fetching only a single object, we put the id directly in the query route,
        // e.g. `/rest/pets/<<id>>`.
        //
        // Otherwise, we need to specify the `filter` as a query parameter.
        if let Some(ids) = args.id.as_ref().filter(|ids| ids.len() == 1) {
            url.push_str(&format!("/{}", ids[0]));
        } else {
            // Given a set of ids and a model name, we embed the ids in an array in the JSON object
            // `{ <<model>>_id: { "$in": [<<id0, id1, ...>>] } }`, then pass that object as the
            // `filter` query parameter.
            if let Some(ids) = args.id.as_ref() {
                url.push_str("filter={\"");
                url.push_str(name);
                url.push_str("_id\":{\"in\":[");

                for (i, id) in ids.into_iter().enumerate() {
                    if i != 0 {
                        url.push_str(",");
                    }

                    url.push('"');
                    url.push_str(&id.to_string());
                    url.push('"');
                }

                url.push_str("]}}");
            };

            // If a limit is passed in, we need to indicate that in the `take` query parameter.
            if let Some(limit) = args.limit {
                if args.id.is_some() {
                    url.push('&');
                }

                url.push_str("take=");
                url.push_str(&limit.to_string());
            }
        }

        let response = self.http_client.get(url).send().await?.json().await?;
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

    pub async fn set_object(&self, object: SetObject) -> Result<Response> {
        let url = format!(
            "{}/rest/{}/{}?select={{ \"$scalars\": true, \"$related\": true }}",
            self.url,
            object.model_name(),
            object.model_id()
        );

        // We use the `connect` operator to specify the foreign key when updating a relation. For
        // example, when setting the clinic that a doctor works at, the JSON body will be
        // `{ "clinics_ref": { "connect": { "id": <<clinic_id>> } } }`.
        let body = serde_json::json!({
            object.ref_name() : {
                "connect": {
                    "id": object.ref_id(),
                }
            }
        });

        let response = self
            .http_client
            .patch(url)
            .json(&body)
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
}
