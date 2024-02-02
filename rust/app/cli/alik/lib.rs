//! app/cli/alik/lib.rs --- Alik Lib

//// Code:
// use net::axum::Router;
use net::axum::http::{HeaderMap, StatusCode};
use tokio::net::TcpListener;
use net::axum::{
  Router,
  extract::State,
  http::{HeaderName, HeaderValue},
  response::{IntoResponse,Response, self},
  routing::get,
  body::{Body, Bytes},
};
use net::http::graphql::http::GraphiQLSource; 
// use net::http::graphql::http::{EmptyMutation, EmptySubscription, Schema};
// use net::http::graphql_axum::GraphQL;
use net::http::tower::trace::TraceLayer;
use logger::tracing::{self, Span};
use net::reqwest::Client;
use net::stream::StreamExt;
use std::{convert::Infallible, time::Duration};
use logger::log;

pub async fn graphiql() -> impl IntoResponse {
    response::Html(GraphiQLSource::build().endpoint("/").finish())
}

//// Server
pub async fn proxy_via_reqwest(State(client): State<Client>) -> Response {
    let reqwest_response = match client.get("http://127.0.0.1:3000/stream").send().await {
        Ok(res) => res,
        Err(err) => {
          log::error!("{} {}", &err, "request failed");
          return (StatusCode::BAD_REQUEST, Body::empty()).into_response();
        }
    };

    let response_builder = Response::builder().status(reqwest_response.status().as_u16());

    // Here the mapping of headers is required due to reqwest and axum differ on the http crate versions
    let mut headers = HeaderMap::with_capacity(reqwest_response.headers().len());
    headers.extend(reqwest_response.headers().into_iter().map(|(name, value)| {
        let name = HeaderName::from_bytes(name.as_ref()).unwrap();
        let value = HeaderValue::from_bytes(value.as_ref()).unwrap();
        (name, value)
    }));

    response_builder
        .body(Body::from_stream(reqwest_response.bytes_stream()))
        // This unwrap is fine because the body is empty here
        .unwrap()
}

pub async fn stream_some_data() -> Body {
    let stream = net::stream::iter(0..5)
        .throttle(Duration::from_secs(1))
        .map(|n| n.to_string())
        .map(Ok::<_, Infallible>);
    Body::from_stream(stream)
}

pub async fn start_service() {
  let client = Client::new();

    let app = Router::new()
        .route("/", get(proxy_via_reqwest))
        .route("/stream", get(stream_some_data))
        // Add some logging so we can see the streams going through
        .layer(TraceLayer::new_for_http().on_body_chunk(
            |chunk: &Bytes, _latency: Duration, _span: &Span| {
                tracing::debug!("streaming {} bytes", chunk.len());
            },
        ))
        .with_state(client);

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();
    log::debug!("listening on {}", listener.local_addr().unwrap());
    net::axum::serve(listener, app).await.unwrap();
}
pub async fn start_graph_service() {
    // let schema = Schema::build(QueryRoot, EmptyMutation, EmptySubscription)
    //     .data(Vec::new())
    //     .finish();

  let app = Router::new().route("/", get(graphiql));
                                  // .post_service(GraphQL::new(schema)));

  println!("GraphiQL IDE: http://localhost:3000");
  net::axum::serve(TcpListener::bind("127.0.0.1:3000").await.unwrap(), app)
    .await
    .unwrap();
}
