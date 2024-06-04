//! alik/graphql.rs --- Alik GraphQL API Service

//

//! Code:
use net::axum::{
  response::{self, IntoResponse},
  routing::get,
  Router,
};

use net::http::graphql::http::GraphiQLSource;
use tokio::net::TcpListener;
// use net::http::graphql::http::{EmptyMutation, EmptySubscription, Schema};
// use net::http::graphql_axum::GraphQL;

pub async fn graphiql() -> impl IntoResponse {
  response::Html(GraphiQLSource::build().endpoint("/").finish())
}

pub async fn start_graphiql(addr: &str) {
  // let schema = Schema::build(QueryRoot, EmptyMutation, EmptySubscription)
  //     .data(Vec::new())
  //     .finish();

  let app = Router::new().route("/", get(graphiql));
  // .post_service(GraphQL::new(schema)));

  let listener = TcpListener::bind(addr).await.unwrap();
  println!(
    "graphiql running on: http://{}",
    listener.local_addr().unwrap()
  );
  net::axum::serve(TcpListener::bind(addr).await.unwrap(), app)
    .await
    .unwrap();
}
