use ts_proto::zer_client::ZerClient;
use ts_proto::*;

pub mod ts_proto {
    tonic::include_proto!("ts");
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = ZerClient::connect("http://[::1]:50051").await?;

    let request = tonic::Request::new(Source {
        val: String::from("fn test(a: u32) {}"),
        // val: String::from("fn testttttttttttttt(a: u32) {}"),
    });

    let response = client.replace(request).await?;

    println!("RESPONSE={:?}", response);

    Ok(())
}
