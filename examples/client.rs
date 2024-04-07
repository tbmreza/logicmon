use ts_proto::zer_client::ZerClient;
use ts_proto::*;

pub mod ts_proto {
    tonic::include_proto!("ts");
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = ZerClient::connect("http://[::1]:50051").await?;

    let request = tonic::Request::new(Source {
        val: "Tonic".into(),
    });

    let response = client.replace(request).await?;

    println!("RESPONSE={:?}", response);

    Ok(())
}

// tree.edit(&InputEdit {
//   start_byte: 8,
//   old_end_byte: 8,
//   new_end_byte: 14,
//   start_position: Point::new(0, 8),
//   old_end_position: Point::new(0, 8),
//   new_end_position: Point::new(0, 14),
// });
