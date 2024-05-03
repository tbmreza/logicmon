// #![allow(unused)]
// use tree_sitter::{Parser, InputEdit, Point, Node};
//
// /// Model of text editor that runs in the browser.
// type NewSource = String;
// fn editor() -> (NewSource, InputEdit) {
//     // let new_src = String::from("fn test(sangatpanjaa: u32) {}");
//     let new_src = String::from("(func a1 a2 a3)");
//
//     // let ie = InputEdit {
//     //     start_byte: 8,
//     //     old_end_byte: 8,
//     //     new_end_byte: 14,
//     //     start_position: Point::new(0, 8),
//     //     old_end_position: Point::new(0, 8),
//     //     new_end_position: Point::new(0, 14),
//     // };
//     let ie = InputEdit {
//         start_byte: 12,
//         old_end_byte: 12,
//         new_end_byte: 18,
//         start_position: Point::new(0, 12),
//         old_end_position: Point::new(0, 12),
//         new_end_position: Point::new(0, 18),
//     };
//
//     (new_src, ie)
// }
//
// type StdOut = String;
// enum InternalError {
//     Todo
// }
//
// /// Model of things being sent back to browser.
// fn goodies() -> (StdOut, Vec<InternalError>) {
//     let mut rust_parser = {
//         let mut p = Parser::new();
//         // let lang = &tree_sitter_rust::language();
//         let lang = &tree_sitter_utlc::language();
//         p.set_language(lang).expect("grammar crate error");
//         p
//     };
//
//     let mut tree = {
//         // let big_omega = String::from("fn test() {}");  // ??
//         // let big_omega = String::from("((lambda (x) (x x)) (lambda (x) (x x)))");
//         let big_omega = String::from("(func a1 a2)");
//         rust_parser.parse(big_omega, None).expect("?? can source make this panic")
//     };
//     println!("{:?}", tree.root_node());
//     let tree_clone = tree.clone();
//     let mut cursor = tree_clone.walk();
//     cursor.goto_first_child();
//     let ss = cursor.node().to_string();
//     println!("{:?}", ss);
//
//     // rerun ascent on edit
//     let (new_src, coords) = editor();
//     tree.edit(&coords);
//     let new_tree = rust_parser.parse(new_src, Some(&tree)).expect("todo");
//     let nt = new_tree.root_node();
//     println!("{:?}", nt);
//
//     let stdout = format!("{:?}", analyze_value_flows_to(&nt));
//     (stdout, Vec::new())
// }
//
// type SourceLambda = Vec<(u32, u32, u32)>;
// type SourceVarRef = Vec<(u32, u32)>;
// type SourceAppl = Vec<(u32, u32, u32)>;
// struct SourceFacts(SourceLambda, SourceVarRef, SourceAppl);
// impl SourceFacts {
//     fn from(_node: &Node) -> Self {
//         SourceFacts(
//             vec![(6,7,8), (1,2,3)],
//             vec![(5,2), (4,2), (10,7), (9,7)],
//             vec![(8,9,10), (0,1,6), (3,4,5)])
//     }
// }
//
// use ascent::ascent;
//
// ascent! {
//     ////////////////////////////////////////////////////////////////////////////
//     ////  OUTPUT  //////////////////////////////////////////////////////////////
//     ////////////////////////////////////////////////////////////////////////////
//     relation value_flows_to(u32, u32);  // crate type ValueFlowsTo
//
//
//     ////////////////////////////////////////////////////////////////////////////
//     ////  LANGUAGE  ////////////////////////////////////////////////////////////
//     ////////////////////////////////////////////////////////////////////////////
//
//     // (lambda (xb) ec)a
//     //                    (exprId:number, varName:number, bodyId:number)
//     relation source_lambda(u32,           u32,            u32);
//
//
//     //                     (exprId:number, varName:number)
//     relation source_var_ref(u32,           u32);
//
//
//     // (e0b e1c)a
//     //                         (exprId:number, varName:number, bodyId:number)
//     relation source_application(u32, u32, u32);
//
//
//     ////////////////////////////////////////////////////////////////////////////
//     ////  RULES  ///////////////////////////////////////////////////////////////
//     ////////////////////////////////////////////////////////////////////////////
//
//     // base: atom flows to itself
//     value_flows_to(expr, expr) <-- source_lambda(expr, _, _);
//
//     // calls: value flows to bound argument
//     value_flows_to(arg_val, x) <-- source_application(_, fun, arg),
//                                    value_flows_to(_, arg),
//                                    source_lambda(fun, x, _),
//                                    value_flows_to(arg_val, arg);
//
//     // returns: returning means value flowing to var ref or lambda body
//     value_flows_to(v, expr) <-- source_var_ref(expr, var),
//                                 value_flows_to(v, var);
//     value_flows_to(v, expr) <-- source_application(expr, fun, _),
//                                 value_flows_to(lam, fun),
//                                 source_lambda(lam, _, body),
//                                 value_flows_to(v, body);
// }
//
// type ValueFlowsTo = Vec<(u32, u32)>;
// fn analyze_value_flows_to(node: &Node) -> ValueFlowsTo {
//     let mut dl = AscentProgram::default();
//     let facts = SourceFacts::from(node);
//
//     dl.source_lambda = facts.0;
//     dl.source_var_ref = facts.1;
//     dl.source_application = facts.2;
//
//     dl.run();
//     dl.value_flows_to
// }
//
// fn main() {
//     let (browser_std_out, browser_internal_errors) = goodies();
//     println!("browser: {:?}", browser_std_out);
// }

// EditorValue: String
// axum ws server listen (new_src)

use std::ops::ControlFlow;
use std::{net::SocketAddr, path::PathBuf};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use tower_http::{
    services::ServeDir,
    trace::DefaultMakeSpan,
};
use axum::{
    extract::ws::{CloseFrame, Message, WebSocket, WebSocketUpgrade},
    response::IntoResponse,
    routing::get,
};
use axum::extract::connect_info::ConnectInfo;
use axum_extra::TypedHeader;

#[tokio::main]
async fn main() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "example_websockets=debug,tower_http=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let axum_app = {
        let assets_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("assets");

        axum::Router::new()
            .fallback_service(ServeDir::new(assets_dir).append_index_html_on_directories(true))
            .route("/ws", get(ws_main))
            .layer(
                tower_http::trace::TraceLayer::new_for_http()
                .make_span_with(DefaultMakeSpan::default().include_headers(true)))
    };

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    tracing::debug!("listening on {}", listener.local_addr().unwrap());

    axum::serve(listener, axum_app.into_make_service_with_connect_info::<SocketAddr>())
        .await
        .unwrap();
}

/// The handler for the HTTP request (this gets called when the HTTP GET lands at the start
/// of websocket negotiation). After this completes, the actual switching from HTTP to
/// websocket protocol will occur.
/// This is the last point where we can extract TCP/IP metadata such as IP address of the client
/// as well as things from HTTP headers such as user-agent of the browser etc.
async fn ws_main(
    ws: WebSocketUpgrade,
    _user_agent: Option<TypedHeader<headers::UserAgent>>,
    ConnectInfo(addr): ConnectInfo<SocketAddr>,
) -> impl IntoResponse {
    // let user_agent = if let Some(TypedHeader(user_agent)) = user_agent {
    //     user_agent.to_string()
    // } else {
    //     String::from("Unknown browser")
    // };
    // println!("`{user_agent}` at {addr} connected.");
    // finalize the upgrade process by returning upgrade callback.
    // we can customize the callback by sending additional info such as address.
    ws.on_upgrade(move |socket| handle_socket(socket, addr))
}

/// Actual websocket statemachine (one will be spawned per connection)
async fn handle_socket(mut socket: WebSocket, cl: SocketAddr) {
    // send a ping (unsupported by some browsers) just to kick things off and get a response
    if socket.send(Message::Ping(vec![1, 2, 3])).await.is_ok() {
        println!("PINGEDDD {cl}...");
    } else {
        println!("Could not send ping {cl}!");
        // no Error here since the only thing we can do is to close the connection.
        // If we can not send messages, there is no way to salvage the statemachine anyway.
        return;
    }

    // receive single message from a client (we can either receive or send with socket).
    // this will likely be the Pong for our Ping or a hello message from client.
    // waiting for message from a client will block this task, but will not block other client's
    // connections.
    if let Some(msg) = socket.recv().await {
        if let Ok(msg) = msg {
            if process_message(msg, cl).is_break() {
                return;
            }
        } else {
            println!("client {cl} abruptly disconnected");
            return;
        }
    }

    // Since each client gets individual statemachine, we can pause handling
    // when necessary to wait for some external event (in this case illustrated by sleeping).
    // Waiting for this client to finish getting its greetings does not prevent other clients from
    // connecting to server and receiving their greetings.
    for i in 1..5 {
        if socket
            .send(Message::Text(format!("Hi {i} times!")))
            .await
            .is_err()
        {
            println!("client {cl} abruptly disconnected");
            return;
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }

    // By splitting socket we can send and receive at the same time. In this example we will send
    // unsolicited messages to client based on some sort of server's internal event (i.e .timer).
    use futures::{sink::SinkExt, stream::StreamExt};
    let (mut sender, mut receiver) = socket.split();

    // Spawn a task that will push several messages to the client (does not matter what client does)
    let mut send_task = tokio::spawn(async move {
        let n_msg = 20;
        for i in 0..n_msg {
            // In case of any websocket error, we exit.
            if sender
                .send(Message::Text(format!("Server message {i} ...")))
                .await
                .is_err()
            {
                return i;
            }

            tokio::time::sleep(std::time::Duration::from_millis(300)).await;
        }

        println!("Sending close to {cl}...");
        if let Err(e) = sender
            .send(Message::Close(Some(CloseFrame {
                code: axum::extract::ws::close_code::NORMAL,
                reason: "Goodbye".into(),
            })))
            .await
        {
            println!("Could not send Close due to {e}, probably it is ok?");
        }
        n_msg
    });

    // This second task will receive messages from client and print them on server console
    let mut recv_task = tokio::spawn(async move {
        let mut cnt = 0;
        while let Some(Ok(msg)) = receiver.next().await {
            cnt += 1;
            // print message and break if instructed to do so
            if process_message(msg, cl).is_break() {
                break;
            }
        }
        cnt
    });

    // If any one of the tasks exit, abort the other.
    tokio::select! {
        rv_a = (&mut send_task) => {
            match rv_a {
                Ok(a) => println!("{a} messages sent to {cl}"),
                Err(a) => println!("Error sending messages {a:?}")
            }
            recv_task.abort();
        },
        rv_b = (&mut recv_task) => {
            match rv_b {
                Ok(b) => println!("Received {b} messages"),
                Err(b) => println!("Error receiving messages {b:?}")
            }
            send_task.abort();
        }
    }

    // returning from the handler closes the websocket connection
    println!("Websocket context {cl} destroyed");
}

/// helper to print contents of messages to stdout. Has special treatment for Close.
fn process_message(msg: Message, cl: SocketAddr) -> ControlFlow<(), ()> {
    use ControlFlow::*;

    match msg {
        Message::Close(c) => {
            if let Some(frame) = c {
                println!(
                    ">>> {} sent close frame with code {} and reason `{}`",
                    cl, frame.code, frame.reason
                    );
            } else {
                println!(">>> {cl} somehow sent close message without CloseFrame");
            }
            return Break(());
        }

        Message::Text(t) => {
            println!(">>> {cl} sent str: {t:?}");  // ?? editor value
        }
        Message::Binary(d) => {
            println!(">>> {} sent {} bytes: {:?}", cl, d.len(), d);
        }

        Message::Pong(v) => {
            println!(">>> {cl} sent pong with {v:?}");
        }
        // You should never need to manually handle Message::Ping, as axum's websocket library
        // will do so for you automagically by replying with Pong and copying the v according to
        // spec. But if you need the contents of the pings you can see them here.
        Message::Ping(v) => {
            println!(">>> {cl} sent ping with {v:?}");
        }
    }
    Continue(())
}
