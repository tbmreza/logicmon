# poster-lang
- [ ] Write the core interpreter in haskell (with step+state) and compile it to WASM.
- [ ] Set up a JS/HTML UI to talk to the interpreter.
- [ ] Define the message format for StepResult and StateSnapshot.
- [ ] Create the playback loop + UI update logic.

## See also
- https://visualgo.net/en

```
// Create your own language definition here
// You can safely look at other samples without losing modifications.
// Modifications are not saved on browser refresh/close though -- copy often!
return {
  // Set defaultToken to invalid to see what you do not tokenize yet
  // defaultToken: 'invalid',

  keywords: [
    'abstract', 'continue', 'for', 'new', 'switch', 'assert', 'goto', 'do',
    'if', 'private', 'this', 'break', 'protected', 'throw', 'else', 'public',
    'enum', 'return', 'catch', 'try', 'interface', 'static', 'class',
    'finally', 'const', 'super', 'while', 'true', 'false'
  , 'to'
  , 'output'
  , "downto"
  ],

  typeKeywords: [
    'boolean', 'double', 'byte', 'int', 'short', 'char', 'void', 'long', 'float'
  ],

  operators: [
    '=', '>', '<', '!', '~', '?', ':', '==', '<=', '>=', '!=',
    '&&', '||', '++', '--', '+', '-', '*', '/', '&', '|', '^', '%',
    '<<', '>>', '>>>', '+=', '-=', '*=', '/=', '&=', '|=', '^=',
    '%=', '<<=', '>>=', '>>>='
  ],

  // we include these common regular expressions
  symbols:  /[=><!~?:&|+\-*\/\^%]+/,

  // C# style strings
  escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

  // The main tokenizer for our languages
  tokenizer: {
    root: [
      // identifiers and keywords
      [/[a-z_$][\w$]*/, { cases: { '@typeKeywords': 'keyword',
                                   '@keywords': 'keyword',
                                   '@default': 'identifier' } }],
      [/[A-Z][\w\$]*/, 'type.identifier' ],  // to show class names nicely

      // whitespace
      { include: '@whitespace' },

      // delimiters and operators
      [/[{}()\[\]]/, '@brackets'],
      [/[<>](?!@symbols)/, '@brackets'],
      [/@symbols/, { cases: { '@operators': 'operator',
                              '@default'  : '' } } ],

      // @ annotations.
      // As an example, we emit a debugging log message on these tokens.
      // Note: message are supressed during the first load -- change some lines to see them.
      [/@\s*[a-zA-Z_\$][\w\$]*/, { token: 'annotation', log: 'annotation token: $0' }],

      // numbers
      [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
      [/0[xX][0-9a-fA-F]+/, 'number.hex'],
      [/\d+/, 'number'],

      // delimiter: after number because of .\d floats
      [/[;,.]/, 'delimiter'],

      // strings
      [/"([^"\\]|\\.)*$/, 'string.invalid' ],  // non-teminated string
      [/"/,  { token: 'string.quote', bracket: '@open', next: '@string' } ],

      // characters
      [/'[^\\']'/, 'string'],
      [/(')(@escapes)(')/, ['string','string.escape','string']],
      [/'/, 'string.invalid'],

      [/#\{/, 'comment', '@comment'],
      [/\/\/.*$/, 'comment'],
      [/[{}]/, 'delimiter.bracket'],
      [/\d+/, 'number'],
      [/[a-z_$][\w$]*/, 'identifier'],

    ],

    comment: [
      [/#\{/, 'comment', '@comment'],  // enter nested block comment
      [/}#/, 'comment', '@pop'],       // exit current block comment
      [/[^#{}]+/, 'comment'],
      [/./, 'comment'],                // match any single character to avoid getting stuck
    ],

    // comment: [
    //   [/[^\/*]+/, 'comment' ],
    //   [/\/\*/,    'comment', '@push' ],    // nested comment
    //   ["\\*/",    'comment', '@pop'  ],
    //   [/[\/*]/,   'comment' ]
    // ],



    string: [
      [/[^\\"]+/,  'string'],
      [/@escapes/, 'string.escape'],
      [/\\./,      'string.escape.invalid'],
      [/"/,        { token: 'string.quote', bracket: '@close', next: '@pop' } ]
    ],
    // whitespace: [
    //   [/[ \t\r\n]+/, 'white'],
    //   [/\/\*/,       'comment', '@comment' ],
    //   // [/\/\*/,       'comment', '@comment' ],
    //   // [/\/\/.*$/,    'comment'],
    //   [/--.*$/,    'comment'],
    // ],

    whitespace: [
      [/[ \t\r\n]+/, 'white'],
      // [/#{/,       'comment', '@comment' ],
      // [/{#.*$/,    'comment'],
      [/--.*$/,    'comment'],
      [/\.\..*$/,    'comment'],
    ],
  },
};
```


# logicmon
Logic program application that monitors subject language source to analyze it incrementally.




- tree-sitter-utlc: ast util and syntax highlight
- incremental-analyzer-web: monaco editor web frontend app
- logicmon: logic program as an rpc service that monitors subject language source to analyze it incrementally
- logicmon-nvim: 
- logicmon-dx-web: 
dioxus web
Render directly to the DOM using WebAssembly
Pre-render with SSR and rehydrate on the client

## Testing
```
cargo r                   # start rpc server in one terminal session/pane...
cargo r --example client  # ...fire request from example client
cargo r --example dl
```

?? cargo workspace
https://github.com/tokio-rs/axum/blob/main/Cargo.toml

// example/client.rs
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


??
disable tonic transport feature
leptos debug, name it logicmon-play
logicmon-play borked trying the following:
if https://github.com/devashishdxt/tonic-web-wasm-client doesn't work, use ace editor and axum (rest http)
```
cd interlocut
cargo r

cd interlocut/web-editor
npm run dev  # npm run build:release in build.rs is an idea
```

## Status
Work in progress. Everything outside interlocut folder are old drafts.
