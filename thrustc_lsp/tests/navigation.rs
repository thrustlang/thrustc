use serde_json::Value;
use std::io::Write;

fn lsp_result(text: &str, request: Value) -> Value {
    let test_id: u128 = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or_default();
    let test_root: std::path::PathBuf = std::env::temp_dir().join(format!(
        "thrustc_lsp_navigation_test_{}_{}",
        std::process::id(),
        test_id
    ));
    let temp_home: std::path::PathBuf = test_root.join("home");
    let document_path: std::path::PathBuf = test_root.join("main.thrust");

    std::fs::create_dir_all(&temp_home).unwrap();

    let uri: String = format!("file://{}", document_path.display());
    let initialize: Value = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {}
    });
    let open: Value = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
            "textDocument": {
                "uri": uri,
                "languageId": "thrust",
                "version": 1,
                "text": text
            }
        }
    });
    let mut request: Value = request;
    let params: &mut Value = request.get_mut("params").unwrap();
    let document: &mut Value = params.get_mut("textDocument").unwrap();

    document["uri"] = Value::String(uri);

    let messages: [Value; 3] = [initialize, open, request];
    let mut input: Vec<u8> = Vec::with_capacity(4096);

    for message in messages {
        let body: String = message.to_string();
        let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

        input.write_all(header.as_bytes()).unwrap();
        input.write_all(body.as_bytes()).unwrap();
    }

    let mut output = std::process::Command::new(env!("CARGO_BIN_EXE_thrustc_lsp"))
        .env("HOME", &temp_home)
        .env("APPDATA", &temp_home)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();

    output.stdin.as_mut().unwrap().write_all(&input).unwrap();

    let output: std::process::Output = output.wait_with_output().unwrap();

    assert!(
        output.status.success(),
        "thrustc_lsp failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let mut rest: &[u8] = &output.stdout;

    while !rest.is_empty() {
        let Some(header_end) = rest.windows(4).position(|window| window == b"\r\n\r\n") else {
            break;
        };
        let headers: String = String::from_utf8_lossy(&rest[..header_end]).into_owned();
        let mut content_length: Option<usize> = None;

        for line in headers.lines() {
            let Some((name, value)) = line.split_once(':') else {
                continue;
            };

            if !name.eq_ignore_ascii_case("content-length") {
                continue;
            }

            content_length = value.trim().parse::<usize>().ok();
            break;
        }

        let Some(content_length) = content_length else {
            break;
        };
        let body_start: usize = header_end.saturating_add(4);
        let body_end: usize = body_start.saturating_add(content_length);

        if body_end > rest.len() {
            break;
        }

        let body: &[u8] = &rest[body_start..body_end];
        let response: Value = serde_json::from_slice(body).unwrap();

        if response.get("id").and_then(Value::as_u64) == Some(2) {
            std::fs::remove_dir_all(&test_root).ok();

            return response.get("result").cloned().unwrap_or(Value::Null);
        }

        rest = &rest[body_end..];
    }

    std::fs::remove_dir_all(&test_root).ok();

    panic!("navigation response was not returned");
}

#[test]
fn document_symbol_returns_top_level_symbols() {
    let result: Value = self::lsp_result(
        "struct Point {\n    x: s32,\n}\n\nfn main() s32 {\n    return 0;\n}\n",
        serde_json::json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "textDocument/documentSymbol",
            "params": {
                "textDocument": {
                    "uri": ""
                }
            }
        }),
    );
    let symbols: &[Value] = result.as_array().map(Vec::as_slice).unwrap_or_default();

    assert!(symbols.iter().any(|symbol| {
        symbol.get("name").and_then(Value::as_str) == Some("Point")
            && symbol.get("kind").and_then(Value::as_u64) == Some(23)
    }));
    assert!(symbols.iter().any(|symbol| {
        symbol.get("name").and_then(Value::as_str) == Some("main")
            && symbol.get("kind").and_then(Value::as_u64) == Some(12)
    }));
}

#[test]
fn definition_returns_local_variable_location() {
    let result: Value = self::lsp_result(
        "fn main() s32 {\n    var value: s32 = 1;\n    return value;\n}\n",
        serde_json::json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "textDocument/definition",
            "params": {
                "textDocument": {
                    "uri": ""
                },
                "position": {
                    "line": 2,
                    "character": 13
                }
            }
        }),
    );
    let line: Option<u64> = result
        .get("range")
        .and_then(|range| range.get("start"))
        .and_then(|start| start.get("line"))
        .and_then(Value::as_u64);
    let character: Option<u64> = result
        .get("range")
        .and_then(|range| range.get("start"))
        .and_then(|start| start.get("character"))
        .and_then(Value::as_u64);

    assert_eq!(line, Some(1));
    assert_eq!(character, Some(8));
}
