use serde_json::Value;
use std::io::Write;

fn complete_labels(text: &str, line: u64, character: u64) -> Vec<String> {
    let test_id: u128 = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or_default();
    let test_root: std::path::PathBuf = std::env::temp_dir().join(format!(
        "thrustc_lsp_completion_test_{}_{}",
        std::process::id(),
        test_id
    ));
    let temp_home: std::path::PathBuf = test_root.join("home");
    let document_path: std::path::PathBuf = test_root.join("main.thrust");

    std::fs::create_dir_all(&temp_home).unwrap();

    let uri: String = format!("file://{}", document_path.display());
    let messages: [Value; 3] = [
        serde_json::json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {}
        }),
        serde_json::json!({
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
        }),
        serde_json::json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {
                    "uri": uri
                },
                "position": {
                    "line": line,
                    "character": character
                }
            }
        }),
    ];
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

    let mut responses: Vec<Value> = Vec::with_capacity(8);
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

        responses.push(response);
        rest = &rest[body_end..];
    }

    std::fs::remove_dir_all(&test_root).ok();

    for response in responses {
        if response.get("id").and_then(Value::as_u64) != Some(2) {
            continue;
        }

        let items: &[Value] = response
            .get("result")
            .and_then(|result| result.get("items"))
            .and_then(Value::as_array)
            .map(Vec::as_slice)
            .unwrap_or_default();
        let mut labels: Vec<String> = Vec::with_capacity(items.len());

        for item in items {
            let Some(label) = item.get("label").and_then(Value::as_str) else {
                continue;
            };

            labels.push(label.to_string());
        }

        return labels;
    }

    panic!("completion response was not returned");
}

#[test]
fn completes_std_module_symbols() {
    let labels: Vec<String> =
        self::complete_labels("import std::mem;\n\nfn main() s32 {\n    mem::\n}\n", 3, 9);

    assert!(labels.contains(&"allocateMemory".to_string()));
    assert!(labels.contains(&"freeMemory".to_string()));
    assert!(labels.contains(&"PROT_READ".to_string()));
    assert!(!labels.contains(&"s32".to_string()));
}

#[test]
fn completes_std_module_alias() {
    let labels: Vec<String> = self::complete_labels(
        "import std::mem as memory;\n\nfn main() s32 {\n    memory::\n}\n",
        3,
        12,
    );

    assert!(labels.contains(&"allocateMemory".to_string()));
    assert!(labels.contains(&"freeMemory".to_string()));
    assert!(labels.contains(&"PROT_READ".to_string()));
}

#[test]
fn completes_std_module_only_import() {
    let labels: Vec<String> = self::complete_labels(
        "import std::mem only { allocateMemory };\n\nfn main() s32 {\n    mem::\n}\n",
        3,
        9,
    );

    assert_eq!(labels, vec!["allocateMemory".to_string()]);
    assert!(!labels.contains(&"freeMemory".to_string()));
}
