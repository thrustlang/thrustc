use serde_json::Value;
use std::io::Write;

fn complete_items(text: &str, line: u64, character: u64) -> Vec<Value> {
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
        let mut completions: Vec<Value> = Vec::with_capacity(items.len());

        for item in items {
            completions.push(item.clone());
        }

        return completions;
    }

    panic!("completion response was not returned");
}

fn complete_labels(text: &str, line: u64, character: u64) -> Vec<String> {
    let items: Vec<Value> = self::complete_items(text, line, character);
    let mut labels: Vec<String> = Vec::with_capacity(items.len());

    for item in items {
        let Some(label) = item.get("label").and_then(Value::as_str) else {
            continue;
        };

        labels.push(label.to_string());
    }

    labels
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

#[test]
fn import_context_only_suggests_std_root() {
    let labels: Vec<String> = self::complete_labels("import ", 0, 7);

    assert_eq!(labels, vec!["std".to_string()]);
}

#[test]
fn import_context_completes_std_root_submodules() {
    let labels: Vec<String> = self::complete_labels("import std::", 0, 12);

    assert!(labels.contains(&"collections".to_string()));
    assert!(labels.contains(&"ffi".to_string()));
    assert!(labels.contains(&"intrinsics".to_string()));
    assert!(labels.contains(&"io".to_string()));
    assert!(labels.contains(&"mem".to_string()));
    assert!(!labels.contains(&"s32".to_string()));
    assert!(!labels.contains(&"fn".to_string()));
    assert!(!labels.contains(&"for-loop".to_string()));
}

#[test]
fn import_context_completes_std_folders() {
    let labels: Vec<String> = self::complete_labels("import std::ffi::", 0, 17);

    assert_eq!(labels, vec!["c".to_string()]);
}

#[test]
fn import_context_completes_nested_std_folder_modules() {
    let labels: Vec<String> = self::complete_labels("import std::ffi::c::", 0, 20);

    assert!(labels.contains(&"float".to_string()));
    assert!(labels.contains(&"int".to_string()));
    assert!(labels.contains(&"io".to_string()));
    assert!(labels.contains(&"math".to_string()));
    assert!(labels.contains(&"mem".to_string()));
    assert!(labels.contains(&"primitives".to_string()));
    assert!(labels.contains(&"random".to_string()));
}

#[test]
fn import_context_filters_partial_std_modules() {
    let labels: Vec<String> = self::complete_labels("import std::collections::v", 0, 26);

    assert_eq!(labels.len(), 2);
    assert!(labels.contains(&"vecdeque".to_string()));
    assert!(labels.contains(&"vector".to_string()));
}

#[test]
fn import_only_context_completes_public_symbols() {
    let labels: Vec<String> = self::complete_labels("import std::mem only { ", 0, 23);

    assert!(labels.contains(&"allocateMemory".to_string()));
    assert!(labels.contains(&"freeMemory".to_string()));
    assert!(labels.contains(&"PROT_READ".to_string()));
    assert!(!labels.contains(&"s32".to_string()));
}

#[test]
fn import_only_context_filters_partial_symbols() {
    let labels: Vec<String> = self::complete_labels("import std::mem only { alloc", 0, 28);

    assert!(labels.contains(&"allocateMemory".to_string()));
    assert!(labels.iter().all(|label| label.starts_with("alloc")));
}

#[test]
fn import_only_context_omits_already_selected_symbols() {
    let labels: Vec<String> =
        self::complete_labels("import std::mem only { allocateMemory, ", 0, 39);

    assert!(!labels.contains(&"allocateMemory".to_string()));
    assert!(labels.contains(&"freeMemory".to_string()));
}

#[test]
fn import_only_context_completes_vector_symbols() {
    let labels: Vec<String> =
        self::complete_labels("import std::collections::vector only { p", 0, 40);

    assert!(labels.contains(&"push".to_string()));
    assert!(labels.contains(&"pop".to_string()));
    assert!(labels.iter().all(|label| label.starts_with('p')));
}

#[test]
fn top_level_completion_uses_stable_declarations() {
    let labels: Vec<String> = self::complete_labels("", 0, 0);

    assert!(labels.contains(&"fn".to_string()));
    assert!(labels.contains(&"intrinsic".to_string()));
    assert!(labels.contains(&"directive".to_string()));
    assert!(labels.contains(&"@if".to_string()));
    assert!(!labels.contains(&"return".to_string()));
    assert!(!labels.contains(&"for-loop".to_string()));
}

#[test]
fn statement_completion_uses_statement_starters_only() {
    let labels: Vec<String> = self::complete_labels("fn main() s32 {\n    \n}\n", 1, 4);

    assert!(labels.contains(&"return".to_string()));
    assert!(labels.contains(&"breakall".to_string()));
    assert!(labels.contains(&"continueall".to_string()));
    assert!(labels.contains(&"deref".to_string()));
    assert!(labels.contains(&"load".to_string()));
    assert!(labels.contains(&"fixed".to_string()));
    assert!(labels.contains(&"new".to_string()));
    assert!(!labels.contains(&"fn".to_string()));
    assert!(!labels.contains(&"import".to_string()));
    assert!(!labels.contains(&"directive".to_string()));
}

#[test]
fn type_completion_uses_real_type_names() {
    let labels: Vec<String> = self::complete_labels("fn main() s32 {\n    var text: \n}\n", 1, 14);

    assert!(labels.contains(&"CString".to_string()));
    assert!(labels.contains(&"Fn".to_string()));
    assert!(labels.contains(&"f80".to_string()));
    assert!(labels.contains(&"fppc_128".to_string()));
    assert!(!labels.contains(&"cstring".to_string()));
    assert!(!labels.contains(&"fnref".to_string()));
    assert!(!labels.contains(&"fx8680".to_string()));
}

#[test]
fn attribute_completion_uses_real_stable_attribute_names() {
    let labels: Vec<String> = self::complete_labels("fn main() s32 @", 0, 15);

    assert!(labels.contains(&"@arbitraryArgs".to_string()));
    assert!(labels.contains(&"@entrypoint".to_string()));
    assert!(labels.contains(&"@inline".to_string()));
    assert!(labels.contains(&"@preciseFloatingPoint".to_string()));
    assert!(!labels.contains(&"@ignore".to_string()));
    assert!(!labels.contains(&"@entryPoint".to_string()));
    assert!(!labels.contains(&"@inlineHint".to_string()));
    assert!(!labels.contains(&"@preciseFloats".to_string()));
    assert!(!labels.contains(&"@promote".to_string()));
}

#[test]
fn builtin_completion_uses_real_builtin_names() {
    let labels: Vec<String> = self::complete_labels("fn main() s32 {\n    \n}\n", 1, 4);

    assert!(labels.contains(&"sizeOf".to_string()));
    assert!(labels.contains(&"isLinux".to_string()));
    assert!(labels.contains(&"staticAssert".to_string()));
    assert!(labels.contains(&"memcpy".to_string()));
    assert!(!labels.contains(&"memCpy".to_string()));
}

#[test]
fn completes_std_submodules_without_global_items() {
    let labels: Vec<String> =
        self::complete_labels("import std;\n\nfn main() s32 {\n    std::\n}\n", 3, 9);

    assert!(labels.contains(&"collections".to_string()));
    assert!(labels.contains(&"ffi".to_string()));
    assert!(labels.contains(&"io".to_string()));
    assert!(labels.contains(&"mem".to_string()));
    assert!(!labels.contains(&"s32".to_string()));
    assert!(!labels.contains(&"fn".to_string()));
    assert!(!labels.contains(&"for-loop".to_string()));
}

#[test]
fn completes_nested_std_submodules() {
    let labels: Vec<String> = self::complete_labels(
        "import std;\n\nfn main() s32 {\n    std::collections::\n}\n",
        3,
        22,
    );

    assert!(labels.contains(&"vector".to_string()));
    assert!(labels.contains(&"hashmap".to_string()));
    assert!(labels.contains(&"hashset".to_string()));
    assert!(labels.contains(&"vecdeque".to_string()));
    assert!(!labels.contains(&"s32".to_string()));
}

#[test]
fn completes_imported_function_call_arguments() {
    let labels: Vec<String> = self::complete_labels(
        "import std::mem;\n\nfn main() s32 {\n    mem::allocateMemory(\n}\n",
        3,
        24,
    );

    assert!(labels.contains(&"size".to_string()));
    assert!(!labels.contains(&"s32".to_string()));
}

#[test]
fn completes_imported_generic_function_call_arguments() {
    let labels: Vec<String> = self::complete_labels(
        "import std::collections::vector;\n\nfn main() s32 {\n    vector::push[s32](\n}\n",
        3,
        22,
    );

    assert_eq!(labels, vec!["vector".to_string(), "value".to_string()]);
}

#[test]
fn completes_variadic_fixed_arguments() {
    let labels: Vec<String> = self::complete_labels(
        "import std::io;\n\nfn main() s32 {\n    io::print(\n}\n",
        3,
        14,
    );

    assert_eq!(labels, vec!["fmt".to_string()]);
}

#[test]
fn variadic_extra_arguments_use_normal_completion() {
    let labels: Vec<String> = self::complete_labels(
        "import std::io;\n\nfn main() s32 {\n    var value: s32 = 1;\n    io::print(\"%d\", \n}\n",
        4,
        21,
    );

    assert!(labels.contains(&"value".to_string()));
    assert!(labels.contains(&"io".to_string()));
    assert!(!labels.contains(&"fmt".to_string()));
}

#[test]
fn completes_local_function_call_arguments() {
    let labels: Vec<String> = self::complete_labels(
        "fn sum(a: s32, b: s32) s32 {\n    return a + b;\n}\n\nfn main() s32 {\n    sum(\n}\n",
        5,
        8,
    );

    assert_eq!(labels, vec!["a".to_string(), "b".to_string()]);
}

#[test]
fn templates_use_dash_labels_and_low_priority_sort_text() {
    let items: Vec<Value> = self::complete_items("fn main() s32 {\n    \n}\n", 1, 4);
    let mut found_for_keyword: bool = false;
    let mut found_for_loop: bool = false;

    for item in items {
        let label: &str = item.get("label").and_then(Value::as_str).unwrap_or("");
        let kind: u64 = item.get("kind").and_then(Value::as_u64).unwrap_or(0);

        if label == "for" {
            found_for_keyword = true;
            assert_eq!(kind, 14);
            assert!(item.get("sortText").is_none());
        }

        if label == "for-loop" {
            found_for_loop = true;
            assert_eq!(kind, 15);
            assert_eq!(
                item.get("sortText").and_then(Value::as_str),
                Some("zzzz_for-loop")
            );
            assert_eq!(
                item.get("insertTextFormat").and_then(Value::as_u64),
                Some(2)
            );
        }
    }

    assert!(found_for_keyword);
    assert!(found_for_loop);
}
