/*

    Copyright (C) 2026  Stevens Benavides

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*/

use std::io::Read;
use std::io::Write;

use serde_json::Value;

mod analysis;
mod completion;
mod documents;

#[cfg(not(target_pointer_width = "64"))]
compile_error!("This compiler requires a 64-bit target.");
#[rustversion::before(1.85)]
compile_error!("This compiler requires Rust 1.85 or newer.");

#[global_allocator]
static GLOBAL: thrustc_heap_allocator::ThrustCompilerHeapAllocator =
    thrustc_heap_allocator::ThrustCompilerHeapAllocator;

fn main() {
    let arguments: Vec<String> = std::env::args().collect();

    if arguments.iter().any(|argument| argument == "--version") {
        println!("{}", thrustc_constants::COMPILER_VERSION);
        return;
    }

    let stdin: std::io::Stdin = std::io::stdin();
    let stdout: std::io::Stdout = std::io::stdout();

    let mut input: std::io::StdinLock<'_> = stdin.lock();
    let mut output: std::io::StdoutLock<'_> = stdout.lock();

    let mut shutdown_requested: bool = false;
    let mut documents: documents::Documents = documents::Documents::new();
    let mut analysis: analysis::Analysis = analysis::Analysis::new();

    while let Some(message) = self::read_message(&mut input) {
        let Ok(payload) = serde_json::from_slice::<Value>(&message) else {
            continue;
        };

        let method: &str = payload
            .get("method")
            .and_then(Value::as_str)
            .expect("LSP message must specify 'method'.");
        let id: Option<Value> = payload.get("id").cloned();

        match method {
            "initialize" => {
                let Some(id) = id else {
                    continue;
                };

                let response: Value = serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "serverInfo": {
                            "name": "thrustc_lsp",
                            "version": thrustc_constants::COMPILER_VERSION
                        },
                        "capabilities": {
                            "textDocumentSync": 1,
                            "hoverProvider": true,
                            "definitionProvider": true,
                            "documentSymbolProvider": true,
                            "completionProvider": {
                                "resolveProvider": false,
                                "triggerCharacters": [".", ":", "@"]
                            }
                        }
                    }
                });
                let body: String = response.to_string();
                let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                let _ = output.write_all(header.as_bytes());
                let _ = output.write_all(body.as_bytes());
                let _ = output.flush();
            }

            "shutdown" => {
                shutdown_requested = true;

                let Some(id) = id else {
                    continue;
                };

                let response: Value = serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": null
                });
                let body: String = response.to_string();
                let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                let _ = output.write_all(header.as_bytes());
                let _ = output.write_all(body.as_bytes());
                let _ = output.flush();
            }

            "textDocument/didOpen" => {
                let Some(uri) = documents.open(&payload) else {
                    continue;
                };

                let document: Option<&documents::Document> = documents.get(&uri);
                let diagnostics: Vec<Value> = analysis.analyze_document(document);

                let notification: Value = serde_json::json!({
                    "jsonrpc": "2.0",
                    "method": "textDocument/publishDiagnostics",
                    "params": {
                        "uri": uri,
                        "diagnostics": diagnostics
                    }
                });

                let body: String = notification.to_string();
                let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                let _ = output.write_all(header.as_bytes());
                let _ = output.write_all(body.as_bytes());
                let _ = output.flush();
            }

            "textDocument/didChange" => {
                let Some(uri) = documents.change(&payload) else {
                    continue;
                };

                let document: Option<&documents::Document> = documents.get(&uri);
                let diagnostics: Vec<Value> = analysis.analyze_document(document);

                let notification: Value = serde_json::json!({
                    "jsonrpc": "2.0",
                    "method": "textDocument/publishDiagnostics",
                    "params": {
                        "uri": uri,
                        "diagnostics": diagnostics
                    }
                });

                let body: String = notification.to_string();
                let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                let _ = output.write_all(header.as_bytes());
                let _ = output.write_all(body.as_bytes());
                let _ = output.flush();
            }

            "textDocument/didClose" => {
                let Some(uri) = documents.close(&payload) else {
                    continue;
                };

                analysis.remove_document(&uri);

                let notification: Value = serde_json::json!({
                    "jsonrpc": "2.0",
                    "method": "textDocument/publishDiagnostics",
                    "params": {
                        "uri": uri,
                        "diagnostics": []
                    }
                });

                let body: String = notification.to_string();
                let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                let _ = output.write_all(header.as_bytes());
                let _ = output.write_all(body.as_bytes());
                let _ = output.flush();
            }

            "textDocument/hover" => {
                let Some(id) = id else {
                    continue;
                };

                let response: Value = serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": null
                });

                let body: String = response.to_string();
                let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                let _ = output.write_all(header.as_bytes());
                let _ = output.write_all(body.as_bytes());
                let _ = output.flush();
            }

            "textDocument/definition" => {
                let Some(id) = id else {
                    continue;
                };

                let response: Value = serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": null
                });

                let body: String = response.to_string();
                let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                let _ = output.write_all(header.as_bytes());
                let _ = output.write_all(body.as_bytes());
                let _ = output.flush();
            }

            "textDocument/completion" => {
                let Some(id) = id else {
                    continue;
                };

                let items: Vec<Value> = completion::complete(&documents, &analysis, &payload);

                let response: Value = serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "isIncomplete": false,
                        "items": items
                    }
                });

                let body: String = response.to_string();
                let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                let _ = output.write_all(header.as_bytes());
                let _ = output.write_all(body.as_bytes());
                let _ = output.flush();
            }

            "textDocument/documentSymbol" => {
                let Some(id) = id else {
                    continue;
                };

                let response: Value = serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": []
                });

                let body: String = response.to_string();
                let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                let _ = output.write_all(header.as_bytes());
                let _ = output.write_all(body.as_bytes());
                let _ = output.flush();
            }

            "exit" => break,

            _ => {
                if let Some(id) = id {
                    let response: Value = if shutdown_requested {
                        serde_json::json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": {
                                "code": -32000,
                                "message": "Server is shutting down"
                            }
                        })
                    } else {
                        serde_json::json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": {
                                "code": -32601,
                                "message": "Method not found"
                            }
                        })
                    };

                    let body: String = response.to_string();
                    let header: String = format!("Content-Length: {}\r\n\r\n", body.len());

                    let _ = output.write_all(header.as_bytes());
                    let _ = output.write_all(body.as_bytes());
                    let _ = output.flush();
                }
            }
        }
    }
}

fn read_message(input: &mut impl Read) -> Option<Vec<u8>> {
    let mut headers: Vec<u8> = Vec::with_capacity(u8::MAX as usize);
    let mut byte: [u8; 1] = [0];

    loop {
        if input.read_exact(&mut byte).is_err() {
            return None;
        }

        headers.push(byte[0]);

        if headers.ends_with(b"\r\n\r\n") {
            break;
        }
    }

    let headers: String = String::from_utf8_lossy(&headers).into_owned();
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

    let content_length: usize = content_length?;

    let mut body: Vec<u8> = vec![0; content_length];

    if input.read_exact(&mut body).is_err() {
        return None;
    }

    Some(body)
}
