fn main() {
    #[cfg(target_os = "windows")]
    {
        use winres::*;

        let mut res: WindowsResource = WindowsResource::new();

        let version_str: &str = env!("CARGO_PKG_VERSION");

        let mut version_parts = version_str
            .split('.')
            .map(|s| s.parse::<u16>().unwrap_or(0));
        let major: u16 = version_parts.next().unwrap_or(0);
        let minor: u16 = version_parts.next().unwrap_or(0);
        let patch: u16 = version_parts.next().unwrap_or(0);
        let build: u16 = version_parts.next().unwrap_or(0);

        let version_hex: u64 = ((major as u64) << 48)
            | ((minor as u64) << 32)
            | ((patch as u64) << 16)
            | (build as u64);

        let icon_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("assets")
            .join("thrustlang-logo.ico");
        res.set_icon(
            icon_path
                .to_str()
                .expect("icon path contains invalid UTF-8"),
        );

        res.set(
            "FileDescription",
            "A general-purpose, statically typed systems programming language",
        );
        res.set("ProductName", "thrustc");
        res.set("OriginalFilename", "thrustc.exe");
        res.set("LegalCopyright", "Copyright © 2026 Stevens Benavides");
        res.set("CompanyName", "Thrust Programming Language");
        res.set_version_info(winres::VersionInfo::PRODUCTVERSION, version_hex);

        res.set_manifest(
            r#"
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
<trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
    <security>
        <requestedPrivileges>
            <requestedExecutionLevel level="asInvoker" uiAccess="false"/>
        </requestedPrivileges>
    </security>
</trustInfo>
</assembly>
"#,
        );

        res.compile().unwrap();
    }
}
