use backupforge::{
    create_backup, load_manifest, restore_backup, update_backup, verify_backup,
};
use std::env;
use std::path::Path;

fn usage() -> ! {
    eprintln!(
        "Uso:\n  backupforge create <origen> <destino>\n  backupforge update <origen> <backup>\n  backupforge verify <backup>\n  backupforge restore <backup> <destino>"
    );
    std::process::exit(2);
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    match args.as_slice() {
        [_, command, source, destination] if command == "create" => {
            let manifest = create_backup(Path::new(source), Path::new(destination))?;
            println!("backup creado: {} archivos", manifest.files.len());
            Ok(())
        }
        [_, command, source, backup] if command == "update" => {
            let report = update_backup(Path::new(source), Path::new(backup))?;
            println!(
                "backup actualizado: {} archivos, {} reutilizados, {} copiados, {} eliminados",
                report.manifest.files.len(),
                report.reused,
                report.copied,
                report.removed
            );
            Ok(())
        }
        [_, command, backup] if command == "verify" => {
            let root = Path::new(backup);
            let manifest = load_manifest(root)?;
            let verification = verify_backup(root, &manifest)?;
            if verification.is_valid() {
                println!("backup válido: {} archivos", verification.checked);
                Ok(())
            } else {
                eprintln!(
                    "backup inválido: {} archivo(s) no coinciden",
                    verification.mismatches.len()
                );
                for path in verification.mismatches {
                    eprintln!("- {path}");
                }
                std::process::exit(1);
            }
        }
        [_, command, backup, destination] if command == "restore" => {
            let root = Path::new(backup);
            let manifest = load_manifest(root)?;
            let restored = restore_backup(root, Path::new(destination), &manifest)?;
            println!("restore completado: {restored} archivos");
            Ok(())
        }
        _ => usage(),
    }
}

fn main() {
    if let Err(error) = run() {
        eprintln!("error: {error}");
        std::process::exit(1);
    }
}
