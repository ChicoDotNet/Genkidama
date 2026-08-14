use backupforge::{
    create_backup, create_snapshot, list_snapshots, load_manifest, restore_backup,
    restore_snapshot, update_backup, verify_backup, verify_snapshot,
};
use std::env;
use std::path::Path;

fn usage() -> ! {
    eprintln!(
        "Uso:\n  backupforge create <origen> <destino>\n  backupforge update <origen> <backup>\n  backupforge verify <backup>\n  backupforge restore <backup> <destino>\n  backupforge snapshot <origen> <repositorio> <nombre>\n  backupforge snapshots <repositorio>\n  backupforge verify-snapshot <repositorio> <nombre>\n  backupforge restore-snapshot <repositorio> <nombre> <destino>"
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
        [_, command, source, repository, name] if command == "snapshot" => {
            let summary = create_snapshot(Path::new(source), Path::new(repository), name)?;
            println!(
                "snapshot creado: {} ({} archivos, {} bytes)",
                summary.name, summary.files, summary.bytes
            );
            Ok(())
        }
        [_, command, repository] if command == "snapshots" => {
            for summary in list_snapshots(Path::new(repository))? {
                println!("{}\t{}\t{}", summary.name, summary.files, summary.bytes);
            }
            Ok(())
        }
        [_, command, repository, name] if command == "verify-snapshot" => {
            let verification = verify_snapshot(Path::new(repository), name)?;
            if verification.is_valid() {
                println!("snapshot válido: {} archivos", verification.checked);
                Ok(())
            } else {
                eprintln!(
                    "snapshot inválido: {} archivo(s) no coinciden",
                    verification.mismatches.len()
                );
                for path in verification.mismatches {
                    eprintln!("- {path}");
                }
                std::process::exit(1);
            }
        }
        [_, command, repository, name, destination] if command == "restore-snapshot" => {
            let restored = restore_snapshot(Path::new(repository), name, Path::new(destination))?;
            println!("snapshot restaurado: {restored} archivos");
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
