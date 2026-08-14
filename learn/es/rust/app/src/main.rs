use backupforge::{
    Manifest, create_backup, create_snapshot, list_snapshots, load_manifest, restore_backup,
    restore_snapshot, update_backup, verify_backup, verify_snapshot,
};
use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::Path;

#[derive(Debug, PartialEq, Eq)]
struct AuditReport {
    checked: usize,
    mismatches: Vec<String>,
    unexpected: Vec<String>,
}

impl AuditReport {
    fn is_clean(&self) -> bool {
        self.mismatches.is_empty() && self.unexpected.is_empty()
    }
}

fn usage() -> ! {
    eprintln!(
        "Uso:\n  backupforge create <origen> <destino>\n  backupforge update <origen> <backup>\n  backupforge verify <backup>\n  backupforge audit <backup>\n  backupforge restore <backup> <destino>\n  backupforge snapshot <origen> <repositorio> <nombre>\n  backupforge snapshots <repositorio>\n  backupforge verify-snapshot <repositorio> <nombre>\n  backupforge restore-snapshot <repositorio> <nombre> <destino>"
    );
    std::process::exit(2);
}

fn collect_entries(
    root: &Path,
    current: &Path,
    out: &mut Vec<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut entries = fs::read_dir(current)?.collect::<Result<Vec<_>, _>>()?;
    entries.sort_by_key(|entry| entry.file_name());

    for entry in entries {
        let path = entry.path();
        let file_type = entry.file_type()?;
        let relative = path
            .strip_prefix(root)?
            .to_string_lossy()
            .replace('\\', "/");

        if file_type.is_dir() {
            collect_entries(root, &path, out)?;
        } else if relative != "manifest.json" {
            out.push(relative);
        }
    }

    Ok(())
}

fn audit_backup(
    root: &Path,
    manifest: &Manifest,
) -> Result<AuditReport, Box<dyn std::error::Error>> {
    let verification = verify_backup(root, manifest)?;
    let expected: HashSet<&str> = manifest.files.iter().map(|entry| entry.path.as_str()).collect();
    let mut observed = Vec::new();
    collect_entries(root, root, &mut observed)?;
    let unexpected = observed
        .into_iter()
        .filter(|path| !expected.contains(path.as_str()))
        .collect();

    Ok(AuditReport {
        checked: verification.checked,
        mismatches: verification.mismatches,
        unexpected,
    })
}

fn print_audit(report: AuditReport) {
    if report.is_clean() {
        println!("backup auditado: {} archivos, sin extras", report.checked);
        return;
    }

    eprintln!(
        "backup con hallazgos: {} mismatch(s), {} entrada(s) inesperada(s)",
        report.mismatches.len(),
        report.unexpected.len()
    );
    for path in report.mismatches {
        eprintln!("- mismatch: {path}");
    }
    for path in report.unexpected {
        eprintln!("- inesperado: {path}");
    }
    std::process::exit(1);
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
        [_, command, backup] if command == "audit" => {
            let root = Path::new(backup);
            let manifest = load_manifest(root)?;
            print_audit(audit_backup(root, &manifest)?);
            Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn audit_accepts_exact_backup() {
        let source = tempdir().unwrap();
        let backup = tempdir().unwrap();
        fs::write(source.path().join("data.txt"), b"abc").unwrap();
        let manifest = create_backup(source.path(), backup.path()).unwrap();

        let report = audit_backup(backup.path(), &manifest).unwrap();

        assert!(report.is_clean());
        assert_eq!(1, report.checked);
    }

    #[test]
    fn audit_detects_unexpected_regular_file() {
        let source = tempdir().unwrap();
        let backup = tempdir().unwrap();
        fs::write(source.path().join("data.txt"), b"abc").unwrap();
        let manifest = create_backup(source.path(), backup.path()).unwrap();
        fs::write(backup.path().join("injected.txt"), b"not in manifest").unwrap();

        let report = audit_backup(backup.path(), &manifest).unwrap();

        assert!(!report.is_clean());
        assert!(report.mismatches.is_empty());
        assert_eq!(vec!["injected.txt"], report.unexpected);
    }
}
