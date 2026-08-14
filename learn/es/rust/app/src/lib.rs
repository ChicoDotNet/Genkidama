//! Núcleo de BackupForge: inventario, copia y verificación determinista.

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};

/// Error recuperable de BackupForge.
#[derive(Debug)]
pub enum BackupError {
    Io(io::Error),
    Json(serde_json::Error),
    InvalidManifest(String),
}

impl std::fmt::Display for BackupError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(error) => write!(f, "error de I/O: {error}"),
            Self::Json(error) => write!(f, "manifest inválido: {error}"),
            Self::InvalidManifest(message) => write!(f, "manifest inconsistente: {message}"),
        }
    }
}

impl std::error::Error for BackupError {}

impl From<io::Error> for BackupError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<serde_json::Error> for BackupError {
    fn from(value: serde_json::Error) -> Self {
        Self::Json(value)
    }
}

/// Entrada verificable de un archivo respaldado.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestEntry {
    pub path: String,
    pub bytes: u64,
    pub sha256: String,
}

/// Manifest portable de un backup.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Manifest {
    pub format_version: u32,
    pub files: Vec<ManifestEntry>,
}

/// Resultado agregado de una verificación.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Verification {
    pub checked: usize,
    pub mismatches: Vec<String>,
}

impl Verification {
    /// Indica si todos los archivos verificados coinciden con el manifest.
    pub fn is_valid(&self) -> bool {
        self.mismatches.is_empty()
    }
}

/// Calcula SHA-256 leyendo el archivo por streaming.
pub fn sha256_file(path: &Path) -> Result<String, BackupError> {
    let mut file = File::open(path)?;
    let mut hasher = Sha256::new();
    let mut buffer = [0_u8; 64 * 1024];

    loop {
        let read = file.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }

    Ok(format!("{:x}", hasher.finalize()))
}

fn collect_files(root: &Path, current: &Path, out: &mut Vec<PathBuf>) -> Result<(), BackupError> {
    let mut entries = fs::read_dir(current)?.collect::<Result<Vec<_>, _>>()?;
    entries.sort_by_key(|entry| entry.file_name());

    for entry in entries {
        let path = entry.path();
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            collect_files(root, &path, out)?;
        } else if file_type.is_file() {
            let relative = path
                .strip_prefix(root)
                .map_err(|_| BackupError::InvalidManifest("ruta fuera de raíz".into()))?;
            out.push(relative.to_path_buf());
        }
    }

    Ok(())
}

/// Crea un backup completo determinista y escribe `manifest.json` al final.
pub fn create_backup(source: &Path, destination: &Path) -> Result<Manifest, BackupError> {
    let mut relative_files = Vec::new();
    collect_files(source, source, &mut relative_files)?;
    fs::create_dir_all(destination)?;

    let mut files = Vec::with_capacity(relative_files.len());
    for relative in relative_files {
        let source_file = source.join(&relative);
        let destination_file = destination.join(&relative);
        if let Some(parent) = destination_file.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::copy(&source_file, &destination_file)?;
        files.push(ManifestEntry {
            path: relative.to_string_lossy().replace('\\', "/"),
            bytes: fs::metadata(&source_file)?.len(),
            sha256: sha256_file(&source_file)?,
        });
    }

    let manifest = Manifest {
        format_version: 1,
        files,
    };
    fs::write(
        destination.join("manifest.json"),
        serde_json::to_vec_pretty(&manifest)?,
    )?;

    Ok(manifest)
}

/// Carga un manifest y rechaza versiones desconocidas o rutas inseguras.
pub fn load_manifest(backup: &Path) -> Result<Manifest, BackupError> {
    let manifest: Manifest = serde_json::from_slice(&fs::read(backup.join("manifest.json"))?)?;

    if manifest.format_version != 1 {
        return Err(BackupError::InvalidManifest(format!(
            "versión {} no soportada",
            manifest.format_version
        )));
    }

    for entry in &manifest.files {
        let path = Path::new(&entry.path);
        if path.is_absolute()
            || path
                .components()
                .any(|component| matches!(component, std::path::Component::ParentDir))
        {
            return Err(BackupError::InvalidManifest(format!(
                "ruta insegura: {}",
                entry.path
            )));
        }
    }

    Ok(manifest)
}

/// Verifica tamaño y SHA-256 de cada archivo descrito por el manifest.
pub fn verify_backup(backup: &Path, manifest: &Manifest) -> Result<Verification, BackupError> {
    let mut mismatches = Vec::new();

    for entry in &manifest.files {
        let path = backup.join(&entry.path);
        match fs::metadata(&path) {
            Ok(metadata) if metadata.is_file() => {
                if metadata.len() != entry.bytes || sha256_file(&path)? != entry.sha256 {
                    mismatches.push(entry.path.clone());
                }
            }
            _ => mismatches.push(entry.path.clone()),
        }
    }

    Ok(Verification {
        checked: manifest.files.len(),
        mismatches,
    })
}
