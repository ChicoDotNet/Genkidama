//! Núcleo de BackupForge: inventario, copia incremental, restauración y verificación determinista.

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, HashSet};
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};

/// Error recuperable de BackupForge.
#[derive(Debug)]
pub enum BackupError {
    Io(io::Error),
    Json(serde_json::Error),
    InvalidManifest(String),
    Integrity(Vec<String>),
}

impl std::fmt::Display for BackupError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(error) => write!(f, "error de I/O: {error}"),
            Self::Json(error) => write!(f, "manifest inválido: {error}"),
            Self::InvalidManifest(message) => write!(f, "manifest inconsistente: {message}"),
            Self::Integrity(paths) => write!(
                f,
                "backup no íntegro: {} archivo(s) no coinciden",
                paths.len()
            ),
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

/// Resultado observable de una actualización incremental.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncrementalReport {
    pub manifest: Manifest,
    pub reused: usize,
    pub copied: usize,
    pub removed: usize,
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

fn manifest_entry(source: &Path, relative: &Path) -> Result<ManifestEntry, BackupError> {
    let source_file = source.join(relative);
    Ok(ManifestEntry {
        path: relative.to_string_lossy().replace('\\', "/"),
        bytes: fs::metadata(&source_file)?.len(),
        sha256: sha256_file(&source_file)?,
    })
}

fn validate_manifest(manifest: &Manifest) -> Result<(), BackupError> {
    if manifest.format_version != 1 {
        return Err(BackupError::InvalidManifest(format!(
            "versión {} no soportada",
            manifest.format_version
        )));
    }

    let mut seen = HashSet::new();
    for entry in &manifest.files {
        let path = Path::new(&entry.path);
        if entry.path.is_empty()
            || path.is_absolute()
            || path
                .components()
                .any(|component| matches!(component, std::path::Component::ParentDir))
        {
            return Err(BackupError::InvalidManifest(format!(
                "ruta insegura: {}",
                entry.path
            )));
        }
        if !seen.insert(entry.path.as_str()) {
            return Err(BackupError::InvalidManifest(format!(
                "ruta duplicada: {}",
                entry.path
            )));
        }
    }

    Ok(())
}

fn write_manifest(destination: &Path, manifest: &Manifest) -> Result<(), BackupError> {
    fs::write(
        destination.join("manifest.json"),
        serde_json::to_vec_pretty(manifest)?,
    )?;
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
        files.push(manifest_entry(source, &relative)?);
    }

    let manifest = Manifest {
        format_version: 1,
        files,
    };
    write_manifest(destination, &manifest)?;
    Ok(manifest)
}

/// Actualiza un backup existente y evita reescribir archivos cuyo contenido ya coincide.
///
/// La reutilización sólo ocurre después de comprobar tamaño y SHA-256 tanto en origen como
/// en el backup actual. Un archivo de destino corrupto se vuelve a copiar desde el origen.
pub fn update_backup(source: &Path, destination: &Path) -> Result<IncrementalReport, BackupError> {
    let previous = if destination.join("manifest.json").is_file() {
        load_manifest(destination)?
    } else {
        Manifest {
            format_version: 1,
            files: Vec::new(),
        }
    };
    let previous_by_path: BTreeMap<&str, &ManifestEntry> = previous
        .files
        .iter()
        .map(|entry| (entry.path.as_str(), entry))
        .collect();

    let mut relative_files = Vec::new();
    collect_files(source, source, &mut relative_files)?;
    fs::create_dir_all(destination)?;

    let mut files = Vec::with_capacity(relative_files.len());
    let mut current_paths = HashSet::new();
    let mut reused = 0;
    let mut copied = 0;

    for relative in relative_files {
        let entry = manifest_entry(source, &relative)?;
        let destination_file = destination.join(&entry.path);
        current_paths.insert(entry.path.clone());

        let can_reuse = match previous_by_path.get(entry.path.as_str()) {
            Some(previous_entry)
                if previous_entry.bytes == entry.bytes
                    && previous_entry.sha256 == entry.sha256
                    && destination_file.is_file() =>
            {
                fs::metadata(&destination_file)?.len() == entry.bytes
                    && sha256_file(&destination_file)? == entry.sha256
            }
            _ => false,
        };

        if can_reuse {
            reused += 1;
        } else {
            if let Some(parent) = destination_file.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::copy(source.join(&relative), &destination_file)?;
            copied += 1;
        }
        files.push(entry);
    }

    let mut removed = 0;
    for previous_entry in &previous.files {
        if !current_paths.contains(&previous_entry.path) {
            let stale = destination.join(&previous_entry.path);
            if stale.is_file() {
                fs::remove_file(stale)?;
                removed += 1;
            }
        }
    }

    let manifest = Manifest {
        format_version: 1,
        files,
    };
    write_manifest(destination, &manifest)?;

    Ok(IncrementalReport {
        manifest,
        reused,
        copied,
        removed,
    })
}

/// Carga un manifest y rechaza versiones desconocidas, rutas inseguras o duplicadas.
pub fn load_manifest(backup: &Path) -> Result<Manifest, BackupError> {
    let manifest: Manifest = serde_json::from_slice(&fs::read(backup.join("manifest.json"))?)?;
    validate_manifest(&manifest)?;
    Ok(manifest)
}

/// Verifica tamaño y SHA-256 de cada archivo descrito por el manifest.
pub fn verify_backup(backup: &Path, manifest: &Manifest) -> Result<Verification, BackupError> {
    validate_manifest(manifest)?;
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

/// Restaura únicamente archivos de un backup previamente verificado.
///
/// La función no borra archivos ajenos que ya existan en `destination`. Si el backup no
/// coincide con su manifest, devuelve `BackupError::Integrity` antes de escribir la salida.
pub fn restore_backup(
    backup: &Path,
    destination: &Path,
    manifest: &Manifest,
) -> Result<usize, BackupError> {
    validate_manifest(manifest)?;
    let verification = verify_backup(backup, manifest)?;
    if !verification.is_valid() {
        return Err(BackupError::Integrity(verification.mismatches));
    }

    fs::create_dir_all(destination)?;
    for entry in &manifest.files {
        let target = destination.join(&entry.path);
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::copy(backup.join(&entry.path), target)?;
    }

    Ok(manifest.files.len())
}
