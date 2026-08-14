use backupforge::{create_backup, load_manifest, sha256_file, verify_backup};
use std::fs;
use tempfile::tempdir;

#[test]
fn creates_nested_backup_and_verifies_it() {
    let source = tempdir().unwrap();
    let backup = tempdir().unwrap();
    fs::create_dir_all(source.path().join("docs")).unwrap();
    fs::write(source.path().join("hello.txt"), b"hola").unwrap();
    fs::write(source.path().join("docs/readme.md"), b"contenido").unwrap();

    let manifest = create_backup(source.path(), backup.path()).unwrap();
    assert_eq!(2, manifest.files.len());
    assert_eq!("docs/readme.md", manifest.files[0].path);
    assert_eq!("hello.txt", manifest.files[1].path);

    let loaded = load_manifest(backup.path()).unwrap();
    let result = verify_backup(backup.path(), &loaded).unwrap();
    assert!(result.is_valid());
    assert_eq!(2, result.checked);
}

#[test]
fn detects_tampering_without_trusting_file_size() {
    let source = tempdir().unwrap();
    let backup = tempdir().unwrap();
    fs::write(source.path().join("data.txt"), b"ABCD").unwrap();
    create_backup(source.path(), backup.path()).unwrap();
    fs::write(backup.path().join("data.txt"), b"WXYZ").unwrap();

    let manifest = load_manifest(backup.path()).unwrap();
    let result = verify_backup(backup.path(), &manifest).unwrap();
    assert!(!result.is_valid());
    assert_eq!(vec!["data.txt"], result.mismatches);
}

#[test]
fn sha256_matches_known_vector() {
    let dir = tempdir().unwrap();
    let file = dir.path().join("vector.txt");
    fs::write(&file, b"abc").unwrap();
    assert_eq!(
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
        sha256_file(&file).unwrap()
    );
}

#[test]
fn rejects_parent_directory_in_manifest() {
    let backup = tempdir().unwrap();
    fs::write(
        backup.path().join("manifest.json"),
        r#"{"format_version":1,"files":[{"path":"../escape.txt","bytes":1,"sha256":"00"}]}"#,
    )
    .unwrap();

    assert!(load_manifest(backup.path()).is_err());
}
