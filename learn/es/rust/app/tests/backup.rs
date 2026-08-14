use backupforge::{
    create_backup, load_manifest, restore_backup, sha256_file, update_backup, verify_backup,
    BackupError, Manifest, ManifestEntry,
};
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

#[test]
fn incremental_update_reuses_unchanged_and_copies_modified_files() {
    let source = tempdir().unwrap();
    let backup = tempdir().unwrap();
    fs::write(source.path().join("same.txt"), b"igual").unwrap();
    fs::write(source.path().join("change.txt"), b"antes").unwrap();
    create_backup(source.path(), backup.path()).unwrap();

    fs::write(source.path().join("change.txt"), b"despues").unwrap();
    let report = update_backup(source.path(), backup.path()).unwrap();

    assert_eq!(1, report.reused);
    assert_eq!(1, report.copied);
    assert_eq!(0, report.removed);
    assert!(verify_backup(backup.path(), &report.manifest)
        .unwrap()
        .is_valid());
}

#[test]
fn incremental_update_removes_files_no_longer_present_in_source() {
    let source = tempdir().unwrap();
    let backup = tempdir().unwrap();
    fs::write(source.path().join("keep.txt"), b"keep").unwrap();
    fs::write(source.path().join("remove.txt"), b"remove").unwrap();
    create_backup(source.path(), backup.path()).unwrap();

    fs::remove_file(source.path().join("remove.txt")).unwrap();
    let report = update_backup(source.path(), backup.path()).unwrap();

    assert_eq!(1, report.reused);
    assert_eq!(0, report.copied);
    assert_eq!(1, report.removed);
    assert!(!backup.path().join("remove.txt").exists());
    assert_eq!(
        vec!["keep.txt"],
        report
            .manifest
            .files
            .iter()
            .map(|entry| entry.path.as_str())
            .collect::<Vec<_>>()
    );
}

#[test]
fn incremental_update_repairs_corrupt_destination_instead_of_reusing_it() {
    let source = tempdir().unwrap();
    let backup = tempdir().unwrap();
    fs::write(source.path().join("data.txt"), b"ABCD").unwrap();
    create_backup(source.path(), backup.path()).unwrap();
    fs::write(backup.path().join("data.txt"), b"WXYZ").unwrap();

    let report = update_backup(source.path(), backup.path()).unwrap();

    assert_eq!(0, report.reused);
    assert_eq!(1, report.copied);
    assert!(verify_backup(backup.path(), &report.manifest)
        .unwrap()
        .is_valid());
}

#[test]
fn restore_copies_only_after_successful_verification() {
    let source = tempdir().unwrap();
    let backup = tempdir().unwrap();
    let restore = tempdir().unwrap();
    fs::create_dir_all(source.path().join("nested")).unwrap();
    fs::write(source.path().join("nested/data.txt"), b"contenido").unwrap();
    let manifest = create_backup(source.path(), backup.path()).unwrap();

    let restored = restore_backup(backup.path(), restore.path(), &manifest).unwrap();

    assert_eq!(1, restored);
    assert_eq!(
        b"contenido",
        fs::read(restore.path().join("nested/data.txt"))
            .unwrap()
            .as_slice()
    );
}

#[test]
fn restore_refuses_corrupt_backup_before_writing_output() {
    let source = tempdir().unwrap();
    let backup = tempdir().unwrap();
    let restore = tempdir().unwrap();
    fs::write(source.path().join("data.txt"), b"ABCD").unwrap();
    let manifest = create_backup(source.path(), backup.path()).unwrap();
    fs::write(backup.path().join("data.txt"), b"WXYZ").unwrap();

    let error = restore_backup(backup.path(), restore.path(), &manifest).unwrap_err();

    assert!(matches!(error, BackupError::Integrity(_)));
    assert!(!restore.path().join("data.txt").exists());
}

#[test]
fn public_verification_rejects_unsafe_constructed_manifest() {
    let backup = tempdir().unwrap();
    let manifest = Manifest {
        format_version: 1,
        files: vec![ManifestEntry {
            path: "../outside.txt".into(),
            bytes: 1,
            sha256: "00".into(),
        }],
    };

    assert!(verify_backup(backup.path(), &manifest).is_err());
}

#[test]
fn rejects_duplicate_paths_in_manifest() {
    let backup = tempdir().unwrap();
    fs::write(
        backup.path().join("manifest.json"),
        r#"{"format_version":1,"files":[{"path":"a.txt","bytes":1,"sha256":"00"},{"path":"a.txt","bytes":1,"sha256":"00"}]}"#,
    )
    .unwrap();

    assert!(load_manifest(backup.path()).is_err());
}
