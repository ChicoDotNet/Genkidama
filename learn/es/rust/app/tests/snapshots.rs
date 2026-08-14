use backupforge::{
    BackupError, create_snapshot, list_snapshots, restore_snapshot, verify_snapshot,
};
use std::fs;
use tempfile::tempdir;

#[test]
fn creates_immutable_snapshot_and_lists_summary() {
    let source = tempdir().unwrap();
    let repository = tempdir().unwrap();
    fs::create_dir_all(source.path().join("docs")).unwrap();
    fs::write(source.path().join("a.txt"), b"abc").unwrap();
    fs::write(source.path().join("docs/b.txt"), b"12345").unwrap();

    let created = create_snapshot(source.path(), repository.path(), "2026-08-14").unwrap();

    assert_eq!("2026-08-14", created.name);
    assert_eq!(2, created.files);
    assert_eq!(8, created.bytes);
    assert!(repository.path().join("snapshots/2026-08-14/manifest.json").is_file());

    let listed = list_snapshots(repository.path()).unwrap();
    assert_eq!(vec![created], listed);
}

#[test]
fn snapshot_name_cannot_escape_repository() {
    let source = tempdir().unwrap();
    let repository = tempdir().unwrap();
    fs::write(source.path().join("data.txt"), b"safe").unwrap();

    let error = create_snapshot(source.path(), repository.path(), "../outside").unwrap_err();

    assert!(matches!(error, BackupError::InvalidSnapshot(_)));
    assert!(!repository.path().join("outside").exists());
}

#[test]
fn snapshot_is_immutable_by_name() {
    let source = tempdir().unwrap();
    let repository = tempdir().unwrap();
    fs::write(source.path().join("data.txt"), b"version-1").unwrap();
    create_snapshot(source.path(), repository.path(), "v1").unwrap();

    fs::write(source.path().join("data.txt"), b"version-2").unwrap();
    let error = create_snapshot(source.path(), repository.path(), "v1").unwrap_err();

    assert!(matches!(error, BackupError::InvalidSnapshot(_)));
    assert_eq!(
        b"version-1",
        fs::read(repository.path().join("snapshots/v1/data.txt"))
            .unwrap()
            .as_slice()
    );
}

#[test]
fn lists_snapshots_in_deterministic_name_order() {
    let source = tempdir().unwrap();
    let repository = tempdir().unwrap();
    fs::write(source.path().join("data.txt"), b"x").unwrap();

    create_snapshot(source.path(), repository.path(), "z-last").unwrap();
    create_snapshot(source.path(), repository.path(), "a-first").unwrap();

    let names = list_snapshots(repository.path())
        .unwrap()
        .into_iter()
        .map(|summary| summary.name)
        .collect::<Vec<_>>();

    assert_eq!(vec!["a-first", "z-last"], names);
}

#[test]
fn restore_snapshot_verifies_before_writing() {
    let source = tempdir().unwrap();
    let repository = tempdir().unwrap();
    let restore = tempdir().unwrap();
    fs::write(source.path().join("data.txt"), b"ABCD").unwrap();
    create_snapshot(source.path(), repository.path(), "good").unwrap();

    let restored = restore_snapshot(repository.path(), "good", restore.path()).unwrap();

    assert_eq!(1, restored);
    assert_eq!(
        b"ABCD",
        fs::read(restore.path().join("data.txt")).unwrap().as_slice()
    );
}

#[test]
fn corrupt_snapshot_is_reported_and_not_restored() {
    let source = tempdir().unwrap();
    let repository = tempdir().unwrap();
    let restore = tempdir().unwrap();
    fs::write(source.path().join("data.txt"), b"ABCD").unwrap();
    create_snapshot(source.path(), repository.path(), "broken").unwrap();
    fs::write(
        repository.path().join("snapshots/broken/data.txt"),
        b"WXYZ",
    )
    .unwrap();

    let verification = verify_snapshot(repository.path(), "broken").unwrap();
    assert!(!verification.is_valid());
    assert_eq!(vec!["data.txt"], verification.mismatches);

    let error = restore_snapshot(repository.path(), "broken", restore.path()).unwrap_err();
    assert!(matches!(error, BackupError::Integrity(_)));
    assert!(!restore.path().join("data.txt").exists());
}

#[test]
fn ignores_partial_snapshot_directories_during_listing() {
    let repository = tempdir().unwrap();
    fs::create_dir_all(repository.path().join("snapshots/.interrupted.partial")).unwrap();

    assert!(list_snapshots(repository.path()).unwrap().is_empty());
}
