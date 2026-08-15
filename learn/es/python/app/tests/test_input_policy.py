import pytest

from ledgermatch.input_policy import InputPolicyError, OutputPolicyError, validate_input_file, validate_report_destinations


def test_validate_input_file_rejects_non_file(tmp_path):
    with pytest.raises(InputPolicyError, match="archivo regular"):
        validate_input_file(tmp_path)


def test_validate_input_file_rejects_file_over_limit(tmp_path):
    path = tmp_path / "large.csv"; path.write_bytes(b"12345")
    with pytest.raises(InputPolicyError, match="excede el límite"):
        validate_input_file(path, max_bytes=4)


def test_report_destination_may_not_equal_source(tmp_path):
    source = tmp_path / "input.csv"; source.write_text("x", encoding="utf-8")
    with pytest.raises(OutputPolicyError, match="misma ruta"):
        validate_report_destinations(source, [source])


def test_report_destinations_must_be_distinct(tmp_path):
    source = tmp_path / "input.csv"; source.write_text("x", encoding="utf-8"); destination = tmp_path / "report.out"
    with pytest.raises(OutputPolicyError, match="misma ruta"):
        validate_report_destinations(source, [destination, destination])
