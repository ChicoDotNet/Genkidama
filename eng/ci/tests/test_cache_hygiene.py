from __future__ import annotations

import sys
import unittest
from datetime import timedelta
from pathlib import Path

CI_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(CI_DIR))

import cache_hygiene  # noqa: E402


def make_cache(
    cache_id: int,
    *,
    key: str,
    created: str,
    ref: str = "refs/heads/dev",
    size: int = 1024,
) -> cache_hygiene.CacheEntry:
    created_at = cache_hygiene.parse_timestamp(created)
    return cache_hygiene.CacheEntry(
        id=cache_id,
        ref=ref,
        key=key,
        version=f"v{cache_id}",
        created_at=created_at,
        last_accessed_at=created_at + timedelta(minutes=1),
        size_in_bytes=size,
    )


class CachePolicyTests(unittest.TestCase):
    def setUp(self) -> None:
        self.cutoff = cache_hygiene.parse_timestamp(
            cache_hygiene.DEFAULT_LEGACY_CUTOFF
        )

    def plan(
        self,
        entries: list[cache_hygiene.CacheEntry],
        *,
        scopes: list[str],
        current_ref: str = "refs/heads/dev",
        open_prs: set[int] | None = None,
    ) -> cache_hygiene.Plan:
        return cache_hygiene.plan_deletions(
            entries,
            current_ref=current_ref,
            scopes=scopes,
            legacy_cutoff=self.cutoff,
            open_pull_requests=open_prs or set(),
        )

    def test_pre_i1_cache_is_obsolete_even_when_namespace_is_unknown(self) -> None:
        old = make_cache(
            1,
            key="setup-zig-cache-v2-build-zig-x86_64-linux-0.16.0-legacy",
            created="2026-08-24T12:00:00Z",
        )
        plan = self.plan([old], scopes=[])
        self.assertEqual([item.cache.id for item in plan.deletions], [1])
        self.assertEqual(plan.deletions[0].reason, "obsolete-pre-I1")

    def test_closed_pull_request_cache_is_obsolete(self) -> None:
        cache = make_cache(
            7,
            key=f"setup-go-Linux-go-1.26.5-{'a' * 64}",
            created="2026-08-29T03:00:00Z",
            ref="refs/pull/321/merge",
        )
        plan = self.plan([cache], scopes=[], open_prs={999})
        self.assertEqual([item.cache.id for item in plan.deletions], [7])
        self.assertEqual(plan.deletions[0].reason, "obsolete-closed-pr")

    def test_open_pull_request_cache_is_preserved_without_selected_scope(self) -> None:
        cache = make_cache(
            7,
            key=f"setup-go-Linux-go-1.26.5-{'a' * 64}",
            created="2026-08-29T03:00:00Z",
            ref="refs/pull/321/merge",
        )
        plan = self.plan([cache], scopes=[], open_prs={321})
        self.assertEqual(plan.deletions, ())

    def test_successful_quality_scope_keeps_newest_related_lineage(self) -> None:
        prefix = "setup-python-Linux-x64-ubuntu24-python-3.14.7-pip"
        older = make_cache(
            1,
            key=f"{prefix}-{'a' * 64}",
            created="2026-08-29T02:00:00Z",
        )
        newest = make_cache(
            2,
            key=f"{prefix}-{'b' * 64}",
            created="2026-08-29T03:00:00Z",
        )
        plan = self.plan([older, newest], scopes=["quality"])
        self.assertEqual([item.cache.id for item in plan.deletions], [1])
        self.assertEqual([item.id for item in plan.kept_related], [2])
        self.assertEqual(plan.deletions[0].reason, "superseded-setup-python-pip")

    def test_unrelated_live_ref_is_preserved(self) -> None:
        prefix = "setup-go-Linux-ubuntu24go-1.26.5"
        current = make_cache(
            1,
            key=f"{prefix}-{'a' * 64}",
            created="2026-08-29T03:00:00Z",
            ref="refs/heads/dev",
        )
        other_ref = make_cache(
            2,
            key=f"{prefix}-{'b' * 64}",
            created="2026-08-29T02:00:00Z",
            ref="refs/pull/999/merge",
        )
        plan = self.plan(
            [current, other_ref],
            scopes=["go"],
            open_prs={999},
        )
        self.assertEqual(plan.deletions, ())
        self.assertEqual([item.id for item in plan.kept_related], [1])

    def test_unselected_namespace_is_preserved(self) -> None:
        java_old = make_cache(
            1,
            key=f"setup-java-Linux-maven-{'a' * 64}",
            created="2026-08-29T02:00:00Z",
        )
        java_new = make_cache(
            2,
            key=f"setup-java-Linux-maven-{'b' * 64}",
            created="2026-08-29T03:00:00Z",
        )
        plan = self.plan([java_old, java_new], scopes=["quality"])
        self.assertEqual(plan.deletions, ())

    def test_maven_dependencies_and_wrapper_are_separate_lineages(self) -> None:
        entries = [
            make_cache(
                1,
                key=f"setup-java-Linux-maven-{'a' * 64}",
                created="2026-08-29T02:00:00Z",
            ),
            make_cache(
                2,
                key=f"setup-java-Linux-maven-{'b' * 64}",
                created="2026-08-29T03:00:00Z",
            ),
            make_cache(
                3,
                key=f"setup-java-Linux-maven-wrapper-{'c' * 64}",
                created="2026-08-29T02:30:00Z",
            ),
        ]
        plan = self.plan(entries, scopes=["jvm"])
        self.assertEqual([item.cache.id for item in plan.deletions], [1])
        self.assertEqual({item.id for item in plan.kept_related}, {2, 3})

    def test_same_exact_key_different_cache_versions_prunes_older(self) -> None:
        key = "setup-go-Linux-ubuntu24go-1.26.5-static"
        older = make_cache(1, key=key, created="2026-08-29T02:00:00Z")
        newer = make_cache(2, key=key, created="2026-08-29T03:00:00Z")
        plan = self.plan([older, newer], scopes=["go"])
        self.assertEqual([item.cache.id for item in plan.deletions], [1])
        self.assertEqual([item.id for item in plan.kept_related], [2])

    def test_delete_bytes_are_accounted(self) -> None:
        old = make_cache(
            1,
            key="legacy-cache",
            created="2026-08-24T00:00:00Z",
            size=3 * 1024,
        )
        current = make_cache(
            2,
            key=f"setup-go-Linux-go-1.26.5-{'d' * 64}",
            created="2026-08-29T03:00:00Z",
            size=5 * 1024,
        )
        plan = self.plan([old, current], scopes=["go"])
        self.assertEqual(plan.total_bytes, 8 * 1024)
        self.assertEqual(plan.delete_bytes, 3 * 1024)

    def test_pull_ref_parser_is_strict(self) -> None:
        self.assertEqual(
            cache_hygiene.pull_number_from_ref("refs/pull/42/merge"),
            42,
        )
        self.assertIsNone(
            cache_hygiene.pull_number_from_ref("refs/heads/pull/42/merge")
        )


class WorkflowContractTests(unittest.TestCase):
    def test_quality_apply_is_success_gated_and_pr_is_dry_run(self) -> None:
        workflow = (
            CI_DIR.parents[1] / ".github" / "workflows" / "quality.yml"
        ).read_text(encoding="utf-8")
        self.assertIn("needs.quality.result == 'success'", workflow)
        self.assertIn("github.event_name == 'pull_request'", workflow)
        self.assertIn("--scope quality --scope jvm --scope go", workflow)
        self.assertIn("--scope quality --apply", workflow)

    def test_polyglot_apply_requires_green_gate_and_non_pr_event(self) -> None:
        workflow = (
            CI_DIR.parents[1] / ".github" / "workflows" / "polyglot.yml"
        ).read_text(encoding="utf-8")
        self.assertIn("needs.gate.result == 'success'", workflow)
        self.assertIn("github.event_name != 'pull_request'", workflow)
        self.assertIn("--scope jvm --scope go --apply", workflow)


if __name__ == "__main__":
    unittest.main()
