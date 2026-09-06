#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable

API_VERSION = "2026-03-10"
DEFAULT_LEGACY_CUTOFF = "2026-08-29T00:54:29Z"
PAGE_SIZE = 100
MAX_LISTED_DELETIONS = 50
SHA256_SUFFIX = re.compile(r"^(?P<prefix>.+)-(?P<hash>[0-9a-fA-F]{64})$")
PULL_REF = re.compile(r"^refs/pull/(?P<number>[1-9][0-9]*)/merge$")

SCOPE_NAMESPACES: dict[str, frozenset[str]] = {
    "quality": frozenset({"setup-python-pip"}),
    "jvm": frozenset({"setup-java-maven", "setup-java-maven-wrapper"}),
    "go": frozenset({"setup-go"}),
}


class CacheHygieneError(RuntimeError):
    pass


@dataclass(frozen=True)
class CacheEntry:
    id: int
    ref: str
    key: str
    version: str
    created_at: datetime
    last_accessed_at: datetime
    size_in_bytes: int


@dataclass(frozen=True)
class Deletion:
    cache: CacheEntry
    reason: str


@dataclass(frozen=True)
class Plan:
    scanned: int
    total_bytes: int
    deletions: tuple[Deletion, ...]
    kept_related: tuple[CacheEntry, ...]

    @property
    def delete_bytes(self) -> int:
        return sum(item.cache.size_in_bytes for item in self.deletions)


def parse_timestamp(value: str) -> datetime:
    normalized = value[:-1] + "+00:00" if value.endswith("Z") else value
    parsed = datetime.fromisoformat(normalized)
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    return parsed.astimezone(timezone.utc)


def cache_from_payload(payload: dict[str, object]) -> CacheEntry:
    return CacheEntry(
        id=int(payload["id"]),
        ref=str(payload["ref"]),
        key=str(payload["key"]),
        version=str(payload.get("version", "")),
        created_at=parse_timestamp(str(payload["created_at"])),
        last_accessed_at=parse_timestamp(str(payload["last_accessed_at"])),
        size_in_bytes=int(payload.get("size_in_bytes", 0)),
    )


def namespace_for_key(key: str) -> str | None:
    if key.startswith("setup-python-") and "-pip-" in key:
        return "setup-python-pip"
    if key.startswith("setup-java-") and "-maven-wrapper-" in key:
        return "setup-java-maven-wrapper"
    if key.startswith("setup-java-") and "-maven-" in key:
        return "setup-java-maven"
    if key.startswith("setup-go-"):
        return "setup-go"
    return None


def lineage_for_key(key: str) -> str:
    match = SHA256_SUFFIX.match(key)
    return match.group("prefix") if match else key


def pull_number_from_ref(ref: str) -> int | None:
    match = PULL_REF.match(ref)
    return int(match.group("number")) if match else None


def namespaces_for_scopes(scopes: Iterable[str]) -> frozenset[str]:
    namespaces: set[str] = set()
    for scope in scopes:
        try:
            namespaces.update(SCOPE_NAMESPACES[scope])
        except KeyError as exc:
            raise CacheHygieneError(f"Unknown cache scope: {scope}") from exc
    return frozenset(namespaces)


def plan_deletions(
    caches: Iterable[CacheEntry],
    *,
    current_ref: str,
    scopes: Iterable[str],
    legacy_cutoff: datetime,
    open_pull_requests: set[int],
) -> Plan:
    cache_list = list(caches)
    selected_namespaces = namespaces_for_scopes(scopes)

    deletions: dict[int, Deletion] = {}
    total_bytes = sum(item.size_in_bytes for item in cache_list)

    # I1 is the clean-slate CI boundary. Anything older belongs to the retired
    # workflow mesh and is intentionally not a supported fallback.
    for item in cache_list:
        if item.created_at < legacy_cutoff:
            deletions[item.id] = Deletion(item, "obsolete-pre-I1")
            continue

        # PR caches live under refs/pull/<n>/merge and GitHub documents that
        # they can only be restored by re-runs of that PR. Once the PR closes,
        # the cache is dead weight and safe to reclaim.
        pull_number = pull_number_from_ref(item.ref)
        if pull_number is not None and pull_number not in open_pull_requests:
            deletions[item.id] = Deletion(item, "obsolete-closed-pr")

    related_groups: dict[tuple[str, str], list[CacheEntry]] = {}
    for item in cache_list:
        if item.id in deletions:
            continue
        if item.ref != current_ref:
            continue
        namespace = namespace_for_key(item.key)
        if namespace not in selected_namespaces:
            continue
        related_groups.setdefault((namespace, lineage_for_key(item.key)), []).append(item)

    kept: list[CacheEntry] = []
    for (namespace, _lineage), items in related_groups.items():
        ordered = sorted(
            items,
            key=lambda item: (item.created_at, item.last_accessed_at, item.id),
            reverse=True,
        )
        kept.append(ordered[0])
        for older in ordered[1:]:
            deletions[older.id] = Deletion(older, f"superseded-{namespace}")

    ordered_deletions = tuple(
        sorted(
            deletions.values(),
            key=lambda item: (item.cache.created_at, item.cache.id),
        )
    )
    return Plan(
        scanned=len(cache_list),
        total_bytes=total_bytes,
        deletions=ordered_deletions,
        kept_related=tuple(sorted(kept, key=lambda item: item.key)),
    )


def gib(value: int) -> str:
    return f"{value / (1024 ** 3):.2f} GiB"


class GitHubCacheClient:
    def __init__(self, *, repository: str, token: str, api_url: str) -> None:
        self.repository = repository
        self.token = token
        self.api_url = api_url.rstrip("/")

    def _request(self, path: str, *, method: str = "GET") -> tuple[int, bytes]:
        request = urllib.request.Request(
            f"{self.api_url}{path}",
            method=method,
            headers={
                "Accept": "application/vnd.github+json",
                "Authorization": f"Bearer {self.token}",
                "X-GitHub-Api-Version": API_VERSION,
                "User-Agent": "genkidama-cache-hygiene",
            },
        )
        try:
            with urllib.request.urlopen(request, timeout=60) as response:
                return response.status, response.read()
        except urllib.error.HTTPError as exc:
            body = exc.read().decode("utf-8", errors="replace")
            if method == "DELETE" and exc.code == 404:
                # Idempotent under concurrent cleanup jobs or GitHub eviction.
                return exc.code, body.encode("utf-8")
            raise CacheHygieneError(
                f"GitHub API {method} {path} failed with HTTP {exc.code}: {body}"
            ) from exc
        except urllib.error.URLError as exc:
            raise CacheHygieneError(f"GitHub API {method} {path} failed: {exc}") from exc

    def _list_paginated(self, path: str) -> list[dict[str, object]]:
        items: list[dict[str, object]] = []
        page = 1
        while True:
            separator = "&" if "?" in path else "?"
            status, body = self._request(
                f"{path}{separator}per_page={PAGE_SIZE}&page={page}"
            )
            if status != 200:
                raise CacheHygieneError(f"Unexpected list status for {path}: {status}")
            payload = json.loads(body.decode("utf-8"))
            if isinstance(payload, list):
                batch = payload
            else:
                batch = payload.get("actions_caches")
            if not isinstance(batch, list):
                raise CacheHygieneError(f"GitHub list response has unexpected shape: {path}")
            items.extend(batch)
            if len(batch) < PAGE_SIZE:
                return items
            page += 1

    def list_caches(self) -> list[CacheEntry]:
        encoded = urllib.parse.urlencode(
            {
                "sort": "created_at",
                "direction": "desc",
            }
        )
        payloads = self._list_paginated(
            f"/repos/{self.repository}/actions/caches?{encoded}"
        )
        return [cache_from_payload(item) for item in payloads]

    def list_open_pull_requests(self) -> set[int]:
        payloads = self._list_paginated(
            f"/repos/{self.repository}/pulls?state=open"
        )
        try:
            return {int(item["number"]) for item in payloads}
        except (KeyError, TypeError, ValueError) as exc:
            raise CacheHygieneError("Pull-request list response is missing number") from exc

    def delete_cache(self, cache_id: int) -> None:
        status, _body = self._request(
            f"/repos/{self.repository}/actions/caches/{cache_id}",
            method="DELETE",
        )
        if status not in (204, 404):
            raise CacheHygieneError(
                f"Unexpected cache-delete status for cache {cache_id}: {status}"
            )


def write_summary(
    plan: Plan,
    *,
    apply: bool,
    deleted_count: int,
    deleted_bytes: int,
    open_pr_count: int,
) -> None:
    summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
    if not summary_path:
        return
    mode = "apply" if apply else "dry-run"
    lines = [
        "### Cache hygiene",
        "",
        f"- mode: `{mode}`",
        f"- scanned: **{plan.scanned}** caches / **{gib(plan.total_bytes)}**",
        f"- open PRs considered live: **{open_pr_count}**",
        f"- planned deletion: **{len(plan.deletions)}** caches / **{gib(plan.delete_bytes)}**",
        f"- deleted: **{deleted_count}** caches / **{gib(deleted_bytes)}**",
        f"- current related lineages kept: **{len(plan.kept_related)}**",
        "",
    ]
    with Path(summary_path).open("a", encoding="utf-8") as handle:
        handle.write("\n".join(lines))


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Prune Genkidama GitHub Actions caches after a successful CI gate. "
            "The workflow must invoke --apply only after the owning gate passes."
        )
    )
    parser.add_argument(
        "--scope",
        action="append",
        default=[],
        choices=sorted(SCOPE_NAMESPACES),
        help="Current CI cache namespace to prune; may be supplied more than once.",
    )
    parser.add_argument(
        "--ref",
        default=os.environ.get("GITHUB_REF", ""),
        help="Git ref whose related current caches may be pruned.",
    )
    parser.add_argument(
        "--repo",
        default=os.environ.get("GITHUB_REPOSITORY", ""),
        help="GitHub repository in owner/name form.",
    )
    parser.add_argument(
        "--legacy-cutoff",
        default=DEFAULT_LEGACY_CUTOFF,
        help="Caches created before this UTC timestamp are obsolete clean-slate debt.",
    )
    parser.add_argument(
        "--apply",
        action="store_true",
        help="Delete planned cache IDs. Without this flag the command is dry-run.",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    if not args.repo or "/" not in args.repo:
        raise CacheHygieneError("--repo or GITHUB_REPOSITORY must be owner/name")
    if not args.ref.startswith("refs/"):
        raise CacheHygieneError("--ref or GITHUB_REF must be a full refs/... name")

    token = os.environ.get("GITHUB_TOKEN")
    if not token:
        raise CacheHygieneError("GITHUB_TOKEN is required to inspect Actions caches")

    client = GitHubCacheClient(
        repository=args.repo,
        token=token,
        api_url=os.environ.get("GITHUB_API_URL", "https://api.github.com"),
    )
    caches = client.list_caches()
    open_pull_requests = client.list_open_pull_requests()
    plan = plan_deletions(
        caches,
        current_ref=args.ref,
        scopes=args.scope,
        legacy_cutoff=parse_timestamp(args.legacy_cutoff),
        open_pull_requests=open_pull_requests,
    )

    print(
        "CACHE_HYGIENE "
        f"mode={'apply' if args.apply else 'dry-run'} "
        f"scanned={plan.scanned} total={gib(plan.total_bytes)} "
        f"planned={len(plan.deletions)} reclaim={gib(plan.delete_bytes)} "
        f"open_prs={len(open_pull_requests)} ref={args.ref} "
        f"scopes={','.join(args.scope) or '-'}"
    )
    for deletion in plan.deletions[:MAX_LISTED_DELETIONS]:
        item = deletion.cache
        print(
            "DELETE "
            f"id={item.id} reason={deletion.reason} size={gib(item.size_in_bytes)} "
            f"created={item.created_at.isoformat()} ref={item.ref} key={item.key}"
        )
    if len(plan.deletions) > MAX_LISTED_DELETIONS:
        print(
            f"DELETE ... {len(plan.deletions) - MAX_LISTED_DELETIONS} "
            "additional planned deletions omitted from log"
        )

    deleted_count = 0
    deleted_bytes = 0
    if args.apply:
        for deletion in plan.deletions:
            client.delete_cache(deletion.cache.id)
            deleted_count += 1
            deleted_bytes += deletion.cache.size_in_bytes
        print(
            f"CACHE_HYGIENE_APPLIED deleted={deleted_count} reclaimed={gib(deleted_bytes)}"
        )
    else:
        print("CACHE_HYGIENE_DRY_RUN no cache entries were deleted")

    write_summary(
        plan,
        apply=args.apply,
        deleted_count=deleted_count,
        deleted_bytes=deleted_bytes,
        open_pr_count=len(open_pull_requests),
    )
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except CacheHygieneError as exc:
        print(f"cache-hygiene: {exc}", file=sys.stderr)
        raise SystemExit(2)
