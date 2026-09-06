#!/usr/bin/env python3
from __future__ import annotations

import json
import os
import subprocess
import sys
import time
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
PROFILE = os.environ.get("GENKIDAMA_JVM_PROFILE", "").strip().lower()


class ContractError(RuntimeError):
    pass


def run(argv: list[str], *, cwd: Path = ROOT, env: dict[str, str] | None = None) -> None:
    print(f"$ {' '.join(argv)}", flush=True)
    completed = subprocess.run(argv, cwd=cwd, env=env, text=True, check=False)
    if completed.returncode != 0:
        raise ContractError(f"command failed with exit {completed.returncode}: {' '.join(argv)}")


def java25_contract() -> None:
    app = ROOT / "learn/es/java/app"
    run(["java", "--version"])
    run(["mvn", "--version"])
    run(["mvn", "-B", "-f", str(app / "pom.xml"), "verify"])

    log_path = Path("/tmp/helpdesk-java.log")
    env = os.environ.copy()
    env["HELPDESK_PORT"] = "18080"
    command = [
        "mvn",
        "-q",
        "-DskipTests",
        "exec:java",
        "-Dexec.mainClass=io.genkidama.learn.java.helpdesk.HelpDeskApplication",
    ]
    print(f"$ {' '.join(command)}", flush=True)
    with log_path.open("w", encoding="utf-8") as log:
        process = subprocess.Popen(command, cwd=app, env=env, stdout=log, stderr=subprocess.STDOUT, text=True)
        try:
            for _ in range(30):
                if process.poll() is not None:
                    break
                try:
                    with urllib.request.urlopen("http://127.0.0.1:18080/health", timeout=2) as response:
                        payload = json.loads(response.read().decode("utf-8"))
                    if response.status == 200 and payload.get("status") == "ok":
                        print("Java HelpDesk smoke: PASS", flush=True)
                        return
                except Exception:
                    time.sleep(1)
            if log_path.exists():
                print(log_path.read_text(encoding="utf-8"), file=sys.stderr)
            raise ContractError("Java HelpDesk process did not become healthy in time")
        finally:
            if process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait(timeout=5)


def jvm17_contract() -> None:
    run(["java", "--version"])
    run(["gradle", "--version"])

    kotlin_app = ROOT / "learn/es/kotlin/app"
    run(["gradle", "test"], cwd=kotlin_app)
    run(["gradle", "run"], cwd=kotlin_app)

    android_home = os.environ.get("ANDROID_HOME") or os.environ.get("ANDROID_SDK_ROOT")
    if not android_home:
        raise ContractError("ANDROID_HOME/ANDROID_SDK_ROOT is required for Kotlin Android proof")
    sdkmanager = Path(android_home) / "cmdline-tools/latest/bin/sdkmanager"
    if not sdkmanager.exists():
        raise ContractError(f"Android sdkmanager not found: {sdkmanager}")
    run([str(sdkmanager), "platforms;android-36", "build-tools;36.0.0"])

    kotlin_android = ROOT / "learn/es/kotlin/android"
    run(["gradle", ":app:assembleDebug", ":app:testDebugUnitTest"], cwd=kotlin_android)
    print("Kotlin JVM/Android Learn contracts: PASS", flush=True)


def main() -> int:
    if PROFILE == "java25":
        java25_contract()
    elif PROFILE == "jvm17":
        jvm17_contract()
    else:
        raise ContractError("GENKIDAMA_JVM_PROFILE must be 'java25' or 'jvm17'")
    print(f"JVM Learn contract: PASS profile={PROFILE}", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ContractError as exc:
        print(f"JVM Learn contract failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
