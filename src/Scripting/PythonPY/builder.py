from __future__ import annotations

from dataclasses import dataclass, field
from typing import Protocol


class ReportBuilder(Protocol):
    def reset(self) -> None: ...
    def add_title(self, title: str) -> None: ...
    def add_section(self, heading: str, body: str) -> None: ...
    def build(self) -> str: ...


@dataclass
class TextReportBuilder:
    parts: list[str] = field(default_factory=list)

    def reset(self) -> None:
        self.parts.clear()

    def add_title(self, title: str) -> None:
        self.parts.append(f"# {title}")

    def add_section(self, heading: str, body: str) -> None:
        self.parts.extend((f"## {heading}", body))

    def build(self) -> str:
        return "\n".join(self.parts)


@dataclass
class HtmlReportBuilder:
    parts: list[str] = field(default_factory=list)

    def reset(self) -> None:
        self.parts.clear()

    def add_title(self, title: str) -> None:
        self.parts.append(f"<h1>{title}</h1>")

    def add_section(self, heading: str, body: str) -> None:
        self.parts.extend((f"<h2>{heading}</h2>", f"<p>{body}</p>"))

    def build(self) -> str:
        return "".join(self.parts)


def build_availability_report(builder: ReportBuilder) -> str:
    builder.reset()
    builder.add_title("Service status")
    builder.add_section("Availability", "99.95%")
    return builder.build()


if __name__ == "__main__":
    print(build_availability_report(TextReportBuilder()))
    print("---")
    print(build_availability_report(HtmlReportBuilder()))
