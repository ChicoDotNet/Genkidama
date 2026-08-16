<?php

declare(strict_types=1);

interface ReportBuilder
{
    public function reset(): void;
    public function addTitle(string $title): void;
    public function addSection(string $heading, string $body): void;
    public function build(): string;
}

final class TextReportBuilder implements ReportBuilder
{
    /** @var list<string> */
    private array $parts = [];

    public function reset(): void { $this->parts = []; }
    public function addTitle(string $title): void { $this->parts[] = '# ' . $title; }
    public function addSection(string $heading, string $body): void
    {
        $this->parts[] = '## ' . $heading;
        $this->parts[] = $body;
    }
    public function build(): string { return implode("\n", $this->parts); }
}

final class HtmlReportBuilder implements ReportBuilder
{
    /** @var list<string> */
    private array $parts = [];

    public function reset(): void { $this->parts = []; }
    public function addTitle(string $title): void { $this->parts[] = '<h1>' . $title . '</h1>'; }
    public function addSection(string $heading, string $body): void
    {
        $this->parts[] = '<h2>' . $heading . '</h2>';
        $this->parts[] = '<p>' . $body . '</p>';
    }
    public function build(): string { return implode('', $this->parts); }
}

function buildAvailabilityReport(ReportBuilder $builder): string
{
    $builder->reset();
    $builder->addTitle('Service status');
    $builder->addSection('Availability', '99.95%');
    return $builder->build();
}

echo buildAvailabilityReport(new TextReportBuilder()), PHP_EOL;
echo '---', PHP_EOL;
echo buildAvailabilityReport(new HtmlReportBuilder()), PHP_EOL;
