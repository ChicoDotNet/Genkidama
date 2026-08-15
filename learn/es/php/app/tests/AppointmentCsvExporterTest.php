<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Tests;

use DateTimeImmutable;
use DateTimeZone;
use Genkidama\Agenda\Domain\Appointment;
use Genkidama\Agenda\Domain\Schedule;
use Genkidama\Agenda\Infrastructure\AppointmentCsvExporter;
use PHPUnit\Framework\TestCase;

final class AppointmentCsvExporterTest extends TestCase
{
    public function testExportsChronologicalProjectionUsingConfiguredTimezone(): void
    {
        $schedule = new Schedule([
            new Appointment('b', 'Beto', 'Seguimiento', new DateTimeImmutable('2026-08-20T18:00:00+00:00'), 45),
            new Appointment('a', 'Ana', 'Consulta, fiscal', new DateTimeImmutable('2026-08-20T16:00:00+00:00'), 60),
        ]);
        $exporter = new AppointmentCsvExporter(new DateTimeZone('America/Mexico_City'));

        $csv = $exporter->export($schedule);

        $lines = preg_split('/\R/', trim($csv));
        self::assertIsArray($lines);
        self::assertSame('Inicio,Cliente,Servicio,DuracionMinutos', $lines[0]);
        self::assertSame('"2026-08-20 10:00",Ana,"Consulta, fiscal",60', $lines[1]);
        self::assertSame('"2026-08-20 12:00",Beto,Seguimiento,45', $lines[2]);
    }
}
