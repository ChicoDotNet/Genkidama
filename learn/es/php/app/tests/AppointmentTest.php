<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Tests;

use DateTimeImmutable;
use DomainException;
use Genkidama\Agenda\Domain\Appointment;
use PHPUnit\Framework\TestCase;

final class AppointmentTest extends TestCase
{
    public function testAdjacentAppointmentsDoNotOverlap(): void
    {
        $first = new Appointment('a', 'Ana', 'Consulta', new DateTimeImmutable('2026-08-20T10:00:00+00:00'), 60);
        $second = new Appointment('b', 'Beto', 'Consulta', new DateTimeImmutable('2026-08-20T11:00:00+00:00'), 30);

        self::assertFalse($first->overlaps($second));
        self::assertFalse($second->overlaps($first));
    }

    public function testPartialIntersectionOverlaps(): void
    {
        $first = new Appointment('a', 'Ana', 'Consulta', new DateTimeImmutable('2026-08-20T10:00:00+00:00'), 60);
        $second = new Appointment('b', 'Beto', 'Consulta', new DateTimeImmutable('2026-08-20T10:30:00+00:00'), 60);

        self::assertTrue($first->overlaps($second));
    }

    public function testDurationOutsideSupportedRangeIsRejected(): void
    {
        $this->expectException(DomainException::class);
        new Appointment('a', 'Ana', 'Consulta', new DateTimeImmutable('2026-08-20T10:00:00+00:00'), 5);
    }
}
