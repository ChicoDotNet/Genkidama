<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Tests;

use DateTimeImmutable;
use DomainException;
use Genkidama\Agenda\Domain\Appointment;
use Genkidama\Agenda\Domain\Schedule;
use PHPUnit\Framework\TestCase;

final class ScheduleTest extends TestCase
{
    public function testAppointmentsAreReturnedChronologically(): void
    {
        $schedule = new Schedule();
        $schedule->add(new Appointment('b', 'Beto', 'Seguimiento', new DateTimeImmutable('2026-08-20T12:00:00+00:00'), 30));
        $schedule->add(new Appointment('a', 'Ana', 'Consulta', new DateTimeImmutable('2026-08-20T10:00:00+00:00'), 30));

        self::assertSame(['a', 'b'], array_map(static fn (Appointment $item): string => $item->id, $schedule->all()));
    }

    public function testOverlappingAppointmentIsRejected(): void
    {
        $schedule = new Schedule([
            new Appointment('a', 'Ana', 'Consulta', new DateTimeImmutable('2026-08-20T10:00:00+00:00'), 60),
        ]);

        $this->expectException(DomainException::class);
        $this->expectExceptionMessage('se cruza');
        $schedule->add(new Appointment('b', 'Beto', 'Seguimiento', new DateTimeImmutable('2026-08-20T10:30:00+00:00'), 30));
    }

    public function testBetweenUsesHalfOpenRangeWithoutMutatingOriginal(): void
    {
        $schedule = new Schedule([
            new Appointment('a', 'Ana', 'Consulta', new DateTimeImmutable('2026-08-20T23:30:00+00:00'), 30),
            new Appointment('b', 'Beto', 'Seguimiento', new DateTimeImmutable('2026-08-21T00:00:00+00:00'), 45),
        ]);

        $filtered = $schedule->between(
            new DateTimeImmutable('2026-08-20T00:00:00+00:00'),
            new DateTimeImmutable('2026-08-21T00:00:00+00:00'),
        );

        self::assertSame(['a'], array_map(static fn (Appointment $item): string => $item->id, $filtered->all()));
        self::assertCount(2, $schedule->all());
    }

    public function testServiceFilterAndBookedMinutesAreDerivedFromProjection(): void
    {
        $schedule = new Schedule([
            new Appointment('a', 'Ana', 'Consulta inicial', new DateTimeImmutable('2026-08-20T10:00:00+00:00'), 60),
            new Appointment('b', 'Beto', 'Seguimiento', new DateTimeImmutable('2026-08-20T12:00:00+00:00'), 45),
            new Appointment('c', 'Carla', 'Consulta fiscal', new DateTimeImmutable('2026-08-20T14:00:00+00:00'), 90),
        ]);

        $filtered = $schedule->matchingService('consulta');

        self::assertSame(['a', 'c'], array_map(static fn (Appointment $item): string => $item->id, $filtered->all()));
        self::assertSame(150, $filtered->bookedMinutes());
        self::assertSame(195, $schedule->bookedMinutes());
    }

    public function testInvalidTemporalRangeIsRejected(): void
    {
        $schedule = new Schedule();

        $this->expectException(DomainException::class);
        $schedule->between(
            new DateTimeImmutable('2026-08-21T00:00:00+00:00'),
            new DateTimeImmutable('2026-08-20T00:00:00+00:00'),
        );
    }
}
