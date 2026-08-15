<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Tests;

use DateTimeZone;
use DomainException;
use Genkidama\Agenda\Application\AppointmentService;
use Genkidama\Agenda\Application\AppointmentStore;
use Genkidama\Agenda\Domain\Schedule;
use PHPUnit\Framework\TestCase;

final class AppointmentServiceTest extends TestCase
{
    public function testBookingPersistsCandidateSchedule(): void
    {
        $store = new MemoryStore();
        $service = new AppointmentService($store, static fn (): string => 'apt-1', new DateTimeZone('UTC'));

        $created = $service->book(' Ana ', ' Consulta ', '2026-08-20T10:00', 60);

        self::assertSame('apt-1', $created->id);
        self::assertSame('Ana', $created->clientName);
        self::assertCount(1, $store->load()->all());
    }

    public function testInvalidWebDateIsRejectedBeforePersistence(): void
    {
        $store = new MemoryStore();
        $service = new AppointmentService($store, static fn (): string => 'apt-1', new DateTimeZone('UTC'));

        try {
            $service->book('Ana', 'Consulta', 'no-es-fecha', 60);
            self::fail('La entrada inválida debía ser rechazada.');
        } catch (DomainException) {
            self::assertCount(0, $store->load()->all());
        }
    }

    public function testUpdatePreservesIdentifierAndRevalidatesConflicts(): void
    {
        $store = new MemoryStore();
        $ids = ['apt-1', 'apt-2'];
        $service = new AppointmentService($store, static function () use (&$ids): string {
            return array_shift($ids) ?? 'unexpected';
        }, new DateTimeZone('UTC'));

        $service->book('Ana', 'Consulta', '2026-08-20T10:00', 60);
        $service->book('Luis', 'Seguimiento', '2026-08-20T12:00', 60);
        $updated = $service->update('apt-1', 'Ana', 'Consulta extendida', '2026-08-20T11:00', 60);

        self::assertSame('apt-1', $updated->id);
        self::assertSame('Consulta extendida', $store->load()->find('apt-1')->serviceName);

        $before = $store->load()->all();
        try {
            $service->update('apt-1', 'Ana', 'Cruce', '2026-08-20T12:30', 60);
            self::fail('La edición conflictiva debía ser rechazada.');
        } catch (DomainException) {
            self::assertEquals($before, $store->load()->all());
        }
    }

    public function testCancelRemovesOnlyRequestedAppointment(): void
    {
        $store = new MemoryStore();
        $ids = ['apt-1', 'apt-2'];
        $service = new AppointmentService($store, static function () use (&$ids): string {
            return array_shift($ids) ?? 'unexpected';
        }, new DateTimeZone('UTC'));

        $service->book('Ana', 'Consulta', '2026-08-20T10:00', 60);
        $service->book('Luis', 'Seguimiento', '2026-08-20T12:00', 60);
        $service->cancel('apt-1');

        self::assertSame(['apt-2'], array_map(static fn ($appointment): string => $appointment->id, $store->load()->all()));
    }
}

final class MemoryStore implements AppointmentStore
{
    private Schedule $schedule;

    public function __construct()
    {
        $this->schedule = new Schedule();
    }

    public function load(): Schedule
    {
        return new Schedule($this->schedule->all());
    }

    public function save(Schedule $schedule): void
    {
        $this->schedule = new Schedule($schedule->all());
    }
}
