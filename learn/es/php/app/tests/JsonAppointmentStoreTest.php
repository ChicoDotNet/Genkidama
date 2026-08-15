<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Tests;

use DateTimeImmutable;
use Genkidama\Agenda\Domain\Appointment;
use Genkidama\Agenda\Domain\Schedule;
use Genkidama\Agenda\Infrastructure\JsonAppointmentStore;
use PHPUnit\Framework\TestCase;
use RuntimeException;

final class JsonAppointmentStoreTest extends TestCase
{
    private string $directory;

    protected function setUp(): void
    {
        $this->directory = sys_get_temp_dir() . '/agenda-php-' . bin2hex(random_bytes(6));
        mkdir($this->directory, 0775, true);
    }

    protected function tearDown(): void
    {
        foreach (glob($this->directory . '/*') ?: [] as $file) {
            @unlink($file);
        }
        @rmdir($this->directory);
    }

    public function testScheduleRoundTripsThroughJson(): void
    {
        $store = new JsonAppointmentStore($this->directory . '/appointments.json');
        $schedule = new Schedule([
            new Appointment('a', 'Ana', 'Consulta', new DateTimeImmutable('2026-08-20T10:00:00+00:00'), 60),
        ]);

        $store->save($schedule);
        $loaded = $store->load()->all();

        self::assertCount(1, $loaded);
        self::assertSame('Ana', $loaded[0]->clientName);
    }

    public function testCorruptJsonIsNotSilentlyIgnored(): void
    {
        $path = $this->directory . '/appointments.json';
        file_put_contents($path, '{broken');
        $store = new JsonAppointmentStore($path);

        $this->expectException(RuntimeException::class);
        $this->expectExceptionMessage('JSON inválido');
        $store->load();
    }
}
