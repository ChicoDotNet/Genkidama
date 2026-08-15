<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Infrastructure;

use DateTimeZone;
use Genkidama\Agenda\Domain\Appointment;
use Genkidama\Agenda\Domain\Schedule;
use RuntimeException;

/** Exports a schedule projection as UTF-8 CSV for local download/reporting. */
final class AppointmentCsvExporter
{
    public function __construct(private readonly DateTimeZone $timeZone)
    {
    }

    /** Returns one CSV document ordered exactly like Schedule::all(). */
    public function export(Schedule $schedule): string
    {
        $stream = fopen('php://temp', 'w+');
        if ($stream === false) {
            throw new RuntimeException('No fue posible preparar la exportación CSV.');
        }

        try {
            fputcsv($stream, ['Inicio', 'Cliente', 'Servicio', 'DuracionMinutos'], escape: '');
            foreach ($schedule->all() as $appointment) {
                $this->writeAppointment($stream, $appointment);
            }
            rewind($stream);
            $csv = stream_get_contents($stream);
            if ($csv === false) {
                throw new RuntimeException('No fue posible leer la exportación CSV.');
            }
            return $csv;
        } finally {
            fclose($stream);
        }
    }

    /** @param resource $stream */
    private function writeAppointment($stream, Appointment $appointment): void
    {
        fputcsv($stream, [
            $appointment->startsAt->setTimezone($this->timeZone)->format('Y-m-d H:i'),
            $appointment->clientName,
            $appointment->serviceName,
            (string) $appointment->durationMinutes,
        ], escape: '');
    }
}
