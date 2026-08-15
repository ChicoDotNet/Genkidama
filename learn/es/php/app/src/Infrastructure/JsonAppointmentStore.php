<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Infrastructure;

use Genkidama\Agenda\Application\AppointmentStore;
use Genkidama\Agenda\Domain\Appointment;
use Genkidama\Agenda\Domain\Schedule;
use JsonException;
use RuntimeException;

/** Stores the complete appointment schedule in one local JSON document. */
final readonly class JsonAppointmentStore implements AppointmentStore
{
    public function __construct(private string $path)
    {
    }

    /** @throws RuntimeException when existing state cannot be read or validated. */
    public function load(): Schedule
    {
        if (!is_file($this->path)) {
            return new Schedule();
        }

        $json = file_get_contents($this->path);
        if ($json === false) {
            throw new RuntimeException('No fue posible leer el archivo de citas.');
        }

        try {
            $decoded = json_decode($json, true, flags: JSON_THROW_ON_ERROR);
        } catch (JsonException $exception) {
            throw new RuntimeException('El archivo de citas contiene JSON inválido.', previous: $exception);
        }

        if (!is_array($decoded) || !array_is_list($decoded)) {
            throw new RuntimeException('El archivo de citas debe contener una lista JSON.');
        }

        $appointments = [];
        foreach ($decoded as $item) {
            if (!is_array($item)) {
                throw new RuntimeException('Una cita persistida no es un objeto válido.');
            }

            /** @var array{id:mixed,clientName:mixed,serviceName:mixed,startsAt:mixed,durationMinutes:mixed} $item */
            $appointments[] = Appointment::fromArray($item);
        }

        return new Schedule($appointments);
    }

    /** @throws RuntimeException when the candidate schedule cannot be written atomically. */
    public function save(Schedule $schedule): void
    {
        $directory = dirname($this->path);
        if (!is_dir($directory) && !mkdir($directory, 0775, true) && !is_dir($directory)) {
            throw new RuntimeException('No fue posible crear el directorio de datos.');
        }

        try {
            $json = json_encode(
                array_map(static fn (Appointment $appointment): array => $appointment->toArray(), $schedule->all()),
                JSON_PRETTY_PRINT | JSON_UNESCAPED_UNICODE | JSON_THROW_ON_ERROR,
            );
        } catch (JsonException $exception) {
            throw new RuntimeException('No fue posible serializar las citas.', previous: $exception);
        }

        $temporaryPath = $this->path . '.tmp';
        if (file_put_contents($temporaryPath, $json . PHP_EOL, LOCK_EX) === false) {
            throw new RuntimeException('No fue posible escribir el archivo temporal de citas.');
        }

        if (!rename($temporaryPath, $this->path)) {
            @unlink($temporaryPath);
            throw new RuntimeException('No fue posible publicar el nuevo estado de citas.');
        }
    }
}
