<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Application;

use DateTimeImmutable;
use DateTimeZone;
use DomainException;
use Genkidama\Agenda\Domain\Appointment;
use Genkidama\Agenda\Domain\Schedule;

/** Coordinates booking input, lifecycle changes, domain rules and persistence. */
final class AppointmentService
{
    /** @param callable():string $idGenerator */
    public function __construct(
        private readonly AppointmentStore $store,
        private readonly mixed $idGenerator,
        private readonly DateTimeZone $timeZone,
    ) {
        if (!is_callable($this->idGenerator)) {
            throw new DomainException('El generador de identificadores debe ser invocable.');
        }
    }

    /** Returns the current durable schedule. */
    public function schedule(): Schedule
    {
        return $this->store->load();
    }

    /** Books one appointment from web-friendly scalar input. */
    public function book(string $clientName, string $serviceName, string $startsAt, int $durationMinutes): Appointment
    {
        $id = ($this->idGenerator)();
        if (!is_string($id) || trim($id) === '') {
            throw new DomainException('No fue posible generar el identificador de la cita.');
        }

        $appointment = $this->appointment(trim($id), $clientName, $serviceName, $startsAt, $durationMinutes);
        $schedule = $this->store->load();
        $schedule->add($appointment);
        $this->store->save($schedule);
        return $appointment;
    }

    /** Replaces one existing appointment while preserving its identifier. */
    public function update(string $id, string $clientName, string $serviceName, string $startsAt, int $durationMinutes): Appointment
    {
        $replacement = $this->appointment(trim($id), $clientName, $serviceName, $startsAt, $durationMinutes);
        $schedule = $this->store->load();
        $candidate = $schedule->replacing($replacement);
        $this->store->save($candidate);
        return $replacement;
    }

    /** Cancels one appointment and persists the candidate schedule. */
    public function cancel(string $id): void
    {
        $schedule = $this->store->load();
        $candidate = $schedule->without(trim($id));
        $this->store->save($candidate);
    }

    /** Creates a validated appointment from form-style values. */
    private function appointment(string $id, string $clientName, string $serviceName, string $startsAt, int $durationMinutes): Appointment
    {
        $start = DateTimeImmutable::createFromFormat('!Y-m-d\\TH:i', $startsAt, $this->timeZone);
        $errors = DateTimeImmutable::getLastErrors();
        if ($start === false || ($errors !== false && ($errors['warning_count'] > 0 || $errors['error_count'] > 0))) {
            throw new DomainException('La fecha y hora no tienen un formato válido.');
        }

        return new Appointment(
            trim($id),
            trim($clientName),
            trim($serviceName),
            $start,
            $durationMinutes,
        );
    }
}
