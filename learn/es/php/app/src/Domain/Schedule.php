<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Domain;

use DateTimeImmutable;
use DomainException;

/** Holds the business rules and deterministic read projections for a single-resource appointment calendar. */
final class Schedule
{
    /** @var list<Appointment> */
    private array $appointments;

    /** @param list<Appointment> $appointments */
    public function __construct(array $appointments = [])
    {
        $this->appointments = [];
        foreach ($appointments as $appointment) {
            $this->add($appointment);
        }
    }

    /** Adds an appointment when its identifier is unique and its interval is free. */
    public function add(Appointment $appointment): void
    {
        foreach ($this->appointments as $existing) {
            if ($existing->id === $appointment->id) {
                throw new DomainException('Ya existe una cita con ese identificador.');
            }
            if ($existing->overlaps($appointment)) {
                throw new DomainException('El horario se cruza con una cita existente.');
            }
        }
        $this->appointments[] = $appointment;
    }

    /** Finds one appointment by identifier. */
    public function find(string $id): Appointment
    {
        foreach ($this->appointments as $appointment) {
            if ($appointment->id === $id) {
                return $appointment;
            }
        }
        throw new DomainException('La cita solicitada no existe.');
    }

    /** Returns a new schedule without the requested appointment. */
    public function without(string $id): self
    {
        $this->find($id);
        return new self(array_values(array_filter(
            $this->appointments,
            static fn (Appointment $appointment): bool => $appointment->id !== $id,
        )));
    }

    /** Returns a new schedule with one appointment replaced and all conflict rules revalidated. */
    public function replacing(Appointment $replacement): self
    {
        $candidate = $this->without($replacement->id);
        $candidate->add($replacement);
        return $candidate;
    }

    /** Returns appointments whose start instant falls inside [from, until). */
    public function between(DateTimeImmutable $fromInclusive, DateTimeImmutable $untilExclusive): self
    {
        if ($untilExclusive <= $fromInclusive) {
            throw new DomainException('El rango de consulta debe terminar después de comenzar.');
        }

        return new self(array_values(array_filter(
            $this->appointments,
            static fn (Appointment $appointment): bool =>
                $appointment->startsAt >= $fromInclusive && $appointment->startsAt < $untilExclusive,
        )));
    }

    /** Returns appointments whose service contains the requested text, ignoring ASCII case. */
    public function matchingService(string $query): self
    {
        $query = trim($query);
        if ($query === '') {
            return new self($this->appointments);
        }

        return new self(array_values(array_filter(
            $this->appointments,
            static fn (Appointment $appointment): bool => stripos($appointment->serviceName, $query) !== false,
        )));
    }

    /** Returns the sum of booked minutes in this schedule projection. */
    public function bookedMinutes(): int
    {
        return array_sum(array_map(
            static fn (Appointment $appointment): int => $appointment->durationMinutes,
            $this->appointments,
        ));
    }

    /** @return list<Appointment> Appointments sorted chronologically, then by identifier. */
    public function all(): array
    {
        $appointments = $this->appointments;
        usort(
            $appointments,
            static fn (Appointment $left, Appointment $right): int =>
                [$left->startsAt->getTimestamp(), $left->id] <=> [$right->startsAt->getTimestamp(), $right->id],
        );
        return $appointments;
    }
}
