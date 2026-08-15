<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Domain;

use DateTimeImmutable;
use DomainException;

/**
 * Represents one scheduled appointment using an immutable time interval.
 */
final readonly class Appointment
{
    /**
     * @throws DomainException when any business field is invalid.
     */
    public function __construct(
        public string $id,
        public string $clientName,
        public string $serviceName,
        public DateTimeImmutable $startsAt,
        public int $durationMinutes,
    ) {
        if (trim($this->id) === '') {
            throw new DomainException('La cita necesita un identificador.');
        }

        if (trim($this->clientName) === '') {
            throw new DomainException('El nombre del cliente es obligatorio.');
        }

        if (trim($this->serviceName) === '') {
            throw new DomainException('El servicio es obligatorio.');
        }

        if ($this->durationMinutes < 15 || $this->durationMinutes > 480) {
            throw new DomainException('La duración debe estar entre 15 y 480 minutos.');
        }
    }

    /** Returns the exclusive end instant of the appointment. */
    public function endsAt(): DateTimeImmutable
    {
        return $this->startsAt->modify(sprintf('+%d minutes', $this->durationMinutes));
    }

    /** Returns true when two appointments occupy any common instant. */
    public function overlaps(self $other): bool
    {
        return $this->startsAt < $other->endsAt() && $other->startsAt < $this->endsAt();
    }

    /** @return array{id:string,clientName:string,serviceName:string,startsAt:string,durationMinutes:int} */
    public function toArray(): array
    {
        return [
            'id' => $this->id,
            'clientName' => $this->clientName,
            'serviceName' => $this->serviceName,
            'startsAt' => $this->startsAt->format(DATE_ATOM),
            'durationMinutes' => $this->durationMinutes,
        ];
    }

    /**
     * Rehydrates one appointment from persisted data.
     *
     * @param array{id:mixed,clientName:mixed,serviceName:mixed,startsAt:mixed,durationMinutes:mixed} $data
     * @throws DomainException when persisted data violates the appointment contract.
     */
    public static function fromArray(array $data): self
    {
        if (!is_string($data['id']) || !is_string($data['clientName']) || !is_string($data['serviceName'])
            || !is_string($data['startsAt']) || !is_int($data['durationMinutes'])) {
            throw new DomainException('La cita persistida tiene tipos inválidos.');
        }

        try {
            $startsAt = new DateTimeImmutable($data['startsAt']);
        } catch (\Exception $exception) {
            throw new DomainException('La cita persistida tiene una fecha inválida.', previous: $exception);
        }

        return new self(
            trim($data['id']),
            trim($data['clientName']),
            trim($data['serviceName']),
            $startsAt,
            $data['durationMinutes'],
        );
    }
}
