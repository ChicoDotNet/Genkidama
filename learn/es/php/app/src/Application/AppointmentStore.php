<?php

declare(strict_types=1);

namespace Genkidama\Agenda\Application;

use Genkidama\Agenda\Domain\Schedule;

/** Persistence boundary used by appointment application services. */
interface AppointmentStore
{
    /** Loads the current durable schedule. */
    public function load(): Schedule;

    /** Persists the complete candidate schedule or throws without reporting success. */
    public function save(Schedule $schedule): void;
}
