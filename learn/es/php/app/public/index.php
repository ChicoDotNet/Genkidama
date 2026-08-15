<?php

declare(strict_types=1);

use Genkidama\Agenda\Application\AppointmentService;
use Genkidama\Agenda\Infrastructure\AppointmentCsvExporter;
use Genkidama\Agenda\Infrastructure\JsonAppointmentStore;

require dirname(__DIR__) . '/vendor/autoload.php';

header('X-Content-Type-Options: nosniff');
header('Referrer-Policy: no-referrer');
header("Content-Security-Policy: default-src 'self'; style-src 'self' 'unsafe-inline'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'");
header('Cache-Control: no-store');

ini_set('session.use_strict_mode', '1');
session_set_cookie_params([
    'httponly' => true,
    'secure' => isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] !== 'off',
    'samesite' => 'Lax',
]);
session_start();
if (!isset($_SESSION['csrf_token']) || !is_string($_SESSION['csrf_token'])) {
    $_SESSION['csrf_token'] = bin2hex(random_bytes(32));
}
$csrfToken = $_SESSION['csrf_token'];

$timeZoneName = getenv('AGENDA_TIMEZONE') ?: 'UTC';
try {
    $timeZone = new DateTimeZone($timeZoneName);
} catch (Exception) {
    http_response_code(500);
    echo 'AGENDA_TIMEZONE no contiene una zona horaria válida.';
    exit;
}

$dataFile = getenv('AGENDA_DATA_FILE') ?: dirname(__DIR__) . '/data/appointments.json';
$service = new AppointmentService(
    new JsonAppointmentStore($dataFile),
    static fn (): string => bin2hex(random_bytes(8)),
    $timeZone,
);

$values = [
    'id' => trim((string) ($_POST['id'] ?? '')),
    'clientName' => trim((string) ($_POST['clientName'] ?? '')),
    'serviceName' => trim((string) ($_POST['serviceName'] ?? '')),
    'startsAt' => trim((string) ($_POST['startsAt'] ?? '')),
    'durationMinutes' => trim((string) ($_POST['durationMinutes'] ?? '60')),
];
$filterDate = trim((string) ($_GET['date'] ?? ''));
$filterService = trim((string) ($_GET['service'] ?? ''));
$error = null;
$status = isset($_GET['created']) ? 'Cita registrada correctamente.' : (isset($_GET['updated']) ? 'Cita actualizada correctamente.' : (isset($_GET['cancelled']) ? 'Cita cancelada correctamente.' : null));
$editing = false;

if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $contentLength = (int) ($_SERVER['CONTENT_LENGTH'] ?? 0);
    $contentType = strtolower(trim(explode(';', (string) ($_SERVER['CONTENT_TYPE'] ?? ''))[0]));
    $submittedCsrfToken = (string) ($_POST['csrfToken'] ?? '');

    if ($contentLength > 16384) {
        http_response_code(413);
        $error = 'La solicitud es demasiado grande.';
    } elseif ($contentType !== 'application/x-www-form-urlencoded') {
        http_response_code(415);
        $error = 'AgendaPHP sólo acepta formularios application/x-www-form-urlencoded para modificar citas.';
    } elseif ($submittedCsrfToken === '' || !hash_equals($csrfToken, $submittedCsrfToken)) {
        http_response_code(403);
        $error = 'La solicitud no pudo verificarse. Recarga la página e inténtalo de nuevo.';
    } else {
        $action = (string) ($_POST['action'] ?? 'book');
        try {
            if ($action === 'cancel') {
                $service->cancel($values['id']);
                header('Location: /?cancelled=1', true, 303);
                exit;
            }

            if (!ctype_digit($values['durationMinutes'])) {
                throw new DomainException('La duración debe ser un número entero de minutos.');
            }

            if ($action === 'update') {
                $service->update($values['id'], $values['clientName'], $values['serviceName'], $values['startsAt'], (int) $values['durationMinutes']);
                header('Location: /?updated=1', true, 303);
                exit;
            }

            $service->book($values['clientName'], $values['serviceName'], $values['startsAt'], (int) $values['durationMinutes']);
            header('Location: /?created=1', true, 303);
            exit;
        } catch (DomainException $exception) {
            http_response_code(422);
            $error = $exception->getMessage();
            $editing = $action === 'update';
        } catch (RuntimeException) {
            http_response_code(503);
            $error = 'No fue posible guardar la cita. Revisa el almacenamiento local e inténtalo de nuevo.';
        }
    }
}

$schedule = null;
$appointments = [];
$bookedMinutes = 0;
try {
    $schedule = $service->schedule();
    $visibleSchedule = $schedule;

    if ($filterDate !== '') {
        $dayStart = parseLocalDate($filterDate, $timeZone);
        $visibleSchedule = $visibleSchedule->between($dayStart, $dayStart->modify('+1 day'));
    }
    if ($filterService !== '') {
        $visibleSchedule = $visibleSchedule->matchingService($filterService);
    }

    $appointments = $visibleSchedule->all();
    $bookedMinutes = $visibleSchedule->bookedMinutes();

    if ($_SERVER['REQUEST_METHOD'] === 'GET' && isset($_GET['export'])) {
        $csv = (new AppointmentCsvExporter($timeZone))->export($visibleSchedule);
        header('Content-Type: text/csv; charset=UTF-8');
        header('Content-Disposition: attachment; filename="agenda-php.csv"');
        echo $csv;
        exit;
    }
} catch (DomainException $exception) {
    http_response_code(422);
    $error = $exception->getMessage();
} catch (RuntimeException) {
    http_response_code(503);
    $error = 'No fue posible leer las citas guardadas. Revisa el archivo de datos antes de continuar.';
}

if ($schedule !== null && $_SERVER['REQUEST_METHOD'] === 'GET' && isset($_GET['edit'])) {
    try {
        $appointment = $schedule->find(trim((string) $_GET['edit']));
        $editing = true;
        $values = [
            'id' => $appointment->id,
            'clientName' => $appointment->clientName,
            'serviceName' => $appointment->serviceName,
            'startsAt' => $appointment->startsAt->setTimezone($timeZone)->format('Y-m-d\\TH:i'),
            'durationMinutes' => (string) $appointment->durationMinutes,
        ];
    } catch (DomainException $exception) {
        http_response_code(404);
        $error = $exception->getMessage();
    }
}

/** Parses one local YYYY-MM-DD value as midnight in the configured agenda timezone. */
function parseLocalDate(string $value, DateTimeZone $timeZone): DateTimeImmutable
{
    $date = DateTimeImmutable::createFromFormat('!Y-m-d', $value, $timeZone);
    $errors = DateTimeImmutable::getLastErrors();
    if ($date === false || ($errors !== false && ($errors['warning_count'] > 0 || $errors['error_count'] > 0))
        || $date->format('Y-m-d') !== $value) {
        throw new DomainException('La fecha de consulta debe usar el formato YYYY-MM-DD.');
    }
    return $date;
}

/** Escapes one value for safe HTML text/attribute output. */
function e(string $value): string
{
    return htmlspecialchars($value, ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8');
}

$exportQuery = http_build_query(array_filter([
    'date' => $filterDate,
    'service' => $filterService,
    'export' => 'csv',
], static fn (string $value): bool => $value !== ''));
?>
<!doctype html>
<html lang="es">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>AgendaPHP — Agenda local de citas</title>
  <style>
    :root { color-scheme: light; font-family: system-ui, sans-serif; line-height: 1.5; }
    body { margin: 0; background: #f5f5f5; color: #1f1f1f; }
    main { width: min(70rem, calc(100% - 2rem)); margin: 2rem auto; display: grid; gap: 1.5rem; }
    header, section { background: white; border: 1px solid #d6d6d6; border-radius: .75rem; padding: 1.25rem; }
    h1, h2 { margin-top: 0; }
    form { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1rem; }
    .inline { display: inline; }
    label { display: grid; gap: .35rem; font-weight: 650; }
    input, select, button, .button-link { font: inherit; padding: .7rem .8rem; border-radius: .4rem; border: 1px solid #8a8a8a; }
    button, .button-link { border: 0; background: #005fb8; color: white; font-weight: 700; cursor: pointer; text-decoration: none; display: inline-block; }
    .secondary { background: #505050; }
    .danger { background: #a4262c; }
    :focus-visible { outline: 3px solid #005fb8; outline-offset: 2px; }
    .full { grid-column: 1 / -1; }
    .actions { display: flex; gap: .5rem; flex-wrap: wrap; align-items: center; }
    .message { padding: .8rem; border-radius: .4rem; }
    .error { background: #fde7e9; border: 1px solid #c42b1c; }
    .success { background: #dff6dd; border: 1px solid #0f7b0f; }
    .summary { display: flex; gap: 1rem; flex-wrap: wrap; color: #424242; }
    table { width: 100%; border-collapse: collapse; }
    th, td { text-align: left; padding: .65rem; border-bottom: 1px solid #e5e5e5; vertical-align: top; }
    th { font-weight: 700; }
    .empty { color: #616161; }
    @media (max-width: 42rem) {
      form { grid-template-columns: 1fr; }
      .full { grid-column: auto; }
      table, thead, tbody, tr, th, td { display: block; }
      thead { position: absolute; inline-size: 1px; block-size: 1px; overflow: hidden; clip: rect(0 0 0 0); }
      tr { padding: .65rem 0; border-bottom: 1px solid #e5e5e5; }
      td { border: 0; padding: .2rem 0; }
      td::before { content: attr(data-label) ': '; font-weight: 700; }
    }
  </style>
</head>
<body>
<main>
  <header>
    <h1>AgendaPHP</h1>
    <p>Agenda local para registrar, editar, consultar y cancelar citas sin cruces de horario. Zona horaria: <strong><?= e($timeZone->getName()) ?></strong>.</p>
  </header>

  <section aria-labelledby="appointment-form-title">
    <h2 id="appointment-form-title"><?= $editing ? 'Editar cita' : 'Nueva cita' ?></h2>
    <?php if ($error !== null): ?>
      <p id="form-error" class="message error" role="alert"><?= e($error) ?></p>
    <?php elseif ($status !== null): ?>
      <p class="message success" role="status"><?= e($status) ?></p>
    <?php endif; ?>
    <form method="post" action="/">
      <input type="hidden" name="csrfToken" value="<?= e($csrfToken) ?>">
      <input type="hidden" name="action" value="<?= $editing ? 'update' : 'book' ?>">
      <input type="hidden" name="id" value="<?= e($values['id']) ?>">
      <label>Cliente
        <input name="clientName" value="<?= e($values['clientName']) ?>" required maxlength="120" autocomplete="name"<?= $error !== null ? ' aria-describedby="form-error"' : '' ?>>
      </label>
      <label>Servicio
        <input name="serviceName" value="<?= e($values['serviceName']) ?>" required maxlength="120"<?= $error !== null ? ' aria-describedby="form-error"' : '' ?>>
      </label>
      <label>Inicio
        <input type="datetime-local" name="startsAt" value="<?= e($values['startsAt']) ?>" required<?= $error !== null ? ' aria-describedby="form-error"' : '' ?>>
      </label>
      <label>Duración
        <select name="durationMinutes" required<?= $error !== null ? ' aria-describedby="form-error"' : '' ?>>
          <?php foreach ([30, 45, 60, 90, 120] as $minutes): ?>
            <option value="<?= $minutes ?>"<?= $values['durationMinutes'] === (string) $minutes ? ' selected' : '' ?>><?= $minutes ?> minutos</option>
          <?php endforeach; ?>
        </select>
      </label>
      <div class="full actions">
        <button type="submit"><?= $editing ? 'Guardar cambios' : 'Registrar cita' ?></button>
        <?php if ($editing): ?><a class="button-link secondary" href="/">Cancelar edición</a><?php endif; ?>
      </div>
    </form>
  </section>

  <section aria-labelledby="schedule-title">
    <h2 id="schedule-title">Consultar agenda</h2>
    <form method="get" action="/">
      <label>Fecha
        <input type="date" name="date" value="<?= e($filterDate) ?>">
      </label>
      <label>Servicio contiene
        <input name="service" value="<?= e($filterService) ?>" maxlength="120">
      </label>
      <div class="full actions">
        <button type="submit">Aplicar filtros</button>
        <a class="button-link secondary" href="/">Limpiar filtros</a>
        <?php if ($error === null): ?><a class="button-link secondary" href="/?<?= e($exportQuery) ?>">Descargar CSV</a><?php endif; ?>
      </div>
    </form>
    <p class="summary" aria-live="polite"><span><strong><?= count($appointments) ?></strong> citas visibles</span><span><strong><?= $bookedMinutes ?></strong> minutos reservados</span></p>

    <?php if ($appointments === []): ?>
      <p class="empty">No hay citas que coincidan con la consulta actual.</p>
    <?php else: ?>
      <table>
        <thead><tr><th>Inicio</th><th>Cliente</th><th>Servicio</th><th>Duración</th><th>Acciones</th></tr></thead>
        <tbody>
        <?php foreach ($appointments as $appointment): ?>
          <tr>
            <td data-label="Inicio"><?= e($appointment->startsAt->setTimezone($timeZone)->format('Y-m-d H:i')) ?></td>
            <td data-label="Cliente"><?= e($appointment->clientName) ?></td>
            <td data-label="Servicio"><?= e($appointment->serviceName) ?></td>
            <td data-label="Duración"><?= $appointment->durationMinutes ?> min</td>
            <td data-label="Acciones"><div class="actions">
              <a class="button-link secondary" href="/?edit=<?= urlencode($appointment->id) ?>">Editar</a>
              <form class="inline" method="post" action="/">
                <input type="hidden" name="csrfToken" value="<?= e($csrfToken) ?>">
                <input type="hidden" name="action" value="cancel">
                <input type="hidden" name="id" value="<?= e($appointment->id) ?>">
                <button class="danger" type="submit">Cancelar cita</button>
              </form>
            </div></td>
          </tr>
        <?php endforeach; ?>
        </tbody>
      </table>
    <?php endif; ?>
  </section>
</main>
</body>
</html>
