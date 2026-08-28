<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('active_record');
$record = new class(1,'Ada') {
    private static array $db=[];
    public function __construct(public int $id, public string $name) {}
    public function save(): void { self::$db[$this->id] = $this->name; }
    public static function findName(int $id): ?string { return self::$db[$id] ?? null; }
};
$record->save(); $check($record::findName(1) === 'Ada');
