<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('data_mapper');
$mapper = new class {
    public function fromRow(array $row): object { return (object)['id'=>$row['id'],'name'=>$row['name']]; }
    public function toRow(object $entity): array { return ['id'=>$entity->id,'name'=>$entity->name]; }
};
$entity = $mapper->fromRow(['id'=>1,'name'=>'Ada']);
$check($mapper->toRow($entity) === ['id'=>1,'name'=>'Ada']);
