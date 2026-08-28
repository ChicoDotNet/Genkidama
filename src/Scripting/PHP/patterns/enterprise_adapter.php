<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('enterprise_adapter');
$legacy = static fn(): array => ['customer_id'=>7,'full_name'=>'Ada'];
$adapter = static fn(array $row): array => ['id'=>$row['customer_id'],'name'=>$row['full_name']];
$check($adapter($legacy()) === ['id'=>7,'name'=>'Ada']);
