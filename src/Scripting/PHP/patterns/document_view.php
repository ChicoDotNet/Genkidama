<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('document_view');
$document = ['title'=>'Plan','body'=>'Ship'];
$summaryView = static fn(array $d): string => $d['title'];
$fullView = static fn(array $d): string => $d['title'].':'.$d['body'];
$check($summaryView($document) === 'Plan' && $fullView($document) === 'Plan:Ship');
