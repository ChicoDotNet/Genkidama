<?php
final class TextStyle { public function __construct(public string $font, public int $size, public string $color) {} }
final class StyleFactory {
    private array $styles = [];
    public function get(string $font,int $size,string $color): TextStyle { $key="$font|$size|$color"; return $this->styles[$key] ??= new TextStyle($font,$size,$color); }
    public function count(): int { return count($this->styles); }
}
$f=new StyleFactory(); $r1=$f->get('Inter',12,'red'); $r2=$f->get('Inter',12,'red'); $f->get('Inter',12,'blue');
echo 'styles='.$f->count().';shared='.(($r1===$r2)?'true':'false').';text=ABC'.PHP_EOL;
