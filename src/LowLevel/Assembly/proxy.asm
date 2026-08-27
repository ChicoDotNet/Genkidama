global _start

section .data
    output db 'backend=1;fetches=1;first=doc(42);second=doc(42)', 10
    output_len equ $ - output

section .bss
    backend_created resb 1
    fetch_count resb 1
    cache_ready resb 1

section .text
_start:
    call proxy_get
    call proxy_get

    mov rax, 1
    mov rdi, 1
    mov rsi, output
    mov rdx, output_len
    syscall

    mov rax, 60
    xor rdi, rdi
    syscall

proxy_get:
    cmp byte [cache_ready], 1
    je .done
    mov byte [backend_created], 1
    inc byte [fetch_count]
    mov byte [cache_ready], 1
.done:
    ret
