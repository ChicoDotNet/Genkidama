global _start

section .data
legacy_message db 'legacy=86F', 10
legacy_length equ $ - legacy_message
adapted_message db 'adapted=30C', 10
adapted_length equ $ - adapted_message

section .text

read_fahrenheit:
    mov eax, 86
    ret

read_celsius:
    call read_fahrenheit
    sub eax, 32
    imul eax, eax, 5
    cdq
    mov ecx, 9
    idiv ecx
    ret

write_stdout:
    mov eax, 1
    mov edi, 1
    syscall
    ret

_start:
    call read_fahrenheit
    cmp eax, 86
    jne fail

    call read_celsius
    cmp eax, 30
    jne fail

    mov rsi, legacy_message
    mov edx, legacy_length
    call write_stdout

    mov rsi, adapted_message
    mov edx, adapted_length
    call write_stdout

    mov eax, 60
    xor edi, edi
    syscall

fail:
    mov eax, 60
    mov edi, 1
    syscall
