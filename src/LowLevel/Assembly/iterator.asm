global _start

section .data
values: db 10, 20, 30
values_end:
success: db "iterator=10,20,30", 10
success_len equ $ - success

section .text
_start:
    mov rsi, values
    xor eax, eax
    xor ecx, ecx

.next:
    cmp rsi, values_end
    je .verify
    movzx edx, byte [rsi]
    add eax, edx
    inc ecx
    inc rsi
    jmp .next

.verify:
    cmp eax, 60
    jne .fail
    cmp ecx, 3
    jne .fail

    mov eax, 1
    mov edi, 1
    mov rsi, success
    mov edx, success_len
    syscall
    xor edi, edi
    mov eax, 60
    syscall

.fail:
    mov edi, 1
    mov eax, 60
    syscall
