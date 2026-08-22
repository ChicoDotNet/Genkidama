global _start

section .bss
registry_count resq 1

section .data
same_true db "same=true", 10
same_true_len equ $ - same_true
count_one db "count=1", 10
count_one_len equ $ - count_one

section .text
instance:
    lea rax, [rel registry_count]
    ret

_start:
    call instance
    mov r12, rax
    call instance
    mov r13, rax

    inc qword [r12]
    cmp r12, r13
    jne .fail

    mov eax, 1
    mov edi, 1
    lea rsi, [rel same_true]
    mov edx, same_true_len
    syscall

    cmp qword [r13], 1
    jne .fail

    mov eax, 1
    mov edi, 1
    lea rsi, [rel count_one]
    mov edx, count_one_len
    syscall

    xor edi, edi
    mov eax, 60
    syscall

.fail:
    mov edi, 1
    mov eax, 60
    syscall
