global _start

section .data
commands:
    db 1, 50
    db 2, 20
command_count equ 2
message db "balance=130;commands=2", 10
message_len equ $ - message

section .text
_start:
    mov eax, 100
    xor ecx, ecx
    lea rsi, [rel commands]

.next_command:
    cmp ecx, command_count
    je .verify
    movzx edx, byte [rsi]
    movzx edi, byte [rsi + 1]
    cmp edx, 1
    je .deposit
    cmp edx, 2
    je .withdraw
    jmp .fail

.deposit:
    add eax, edi
    jmp .advance

.withdraw:
    sub eax, edi

.advance:
    add rsi, 2
    inc ecx
    jmp .next_command

.verify:
    cmp eax, 130
    jne .fail
    mov eax, 1
    mov edi, 1
    lea rsi, [rel message]
    mov edx, message_len
    syscall
    mov eax, 60
    xor edi, edi
    syscall

.fail:
    mov eax, 60
    mov edi, 1
    syscall
