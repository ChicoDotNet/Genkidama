section .rodata
    locked_msg db "locked", 10
    locked_len equ $ - locked_msg
    unlocked_msg db "unlocked", 10
    unlocked_len equ $ - unlocked_msg
    invalid_msg db "invalid", 10
    invalid_len equ $ - invalid_msg
    passed_msg db "assembly-state: passed", 10
    passed_len equ $ - passed_msg

section .text
    global _start

; State values: 0 = locked, 1 = unlocked.
; Event values: 0 = coin, 1 = push.
; transition keeps the current state for invalid/no-op events and returns -1
; when the current state itself is unknown.
transition:
    cmp r12, 0
    je .locked
    cmp r12, 1
    je .unlocked
    mov r12, -1
    ret

.locked:
    cmp r13, 0
    jne .done
    mov r12, 1
    ret

.unlocked:
    cmp r13, 1
    jne .done
    mov r12, 0

.done:
    ret

print_state:
    cmp r12, 0
    je .print_locked
    cmp r12, 1
    je .print_unlocked
    lea rsi, [rel invalid_msg]
    mov rdx, invalid_len
    jmp print

.print_locked:
    lea rsi, [rel locked_msg]
    mov rdx, locked_len
    jmp print

.print_unlocked:
    lea rsi, [rel unlocked_msg]
    mov rdx, unlocked_len

print:
    mov rax, 1
    mov rdi, 1
    syscall
    ret

_start:
    xor r12, r12
    call print_state

    mov r13, 1
    call transition
    call print_state

    xor r13, r13
    call transition
    call print_state

    xor r13, r13
    call transition
    call print_state

    mov r13, 1
    call transition
    call print_state

    mov r12, 99
    xor r13, r13
    call transition
    call print_state

    lea rsi, [rel passed_msg]
    mov rdx, passed_len
    call print

    mov rax, 60
    xor rdi, rdi
    syscall
