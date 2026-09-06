section .rodata
    success_msg db "Assembly Mediator: passed", 10
    success_len equ $ - success_msg

section .text
    global _start

_start:
    xor r12, r12            ; bit 0 = payment receiver called, bit 1 = inventory receiver called
    xor r13, r13            ; unknown-colleague flag

    mov edi, 1              ; sender: payment
    mov esi, 2              ; recipient: inventory
    call mediator_send

    mov edi, 2              ; sender: inventory
    mov esi, 1              ; recipient: payment
    call mediator_send

    mov edi, 1              ; sender: payment
    mov esi, 3              ; recipient: unknown shipping colleague
    call mediator_send

    cmp r12, 3
    jne fail
    cmp r13, 1
    jne fail

    mov rax, 1
    mov rdi, 1
    lea rsi, [rel success_msg]
    mov rdx, success_len
    syscall

    mov rax, 60
    xor rdi, rdi
    syscall

; Routing authority: colleagues are selected only here.
; edi = sender id, esi = recipient id.
mediator_send:
    cmp esi, 1
    je payment_receive
    cmp esi, 2
    je inventory_receive
    mov r13, 1
    ret

payment_receive:
    or r12, 1
    ret

inventory_receive:
    or r12, 2
    ret

fail:
    mov rax, 60
    mov rdi, 1
    syscall
