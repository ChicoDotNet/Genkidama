section .data
    subscribers dq audit_observer, dashboard_observer
    subscriber_count dq 2
    audit_count dq 0
    dashboard_count dq 0
    duplicate_rejected db 0
    second_unsubscribe_rejected db 0

section .rodata
    summary db "audit=2;dashboard=1;duplicate=rejected;second-unsubscribe=rejected", 10
    summary_len equ $ - summary

section .text
    global _start

; rdi = callback address
; rax = 1 when added, 0 when duplicate/full
subscribe:
    xor rcx, rcx
.scan:
    cmp rcx, [subscriber_count]
    jae .append
    cmp [subscribers + rcx * 8], rdi
    je .rejected
    inc rcx
    jmp .scan
.append:
    cmp rcx, 2
    jae .rejected
    mov [subscribers + rcx * 8], rdi
    inc qword [subscriber_count]
    mov rax, 1
    ret
.rejected:
    xor rax, rax
    ret

; rdi = callback address
; rax = 1 when removed, 0 when absent
unsubscribe:
    xor rcx, rcx
.find:
    cmp rcx, [subscriber_count]
    jae .absent
    cmp [subscribers + rcx * 8], rdi
    je .remove
    inc rcx
    jmp .find
.remove:
    mov rdx, [subscriber_count]
    dec rdx
.shift:
    cmp rcx, rdx
    jae .finish
    mov r8, [subscribers + rcx * 8 + 8]
    mov [subscribers + rcx * 8], r8
    inc rcx
    jmp .shift
.finish:
    mov qword [subscribers + rdx * 8], 0
    mov [subscriber_count], rdx
    mov rax, 1
    ret
.absent:
    xor rax, rax
    ret

publish:
    xor rbx, rbx
.next:
    cmp rbx, [subscriber_count]
    jae .done
    mov rax, [subscribers + rbx * 8]
    call rax
    inc rbx
    jmp .next
.done:
    ret

audit_observer:
    inc qword [audit_count]
    ret

dashboard_observer:
    inc qword [dashboard_count]
    ret

_start:
    ; Duplicate subscriptions are rejected rather than duplicating delivery.
    lea rdi, [rel audit_observer]
    call subscribe
    test rax, rax
    jnz .fail
    mov byte [duplicate_rejected], 1

    ; First publication reaches both observers.
    call publish

    ; Dashboard unsubscribes and a repeated unsubscribe is rejected.
    lea rdi, [rel dashboard_observer]
    call unsubscribe
    test rax, rax
    jz .fail
    lea rdi, [rel dashboard_observer]
    call unsubscribe
    test rax, rax
    jnz .fail
    mov byte [second_unsubscribe_rejected], 1

    ; Second publication reaches only audit.
    call publish

    cmp qword [audit_count], 2
    jne .fail
    cmp qword [dashboard_count], 1
    jne .fail
    cmp byte [duplicate_rejected], 1
    jne .fail
    cmp byte [second_unsubscribe_rejected], 1
    jne .fail

    mov rax, 1
    mov rdi, 1
    lea rsi, [rel summary]
    mov rdx, summary_len
    syscall

    mov rax, 60
    xor rdi, rdi
    syscall

.fail:
    mov rax, 60
    mov rdi, 1
    syscall
