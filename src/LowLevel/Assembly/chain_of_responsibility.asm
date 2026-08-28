section .rodata
    faq_msg db "visited=faq;handled=faq;result=refund(250)", 10
    faq_len equ $ - faq_msg
    billing_msg db "visited=faq>billing;handled=billing;result=refund(250)", 10
    billing_len equ $ - billing_msg
    escalation_msg db "visited=faq>billing>escalation;handled=escalation;result=refund(250)", 10
    escalation_len equ $ - escalation_msg

section .text
    global _start

_start:
    mov r12, 250
    jmp faq_handler

faq_handler:
    cmp r12, 50
    jle handled_faq
    jmp billing_handler

billing_handler:
    cmp r12, 500
    jle handled_billing
    jmp escalation_handler

escalation_handler:
    lea rsi, [rel escalation_msg]
    mov rdx, escalation_len
    jmp print_and_exit

handled_faq:
    lea rsi, [rel faq_msg]
    mov rdx, faq_len
    jmp print_and_exit

handled_billing:
    lea rsi, [rel billing_msg]
    mov rdx, billing_len

print_and_exit:
    mov rax, 1
    mov rdi, 1
    syscall

    mov rax, 60
    xor rdi, rdi
    syscall
