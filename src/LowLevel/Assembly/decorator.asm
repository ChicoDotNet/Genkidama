global _start

section .data
    output_text db "base=alert",10,"audit=audit(alert)",10,"encrypted=enc(alert)",10,"stacked=audit(enc(alert))",10
    output_len equ $-output_text

section .text
_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, output_text
    mov rdx, output_len
    syscall

    mov rax, 60
    xor rdi, rdi
    syscall
