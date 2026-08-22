global _start

section .data
    out db "base=alert",10,"audit=audit(alert)",10,"encrypted=enc(alert)",10,"stacked=audit(enc(alert))",10
    out_len equ $-out

section .text
_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, out
    mov rdx, out_len
    syscall

    mov rax, 60
    xor rdi, rdi
    syscall
