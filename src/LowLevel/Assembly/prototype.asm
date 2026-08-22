; Linux x86-64 / NASM. Prototype is represented by a fixed-layout value record.
; _start clones the 60-byte record, mutates only the clone, prints both, and exits 0.

section .data
    original_profile:
        db "orders", 0
        times 13 db 0
        db "metrics", 0
        times 32 db 0

    clone_name_value db "orders-canary", 0
    clone_features_value db "metrics,tracing", 0
    original_prefix db "original="
    clone_prefix db "clone="
    separator db ": "
    newline db 10

section .bss
    clone_profile resb 60

section .text
    global _start

; write_stdout
; Input: RSI=buffer, RDX=byte count. Clobbers RAX,RDI.
write_stdout:
    mov rax, 1
    mov rdi, 1
    syscall
    ret

_start:
    ; Clone the complete value record before applying canary-specific changes.
    cld
    mov rsi, original_profile
    mov rdi, clone_profile
    mov rcx, 60
    rep movsb

    mov rsi, clone_name_value
    mov rdi, clone_profile
    mov rcx, 14
    rep movsb

    mov rsi, clone_features_value
    mov rdi, clone_profile + 20
    mov rcx, 16
    rep movsb

    mov rsi, original_prefix
    mov rdx, 9
    call write_stdout
    mov rsi, original_profile
    mov rdx, 6
    call write_stdout
    mov rsi, separator
    mov rdx, 2
    call write_stdout
    mov rsi, original_profile + 20
    mov rdx, 7
    call write_stdout
    mov rsi, newline
    mov rdx, 1
    call write_stdout

    mov rsi, clone_prefix
    mov rdx, 6
    call write_stdout
    mov rsi, clone_profile
    mov rdx, 13
    call write_stdout
    mov rsi, separator
    mov rdx, 2
    call write_stdout
    mov rsi, clone_profile + 20
    mov rdx, 15
    call write_stdout
    mov rsi, newline
    mov rdx, 1
    call write_stdout

    mov rax, 60
    xor rdi, rdi
    syscall
