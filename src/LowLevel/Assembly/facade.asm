global _start

section .data
    checkout_prefix db 'checkout='
    checkout_prefix_len equ $-checkout_prefix
    auth_text db 'auth(alice)'
    auth_len equ $-auth_text
    sep db '>'
    sep_len equ $-sep
    reserve_text db 'reserve(SKU-42)'
    reserve_len equ $-reserve_text
    charge_text db 'charge(499)', 10
    charge_len equ $-charge_text

section .text
write_text:
    mov rax, 1
    mov rdi, 1
    syscall
    ret

authenticate:
    mov rsi, auth_text
    mov rdx, auth_len
    call write_text
    ret

reserve_inventory:
    mov rsi, reserve_text
    mov rdx, reserve_len
    call write_text
    ret

charge:
    mov rsi, charge_text
    mov rdx, charge_len
    call write_text
    ret

checkout:
    mov rsi, checkout_prefix
    mov rdx, checkout_prefix_len
    call write_text
    call authenticate
    mov rsi, sep
    mov rdx, sep_len
    call write_text
    call reserve_inventory
    mov rsi, sep
    mov rdx, sep_len
    call write_text
    call charge
    ret

_start:
    call checkout
    mov rax, 60
    xor rdi, rdi
    syscall
