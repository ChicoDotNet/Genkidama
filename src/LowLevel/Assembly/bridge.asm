global _start

section .data
    tv_on db 'TV:on', 0
    tv_mute db 'TV:muted', 0
    radio_on db 'Radio:on', 0
    radio_mute db 'Radio:muted', 0

    tv_device dq tv_on, tv_mute
    radio_device dq radio_on, radio_mute

    basic_tv_prefix db 'basic-tv=', 0
    basic_radio_prefix db 'basic-radio=', 0
    mute_tv_prefix db 'mute-tv=', 0
    mute_radio_prefix db 'mute-radio=', 0
    newline db 10

section .text
basic_activate:
    mov rax, [rdi]
    ret

mute_activate:
    mov rax, [rdi + 8]
    ret

print_z:
    push rdi
    xor rdx, rdx
.count:
    cmp byte [rdi + rdx], 0
    je .write
    inc rdx
    jmp .count
.write:
    mov rsi, rdi
    mov rax, 1
    mov rdi, 1
    syscall
    pop rdi
    ret

print_newline:
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall
    ret

_start:
    mov rdi, basic_tv_prefix
    call print_z
    mov rdi, tv_device
    call basic_activate
    mov rdi, rax
    call print_z
    call print_newline

    mov rdi, basic_radio_prefix
    call print_z
    mov rdi, radio_device
    call basic_activate
    mov rdi, rax
    call print_z
    call print_newline

    mov rdi, mute_tv_prefix
    call print_z
    mov rdi, tv_device
    call mute_activate
    mov rdi, rax
    call print_z
    call print_newline

    mov rdi, mute_radio_prefix
    call print_z
    mov rdi, radio_device
    call mute_activate
    mov rdi, rax
    call print_z
    call print_newline

    mov rax, 60
    xor rdi, rdi
    syscall
