section .data
    dark_button_msg db "Dark Button", 0
    light_button_msg db "Light Button", 0
    dark_checkbox_msg db "Dark Checkbox", 0
    light_checkbox_msg db "Light Checkbox", 0

section .bss

section .text
    global _start

_start:
    ; Example usage
    mov rdi, dark_button_msg
    call print
    mov rdi, light_checkbox_msg
    call print

    ; Exit
    mov rax, 60
    xor rdi, rdi
    syscall

print:
    mov rax, 1
    mov rdi, 1
    mov rsi, rdi
    mov rdx, [rsi-1] ; calculate string length
    syscall
    ret
