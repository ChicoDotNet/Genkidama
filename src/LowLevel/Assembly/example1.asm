section .rodata
    dark_button_msg db "Dark Button", 10
    dark_button_len equ $ - dark_button_msg
    dark_checkbox_msg db "Dark Checkbox", 10
    dark_checkbox_len equ $ - dark_checkbox_msg
    light_button_msg db "Light Button", 10
    light_button_len equ $ - light_button_msg
    light_checkbox_msg db "Light Checkbox", 10
    light_checkbox_len equ $ - light_checkbox_msg

    ; Each factory is one table containing both related product constructors.
    ; Selecting the table once prevents mixing dark and light products.
    dark_factory dq create_dark_button, create_dark_checkbox
    light_factory dq create_light_button, create_light_checkbox

section .text
    global _start

_start:
    lea rbx, [rel dark_factory]
    call create_family

    lea rbx, [rel light_factory]
    call create_family

    mov rax, 60
    xor rdi, rdi
    syscall

create_family:
    push rbx
    call [rbx]
    pop rbx

    push rbx
    call [rbx + 8]
    pop rbx
    ret

create_dark_button:
    lea rsi, [rel dark_button_msg]
    mov rdx, dark_button_len
    jmp print

create_dark_checkbox:
    lea rsi, [rel dark_checkbox_msg]
    mov rdx, dark_checkbox_len
    jmp print

create_light_button:
    lea rsi, [rel light_button_msg]
    mov rdx, light_button_len
    jmp print

create_light_checkbox:
    lea rsi, [rel light_checkbox_msg]
    mov rdx, light_checkbox_len
    jmp print

print:
    mov rax, 1
    mov rdi, 1
    syscall
    ret
