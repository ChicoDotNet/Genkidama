section .data
    red_id      dq 0
    blue_id     dq 0
    style_count dq 0
    output_text db "styles=2;shared=true;text=ABC", 10
    output_len  equ $ - output_text

section .text
    global _start

get_red:
    mov rax, [red_id]
    test rax, rax
    jnz .done
    inc qword [style_count]
    mov rax, [style_count]
    mov [red_id], rax
.done:
    ret

get_blue:
    mov rax, [blue_id]
    test rax, rax
    jnz .done
    inc qword [style_count]
    mov rax, [style_count]
    mov [blue_id], rax
.done:
    ret

_start:
    call get_red
    mov r12, rax
    call get_red
    cmp r12, rax
    jne failure
    call get_blue
    cmp r12, rax
    je failure
    cmp qword [style_count], 2
    jne failure

    mov rax, 1
    mov rdi, 1
    mov rsi, output_text
    mov rdx, output_len
    syscall
    xor rdi, rdi
    mov rax, 60
    syscall

failure:
    mov rdi, 1
    mov rax, 60
    syscall
