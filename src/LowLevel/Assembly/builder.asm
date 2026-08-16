; Builder pattern — Linux x86-64 / NASM.
; rbx selects the concrete representation once; the same director routine
; invokes title and section steps through a two-entry builder table.
section .rodata
    text_title db "# Service status", 10
    text_title_len equ $ - text_title
    text_section db "## Availability", 10, "99.95%", 10
    text_section_len equ $ - text_section
    html_title db "<h1>Service status</h1>", 10
    html_title_len equ $ - html_title
    html_section db "<h2>Availability</h2><p>99.95%</p>", 10
    html_section_len equ $ - html_section
    separator db "---", 10
    separator_len equ $ - separator

    text_builder dq text_add_title, text_add_section
    html_builder dq html_add_title, html_add_section

section .text
    global _start

_start:
    lea rbx, [rel text_builder]
    call build_availability_report
    lea rsi, [rel separator]
    mov rdx, separator_len
    call print
    lea rbx, [rel html_builder]
    call build_availability_report
    mov rax, 60
    xor rdi, rdi
    syscall

build_availability_report:
    push rbx
    call [rbx]
    pop rbx
    push rbx
    call [rbx + 8]
    pop rbx
    ret

text_add_title:
    lea rsi, [rel text_title]
    mov rdx, text_title_len
    jmp print
text_add_section:
    lea rsi, [rel text_section]
    mov rdx, text_section_len
    jmp print
html_add_title:
    lea rsi, [rel html_title]
    mov rdx, html_title_len
    jmp print
html_add_section:
    lea rsi, [rel html_section]
    mov rdx, html_section_len
    jmp print

print:
    mov rax, 1
    mov rdi, 1
    syscall
    ret
