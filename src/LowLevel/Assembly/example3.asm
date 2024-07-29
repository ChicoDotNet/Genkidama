section .data
    pdf_report_msg db "Generating PDF report", 0
    html_report_msg db "Generating HTML report", 0

section .bss

section .text
    global _start

_start:
    ; Example usage
    mov rdi, pdf_report_msg
    call print
    mov rdi, html_report_msg
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
